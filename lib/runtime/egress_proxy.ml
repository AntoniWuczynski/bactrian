open Bactrian_core
open Proxy_reply

type tls = {
  terminate : host:string -> Upstream.flow -> (Upstream.flow, string) result;
  failure : exn -> string option;
}

type interception = { tls : sim_now:string -> tls; bundle : string }

type run = {
  policy : Egress.t;
  tls : tls;
  shared : Mcp_proxy.shared;
  call :
    sw:Eio.Switch.t ->
    Upstream.connector ->
    Upstream.request ->
    on_head:(Upstream.head -> unit) ->
    on_chunk:(dt_ms:int -> string -> unit) ->
    (unit, Upstream.error) result;
  max_body : int;
  logged : Trace_builder.http_call -> unit;
  ended : unit -> bool;
}

exception Tunnel_closed

(* The agent's connection after the CONNECT head, as a flow for TLS. Bytes it
   sent after the head are already in [ic], so reading goes through it. *)
module Tunnel = struct
  type t = { ic : Eio.Buf_read.t; oc : Eio.Buf_write.t }

  let read_methods = []

  let single_read t buf =
    Eio.Buf_read.ensure t.ic 1;
    let available = Eio.Buf_read.peek t.ic in
    let n = Int.min (Cstruct.length buf) (Cstruct.length available) in
    Cstruct.blit available 0 buf 0 n;
    Eio.Buf_read.consume t.ic n;
    n

  let single_write t bufs =
    List.iter (Eio.Buf_write.cstruct t.oc) bufs;
    Eio.Buf_write.flush t.oc;
    Cstruct.lenv bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  (* The listener owns the socket and closes it when the tunnel ends. *)
  let shutdown _ _ = ()
  let close _ = ()

  let handler =
    Eio.Resource.handler
      (H (Eio.Resource.Close, close)
      :: Eio.Resource.bindings
           (Eio.Flow.Pi.two_way
              (module struct
                type nonrec t = t

                let read_methods = read_methods
                let single_read = single_read
                let single_write = single_write
                let copy = copy
                let shutdown = shutdown
              end)))

  let flow ic oc : Upstream.flow = Eio.Resource.T ({ ic; oc }, handler)
end

(* What a request on a tunnel gets, fixed by the host's policy at CONNECT. *)
type act = Forward of { record : bool } | From_recording | No_recording
type body = Body of string | Too_large

let read_body ~max_body body =
  match
    Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:max_body body)
  with
  | s -> Body s
  | exception Eio.Buf_read.Buffer_limit_exceeded -> Too_large

let header name headers =
  List.find_map
    (fun (k, v) ->
      if String.equal (String.lowercase_ascii k) name then Some v else None)
    headers

let dropped_headers =
  [
    "connection";
    "keep-alive";
    "proxy-authorization";
    "proxy-connection";
    "te";
    "trailer";
    "upgrade";
  ]

let forwarded headers =
  List.filter
    (fun (k, _) -> not (List.mem (String.lowercase_ascii k) dropped_headers))
    headers

(* One request on a tunnel. *)
type pending = {
  meth : string;
  path : string;  (** Redacted, as it is stored and logged. *)
  raw_path : string;  (** As sent, for the upstream call. *)
  raw_query : string option;
  headers : (string * string) list;
  body : string;
  scrubbed : string;
  request : Matcher.request;
  query : (string * string) list;
  occurrence : int;
  t_ms : int;
}

let exchange r x outcome : Cassette.exchange =
  {
    seq = 0;
    t_ms = x.t_ms;
    channel = Http;
    protocol = "http";
    meth = x.meth;
    path = x.path;
    query = x.query;
    key = x.request.key;
    occurrence = x.occurrence;
    canonical = x.request.canonical;
    request = Store.payload r.shared.store x.scrubbed;
    outcome;
  }

(* The URL is built from the CONNECT host and port, never from text the agent
   sent, so that the request goes nowhere else. The path and query, which
   [Egress.target] has checked, are sent as they came: an API that signs the
   query, or reads a literal [+], needs their bytes. *)
let live r ~host ~port ~authority x ~record ~log =
  let url = Uri.make ~scheme:"https" ~host ~port () in
  let target =
    match x.raw_query with Some q -> x.raw_path ^ "?" ^ q | None -> x.raw_path
  in
  let head = ref None and chunks = ref [] in
  let result =
    Eio.Switch.run @@ fun sw ->
    r.call ~sw r.shared.connector
      {
        meth = x.meth;
        url;
        target = Some target;
        headers = forwarded x.headers;
        body = x.body;
      }
      ~on_head:(fun h -> head := Some h)
      ~on_chunk:(fun ~dt_ms:_ c -> chunks := c :: !chunks)
  in
  let text = String.concat "" (List.rev !chunks) in
  let failed e =
    let reason =
      Printf.sprintf "egress to %s: %s" authority (Upstream.describe_error e)
    in
    r.shared.failed reason;
    reason
  in
  let response (h : Upstream.head) ~complete : Cassette.outcome =
    Response
      {
        status = h.status;
        headers = h.headers;
        body =
          Whole
            {
              payload = Store.payload r.shared.store (r.shared.scrub text);
              complete;
            };
      }
  in
  match (result, !head) with
  | Ok (), Some h ->
      r.shared.exchange ~record (exchange r x (response h ~complete:true));
      log h.status;
      respond ~status:h.status ~headers:h.headers (of_list [ text ])
  | Error e, Some h ->
      ignore (failed e : string);
      r.shared.exchange ~record (exchange r x (response h ~complete:false));
      log h.status;
      let next = of_list [ text ] in
      respond ~status:h.status ~headers:h.headers (fun () ->
          match next () with
          | Finished -> Dropped
          | (Chunk _ | Dropped) as item -> item)
  | Error e, None ->
      let reason = failed e in
      r.shared.exchange ~record (exchange r x (Transport_error reason));
      log 502;
      error_response ~status:502 ~type_:"bactrian_upstream_error" reason
  | Ok (), None ->
      let reason = failed (Failed_after_send "no response head") in
      log 502;
      error_response ~status:502 ~type_:"bactrian_upstream_error" reason

let miss r ~log d =
  log 409;
  r.shared.note (Replay_miss.describe d);
  error_response ~status:409 ~type_:"bactrian_replay_miss"
    ~extra:[ ("divergence", Replay_miss.json d) ]
    (Replay_miss.describe d)

let recorded r ~log (e : Cassette.exchange) =
  match e.outcome with
  | Transport_error reason ->
      log 502;
      error_response ~status:502 ~type_:"bactrian_upstream_error" reason
  | Response { status; headers; body } -> (
      let text, complete =
        match body with
        | Whole { payload; complete } ->
            (Store.read_payload r.shared.store payload, complete)
        | Stream { chunks; complete } ->
            (Ok (String.concat "" (List.map snd chunks)), complete)
      in
      match text with
      | Error err ->
          log 500;
          error_response ~status:500 ~type_:"bactrian_internal_error"
            (Format.asprintf "%a" Store.pp_error err)
      | Ok text ->
          log status;
          let next = of_list [ text ] in
          respond ~status ~headers (fun () ->
              match next () with
              | Finished when not complete -> Dropped
              | (Chunk _ | Finished | Dropped) as item -> item))

let normalised_query raw =
  Uri.query_of_encoded (Option.value raw ~default:"")
  |> List.concat_map (fun (k, vs) ->
      match vs with
      | [] -> [ (k, "") ]
      | _ :: _ -> List.map (fun v -> (k, v)) vs)
  |> List.sort (fun (k1, v1) (k2, v2) ->
      match String.compare k1 k2 with 0 -> String.compare v1 v2 | n -> n)

let answer r ~host ~port ~authority act request body : response =
  let meth = Http.Method.to_string (Http.Request.meth request) in
  let resource = Http.Request.resource request in
  let target = Egress.target ~host ~port resource in
  let raw_path = match target with Ok (p, _) -> p | Error _ -> resource in
  let path = r.shared.scrub raw_path in
  let headers = Http.Header.to_list (Http.Request.headers request) in
  let t_ms = r.shared.t_ms () in
  let log status = r.logged { t_ms; meth; host; path; status } in
  match (read_body ~max_body:r.max_body body, target) with
  | Too_large, _ ->
      log 413;
      r.shared.note
        (Printf.sprintf "egress to %s: request body too large" authority);
      error_response ~status:413 ~type_:"bactrian_payload_too_large"
        (Printf.sprintf "the request body is over %d bytes" r.max_body)
  | Body _, _ when Option.is_some (header "upgrade" headers) ->
      log 501;
      r.shared.note
        (Printf.sprintf "egress to %s: an upgrade (WebSocket) was refused"
           authority);
      error_response ~status:501 ~type_:"bactrian_unsupported"
        "bactrian does not intercept WebSocket or other upgraded connections"
  | Body _, Error reason ->
      log 400;
      r.shared.note (Printf.sprintf "egress to %s: %s" authority reason);
      error_response ~status:400 ~type_:"bactrian_bad_request" reason
  | Body body, Ok (_, query) -> (
      let raw_query = match query with Some "" -> None | q -> q in
      let scrubbed = r.shared.scrub body in
      let query =
        List.map
          (fun (k, v) -> (k, r.shared.scrub v))
          (normalised_query raw_query)
      in
      let canonical =
        r.shared.mask
          (Egress.canonical ~authority ~meth ~path ~query
             ~content_type:(header "content-type" headers)
             scrubbed)
      in
      let key = Matcher.key ~meth ~path ~query canonical in
      let x =
        {
          meth;
          path;
          raw_path;
          raw_query;
          headers;
          body;
          scrubbed;
          request = { channel = Http; key; canonical; shape = Whole_body };
          query;
          occurrence = r.shared.occurrence key;
          t_ms;
        }
      in
      match (act, r.shared.serving) with
      | Forward { record }, _ -> live r ~host ~port ~authority x ~record ~log
      | From_recording, Replaying { decide } -> (
          match decide ~t_ms x.request with
          | Serve { exchange = e; _ } -> recorded r ~log e
          | Refuse d -> miss r ~log d
          | Go_live -> live r ~host ~port ~authority x ~record:true ~log)
      | No_recording, _ | From_recording, Live _ ->
          miss r ~log (Missing_request { request = x.request; nearest = None }))

(* Neither the answer to a HEAD nor a 204 or 304 has a body, so writing one
   would be read as the start of the next response. *)
let handle r ~host ~port ~authority act request body =
  if r.ended () then (
    let head, write =
      error_response ~status:403 ~type_:"bactrian_forbidden"
        ~headers:[ ("connection", "close") ]
        "the run has ended"
    in
    ( head,
      fun ic oc ->
        write ic oc;
        Eio.Buf_write.flush oc;
        raise Tunnel_closed ))
  else
    let reply = answer r ~host ~port ~authority act request body in
    let head, _ = reply in
    if
      String.equal (Http.Method.to_string (Http.Request.meth request)) "HEAD"
      || not (Http.Status.body_allowed (Http.Response.status head))
    then bodiless reply
    else reply

(* The request line and headers must fit in the reader's buffer. Bodies stream
   through it, and [max_body] caps them. *)
let max_head = 1_048_576

let serve r ~conn ~host ~port ~authority act tls =
  let server =
    Cohttp_eio.Server.make_expert
      ~callback:(fun _conn request body ->
        handle r ~host ~port ~authority act request body)
      ()
  in
  try
    Eio.Buf_write.with_flow tls (fun oc ->
        Cohttp_eio.Server.callback server conn
          (Eio.Buf_read.of_flow ~max_size:max_head tls)
          oc)
  with
  | End_of_file -> ()
  | Eio.Buf_read.Buffer_limit_exceeded ->
      r.shared.note
        (Printf.sprintf
           "egress to %s: a request line or headers over %d bytes closed the \
            tunnel"
           authority max_head)
  | ex when Option.is_some (r.tls.failure ex) ->
      r.shared.note
        (Printf.sprintf "egress to %s: %s" authority
           (Option.value (r.tls.failure ex) ~default:""))

let connect r ~conn ~target =
  match Egress.authority target with
  | Error reason ->
      r.shared.note ("egress: " ^ reason);
      error_response ~status:400 ~type_:"bactrian_bad_request" reason
  | Ok (host, port) -> (
      let authority = Egress.host_port host port in
      let act =
        match (Egress.policy r.policy ~host ~port, r.shared.serving) with
        | Block, _ -> None
        | Passthrough, _ -> Some (Forward { record = false })
        | Record, Live { record } -> Some (Forward { record })
        | (Record | Replay), Replaying _ -> Some From_recording
        | Replay, Live _ -> Some No_recording
      in
      match act with
      | None ->
          r.logged
            {
              t_ms = r.shared.t_ms ();
              meth = "CONNECT";
              host;
              path = "";
              status = 403;
            };
          let message =
            Printf.sprintf
              "egress to %s is blocked: list the host under egress.hosts (or \
               record --egress) to allow it"
              authority
          in
          r.shared.note message;
          error_response ~status:403 ~type_:"bactrian_egress_blocked" message
      | Some act ->
          ( Http.Response.make ~status:`OK (),
            fun ic oc ->
              Eio.Buf_write.flush oc;
              (match r.tls.terminate ~host (Tunnel.flow ic oc) with
              | Error reason ->
                  r.shared.note
                    (Printf.sprintf "egress to %s: %s" authority reason)
              | Ok tls -> serve r ~conn ~host ~port ~authority act tls);
              raise Tunnel_closed ))
