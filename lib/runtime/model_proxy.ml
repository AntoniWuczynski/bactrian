open Bactrian_core

type provider = Openai | Anthropic | Gemini | Openrouter

let providers = [ Openai; Anthropic; Gemini; Openrouter ]

let default_upstream p =
  Uri.of_string
    (match p with
    | Openai -> "https://api.openai.com"
    | Anthropic -> "https://api.anthropic.com"
    | Gemini -> "https://generativelanguage.googleapis.com"
    | Openrouter -> "https://openrouter.ai")

let segment = function
  | Openai -> "openai"
  | Anthropic -> "anthropic"
  | Gemini -> "gemini"
  | Openrouter -> "openrouter"

let provider_of_segment s =
  List.find_opt (fun p -> String.equal (segment p) s) providers

let provider_equal a b = String.equal (segment a) (segment b)

(* The part of the path that SDK base URLs include. *)
let sdk_base = function
  | Openai -> "/v1"
  | Openrouter -> "/api/v1"
  | Anthropic | Gemini -> ""

let key_header = function
  | Openai | Openrouter -> "authorization"
  | Anthropic -> "x-api-key"
  | Gemini -> "x-goog-api-key"

let key_value p key =
  match p with
  | Openai | Openrouter -> "Bearer " ^ key
  | Anthropic | Gemini -> key

type timing = Fast | Recorded

type mode =
  | Record
  | Replay of {
      recording : Cassette.t;
      matcher : Matcher.mode;
      allow_reorder : bool;
      timing : timing;
    }
  | Passthrough

type run = {
  run_id : string;
  token : string;
  mode : mode;
  store : Store.t;
  cassette : Store.writer option;
  ledger : Ledger.t;
  keys : (provider * string) list;
  upstream : provider -> Uri.t;
  connector : Upstream.connector;
  run_started : float;
}

type findings = {
  divergences : Matcher.divergence list;
  exchanges : int;
  transport_errors : int;
  usage_gaps : int;
  refused_keys : int;
  cap_refusals : Ledger.refusal list;
}

module Counts = Map.Make (String)
module Runs = Hashtbl.Make (String)

type replaying = { mutable matcher : Matcher.state; timing : timing }
type serving = Live of { record : bool } | Replaying of replaying

(* Everything mutable about a run. Guarded by [lock]. *)
type state = {
  run : run;
  serving : serving;
  lock : Eio.Mutex.t;
  mutable redact : Redact.state;
  mutable seen : int Counts.t;  (** Requests so far per key. *)
  mutable next_seq : int;
  mutable divergences : Matcher.divergence list;  (** Newest first. *)
  mutable exchanges : int;
  mutable transport_errors : int;
  mutable usage_gaps : int;
  mutable refused_keys : int;
  mutable cap_refusals : Ledger.refusal list;  (** Newest first. *)
}

type t = {
  port : int;
  runs : state Runs.t;
  now : unit -> float;
  sleep : float -> unit;
  call :
    sw:Eio.Switch.t ->
    Upstream.connector ->
    Upstream.request ->
    on_head:(Upstream.head -> unit) ->
    on_chunk:(dt_ms:int -> string -> unit) ->
    (unit, Upstream.error) result;
}

let locked st f = Eio.Mutex.use_rw ~protect:true st.lock f

(* Ten minutes covers the slowest long generation. *)
let upstream_timeout = 600.

(* Responses. Bodies are always written chunk by chunk by hand so that each
   upstream piece reaches the agent as its own flushed HTTP chunk, and so that
   a dropped upstream can drop the agent's connection before the terminal
   chunk. *)

type item = Chunk of string | Finished | Dropped

exception Upstream_dropped

type response = Http.Response.t * (Eio.Buf_read.t -> Eio.Buf_write.t -> unit)

let respond ~status ~headers (next : unit -> item) : response =
  let headers =
    Http.Header.add (Http.Header.of_list headers) "transfer-encoding" "chunked"
  in
  ( Http.Response.make ~status:(Http.Status.of_int status) ~headers (),
    fun _ic oc ->
      Eio.Buf_write.flush oc;
      let rec loop () =
        match next () with
        | Chunk "" -> loop ()
        | Chunk s ->
            Eio.Buf_write.printf oc "%x\r\n" (String.length s);
            Eio.Buf_write.string oc s;
            Eio.Buf_write.string oc "\r\n";
            Eio.Buf_write.flush oc;
            loop ()
        | Finished ->
            Eio.Buf_write.string oc "0\r\n\r\n";
            Eio.Buf_write.flush oc
        | Dropped -> raise Upstream_dropped
      in
      loop () )

let of_list pieces =
  let rest = ref pieces in
  fun () ->
    match !rest with
    | [] -> Finished
    | x :: tl ->
        rest := tl;
        Chunk x

let obj fields =
  match Canon.obj fields with
  | Ok v -> v
  | Error _ -> assert false (* every caller passes distinct literal names *)

let error_response ?(extra = []) ~status ~type_ message =
  let error =
    obj
      (("type", Canon.string type_)
      :: ("message", Canon.string message)
      :: extra)
  in
  respond ~status
    ~headers:[ ("content-type", "application/json") ]
    (of_list [ Canon.to_string (obj [ ("error", error) ]) ])

let divergence_json : Matcher.divergence -> Canon.t = function
  | Missing_request { nearest = None; _ } ->
      obj [ ("kind", Canon.string "missing_request") ]
  | Missing_request { nearest = Some (e, changes, similarity); _ } ->
      obj
        ([
           ("kind", Canon.string "missing_request");
           ("nearest_seq", Canon.int e.seq);
           ( "diff",
             Canon.list
               (List.map (fun (c : Diff.change) -> Canon.string c.path) changes)
           );
         ]
        @
        match Canon.float similarity with
        | Ok v -> [ ("similarity", v) ]
        | Error _ -> [])
  | Reordered { expected; _ } ->
      obj
        [
          ("kind", Canon.string "reordered");
          ("expected_seq", Canon.int expected.seq);
        ]
  | Shape_mismatch { recorded; _ } ->
      obj
        [
          ("kind", Canon.string "shape_mismatch");
          ("recorded_seq", Canon.int recorded.seq);
        ]
  | Unconsumed e ->
      obj [ ("kind", Canon.string "unconsumed"); ("seq", Canon.int e.seq) ]

let describe_divergence : Matcher.divergence -> string = function
  | Missing_request { nearest = None; _ } ->
      "the request is not in the recording"
  | Missing_request { nearest = Some (e, _, sim); _ } ->
      Printf.sprintf
        "the request is not in the recording (nearest: seq %d, similarity %.3f)"
        e.seq sim
  | Reordered { expected; _ } ->
      Printf.sprintf "the request arrived out of order (expected seq %d)"
        expected.seq
  | Shape_mismatch { recorded; _ } ->
      Printf.sprintf "streaming differs from the recording (seq %d)"
        recorded.seq
  | Unconsumed e -> Printf.sprintf "seq %d was never requested" e.seq

let describe_upstream_error : Upstream.error -> string = function
  | Connect_failed reason -> "upstream unreachable: " ^ reason
  | Timed_out -> "upstream timed out"
  | Truncated { bytes; reason } ->
      Printf.sprintf "upstream body cut off after %d bytes: %s" bytes reason

(* Request analysis *)

(* One model request on its way through the proxy. *)
type pending = {
  provider : provider;
  adapter : (module Protocol.S);
  meth : string;
  path : string;  (** Relative to the provider base. *)
  raw_query : string option;
  query : (string * string) list;  (** Normalised. *)
  headers : (string * string) list;  (** As the agent sent them. *)
  body : string;  (** As the agent sent it. *)
  scrubbed : string;
  parsed : Canon.t;  (** [scrubbed], parsed. *)
  request : Matcher.request;
  occurrence : int;
  t_ms : int;
}

let pair_compare (n1, v1) (n2, v2) =
  match String.compare n1 n2 with 0 -> String.compare v1 v2 | n -> n

let normalised_query uri =
  Uri.query uri
  |> List.concat_map (fun (k, vs) ->
      match vs with
      | [] -> [ (k, "") ]
      | _ :: _ -> List.map (fun v -> (k, v)) vs)
  |> List.filter (fun (k, _) -> not (String.equal k "key"))
  |> List.sort pair_compare

let known_secrets run = run.token :: List.map snd run.keys

let scrub st text =
  let redact, text =
    Redact.scrub ~patterns:Redact.provider_key_patterns
      ~known:(known_secrets st.run) st.redact text
  in
  st.redact <- redact;
  text

let max_output_paths =
  [
    [ "max_tokens" ];
    [ "max_completion_tokens" ];
    [ "max_output_tokens" ];
    [ "generationConfig"; "maxOutputTokens" ];
    [ "generation_config"; "max_output_tokens" ];
  ]

let rec int_at v = function
  | [] -> Canon.to_int_opt v
  | k :: rest -> Option.bind (Canon.member k v) (fun c -> int_at c rest)

let max_output body = List.find_map (int_at body) max_output_paths

let date_of time =
  let tm = Unix.gmtime time in
  Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

let header_value name headers =
  List.find_map
    (fun (k, v) ->
      if String.equal (String.lowercase_ascii k) name then Some v else None)
    headers

(* Headers never forwarded: hop-by-hop, framing, cookies and every key header
   (the real key is added for the provider). *)
let dropped_headers =
  [
    "host";
    "content-length";
    "transfer-encoding";
    "connection";
    "keep-alive";
    "te";
    "upgrade";
    "accept-encoding";
    "proxy-authorization";
    "proxy-connection";
    "cookie";
    "authorization";
    "x-api-key";
    "x-goog-api-key";
  ]

let forward_headers headers p real_key =
  (key_header p, key_value p real_key)
  :: List.filter
       (fun (k, _) -> not (List.mem (String.lowercase_ascii k) dropped_headers))
       headers

let upstream_url run x =
  let base = Uri.to_string (run.upstream x.provider) in
  let base =
    if String.ends_with ~suffix:"/" base then
      String.sub base 0 (String.length base - 1)
    else base
  in
  Uri.of_string
    (base ^ x.path ^ match x.raw_query with Some q -> "?" ^ q | None -> "")

(* Response bodies for summaries *)

let is_sse headers =
  match header_value "content-type" headers with
  | Some ct -> String.starts_with ~prefix:"text/event-stream" ct
  | None -> false

let response_body x (head : Upstream.head) chunks : Protocol.body option =
  if is_sse head.headers then
    let st, rev =
      List.fold_left
        (fun (st, acc) piece ->
          let st, items = Sse.feed st piece in
          (st, List.rev_append items acc))
        (Sse.init, []) chunks
    in
    match Sse.finish st with
    | `Clean -> Some (Sse (List.rev rev))
    | `Truncated _ -> None
  else
    match Canon.of_string (String.concat "" chunks) with
    | Error _ -> None
    | Ok v -> (
        match (x.request.shape, Canon.to_list_opt v) with
        | Streamed, Some items -> Some (Json_stream items)
        | Streamed, None | Whole_body, (Some _ | None) -> Some (Json v))

(* The actual cost of a finished call, or [None] for a usage gap. Providers do
   not bill requests they reject, so an error status costs nothing. *)
let actual_cost x ~model ~date (head : Upstream.head) chunks =
  if head.status >= 400 then Some 0.
  else
    let (module A : Protocol.S) = x.adapter in
    match Option.map A.summarise (response_body x head chunks) with
    | Some (Ok { usage = Some u; _ }) -> Pricing.cost ~model ~date u
    | Some (Ok { usage = None; _ }) | Some (Error _) | None -> None

(* Live calls *)

let record_live st x ~head ~chunks result =
  let outcome : Cassette.outcome =
    match (head, result) with
    | None, Error e -> Transport_error (describe_upstream_error e)
    | None, Ok () -> Transport_error "no response head"
    | Some (h : Upstream.head), (Ok () | Error _) ->
        let body : Cassette.response_body =
          match x.request.shape with
          | Streamed ->
              Stream (List.map (fun (dt, piece) -> (dt, scrub st piece)) chunks)
          | Whole_body ->
              Whole
                (Store.payload st.run.store
                   (scrub st (String.concat "" (List.map snd chunks))))
        in
        Response { status = h.status; headers = h.headers; body }
  in
  let (module A : Protocol.S) = x.adapter in
  {
    Cassette.seq = st.next_seq;
    t_ms = x.t_ms;
    channel = Model;
    protocol = A.name;
    meth = x.meth;
    path = x.path;
    query = x.query;
    key = x.request.key;
    occurrence = x.occurrence;
    canonical = x.request.canonical;
    request = Store.payload st.run.store x.scrubbed;
    outcome;
  }

(* Settle the ledger and record the exchange. Runs before the agent sees the
   end of the response, so findings are complete when the agent's call returns.
*)
let finish_live st x ~record ~model ~date ~reservation ~head ~chunks result =
  let actual =
    match (result, head) with
    | Error (Upstream.Connect_failed _), _ -> Some 0.
    | Error (Timed_out | Truncated _), _ | Ok (), None -> None
    | Ok (), Some h -> actual_cost x ~model ~date h (List.map snd chunks)
  in
  Ledger.settle st.run.ledger reservation ~actual;
  locked st (fun () ->
      if Option.is_none actual then st.usage_gaps <- st.usage_gaps + 1;
      (match result with
      | Error _ -> st.transport_errors <- st.transport_errors + 1
      | Ok () -> ());
      st.exchanges <- st.exchanges + 1;
      match st.run.cassette with
      | Some writer when record ->
          Store.append writer (record_live st x ~head ~chunks result);
          st.next_seq <- st.next_seq + 1
      | Some _ | None -> ())

let refuse_spend st (refusal : Ledger.refusal) =
  locked st (fun () -> st.cap_refusals <- refusal :: st.cap_refusals);
  match refusal with
  | Unknown_model model ->
      error_response ~status:402 ~type_:"bactrian_unknown_model"
        (Printf.sprintf "no price is known for model %S" model)
  | Cap_reached { provider; cap; spent; requested } ->
      error_response ~status:402 ~type_:"bactrian_cap_reached"
        (Printf.sprintf
           "the %s spend cap of $%.2f would be exceeded ($%.4f spent or \
            reserved, $%.4f requested)"
           provider cap spent requested)

(* Forward to the provider. The upstream copy runs in its own fibre under its
   own switch, attached to the agent's connection: if the agent goes away, the
   connection's switch ends and cancels the copy. Chunks pass through a
   bounded stream, so a slow agent slows the upstream read. *)
let live t ~conn_sw st x ~record =
  let run = st.run in
  let model =
    Option.value (Canon.str_member "model" x.request.canonical) ~default:""
  in
  let date = date_of (t.now ()) in
  match
    List.find_map
      (fun (p, k) -> if provider_equal p x.provider then Some k else None)
      run.keys
  with
  | None ->
      error_response ~status:502 ~type_:"bactrian_no_key"
        ("no API key is configured for " ^ segment x.provider)
  | Some real_key -> (
      let reservation =
        match
          Pricing.worst_case ~model ~date ~input_bytes:(String.length x.body)
            ~max_output:(max_output x.parsed)
        with
        | None -> Error (Ledger.Unknown_model model)
        | Some usd ->
            Ledger.reserve run.ledger ~provider:(segment x.provider) ~model ~usd
      in
      match reservation with
      | Error refusal -> refuse_spend st refusal
      | Ok reservation -> (
          let request =
            {
              Upstream.meth = x.meth;
              url = upstream_url run x;
              headers = forward_headers x.headers x.provider real_key;
              body = x.body;
            }
          in
          let head_p, head_r = Eio.Promise.create () in
          let items = Eio.Stream.create 16 in
          Eio.Fiber.fork_daemon ~sw:conn_sw (fun () ->
              Eio.Switch.run (fun sw ->
                  let head = ref None and chunks = ref [] in
                  let result =
                    t.call ~sw run.connector request
                      ~on_head:(fun h ->
                        head := Some h;
                        Eio.Promise.resolve head_r (Ok h))
                      ~on_chunk:(fun ~dt_ms piece ->
                        chunks := (dt_ms, piece) :: !chunks;
                        Eio.Stream.add items (Chunk piece))
                  in
                  finish_live st x ~record ~model ~date ~reservation ~head:!head
                    ~chunks:(List.rev !chunks) result;
                  match (result, !head) with
                  | Ok (), _ -> Eio.Stream.add items Finished
                  | Error _, Some _ -> Eio.Stream.add items Dropped
                  | Error e, None -> Eio.Promise.resolve head_r (Error e));
              `Stop_daemon);
          match Eio.Promise.await head_p with
          | Ok (h : Upstream.head) ->
              respond ~status:h.status ~headers:h.headers (fun () ->
                  Eio.Stream.take items)
          | Error e ->
              error_response ~status:502 ~type_:"bactrian_upstream_error"
                (describe_upstream_error e)))

(* Replay *)

let serve_recorded t st ~timing (e : Cassette.exchange) =
  match e.outcome with
  | Transport_error reason ->
      error_response ~status:502 ~type_:"bactrian_upstream_error" reason
  | Response { status; headers; body = Whole payload } -> (
      match Store.read_payload st.run.store payload with
      | Ok bytes -> respond ~status ~headers (of_list [ bytes ])
      | Error err ->
          error_response ~status:500 ~type_:"bactrian_internal_error"
            (Format.asprintf "%a" Store.pp_error err))
  | Response { status; headers; body = Stream chunks } ->
      let rest = ref chunks in
      respond ~status ~headers (fun () ->
          match !rest with
          | [] -> Finished
          | (dt_ms, piece) :: tl ->
              rest := tl;
              (match timing with
              | Recorded -> t.sleep (float_of_int dt_ms /. 1000.)
              | Fast -> ());
              Chunk piece)

let replay t ~conn_sw st x (r : replaying) =
  let decision =
    locked st (fun () ->
        let m, decision, found = Matcher.decide r.matcher x.request in
        r.matcher <- m;
        st.divergences <- List.rev_append found st.divergences;
        (match decision with
        | Matcher.Refuse d -> st.divergences <- d :: st.divergences
        | Serve _ -> st.exchanges <- st.exchanges + 1
        | Go_live -> ());
        decision)
  in
  match decision with
  | Serve { exchange; _ } -> serve_recorded t st ~timing:r.timing exchange
  | Go_live -> live t ~conn_sw st x ~record:true
  | Refuse d ->
      error_response ~status:409 ~type_:"bactrian_replay_miss"
        ~extra:[ ("divergence", divergence_json d) ]
        (describe_divergence d)

(* Routing *)

let model_request t ~conn_sw st p ~meth ~uri ~path ~headers ~body =
  match Protocols.find ~meth ~path with
  | None ->
      error_response ~status:404 ~type_:"bactrian_unknown_endpoint"
        (Printf.sprintf "%s %s is not a model endpoint bactrian knows" meth path)
  | Some ((module A : Protocol.S) as adapter) -> (
      let scrubbed = locked st (fun () -> scrub st body) in
      let parsed =
        match Canon.of_string scrubbed with
        | Error e -> Error (Format.asprintf "%a" Canon.pp_error e)
        | Ok parsed -> (
            match A.canonical_request ~path parsed with
            | Error e -> Error (Format.asprintf "%a" Protocol.pp_error e)
            | Ok canonical -> Ok (parsed, canonical))
      in
      match parsed with
      | Error reason ->
          error_response ~status:400 ~type_:"bactrian_bad_request" reason
      | Ok (parsed, canonical) -> (
          let raw_query = Uri.verbatim_query uri in
          let query = normalised_query uri in
          let key = Matcher.key ~headers ~meth ~path ~query canonical in
          let shape : Matcher.shape =
            if
              A.streaming ~path
                ~query:(Option.value raw_query ~default:"")
                parsed
            then Streamed
            else Whole_body
          in
          let occurrence =
            locked st (fun () ->
                let n = Option.value (Counts.find_opt key st.seen) ~default:0 in
                st.seen <- Counts.add key (n + 1) st.seen;
                n)
          in
          let x =
            {
              provider = p;
              adapter;
              meth;
              path;
              raw_query;
              query;
              headers;
              body;
              scrubbed;
              parsed;
              request = { channel = Model; key; canonical; shape };
              occurrence;
              t_ms = Float.to_int ((t.now () -. st.run.run_started) *. 1000.);
            }
          in
          match st.serving with
          | Live { record } -> live t ~conn_sw st x ~record
          | Replaying r -> replay t ~conn_sw st x r))

let route path =
  match String.split_on_char '/' path with
  | "" :: "r" :: token :: provider :: rest ->
      Some (token, provider, "/" ^ String.concat "/" rest)
  | _ -> None

(* The body is read before any check, so a refused request leaves a keep-alive
   connection ready for the next one. *)
let handle t ((conn_sw, _), _) request body : response =
  let headers = Http.Request.headers request in
  let uri = Uri.of_string (Http.Request.resource request) in
  match
    ( Eio.Buf_read.parse ~max_size:max_int Eio.Buf_read.take_all body,
      route (Uri.path uri) )
  with
  | Error (`Msg reason), _ ->
      error_response ~status:400 ~type_:"bactrian_bad_request" reason
  | Ok _, None ->
      error_response ~status:404 ~type_:"bactrian_not_found"
        "expected /r/<token>/<provider>/<path>"
  | Ok body, Some (token, provider, path) -> (
      match Runs.find_opt t.runs token with
      | None ->
          error_response ~status:403 ~type_:"bactrian_forbidden"
            "unknown or ended run"
      | Some st -> (
          match provider_of_segment provider with
          | None ->
              error_response ~status:404 ~type_:"bactrian_unknown_provider"
                ("unknown provider " ^ provider)
          | Some p -> (
              let expected = key_value p ("bactrian-" ^ st.run.token) in
              match Http.Header.get headers (key_header p) with
              | Some v when String.equal v expected ->
                  model_request t ~conn_sw st p
                    ~meth:(Http.Method.to_string (Http.Request.meth request))
                    ~uri ~path
                    ~headers:(Http.Header.to_list headers)
                    ~body
              | Some _ | None ->
                  locked st (fun () -> st.refused_keys <- st.refused_keys + 1);
                  error_response ~status:403 ~type_:"bactrian_forbidden"
                    ("missing or wrong placeholder key in " ^ key_header p))))

let on_error = function
  | Upstream_dropped | Eio.Io _ -> ()
  | ex -> Printf.eprintf "bactrian model proxy: %s\n%!" (Printexc.to_string ex)

let start ~sw ~net ~clock =
  match
    Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  with
  | exception (Eio.Io _ as ex) -> Error (Printexc.to_string ex)
  | sock -> (
      match Eio.Net.listening_addr sock with
      | `Unix _ -> Error "the listener is not TCP"
      | `Tcp (_, port) ->
          let t =
            {
              port;
              runs = Runs.create 8;
              now = (fun () -> Eio.Time.now clock);
              sleep = Eio.Time.sleep clock;
              call =
                (fun ~sw connector request ~on_head ~on_chunk ->
                  Upstream.call ~sw ~net ~clock ~timeout:upstream_timeout
                    connector request ~on_head ~on_chunk);
            }
          in
          let server =
            Cohttp_eio.Server.make_expert
              ~callback:(fun conn request body -> handle t conn request body)
              ()
          in
          Eio.Fiber.fork_daemon ~sw (fun () ->
              Cohttp_eio.Server.run sock server ~on_error);
          Ok t)

let port t = t.port

let register t run =
  if Runs.mem t.runs run.token then
    invalid_arg "Model_proxy.register: token already registered";
  let serving =
    match run.mode with
    | Record -> Live { record = true }
    | Passthrough -> Live { record = false }
    | Replay { recording; matcher; allow_reorder; timing } ->
        Replaying
          {
            matcher =
              Matcher.start ~mode:matcher ~allow_reorder recording.exchanges;
            timing;
          }
  in
  Runs.replace t.runs run.token
    {
      run;
      serving;
      lock = Eio.Mutex.create ();
      redact = Redact.empty;
      seen = Counts.empty;
      next_seq = 0;
      divergences = [];
      exchanges = 0;
      transport_errors = 0;
      usage_gaps = 0;
      refused_keys = 0;
      cap_refusals = [];
    }

let unregister t ~token =
  match Runs.find_opt t.runs token with
  | None -> invalid_arg "Model_proxy.unregister: unknown token"
  | Some st ->
      Runs.remove t.runs token;
      locked st (fun () ->
          let unconsumed =
            match st.serving with
            | Replaying r -> Matcher.finish r.matcher
            | Live _ -> []
          in
          {
            divergences = List.rev_append st.divergences unconsumed;
            exchanges = st.exchanges;
            transport_errors = st.transport_errors;
            usage_gaps = st.usage_gaps;
            refused_keys = st.refused_keys;
            cap_refusals = List.rev st.cap_refusals;
          })

let base_url t ~token p =
  Printf.sprintf "http://127.0.0.1:%d/r/%s/%s%s" t.port token (segment p)
    (sdk_base p)
