open Bactrian_core

type server =
  | Stdio of {
      argv : string list;
      env : (string * string) list;
      cwd : Eio.Fs.dir_ty Eio.Path.t option;
    }
  | Http of { url : Uri.t }

type policy = Record | Replay | Passthrough | Block
type config = { name : string; server : server; policy : policy }

let url_variable = "BACTRIAN_MCP_URL"

let http_variable name =
  "BACTRIAN_MCP_HTTP_"
  ^ String.map
      (fun c ->
        match c with
        | 'a' .. 'z' -> Char.uppercase_ascii c
        | 'A' .. 'Z' | '0' .. '9' -> c
        | _ -> '_')
      name

type serving =
  | Live of { record : bool }
  | Replaying of { decide : t_ms:int -> Matcher.request -> Matcher.decision }

type shared = {
  serving : serving;
  store : Store.t;
  connector : Upstream.connector;
  t_ms : unit -> int;
  scrub : string -> string;
  mask : Canon.t -> Canon.t;
  occurrence : string -> int;
  exchange : record:bool -> Cassette.exchange -> unit;
  failed : string -> unit;
  note : string -> unit;
}

type env = {
  sw : Eio.Switch.t;
  process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  call :
    sw:Eio.Switch.t ->
    Upstream.connector ->
    Upstream.request ->
    on_head:(Upstream.head -> unit) ->
    on_chunk:(dt_ms:int -> string -> unit) ->
    (unit, Upstream.error) result;
}

type reply = { status : int; headers : (string * string) list; body : string }

(* Stdio servers *)

module Ids = Hashtbl.Make (Int)

type proc = {
  stdin : [ Eio.Flow.sink_ty | Eio.Resource.close_ty ] Eio.Resource.t;
  write_lock : Eio.Mutex.t;
  pending : (Mcp_msg.message, string) result Eio.Promise.u Ids.t;
      (** Requests sent and not yet answered, by the id bactrian gave them. *)
  mutable next_id : int;
  mutable dead : string option;  (** Why the server can no longer answer. *)
  stop : unit Eio.Promise.u;
  stopped : unit Eio.Promise.t;
}

type running = Not_started | Running of proc | Failed of string

type entry = {
  config : config;
  spawn_lock : Eio.Mutex.t;
  mutable running : running;
}

type t = {
  env : env;
  shared : shared;
  entries : entry list;
  mutable ended : bool;
}

let create env shared configs =
  {
    env;
    shared;
    entries =
      List.map
        (fun config ->
          { config; spawn_lock = Eio.Mutex.create (); running = Not_started })
        configs;
    ended = false;
  }

(* Seconds between SIGTERM and SIGKILL, and the longest wait for one answer
   (the model proxy's upstream timeout). *)
let kill_grace = 2.
let answer_timeout = 600.
let max_line = 64 * 1024 * 1024

(* The agent's environment never reaches a tool server: only these and the
   declared variables do. *)
let inherited = [ "PATH"; "HOME"; "LANG" ]

let server_env declared =
  let base =
    List.filter_map
      (fun k -> Option.map (fun v -> (k, v)) (Sys.getenv_opt k))
      inherited
  in
  List.fold_left
    (fun acc (k, v) ->
      (k, v) :: List.filter (fun (k', _) -> not (String.equal k k')) acc)
    [] (base @ declared)

let fail_pending p reason =
  if Option.is_none p.dead then p.dead <- Some reason;
  let waiting = List.of_seq (Ids.to_seq_values p.pending) in
  Ids.reset p.pending;
  List.iter
    (fun u -> ignore (Eio.Promise.try_resolve u (Error reason) : bool))
    waiting

(* Answers go to the request that bactrian numbered. Anything else the server
   writes (notifications, and the requests a legacy server may send) has no
   way back to the agent through the shim, and is dropped. *)
let deliver p line =
  match Mcp_msg.decode line with
  | Ok
      (( Response { id = Int_id n; _ }
       | Error_response { id = Some (Int_id n); _ } ) as m) -> (
      match Ids.find_opt p.pending n with
      | Some u ->
          Ids.remove p.pending n;
          ignore (Eio.Promise.try_resolve u (Ok m) : bool)
      | None -> ())
  | Ok
      ( Response { id = String_id _; _ }
      | Error_response { id = Some (String_id _) | None; _ }
      | Request _ | Notification _ )
  | Error _ ->
      ()

let describe_status = function
  | `Exited n -> Printf.sprintf "exited with status %d" n
  | `Signaled s -> Printf.sprintf "was killed by signal %d" s

let read_lines flow f =
  let r = Eio.Buf_read.of_flow ~max_size:max_line flow in
  let rec loop () =
    match Eio.Buf_read.line r with
    | line ->
        f line;
        loop ()
    | exception (End_of_file | Eio.Buf_read.Buffer_limit_exceeded | Eio.Io _) ->
        ()
  in
  loop ()

let terminate env pgid =
  if
    Process_group.signal pgid Sys.sigterm
    && not (Process_group.wait_gone ~clock:env.clock ~timeout:kill_grace pgid)
  then begin
    ignore (Process_group.signal pgid Sys.sigkill : bool);
    ignore
      (Process_group.wait_gone ~clock:env.clock ~timeout:kill_grace pgid : bool)
  end

(* Runs in its own fibre for the life of the server: spawns it, routes its
   answers, and kills its process group when the run stops it or the proxy's
   switch ends. *)
let supervise env ~argv ~bindings ~cwd ~started ~stop_p ~stop ~stopped =
  Eio.Switch.run @@ fun psw ->
  let stdin_r, stdin_w = Eio_unix.pipe psw in
  let stdout_r, stdout_w = Eio_unix.pipe psw in
  let stderr_r, stderr_w = Eio_unix.pipe psw in
  let proc =
    Eio_unix.Process.spawn_unix ~sw:psw env.process_mgr ?cwd ~pgid:0
      ~fds:
        [
          (0, Eio_unix.Resource.fd stdin_r, `Blocking);
          (1, Eio_unix.Resource.fd stdout_w, `Blocking);
          (2, Eio_unix.Resource.fd stderr_w, `Blocking);
        ]
      ~env:(Eio.Process.Env.of_bindings bindings)
      argv
  in
  (* The parent drops the child's ends, or stdout never reaches end of file. *)
  Eio.Resource.close stdin_r;
  Eio.Resource.close stdout_w;
  Eio.Resource.close stderr_w;
  let pgid = Eio.Process.pid proc in
  Eio.Switch.on_release psw (fun () ->
      ignore (Process_group.signal pgid Sys.sigkill : bool));
  let p =
    {
      stdin =
        (stdin_w :> [ Eio.Flow.sink_ty | Eio.Resource.close_ty ] Eio.Resource.t);
      write_lock = Eio.Mutex.create ();
      pending = Ids.create 8;
      next_id = 1;
      dead = None;
      stop;
      stopped;
    }
  in
  Eio.Promise.resolve started (Ok p);
  let last_stderr = ref "" in
  Eio.Fiber.fork_daemon ~sw:psw (fun () ->
      read_lines stderr_r (fun line -> last_stderr := line);
      `Stop_daemon);
  Eio.Fiber.fork_daemon ~sw:psw (fun () ->
      read_lines stdout_r (deliver p);
      let status =
        match
          Eio.Time.with_timeout env.clock 1. (fun () ->
              Ok (Eio.Process.await proc))
        with
        | Ok status -> describe_status status
        | Error `Timeout -> "closed its stdout"
      in
      fail_pending p
        (match !last_stderr with
        | "" -> "the server " ^ status
        | line ->
            Printf.sprintf "the server %s (last stderr line: %s)" status line);
      `Stop_daemon);
  Eio.Promise.await stop_p;
  fail_pending p "the run has ended";
  (try Eio.Resource.close stdin_w with Eio.Io _ -> ());
  terminate env pgid

let spawn env ~argv ~bindings ~cwd =
  let started_p, started = Eio.Promise.create () in
  let stop_p, stop = Eio.Promise.create () in
  let stopped, stopped_r = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw:env.sw (fun () ->
      Fun.protect ~finally:(fun () ->
          ignore (Eio.Promise.try_resolve stopped_r () : bool))
      @@ fun () ->
      (try supervise env ~argv ~bindings ~cwd ~started ~stop_p ~stop ~stopped
       with (Eio.Io _ | Unix.Unix_error _) as ex ->
         ignore
           (Eio.Promise.try_resolve started (Error (Printexc.to_string ex))
             : bool));
      `Stop_daemon);
  Eio.Promise.await started_p

let running t entry ~argv ~declared ~cwd =
  Eio.Mutex.use_rw ~protect:true entry.spawn_lock (fun () ->
      match entry.running with
      | Running p -> Ok p
      | Failed reason -> Error reason
      | Not_started when t.ended -> Error "the run has ended"
      | Not_started -> (
          match spawn t.env ~argv ~bindings:(server_env declared) ~cwd with
          | Ok p ->
              entry.running <- Running p;
              Ok p
          | Error e ->
              let reason = "could not start the server: " ^ e in
              entry.running <- Failed reason;
              Error reason))

let write p line =
  match p.dead with
  | Some reason -> Error reason
  | None ->
      Eio.Mutex.use_rw ~protect:false p.write_lock (fun () ->
          match Eio.Flow.copy_string line p.stdin with
          | () -> Ok ()
          | exception (Eio.Io _ as ex) ->
              Error ("could not write to the server: " ^ Printexc.to_string ex))

(* One request, renumbered so that concurrent requests from any client never
   share an id on the server's stdin. *)
let ask t p msg =
  let n = p.next_id in
  p.next_id <- n + 1;
  let answer, u = Eio.Promise.create () in
  Ids.replace p.pending n u;
  match write p (Mcp_msg.encode (Mcp_msg.with_id (Int_id n) msg) ^ "\n") with
  | Error reason ->
      Ids.remove p.pending n;
      Error reason
  | Ok () -> (
      match
        Eio.Time.with_timeout t.env.clock answer_timeout (fun () ->
            Ok (Eio.Promise.await answer))
      with
      | Ok r -> r
      | Error `Timeout ->
          Ids.remove p.pending n;
          Error (Printf.sprintf "no answer within %.0f seconds" answer_timeout))

let stop t =
  Eio.Cancel.protect @@ fun () ->
  t.ended <- true;
  List.iter
    (fun e ->
      Eio.Mutex.use_rw ~protect:true e.spawn_lock (fun () ->
          match e.running with
          | Running p ->
              ignore (Eio.Promise.try_resolve p.stop () : bool);
              Eio.Promise.await p.stopped
          | Not_started | Failed _ -> ()))
    t.entries

(* JSON-RPC replies *)

let obj fields =
  match Canon.obj fields with
  | Ok v -> v
  | Error _ -> assert false (* every caller passes distinct literal names *)

let json_type = [ ("content-type", "application/json") ]
let json status body = { status; headers = json_type; body }
let accepted = { status = 202; headers = []; body = "" }

let rpc_error ?data ~id ~code message =
  Mcp_msg.encode (Error_response { id; code; message; data })

(* Errors bactrian itself gives, in the implementation-defined range. *)
let bactrian_error ?(extra = []) ~id ~type_ message =
  rpc_error ~id:(Some id) ~code:(-32000)
    ~data:(obj (("type", Canon.string type_) :: extra))
    message

let describe_decode_error : Mcp_msg.error -> string = function
  | Invalid_json reason -> "not JSON: " ^ reason
  | Not_jsonrpc reason -> "not a JSON-RPC message: " ^ reason

(* Keys *)

let path name = "/mcp/" ^ name

type keyed = {
  request : Matcher.request;
  occurrence : int;
  scrubbed : string;
  t_ms : int;
}

let keyed t ~name body =
  let t_ms = t.shared.t_ms () in
  let scrubbed = t.shared.scrub body in
  match Mcp_msg.decode scrubbed with
  | Error e -> Error (describe_decode_error e)
  | Ok m -> (
      match Mcp_msg.canonical_request m with
      | None -> Error "redaction changed the kind of message"
      | Some c ->
          let canonical = t.shared.mask c in
          let key =
            Matcher.key ~meth:"POST" ~path:(path name) ~query:[] canonical
          in
          Ok
            {
              request = { channel = Mcp; key; canonical; shape = Whole_body };
              occurrence = t.shared.occurrence key;
              scrubbed;
              t_ms;
            })

let record t ~name ~record k outcome =
  t.shared.exchange ~record
    {
      Cassette.seq = 0;
      t_ms = k.t_ms;
      channel = Mcp;
      protocol = "mcp";
      meth = "POST";
      path = path name;
      query = [];
      key = k.request.key;
      occurrence = k.occurrence;
      canonical = k.request.canonical;
      request = Store.payload t.shared.store k.scrubbed;
      outcome;
    }

let response t ~status ~headers body : Cassette.outcome =
  Response
    {
      status;
      headers;
      body =
        Whole
          {
            payload = Store.payload t.shared.store (t.shared.scrub body);
            complete = true;
          };
    }

(* What a server's policy means in this run. *)
type action =
  | Forward of { record : bool }
  | From_recording of (t_ms:int -> Matcher.request -> Matcher.decision)
  | Blocked
  | No_recording

let action t (c : config) =
  match (c.policy, t.shared.serving) with
  | Block, (Live _ | Replaying _) -> Blocked
  | Passthrough, (Live _ | Replaying _) -> Forward { record = false }
  | (Record | Replay), Replaying { decide } -> From_recording decide
  | Record, Live { record } -> Forward { record }
  | Replay, Live _ -> No_recording

(* Replay *)

let miss_message (msg : Mcp_msg.message) d =
  match msg with
  | Request { method_ = ("initialize" | "server/discover") as m; _ } ->
      Printf.sprintf
        "the MCP handshake %s is not in the recording (it was recorded in the \
         other protocol era, or without this server): %s"
        m (Replay_miss.describe d)
  | Request _ | Notification _ | Response _ | Error_response _ ->
      Replay_miss.describe d

type recorded = Served of Cassette.outcome | Missed of string * Canon.t

let from_recording t decide k msg =
  match decide ~t_ms:k.t_ms k.request with
  | Matcher.Serve { exchange; _ } -> Served exchange.Cassette.outcome
  | Refuse d ->
      let message = miss_message msg d in
      t.shared.note message;
      Missed (message, Replay_miss.json d)
  | Go_live ->
      assert false (* the matcher goes live on the model channel only *)

let recorded_body t (r : Cassette.response) =
  match r.body with
  | Whole { payload; complete = _ } ->
      Result.map_error
        (Format.asprintf "%a" Store.pp_error)
        (Store.read_payload t.shared.store payload)
  | Stream { chunks; complete = _ } ->
      Ok (String.concat "" (List.map snd chunks))

let with_live_id id text =
  match Mcp_msg.decode text with
  | Ok m -> Some (Mcp_msg.encode (Mcp_msg.with_id id m))
  | Error _ -> None

let header_value name headers =
  List.find_map
    (fun (k, v) ->
      if String.equal (String.lowercase_ascii k) name then Some v else None)
    headers

(* The recorded reply with the live id: the whole body, or each [data:] line of
   an event stream. *)
let rewrite_ids id ~headers body =
  match header_value "content-type" headers with
  | Some ct when String.starts_with ~prefix:"text/event-stream" ct ->
      String.split_on_char '\n' body
      |> List.map (fun line ->
          match String.starts_with ~prefix:"data:" line with
          | false -> line
          | true -> (
              let payload = String.sub line 5 (String.length line - 5) in
              let cr = String.ends_with ~suffix:"\r" payload in
              let payload =
                if cr then String.sub payload 0 (String.length payload - 1)
                else payload
              in
              match with_live_id id (String.trim payload) with
              | Some j -> "data: " ^ j ^ if cr then "\r" else ""
              | None -> line))
      |> String.concat "\n"
  | Some _ | None -> Option.value (with_live_id id body) ~default:body

let replay_miss ~id (message, divergence) =
  bactrian_error ~id ~type_:"bactrian_replay_miss"
    ~extra:[ ("divergence", divergence) ]
    message

let server_error ~id reason =
  bactrian_error ~id ~type_:"bactrian_mcp_server_error" reason

let no_recording t ~name ~id =
  let message =
    Printf.sprintf
      "the MCP server %s has policy replay but the run has no recording" name
  in
  t.shared.note message;
  bactrian_error ~id ~type_:"bactrian_replay_miss" message

(* Routes *)

let find t name =
  List.find_opt (fun e -> String.equal e.config.name name) t.entries

let unknown_server t name =
  let message = "no MCP server named " ^ name ^ " in this run" in
  t.shared.note message;
  json 404 (rpc_error ~id:None ~code:(-32601) message)

let bad_body t reason =
  let message = "bad MCP message: " ^ reason in
  t.shared.note message;
  json 400 (rpc_error ~id:None ~code:(-32700) message)

let blocked_message name = "the experiment blocks the MCP server " ^ name

let stdio_request t entry ~name ~argv ~declared ~cwd ~id msg body =
  match keyed t ~name body with
  | Error reason -> bad_body t reason
  | Ok k -> (
      match action t entry.config with
      | Blocked ->
          t.shared.note (blocked_message name);
          json 200
            (rpc_error ~id:(Some id) ~code:(-32601) (blocked_message name))
      | No_recording -> json 200 (no_recording t ~name ~id)
      | From_recording decide -> (
          match from_recording t decide k msg with
          | Missed (message, d) -> json 200 (replay_miss ~id (message, d))
          | Served (Transport_error reason) ->
              json 200 (server_error ~id ("recorded: " ^ reason))
          | Served (Response r) -> (
              match recorded_body t r with
              | Ok text ->
                  json 200 (Option.value (with_live_id id text) ~default:text)
              | Error e -> json 200 (server_error ~id e)))
      | Forward { record = r } -> (
          let answer =
            Result.bind (running t entry ~argv ~declared ~cwd) (fun p ->
                ask t p msg)
          in
          match answer with
          | Ok m ->
              let text = Mcp_msg.encode (Mcp_msg.with_id id m) in
              record t ~name ~record:r k
                (response t ~status:200 ~headers:json_type text);
              json 200 text
          | Error reason ->
              let reason = Printf.sprintf "MCP server %s: %s" name reason in
              t.shared.failed reason;
              record t ~name ~record:r k (Transport_error reason);
              json 200 (server_error ~id reason)))

let stdio t ~name body =
  match find t name with
  | None | Some { config = { server = Http _; _ }; _ } -> unknown_server t name
  | Some ({ config = { server = Stdio { argv; env; cwd }; _ }; _ } as entry)
    -> (
      match Mcp_msg.decode body with
      | Error e -> bad_body t (describe_decode_error e)
      | Ok (Request { id; _ } as msg) ->
          stdio_request t entry ~name ~argv ~declared:env ~cwd ~id msg body
      | Ok (Notification _ | Response _ | Error_response _) -> (
          match action t entry.config with
          | Forward _ -> (
              let sent =
                Result.bind (running t entry ~argv ~declared:env ~cwd) (fun p ->
                    write p (String.trim body ^ "\n"))
              in
              match sent with
              | Ok () -> accepted
              | Error reason ->
                  t.shared.note
                    (Printf.sprintf "MCP server %s: notification lost: %s" name
                       reason);
                  accepted)
          | From_recording _ | Blocked | No_recording -> accepted))

(* Streamable HTTP *)

let forwarded_header name =
  List.exists (String.equal name)
    [
      "content-type";
      "accept";
      "mcp-protocol-version";
      "mcp-method";
      "mcp-name";
      "mcp-session-id";
    ]
  || String.starts_with ~prefix:"mcp-param-" name

let target url ~rest ~query =
  let u = Uri.with_path url (Uri.path url ^ rest) in
  match query with
  | None -> u
  | Some q -> Uri.of_string (Uri.to_string (Uri.with_query u []) ^ "?" ^ q)

let forward_http t ~url ~rest ~query ~headers body =
  let headers =
    List.filter_map
      (fun (k, v) ->
        let k = String.lowercase_ascii k in
        if forwarded_header k then Some (k, v) else None)
      headers
  in
  let head = ref None and chunks = ref [] in
  let result =
    Eio.Switch.run @@ fun sw ->
    t.env.call ~sw t.shared.connector
      { meth = "POST"; url = target url ~rest ~query; headers; body }
      ~on_head:(fun h -> head := Some h)
      ~on_chunk:(fun ~dt_ms:_ c -> chunks := c :: !chunks)
  in
  match (result, !head) with
  | Ok (), Some h -> Ok (h, String.concat "" (List.rev !chunks))
  | Ok (), None -> Error "no response head"
  | Error e, _ -> Error (Upstream.describe_error e)

let blocked_http t ~name =
  t.shared.note (blocked_message name);
  { status = 403; headers = []; body = "" }

let http_request t act ~name ~url ~rest ~query ~headers ~id msg body =
  match keyed t ~name body with
  | Error reason -> bad_body t reason
  | Ok k -> (
      match act with
      | Blocked -> blocked_http t ~name
      | No_recording -> json 409 (no_recording t ~name ~id)
      | From_recording decide -> (
          match from_recording t decide k msg with
          | Missed (message, d) -> json 409 (replay_miss ~id (message, d))
          | Served (Transport_error reason) ->
              json 502 (server_error ~id ("recorded: " ^ reason))
          | Served (Response r) -> (
              match recorded_body t r with
              | Ok text ->
                  {
                    status = r.status;
                    headers = r.headers;
                    body = rewrite_ids id ~headers:r.headers text;
                  }
              | Error e -> json 500 (server_error ~id e)))
      | Forward { record = r } -> (
          match forward_http t ~url ~rest ~query ~headers body with
          | Ok ((h : Upstream.head), text) ->
              record t ~name ~record:r k
                (response t ~status:h.status ~headers:h.headers text);
              { status = h.status; headers = h.headers; body = text }
          | Error reason ->
              let reason = Printf.sprintf "MCP server %s: %s" name reason in
              t.shared.failed reason;
              record t ~name ~record:r k (Transport_error reason);
              json 502 (server_error ~id reason)))

let http t ~name ~meth ~rest ~query ~headers body =
  match find t name with
  | None | Some { config = { server = Stdio _; _ }; _ } -> unknown_server t name
  | Some ({ config = { server = Http { url }; _ }; _ } as entry) -> (
      let act = action t entry.config in
      if not (String.equal meth "POST") then
        { status = 405; headers = [ ("allow", "POST") ]; body = "" }
      else
        match act with
        | Blocked -> blocked_http t ~name
        | Forward _ | From_recording _ | No_recording -> (
            match Mcp_msg.decode body with
            | Error e -> bad_body t (describe_decode_error e)
            | Ok (Request { id; _ } as msg) ->
                http_request t act ~name ~url ~rest ~query ~headers ~id msg body
            | Ok (Notification _ | Response _ | Error_response _) -> (
                match act with
                | Forward _ -> (
                    match forward_http t ~url ~rest ~query ~headers body with
                    | Ok ((h : Upstream.head), text) ->
                        { status = h.status; headers = h.headers; body = text }
                    | Error reason ->
                        t.shared.note
                          (Printf.sprintf "MCP server %s: notification lost: %s"
                             name reason);
                        { status = 502; headers = []; body = "" })
                | From_recording _ | No_recording | Blocked -> accepted)))
