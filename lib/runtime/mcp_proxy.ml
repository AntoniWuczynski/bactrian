open Bactrian_core

type server =
  | Stdio of {
      argv : string list;
      env : (string * string) list;
      cwd : Eio.Fs.dir_ty Eio.Path.t option;
    }
  | Http of { url : Uri.t }

type policy = Experiment.policy = Record | Replay | Passthrough | Block
type config = { name : string; server : server; policy : policy }

let url_variable = "BACTRIAN_MCP_URL"
let http_variable = Experiment.mcp_http_variable

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
  answer_timeout : float;
}

type reply = { status : int; headers : (string * string) list; body : string }

(* Stdio servers *)

module Ids = Hashtbl.Make (Int)

(* Why a request got no answer from its server. Only [Server_failed] is the
   server's fault: a request the client cancelled, or one the end of the run
   cut off, has no answer to record and is not a transport error. *)
type failure = Server_failed of string | Cancelled | Run_ended

let describe_failure = function
  | Server_failed reason -> reason
  | Cancelled -> "the client cancelled the request"
  | Run_ended -> "the run has ended"

type waiting = {
  agent_id : Mcp_msg.id;  (** The id the agent gave the request. *)
  answer : (Mcp_msg.message, failure) result Eio.Promise.u;
}

type proc = {
  stdin : [ Eio.Flow.sink_ty | Eio.Resource.close_ty ] Eio.Resource.t;
  write_lock : Eio.Mutex.t;
  pending : waiting Ids.t;
      (** Requests sent and not yet answered, by the id bactrian gave them. *)
  mutable next_id : int;
  mutable dead : failure option;  (** Why the server can no longer answer. *)
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

(* Seconds between SIGTERM and SIGKILL. *)
let kill_grace = 2.
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

let fail_pending p failure =
  if Option.is_none p.dead then p.dead <- Some failure;
  let waiting = List.of_seq (Ids.to_seq_values p.pending) in
  Ids.reset p.pending;
  List.iter
    (fun w -> ignore (Eio.Promise.try_resolve w.answer (Error failure) : bool))
    waiting

let settle p n result =
  match Ids.find_opt p.pending n with
  | Some w ->
      Ids.remove p.pending n;
      ignore (Eio.Promise.try_resolve w.answer result : bool)
  | None -> ()

(* A write cut off part way leaves part of a line on the server's stdin, after
   which nothing written there can be trusted, so the server counts as dead. *)
let write p line =
  Eio.Mutex.use_ro p.write_lock (fun () ->
      match p.dead with
      | Some failure -> Error failure
      | None -> (
          match Eio.Flow.copy_string line p.stdin with
          | () -> Ok ()
          | exception (Eio.Io _ as ex) ->
              Error
                (Server_failed
                   ("could not write to the server: " ^ Printexc.to_string ex))
          | exception (Eio.Cancel.Cancelled _ as ex) ->
              if Option.is_none p.dead then
                p.dead <-
                  Some (Server_failed "a write to the server was cut off");
              raise ex))

(* A server whose stdin stays full is given up on: its pending requests fail
   and later ones are refused at once. *)
let stalled env p =
  let failure =
    Server_failed
      (Printf.sprintf "the server stopped reading its stdin for %g seconds"
         env.answer_timeout)
  in
  p.dead <- Some failure;
  fail_pending p failure;
  failure

let write_within env p line =
  match
    Eio.Time.with_timeout env.clock env.answer_timeout (fun () ->
        Ok (write p line))
  with
  | Ok result -> result
  | Error `Timeout -> Error (stalled env p)

(* Answers go to the request that bactrian numbered. An error without an id
   can only be for the one request in flight, if there is just one. A request
   from the server (sampling, roots, elicitation) has no way back to the agent
   through the shim, so [refuse] gets an error for it, which spares a server
   that awaits the answer the wait. Notifications are dropped. *)
let deliver p ~note ~refuse line =
  match Mcp_msg.decode line with
  | Ok
      (( Response { id = Int_id n; _ }
       | Error_response { id = Some (Int_id n); _ } ) as m) ->
      settle p n (Ok m)
  | Ok (Error_response { id = None; message; _ } as m) -> (
      match List.of_seq (Ids.to_seq_keys p.pending) with
      | [ n ] -> settle p n (Ok m)
      | [] | _ :: _ :: _ ->
          note ("the server sent an error without an id: " ^ message))
  | Ok (Request { id; method_; params = _ }) ->
      refuse
        (Mcp_msg.encode
           (Error_response
              {
                id = Some id;
                code = -32601;
                message = method_ ^ " is not supported through bactrian";
                data = None;
              })
        ^ "\n")
  | Ok
      ( Response { id = String_id _; _ }
      | Error_response { id = Some (String_id _); _ }
      | Notification _ )
  | Error _ ->
      ()

let describe_status = function
  | `Exited n -> Printf.sprintf "exited with status %d" n
  | `Signaled s -> "was killed by " ^ Proc_util.signal_name s

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

(* Runs in its own fibre for the life of the server: spawns it, routes its
   answers, and kills its process group when the run stops it or the proxy's
   switch ends. *)
let supervise env ~note ~argv ~bindings ~cwd ~started ~stop_p ~stop ~stopped =
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
  let refuse line =
    Eio.Fiber.fork_daemon ~sw:psw (fun () ->
        ignore (write_within env p line : (unit, failure) result);
        `Stop_daemon)
  in
  Eio.Fiber.fork_daemon ~sw:psw (fun () ->
      read_lines stdout_r (deliver p ~note ~refuse);
      let status =
        match
          Eio.Time.with_timeout env.clock 1. (fun () ->
              Ok (Eio.Process.await proc))
        with
        | Ok status -> describe_status status
        | Error `Timeout -> "closed its stdout"
      in
      fail_pending p
        (Server_failed
           (match !last_stderr with
           | "" -> "the server " ^ status
           | line ->
               Printf.sprintf "the server %s (last stderr line: %s)" status line));
      `Stop_daemon);
  Eio.Promise.await stop_p;
  fail_pending p Run_ended;
  (try Eio.Resource.close stdin_w with Eio.Io _ -> ());
  Process_group.terminate ~clock:env.clock ~grace:kill_grace pgid

let spawn env ~note ~argv ~bindings ~cwd =
  let started_p, started = Eio.Promise.create () in
  let stop_p, stop = Eio.Promise.create () in
  let stopped, stopped_r = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw:env.sw (fun () ->
      Fun.protect ~finally:(fun () ->
          ignore (Eio.Promise.try_resolve stopped_r () : bool))
      @@ fun () ->
      (try
         supervise env ~note ~argv ~bindings ~cwd ~started ~stop_p ~stop
           ~stopped
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
      | Failed reason -> Error (Server_failed reason)
      | Not_started when t.ended -> Error Run_ended
      | Not_started -> (
          let note m =
            t.shared.note
              (Printf.sprintf "MCP server %s: %s" entry.config.name m)
          in
          match
            spawn t.env ~note ~argv ~bindings:(server_env declared) ~cwd
          with
          | Ok p ->
              entry.running <- Running p;
              Ok p
          | Error e ->
              let reason = "could not start the server: " ^ e in
              entry.running <- Failed reason;
              Error (Server_failed reason)))

(* One request, renumbered so that concurrent requests from any client never
   share an id on the server's stdin. Writing it and awaiting the answer share
   one deadline. *)
let ask t p ~agent_id msg =
  let n = p.next_id in
  p.next_id <- n + 1;
  let answer, u = Eio.Promise.create () in
  Ids.replace p.pending n { agent_id; answer = u };
  let line = Mcp_msg.encode (Mcp_msg.with_id (Int_id n) msg) ^ "\n" in
  let written = ref false in
  match
    Eio.Time.with_timeout t.env.clock t.env.answer_timeout (fun () ->
        match write p line with
        | Error failure -> Ok (Error failure)
        | Ok () ->
            written := true;
            Ok (Eio.Promise.await answer))
  with
  | Ok (Ok m) -> Ok m
  | Ok (Error failure) ->
      Ids.remove p.pending n;
      Error failure
  | Error `Timeout when !written ->
      Ids.remove p.pending n;
      Error
        (Server_failed
           (Printf.sprintf "no answer within %g seconds" t.env.answer_timeout))
  | Error `Timeout -> Error (stalled t.env p)

let same_id (a : Mcp_msg.id) (b : Mcp_msg.id) =
  match (a, b) with
  | Int_id x, Int_id y -> Int.equal x y
  | String_id x, String_id y -> String.equal x y
  | Int_id _, String_id _ | String_id _, Int_id _ -> false

let id_of_json v : Mcp_msg.id option =
  match (Canon.to_int_opt v, Canon.to_string_opt v) with
  | Some n, _ -> Some (Int_id n)
  | None, Some s -> Some (String_id s)
  | None, None -> None

(* A cancellation names the agent's id, which the server never saw. It gets
   the id bactrian gave that request, or an explanation of why no single
   request in flight has the agent's id (two clients may use the same one). *)
let cancellation p params =
  let in_flight =
    match Option.bind params (Canon.member "requestId") with
    | None -> []
    | Some v -> (
        match id_of_json v with
        | None -> []
        | Some id ->
            Ids.fold
              (fun n w acc -> if same_id w.agent_id id then n :: acc else acc)
              p.pending [])
  in
  match (in_flight, params) with
  | [ n ], Some params ->
      Ok (n, Canon.set_member "requestId" (Canon.int n) params)
  | [], _ | _, None -> Error "it names no request in flight"
  | _ :: _ :: _, Some _ -> Error "several requests in flight have that id"

(* The server answers no request it cancelled, so bactrian answers it. *)
let forward_cancellation t entry ~name params =
  let dropped why =
    t.shared.note
      (Printf.sprintf "MCP server %s: a cancellation was dropped: %s" name why)
  in
  match entry.running with
  | Not_started | Failed _ -> dropped "it names no request in flight"
  | Running p -> (
      match cancellation p params with
      | Error why -> dropped why
      | Ok (n, params) -> (
          let line =
            Mcp_msg.encode
              (Notification
                 { method_ = "notifications/cancelled"; params = Some params })
            ^ "\n"
          in
          match write_within t.env p line with
          | Ok () -> settle p n (Error Cancelled)
          | Error failure -> dropped (describe_failure failure)))

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

type recorded = Served of Cassette.exchange | Missed of string * Canon.t

let from_recording t decide k msg =
  match decide ~t_ms:k.t_ms k.request with
  | Matcher.Serve { exchange; _ } -> Served exchange
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

(* The id of the request a recorded exchange answered. *)
let recorded_id t (e : Cassette.exchange) =
  match Store.read_payload t.shared.store e.request with
  | Error _ -> None
  | Ok text -> (
      match Mcp_msg.decode text with
      | Ok (Request { id; _ }) -> Some id
      | Ok (Notification _ | Response _ | Error_response _) | Error _ -> None)

let header_value name headers =
  List.find_map
    (fun (k, v) ->
      if String.equal (String.lowercase_ascii k) name then Some v else None)
    headers

(* The answer to the recorded request, given the live id. A request or a
   notification the server sent on the same stream keeps its own id. *)
let answer_with_id ~recorded ~live text =
  match Mcp_msg.decode text with
  | Ok ((Response { id; _ } | Error_response { id = Some id; _ }) as m)
    when same_id id recorded ->
      Some (Mcp_msg.encode (Mcp_msg.with_id live m))
  | Ok (Response _ | Error_response _ | Request _ | Notification _) | Error _ ->
      None

(* An event stream's events are separated by blank lines, and an event's data
   is its [data:] lines joined with newlines. An event that [rewrite] changes
   keeps its other fields in place and gets one [data:] line. *)
let rewrite_stream rewrite body =
  let lines =
    List.map
      (fun l ->
        if String.ends_with ~suffix:"\r" l then
          (String.sub l 0 (String.length l - 1), "\r")
        else (l, ""))
      (String.split_on_char '\n' body)
  in
  let is_data (l, _) = String.starts_with ~prefix:"data:" l in
  let value l =
    let v = String.sub l 5 (String.length l - 5) in
    if String.starts_with ~prefix:" " v then String.sub v 1 (String.length v - 1)
    else v
  in
  let event ev =
    match
      List.filter_map
        (fun ((l, _) as x) -> if is_data x then Some (value l) else None)
        ev
    with
    | [] -> ev
    | data -> (
        match rewrite (String.concat "\n" data) with
        | None -> ev
        | Some json ->
            let _, out =
              List.fold_left
                (fun (first, out) ((_, cr) as x) ->
                  match (is_data x, first) with
                  | false, _ -> (first, x :: out)
                  | true, true -> (false, ("data: " ^ json, cr) :: out)
                  | true, false -> (false, out))
                (true, []) ev
            in
            List.rev out)
  in
  let rec go out ev = function
    | [] -> List.rev (List.rev_append (event (List.rev ev)) out)
    | ((l, _) as x) :: rest when String.equal l "" ->
        go (x :: List.rev_append (event (List.rev ev)) out) [] rest
    | x :: rest -> go out (x :: ev) rest
  in
  String.concat "\n" (List.map (fun (l, cr) -> l ^ cr) (go [] [] lines))

(* The recorded reply with the live id, in a JSON body or in the answer's event
   of an event stream. *)
let rewrite_ids ~recorded ~live ~headers body =
  match recorded with
  | None -> body
  | Some recorded -> (
      let rewrite = answer_with_id ~recorded ~live in
      match header_value "content-type" headers with
      | Some ct when String.starts_with ~prefix:"text/event-stream" ct ->
          rewrite_stream rewrite body
      | Some _ | None -> Option.value (rewrite body) ~default:body)

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

(* A refused request gets its own id back, or the agent would never match the
   answer and wait for one. *)
let refuse_route t ~status ~code message body =
  t.shared.note message;
  match Mcp_msg.decode body with
  | Ok (Request { id; _ }) ->
      json status (rpc_error ~id:(Some id) ~code message)
  | Ok (Notification _ | Response _ | Error_response _) | Error _ ->
      { status; headers = []; body = "" }

let unknown_server t name body =
  refuse_route t ~status:404 ~code:(-32601)
    ("no MCP server named " ^ name ^ " in this run")
    body

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
          | Served { outcome = Transport_error reason; _ } ->
              json 200 (server_error ~id ("recorded: " ^ reason))
          | Served { outcome = Response r; _ } -> (
              match recorded_body t r with
              | Ok text ->
                  json 200 (Option.value (with_live_id id text) ~default:text)
              | Error e -> json 200 (server_error ~id e)))
      | Forward { record = r } -> (
          let answer =
            Result.bind (running t entry ~argv ~declared ~cwd) (fun p ->
                ask t p ~agent_id:id msg)
          in
          let failed failure =
            Printf.sprintf "MCP server %s: %s" name (describe_failure failure)
          in
          match answer with
          | Ok m ->
              let text = Mcp_msg.encode (Mcp_msg.with_id id m) in
              record t ~name ~record:r k
                (response t ~status:200 ~headers:json_type text);
              json 200 text
          | Error (Server_failed _ as failure) ->
              let reason = failed failure in
              t.shared.failed reason;
              record t ~name ~record:r k (Transport_error reason);
              json 200 (server_error ~id reason)
          (* Neither has an answer to record, and neither is the server's
             fault. *)
          | Error Cancelled -> json 200 (server_error ~id (failed Cancelled))
          | Error Run_ended ->
              t.shared.note
                (Printf.sprintf
                   "MCP server %s: a request was cut off by the end of the run"
                   name);
              json 200 (server_error ~id (failed Run_ended))))

let stdio t ~name body =
  match find t name with
  | None | Some { config = { server = Http _; _ }; _ } ->
      unknown_server t name body
  | Some ({ config = { server = Stdio { argv; env; cwd }; _ }; _ } as entry)
    -> (
      match Mcp_msg.decode body with
      | Error e -> bad_body t (describe_decode_error e)
      | Ok (Request { id; _ } as msg) ->
          stdio_request t entry ~name ~argv ~declared:env ~cwd ~id msg body
      | Ok ((Notification _ | Response _ | Error_response _) as msg) -> (
          match (action t entry.config, msg) with
          | ( Forward _,
              Notification { method_ = "notifications/cancelled"; params } ) ->
              forward_cancellation t entry ~name params;
              accepted
          | ( Forward _,
              (Notification _ | Response _ | Error_response _ | Request _) )
            -> (
              let sent =
                Result.bind (running t entry ~argv ~declared:env ~cwd) (fun p ->
                    write_within t.env p (String.trim body ^ "\n"))
              in
              match sent with
              | Ok () -> accepted
              | Error failure ->
                  t.shared.note
                    (Printf.sprintf "MCP server %s: notification lost: %s" name
                       (describe_failure failure));
                  accepted)
          | (From_recording _ | Blocked | No_recording), _ -> accepted))

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

(* The agent's path goes below the declared one and its query after the
   declared one. A [.] or [..] segment, encoded or not, could reach other paths
   on the server's host. *)
let climbs rest =
  List.exists
    (fun seg -> match Uri.pct_decode seg with "." | ".." -> true | _ -> false)
    (String.split_on_char '/' rest)

let target url ~rest ~query =
  let u = Uri.with_path url (Uri.path url ^ rest) in
  let with_query q =
    Uri.of_string (Uri.to_string (Uri.with_query u []) ^ "?" ^ q)
  in
  match (Uri.verbatim_query url, query) with
  | _, None -> u
  | (None | Some ""), Some q -> with_query q
  | Some declared, Some q -> with_query (declared ^ "&" ^ q)

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
          | Served { outcome = Transport_error reason; _ } ->
              json 502 (server_error ~id ("recorded: " ^ reason))
          | Served ({ outcome = Response r; _ } as e) -> (
              match recorded_body t r with
              | Ok text ->
                  {
                    status = r.status;
                    headers = r.headers;
                    body =
                      rewrite_ids ~recorded:(recorded_id t e) ~live:id
                        ~headers:r.headers text;
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
  | None | Some { config = { server = Stdio _; _ }; _ } ->
      unknown_server t name body
  | Some ({ config = { server = Http { url }; _ }; _ } as entry) -> (
      let act = action t entry.config in
      if not (String.equal meth "POST") then
        { status = 405; headers = [ ("allow", "POST") ]; body = "" }
      else if climbs rest then
        refuse_route t ~status:400 ~code:(-32600)
          (Printf.sprintf "MCP server %s: the path %s has a . or .. segment"
             name rest)
          body
      else
        match Mcp_msg.decode body with
        | Error e -> bad_body t (describe_decode_error e)
        | Ok (Request { id; _ } as msg) -> (
            match act with
            | Blocked -> blocked_http t ~name
            | Forward _ | From_recording _ | No_recording ->
                http_request t act ~name ~url ~rest ~query ~headers ~id msg body
            )
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
            | From_recording _ | No_recording | Blocked -> accepted))
