type forward = { port : int; socket : string }
type config = { forwards : forward list; agent : string list }

type parse_error =
  | Bad_pair of string
  | Bad_port of string
  | Bad_socket of string
  | Duplicate_port of int
  | Missing_separator
  | Empty_agent

let ( let* ) = Result.bind
let log_variable = "BACTRIAN_SANDBOX_LOG_FD"

(* SAFETY: the C function ignores its argument, allocates nothing on the OCaml
   heap and only calls prctl, so it may be [@@noalloc]. Its result is an
   immediate OCaml bool. *)
external set_undumpable : unit -> bool = "bactrian_set_undumpable" [@@noalloc]

(* Parsing *)

let parse_port text =
  let digits = String.for_all (function '0' .. '9' -> true | _ -> false) in
  match int_of_string_opt text with
  | Some port
    when String.length text <= 5 && digits text && port >= 1 && port <= 65535 ->
      Ok port
  | Some _ | None -> Error (Bad_port text)

let parse_pair text =
  match String.index_opt text '=' with
  | None -> Error (Bad_pair text)
  | Some i ->
      let socket = String.sub text (i + 1) (String.length text - i - 1) in
      let* port = parse_port (String.sub text 0 i) in
      if String.starts_with ~prefix:"/" socket then Ok { port; socket }
      else Error (Bad_socket socket)

let rec parse_pairs = function
  | [] -> Ok []
  | pair :: rest ->
      let* forward = parse_pair pair in
      let* forwards = parse_pairs rest in
      Ok (forward :: forwards)

let rec first_duplicate seen = function
  | [] -> None
  | { port; _ } :: rest ->
      if List.exists (Int.equal port) seen then Some port
      else first_duplicate (port :: seen) rest

let parse args =
  let rec split pairs = function
    | [] -> Error Missing_separator
    | "--" :: agent -> Ok (List.rev pairs, agent)
    | pair :: rest -> split (pair :: pairs) rest
  in
  let* pairs, agent = split [] args in
  let* () = match agent with [] -> Error Empty_agent | _ :: _ -> Ok () in
  let* forwards = parse_pairs pairs in
  match first_duplicate [] forwards with
  | Some port -> Error (Duplicate_port port)
  | None -> Ok { forwards; agent }

let describe_parse_error = function
  | Bad_pair text -> Printf.sprintf "%S is not of the form PORT=SOCKET" text
  | Bad_port text ->
      Printf.sprintf "%S is not a port number from 1 to 65535" text
  | Bad_socket text -> Printf.sprintf "%S is not an absolute socket path" text
  | Duplicate_port port -> Printf.sprintf "port %d is given twice" port
  | Missing_separator -> "missing -- before the agent command"
  | Empty_agent -> "no agent command after --"

(* Relaying *)

type error =
  | Listen_failed of { port : int; reason : string }
  | Spawn_failed of string

let describe_error = function
  | Listen_failed { port; reason } ->
      Printf.sprintf "cannot listen on 127.0.0.1:%d: %s" port reason
  | Spawn_failed reason -> "cannot start the agent: " ^ reason

let violation_line ~port ~socket ~reason =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("kind", `String "relay_failed");
         ("port", `Int port);
         ("socket", `String socket);
         ("reason", `String reason);
       ])

(* One [write] per line on a descriptor opened for append, so lines from
   concurrent connections do not interleave. *)
let record ~log line =
  Option.iter
    (fun fd ->
      let text = line ^ "\n" in
      try
        Eio_unix.Fd.use_exn "violation log" fd (fun fd ->
            ignore (Unix.write_substring fd text 0 (String.length text) : int))
      with (Eio.Io _ | Unix.Unix_error _) as e ->
        prerr_endline
          ("bactrian sandbox-init: cannot write the violation log: "
         ^ Printexc.to_string e))
    log

(* The peer may already have reset the connection, which makes shutdown fail
   and leaves nothing to do. *)
let shut_send flow = try Eio.Flow.shutdown flow `Send with Eio.Io _ -> ()

let pump ~src ~dst =
  Eio.Flow.copy src dst;
  shut_send dst

let relay ~net ~log ~port ~socket client =
  Eio.Switch.run @@ fun sw ->
  match Eio.Net.connect ~sw net (`Unix socket) with
  | exception (Eio.Io _ as e) ->
      record ~log (violation_line ~port ~socket ~reason:(Printexc.to_string e))
  | upstream -> (
      (* An error on one side fails [both], which cancels the other side and
         ends this connection only. *)
      try
        Eio.Fiber.both
          (fun () -> pump ~src:client ~dst:upstream)
          (fun () -> pump ~src:upstream ~dst:client)
      with Eio.Io _ -> ())

let bound_port ~requested listener =
  match Eio.Net.listening_addr listener with
  | `Tcp (_, port) -> port
  | `Unix _ -> requested

let listen ~sw ~net { port; _ } =
  match
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:128 net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  with
  | listener -> Ok listener
  | exception ((Eio.Io _ | Unix.Unix_error _) as e) ->
      Error (Listen_failed { port; reason = Printexc.to_string e })

let serve ~sw ~net ~log ~on_listening ({ port = requested; socket } as forward)
    =
  let* listener = listen ~sw ~net forward in
  let port = bound_port ~requested listener in
  on_listening ~requested ~bound:(Eio.Net.listening_addr listener);
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server listener
        ~on_error:(fun e ->
          prerr_endline
            ("bactrian sandbox-init: connection failed: " ^ Printexc.to_string e))
        (fun client _ -> relay ~net ~log ~port ~socket client));
  Ok ()

(* The agent *)

(* Eio reports a signal by its OCaml number: negative for the portable ones, the
   system's own number otherwise. *)
let system_signal s =
  if s >= 0 then Some s
  else
    List.assoc_opt s
      [
        (Sys.sighup, 1);
        (Sys.sigint, 2);
        (Sys.sigquit, 3);
        (Sys.sigill, 4);
        (Sys.sigtrap, 5);
        (Sys.sigabrt, 6);
        (Sys.sigfpe, 8);
        (Sys.sigkill, 9);
        (Sys.sigsegv, 11);
        (Sys.sigpipe, 13);
        (Sys.sigalrm, 14);
        (Sys.sigterm, 15);
      ]

let exit_code = function
  | `Exited code -> code
  | `Signaled s -> (
      match system_signal s with Some n -> 128 + n | None -> 255)

(* Handlers are installed before the agent exists, so a signal that arrives
   during start-up is forwarded as soon as there is a group to receive it.
   [f] gets [forward], which never returns: it waits for signals and sends them
   to the group it is given. A handler may only set a flag and broadcast. *)
let with_signal_forwarding f =
  let arrived = Eio.Condition.create () in
  let flags = List.map (fun s -> (s, Atomic.make false)) Interrupt.signals in
  let handle flag _ =
    Atomic.set flag true;
    Eio.Condition.broadcast arrived
  in

  let rec forward pgid =
    List.iter
      (fun (s, flag) ->
        if Atomic.exchange flag false then
          ignore (Process_group.signal pgid s : bool))
      flags;
    Eio.Condition.await_no_mutex arrived;
    forward pgid
  in
  Interrupt.with_handlers
    (List.map (fun (s, flag) -> (s, handle flag)) flags)
    (fun () -> f forward)

let rec serve_all ~sw ~net ~log ~on_listening = function
  | [] -> Ok ()
  | forward :: rest -> (
      match serve ~sw ~net ~log ~on_listening forward with
      | Ok () -> serve_all ~sw ~net ~log ~on_listening rest
      | Error _ as failed -> failed)

let spawn ~sw ~process_mgr ~stdout ~hidden_fd agent =
  let env =
    Eio.Process.Env.override
      [ (log_variable, None) ]
      (Eio.Process.environment process_mgr)
  in
  match
    let hidden =
      Option.map
        (fun n ->
          let null = Unix.openfile "/dev/null" [ O_RDWR; O_CLOEXEC ] 0 in
          (n, Eio_unix.Fd.of_unix ~sw ~close_unix:true null, `Blocking))
        hidden_fd
    in
    let fds =
      [
        (0, Eio_unix.Fd.stdin, `Blocking);
        (1, stdout, `Blocking);
        (2, Eio_unix.Fd.stderr, `Blocking);
      ]
      @ Option.to_list hidden
    in
    Eio_unix.Process.spawn_unix ~sw process_mgr ~pgid:0 ~fds ~env agent
  with
  | proc -> Ok proc
  | exception ((Eio.Io _ | Unix.Unix_error _) as e) ->
      Error (Spawn_failed (Printexc.to_string e))

let run ?(on_listening = fun ~requested:_ ~bound:_ -> ())
    ?(stdout = Eio_unix.Fd.stdout) ?log ?hidden_fd ~net ~process_mgr config =
  Eio.Switch.run @@ fun sw ->
  with_signal_forwarding @@ fun forward ->
  let* () = serve_all ~sw ~net ~log ~on_listening config.forwards in
  let* proc = spawn ~sw ~process_mgr ~stdout ~hidden_fd config.agent in
  let pgid = Eio.Process.pid proc in
  (* The leader's death leaves its children behind unless the group goes too. *)
  Eio.Switch.on_release sw (fun () ->
      ignore (Process_group.signal pgid Sys.sigkill : bool));
  let status =
    Eio.Fiber.first (fun () -> Eio.Process.await proc) (fun () -> forward pgid)
  in
  Ok (exit_code status)
