module Frame = Bactrian_core.Vm_frame

type error =
  | Spawn_failed of string
  | Bad_frame of string
  | No_exit of Eio.Process.exit_status

let describe_error = function
  | Spawn_failed m -> "cannot start the container runtime: " ^ m
  | Bad_frame m -> "the relay sent a bad frame: " ^ m
  | No_exit status ->
      Printf.sprintf
        "the container runtime ended (%s) before the relay reported the \
         agent's status"
        (Proc_util.describe_exit status)

exception Closed_by_relay

(* A connection's queue towards the host: bounded, so a host side that does
   not read stops the frame reader, never memory. *)
type conn = {
  queue : [ `Data of string | `End ] Queue.t;
  mutable queued : int;
  mutable alive : bool;
  changed : Eio.Condition.t;
  mutable stop : unit -> unit;
}

let queue_limit = 1024 * 1024

(* The relay's MAX_CONNS: a relay that opens more is not the one bactrian
   built, and each would hold a host connection to the model proxy. *)
let max_channels = 64

let push c item =
  let n = match item with `Data s -> String.length s | `End -> 0 in
  while c.alive && c.queued > 0 && c.queued + n > queue_limit do
    Eio.Condition.await_no_mutex c.changed
  done;
  if c.alive then begin
    Queue.push item c.queue;
    c.queued <- c.queued + n;
    Eio.Condition.broadcast c.changed
  end

let rec take c =
  match Queue.take_opt c.queue with
  | Some item ->
      (match item with
      | `Data s -> c.queued <- c.queued - String.length s
      | `End -> ());
      Eio.Condition.broadcast c.changed;
      item
  | None ->
      Eio.Condition.await_no_mutex c.changed;
      take c

let failure_line ~port reason =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("kind", `String "relay_failed");
         ("port", `Int port);
         ("socket", `String "host loopback");
         ("reason", `String reason);
       ])

(* Writes to the relay. The runtime may be gone, which ends every sender. *)
let sender sink =
  let lock = Eio.Mutex.create () in
  fun frames ->
    Eio.Mutex.use_rw ~protect:true lock (fun () ->
        List.iter (fun f -> Eio.Flow.copy_string (Frame.encode f) sink) frames)

let chunk = 16384

let serve_conn ~net ~send ~log ~port ~channel ~forget c =
  Eio.Switch.run @@ fun sw ->
  c.stop <- (fun () -> Eio.Switch.fail sw Closed_by_relay);
  (match Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) with
  | exception (Eio.Io _ as e) -> log (failure_line ~port (Printexc.to_string e))
  | flow -> (
      let to_host () =
        let rec loop () =
          match take c with
          | `Data s ->
              Eio.Flow.copy_string s flow;
              loop ()
          | `End -> ( try Eio.Flow.shutdown flow `Send with Eio.Io _ -> ())
        in
        loop ()
      in
      let from_host () =
        let buf = Cstruct.create chunk in
        let rec loop () =
          match Eio.Flow.single_read flow buf with
          | n ->
              send (Frame.data ~channel (Cstruct.to_string ~len:n buf));
              loop ()
          | exception End_of_file -> send [ Frame.eof ~channel ]
        in
        loop ()
      in
      try Eio.Fiber.both to_host from_host with Eio.Io _ -> ()));
  forget ();
  try send [ Frame.close ~channel ] with Eio.Io _ -> ()

let serve_conn ~net ~send ~log ~port ~channel ~forget c =
  try serve_conn ~net ~send ~log ~port ~channel ~forget c
  with Closed_by_relay -> forget ()

let pump_stdin ~send stdin =
  let buf = Cstruct.create chunk in
  let rec loop () =
    match Eio.Flow.single_read stdin buf with
    | n ->
        send (Frame.data ~channel:0 (Cstruct.to_string ~len:n buf));
        loop ()
    | exception (End_of_file | Eio.Io _) -> send [ Frame.eof ~channel:0 ]
  in
  try loop () with Eio.Io _ -> ()

(* Reads frames until the runtime's output ends. The agent's exit code, from
   the relay's last frame, when it came. *)
let demux ~net ~send ~log ~forwards ~stdout source =
  Eio.Switch.run @@ fun conn_sw ->
  let conns = Hashtbl.create 16 in
  let exit_code = ref None in
  let stdout_open = ref true in
  let handle (f : Frame.t) =
    match (f.kind, f.channel) with
    | Data, 0 ->
        (if !stdout_open then
           try Eio.Flow.copy_string f.payload stdout
           with Eio.Io _ -> stdout_open := false);
        Ok ()
    | Eof, 0 -> Ok ()
    | Exit, _ -> (
        match Frame.status f with
        | Some s ->
            exit_code := Some s;
            Ok ()
        | None -> Error "an exit frame without a status")
    | Heartbeat, _ -> Error "a heartbeat from the relay"
    | Log, _ ->
        log
          (String.map (fun c -> if Char.equal c '\n' then ' ' else c) f.payload);
        Ok ()
    | Open, channel -> (
        match Frame.port f with
        | None -> Error "an open frame without a port"
        | Some _ when channel mod 2 = 0 ->
            Error "an open frame on an even channel"
        | Some _ when Hashtbl.mem conns channel ->
            Error (Printf.sprintf "channel %d opened twice" channel)
        | Some port when not (List.mem port forwards) ->
            log (failure_line ~port "the port is not forwarded");
            send [ Frame.close ~channel ];
            Ok ()
        | Some port when Hashtbl.length conns >= max_channels ->
            log (failure_line ~port "too many open connections");
            send [ Frame.close ~channel ];
            Ok ()
        | Some port ->
            let c =
              {
                queue = Queue.create ();
                queued = 0;
                alive = true;
                changed = Eio.Condition.create ();
                stop = (fun () -> ());
              }
            in
            Hashtbl.replace conns channel c;
            let forget () =
              c.alive <- false;
              Eio.Condition.broadcast c.changed;
              Hashtbl.remove conns channel
            in
            Eio.Fiber.fork_daemon ~sw:conn_sw (fun () ->
                serve_conn ~net ~send ~log ~port ~channel ~forget c;
                `Stop_daemon);
            Ok ())
    | Data, channel ->
        Option.iter
          (fun c -> push c (`Data f.payload))
          (Hashtbl.find_opt conns channel);
        Ok ()
    | Eof, channel ->
        Option.iter (fun c -> push c `End) (Hashtbl.find_opt conns channel);
        Ok ()
    | Close, channel ->
        Option.iter (fun c -> c.stop ()) (Hashtbl.find_opt conns channel);
        Ok ()
  in
  let decoder = Frame.decoder () in
  let buf = Cstruct.create 65536 in
  let rec loop () =
    match Eio.Flow.single_read source buf with
    | exception (End_of_file | Eio.Io _) -> (
        match Frame.finish decoder with
        | Ok () -> Ok !exit_code
        | Error e -> Error (Bad_frame (Frame.describe_error e)))
    | n -> (
        match Frame.feed decoder (Cstruct.to_string ~len:n buf) with
        | Error e -> Error (Bad_frame (Frame.describe_error e))
        | Ok frames -> (
            let rec each = function
              | [] -> Ok ()
              | f :: rest -> (
                  match handle f with Ok () -> each rest | Error m -> Error m)
            in
            match each frames with
            | Ok () -> loop ()
            | Error m -> Error (Bad_frame m)))
  in
  loop ()

(* The relay ends the agent when bactrian has been silent for
   [Frame.liveness_timeout], which is what notices a bactrian and a bridge that
   were both killed: the runtime's client keeps the container's input open. *)
let rec heartbeat ~clock ~send =
  send [ Frame.heartbeat ];
  Eio.Time.sleep clock Frame.heartbeat_interval;
  heartbeat ~clock ~send

let run ?stderr ~net ~clock ~process_mgr ~env ~forwards ~stdin ~stdout ~log argv
    =
  Eio.Switch.run @@ fun sw ->
  let to_relay_r, to_relay_w = Eio_unix.pipe sw in
  let from_relay_r, from_relay_w = Eio_unix.pipe sw in
  match
    Eio.Process.spawn ~sw process_mgr ~env ~stdin:to_relay_r
      ~stdout:from_relay_w ?stderr argv
  with
  | exception (Eio.Io _ as e) -> Error (Spawn_failed (Printexc.to_string e))
  | proc -> (
      Eio.Resource.close to_relay_r;
      Eio.Resource.close from_relay_w;
      let send = sender to_relay_w in
      let send frames = try send frames with Eio.Io _ -> () in
      let outcome =
        Eio.Fiber.first
          (fun () -> demux ~net ~send ~log ~forwards ~stdout from_relay_r)
          (fun () ->
            Eio.Fiber.both
              (fun () -> pump_stdin ~send stdin)
              (fun () -> heartbeat ~clock ~send);
            Eio.Fiber.await_cancel ())
      in
      (* The relay takes the end of its input for bactrian's death and kills
         the agent: only now, when the agent is gone or the frames are bad. *)
      Eio.Resource.close to_relay_w;
      let status = Eio.Process.await proc in
      match outcome with
      | Error e -> Error e
      | Ok (Some code) -> Ok code
      | Ok None -> Error (No_exit status))

type config = { forwards : int list; container : string; argv : string list }

(* Words rather than options, which the command-line parser would take for
   bactrian's own. *)
let parse args =
  let value ~prefix a =
    if String.starts_with ~prefix a then
      Some
        (String.sub a (String.length prefix)
           (String.length a - String.length prefix))
    else None
  in
  let rec go forwards container = function
    | "--" :: (_ :: _ as argv) -> (
        match container with
        | Some container -> Ok { forwards = List.rev forwards; container; argv }
        | None -> Error "missing container=NAME")
    | [ "--" ] | [] -> Error "missing -- and the runtime's command"
    | a :: rest -> (
        match (value ~prefix:"forward=" a, value ~prefix:"container=" a) with
        | Some p, _ -> (
            match int_of_string_opt p with
            | Some port when port >= 1 && port <= 65535 ->
                go (port :: forwards) container rest
            | Some _ | None -> Error (Printf.sprintf "%S is not a port" p))
        | None, Some name when not (String.equal name "") ->
            go forwards (Some name) rest
        | None, (Some _ | None) ->
            Error (Printf.sprintf "unexpected argument %S" a))
  in
  go [] None args

(* The bridge outlives a SIGTERM to its group: the runtime's client gets it
   too and passes it on, and the agent's last output still has to come
   through. A handler, unlike ignoring, is reset by exec in the client. *)
let keep_running () =
  List.iter
    (fun s -> Sys.set_signal s (Signal_handle (fun (_ : int) -> ())))
    Interrupt.signals

let rec watch_parent ~clock parent =
  Eio.Time.sleep clock 1.;
  if Unix.getppid () = parent then watch_parent ~clock parent

let main ~env ~log config =
  keep_running ();
  let parent = Unix.getppid () in
  let clock = Eio.Stdenv.clock env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  let vars = Unix.environment () in
  let write_log line =
    Option.iter
      (fun fd ->
        let text = line ^ "\n" in
        try
          Eio_unix.Fd.use_exn "violation log" fd (fun fd ->
              ignore (Unix.write_substring fd text 0 (String.length text) : int))
        with Eio.Io _ | Unix.Unix_error _ -> ())
      log
  in
  match
    Eio.Fiber.first
      (fun () ->
        `Done
          (run ~net:(Eio.Stdenv.net env) ~clock ~process_mgr ~env:vars
             ~forwards:config.forwards ~stdin:(Eio.Stdenv.stdin env)
             ~stdout:(Eio.Stdenv.stdout env) ~log:write_log config.argv))
      (fun () ->
        watch_parent ~clock parent;
        `Orphaned)
  with
  | `Done (Ok code) -> Ok code
  | `Done (Error e) -> Error (describe_error e)
  | `Orphaned ->
      let orphaned = "bactrian has gone, so the container was removed" in
      (match config.argv with
      | runtime :: _ -> (
          match
            Eio.Time.with_timeout clock 30. (fun () ->
                Ok
                  (Eio.Process.run process_mgr ~env:vars
                     ~is_success:(fun (_ : int) -> true)
                     ~stdout:(Eio.Flow.buffer_sink (Buffer.create 64))
                     ~stderr:(Eio.Flow.buffer_sink (Buffer.create 64))
                     [ runtime; "rm"; "-f"; config.container ]))
          with
          | Ok () | Error `Timeout -> ()
          | exception Eio.Io _ -> ())
      | [] -> ());
      Error orphaned
