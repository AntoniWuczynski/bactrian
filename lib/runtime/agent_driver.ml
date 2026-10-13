module Agent_protocol = Bactrian_core.Agent_protocol

type config = {
  argv : string list;
  env : (string * string) list;
  cwd : Eio.Fs.dir_ty Eio.Path.t option;
  stderr_path : Eio.Fs.dir_ty Eio.Path.t;
  turn_timeout : float;
  total_timeout : float;
  exit_grace : float;
  kill_grace : float;
}

type error =
  | Spawn_failed of string
  | Write_failed of string
  | Protocol_violation of { line : string; error : Agent_protocol.error }
  | Line_too_long
  | Turn_timeout
  | Total_timeout
  | Agent_exited of Eio.Process.exit_status option

type t = {
  proc : [ `Generic | `Unix ] Eio.Process.ty Eio.Resource.t;
  stdin : [ Eio.Flow.sink_ty | Eio.Resource.close_ty ] Eio.Resource.t;
  stdout : Eio.Buf_read.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  config : config;
  deadline : float;  (** Absolute [clock] time of the total timeout. *)
  mutable aborted : bool;
      (** The run is being stopped, not finished: no exit grace. *)
}

let max_line = 1 lsl 20
let pid t = Eio.Process.pid t.proc

let send t msg =
  let line = Agent_protocol.encode msg in
  let line = if String.ends_with ~suffix:"\n" line then line else line ^ "\n" in
  (* An agent that stops reading fills the pipe and blocks the write. *)
  let remaining = Float.max 0. (t.deadline -. Eio.Time.now t.clock) in
  match
    Eio.Time.with_timeout t.clock remaining (fun () ->
        Ok (Eio.Flow.copy_string line t.stdin))
  with
  | Ok () -> Ok ()
  | Error `Timeout ->
      t.aborted <- true;
      Error Total_timeout
  | exception (Eio.Io _ as e) -> Error (Write_failed (Printexc.to_string e))

let status_within t seconds =
  match
    Eio.Time.with_timeout t.clock seconds (fun () ->
        Ok (Eio.Process.await t.proc))
  with
  | Ok status -> Some status
  | Error `Timeout -> None

let read_line t =
  match Eio.Buf_read.line t.stdout with
  | line -> Ok line
  | exception End_of_file ->
      Error (Agent_exited (status_within t t.config.exit_grace))
  | exception Eio.Buf_read.Buffer_limit_exceeded -> Error Line_too_long

let recv_message t =
  let remaining = t.deadline -. Eio.Time.now t.clock in
  if remaining <= 0. then Error Total_timeout
  else
    let limit, timeout_error =
      if t.config.turn_timeout <= remaining then
        (t.config.turn_timeout, Turn_timeout)
      else (remaining, Total_timeout)
    in
    let read () = Result.map_error (fun e -> `Read e) (read_line t) in
    match Eio.Time.with_timeout t.clock limit read with
    | Error `Timeout -> Error timeout_error
    | Error (`Read e) -> Error e
    | Ok line -> (
        match Agent_protocol.decode line with
        | Ok msg -> Ok msg
        | Error error -> Error (Protocol_violation { line; error }))

let recv t =
  let result = recv_message t in
  (match result with
  | Error (Turn_timeout | Total_timeout | Protocol_violation _ | Line_too_long)
    ->
      t.aborted <- true
  | Error (Spawn_failed _ | Write_failed _ | Agent_exited _) | Ok _ -> ());
  result

(* The leader's pid is also the group id. [Unix.kill] with a negative pid signals
   the whole group; ESRCH means the group is already empty. macOS reports EPERM
   instead when only zombies are left, and the agent runs as our own user, so
   EPERM cannot mean a foreign process here. *)
let signal_group pgid signal =
  match Unix.kill (-pgid) signal with
  | () -> true
  | exception Unix.Unix_error ((Unix.ESRCH | Unix.EPERM), _, _) -> false

let group_gone pgid = not (signal_group pgid 0)

let wait_group_gone t pgid seconds =
  let rec poll () =
    if group_gone pgid then ()
    else (
      Eio.Time.sleep t.clock 0.02;
      poll ())
  in
  match Eio.Time.with_timeout t.clock seconds (fun () -> Ok (poll ())) with
  | Ok () -> true
  | Error `Timeout -> false

let shutdown t =
  Eio.Cancel.protect @@ fun () ->
  Eio.Resource.close t.stdin;
  let pgid = pid t in
  let exit_grace = if t.aborted then 0. else t.config.exit_grace in
  let status =
    match status_within t exit_grace with
    | Some status -> status
    | None -> (
        ignore (signal_group pgid Sys.sigterm : bool);
        match status_within t t.config.kill_grace with
        | Some status -> status
        | None ->
            ignore (signal_group pgid Sys.sigkill : bool);
            Eio.Process.await t.proc)
  in
  (* The leader is gone; children may remain. *)
  if
    signal_group pgid Sys.sigterm
    && not (wait_group_gone t pgid t.config.kill_grace)
  then begin
    ignore (signal_group pgid Sys.sigkill : bool);
    ignore (wait_group_gone t pgid 5. : bool)
  end;
  status

let spawn_error e = Error (Spawn_failed (Printexc.to_string e))

let open_stderr ~sw path =
  let file = Eio.Path.open_out ~sw ~create:(`Or_truncate 0o600) path in
  match Eio_unix.Resource.fd_opt file with
  | Some fd -> (file, fd)
  | None ->
      raise
        (Unix.Unix_error
           (Unix.EBADF, "open_out", "stderr file has no descriptor"))

(* The agent's switch is protected from the caller's cancellation: its reaper
   fibre has to keep running to learn the exit status during shutdown, which
   happens exactly when the caller is cancelled. [f] runs in a fibre of the
   caller's own switch, so it is still cancelled with the caller. *)
let with_agent ~process_mgr ~clock config f =
  Eio.Switch.run @@ fun caller_sw ->
  Eio.Cancel.protect @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  match
    let stdin_r, stdin_w = Eio_unix.pipe sw in
    let stdout_r, stdout_w = Eio_unix.pipe sw in
    let stderr_file, stderr_fd = open_stderr ~sw config.stderr_path in
    let fds =
      [
        (0, Eio_unix.Resource.fd stdin_r, `Blocking);
        (1, Eio_unix.Resource.fd stdout_w, `Blocking);
        (2, stderr_fd, `Blocking);
      ]
    in
    let proc =
      Eio_unix.Process.spawn_unix ~sw process_mgr ?cwd:config.cwd ~pgid:0 ~fds
        ~env:(Eio.Process.Env.of_bindings config.env)
        config.argv
    in
    (* The parent must drop the child's ends, or stdout never reaches end of file. *)
    Eio.Resource.close stdin_r;
    Eio.Resource.close stdout_w;
    Eio.Resource.close stderr_file;
    (proc, stdin_w, stdout_r)
  with
  | exception (Eio.Io _ as e) -> spawn_error e
  | exception (Unix.Unix_error _ as e) -> spawn_error e
  | proc, stdin_w, stdout_r -> (
      let clock = (clock :> float Eio.Time.clock_ty Eio.Resource.t) in
      let t =
        {
          proc :> [ `Generic | `Unix ] Eio.Process.ty Eio.Resource.t;
          stdin =
            (stdin_w
              :> [ Eio.Flow.sink_ty | Eio.Resource.close_ty ] Eio.Resource.t);
          stdout = Eio.Buf_read.of_flow ~max_size:max_line stdout_r;
          clock;
          config;
          deadline = Eio.Time.now clock +. config.total_timeout;
          aborted = false;
        }
      in
      let result =
        Eio.Promise.await (Eio.Fiber.fork_promise ~sw:caller_sw (fun () -> f t))
      in
      if Result.is_error result then t.aborted <- true;
      let status = shutdown t in
      match result with Ok v -> Ok (v, status) | Error e -> raise e)
