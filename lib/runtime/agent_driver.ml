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

type http_config = {
  url : Uri.t;
  namespace_header : string;
  run_id : string;
  turn_timeout : float;
  total_timeout : float;
}

type error =
  | Spawn_failed of string
  | Write_failed of string
  | Protocol_violation of { line : string; error : Agent_protocol.error }
  | Line_too_long
  | Turn_timeout
  | Total_timeout
  | Agent_exited of Eio.Process.exit_status option
  | Http_error of string
  | Http_status of { status : int; body : string }
  | No_message

type http = {
  net : [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
  http_clock : float Eio.Time.clock_ty Eio.Resource.t;
  connector : Upstream.connector;
  http_config : http_config;
  http_deadline : float;
  pending : Agent_protocol.from_agent Queue.t;
}

type process = {
  proc : [ `Generic | `Unix ] Eio.Process.ty Eio.Resource.t;
  stdin : [ Eio.Flow.sink_ty | Eio.Resource.close_ty ] Eio.Resource.t;
  stdout : Eio.Buf_read.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  config : config;
  deadline : float;  (** Absolute [clock] time of the total timeout. *)
  mutable aborted : bool;
      (** The run is being stopped, not finished: no exit grace. *)
}

type t = Process of process | Http of http

let max_line = 1 lsl 20
let max_error_body = 2048

(* How long one step may take, and the error when that runs out. *)
let step_limit ~clock ~deadline ~turn_timeout =
  let remaining = deadline -. Eio.Time.now clock in
  if remaining <= 0. then Error Total_timeout
  else if turn_timeout <= remaining then Ok (turn_timeout, Turn_timeout)
  else Ok (remaining, Total_timeout)

let send_process t msg =
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
  match
    step_limit ~clock:t.clock ~deadline:t.deadline
      ~turn_timeout:t.config.turn_timeout
  with
  | Error _ as e -> e
  | Ok (limit, timeout_error) -> (
      let read () = Result.map_error (fun e -> `Read e) (read_line t) in
      match Eio.Time.with_timeout t.clock limit read with
      | Error `Timeout -> Error timeout_error
      | Error (`Read e) -> Error e
      | Ok line -> (
          match Agent_protocol.decode line with
          | Ok msg -> Ok msg
          | Error error -> Error (Protocol_violation { line; error })))

let recv_process t =
  let result = recv_message t in
  (match result with
  | Error (Turn_timeout | Total_timeout | Protocol_violation _ | Line_too_long)
    ->
      t.aborted <- true
  | Error
      ( Spawn_failed _ | Write_failed _ | Agent_exited _ | Http_error _
      | Http_status _ | No_message )
  | Ok _ ->
      ());
  result

let shutdown t =
  Eio.Cancel.protect @@ fun () ->
  Eio.Resource.close t.stdin;
  let pgid = Eio.Process.pid t.proc in
  let exit_grace = if t.aborted then 0. else t.config.exit_grace in
  let status =
    match status_within t exit_grace with
    | Some status -> status
    | None -> (
        ignore (Process_group.signal pgid Sys.sigterm : bool);
        match status_within t t.config.kill_grace with
        | Some status -> status
        | None ->
            ignore (Process_group.signal pgid Sys.sigkill : bool);
            Eio.Process.await t.proc)
  in
  (* The leader is gone; children may remain. *)
  Process_group.terminate ~clock:t.clock ~grace:t.config.kill_grace pgid;
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
let with_agent ?(fds = []) ~process_mgr ~clock config f =
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
      @ List.map (fun (n, fd) -> (n, fd, `Blocking)) fds
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
        Eio.Promise.await
          (Eio.Fiber.fork_promise ~sw:caller_sw (fun () -> f (Process t)))
      in
      if Result.is_error result then t.aborted <- true;
      let status = shutdown t in
      match result with Ok v -> Ok (v, status) | Error e -> raise e)

let clip body =
  if String.length body <= max_error_body then body
  else String.sub body 0 max_error_body

let describe_upstream : Upstream.error -> error option = function
  | Timed_out -> None
  | Connect_failed reason -> Some (Http_error ("agent unreachable: " ^ reason))
  | Failed_after_send reason ->
      Some
        (Http_error ("connection failed after the request was sent: " ^ reason))
  | Truncated { bytes; reason } ->
      Some
        (Http_error
           (Printf.sprintf "response cut off after %d bytes: %s" bytes reason))

let ( let* ) = Result.bind

let send_http (h : http) msg =
  let* limit, timeout_error =
    step_limit ~clock:h.http_clock ~deadline:h.http_deadline
      ~turn_timeout:h.http_config.turn_timeout
  in
  let status = ref 0 and body = Buffer.create 256 in
  let id = h.http_config.run_id in
  let request : Upstream.request =
    {
      meth = "POST";
      url = h.http_config.url;
      headers =
        [
          ("Content-Type", "application/json");
          ("X-Bactrian-Run", id);
          (h.http_config.namespace_header, id);
        ];
      body = Agent_protocol.encode msg;
    }
  in
  let called =
    Eio.Switch.run @@ fun sw ->
    Upstream.call ~sw ~net:h.net ~clock:h.http_clock ~timeout:limit h.connector
      request
      ~on_head:(fun head -> status := head.status)
      ~on_chunk:(fun ~dt_ms:_ chunk -> Buffer.add_string body chunk)
  in
  let* () =
    Result.map_error
      (fun e -> Option.value (describe_upstream e) ~default:timeout_error)
      called
  in
  let body = Buffer.contents body in
  if !status < 200 || !status > 299 then
    Error (Http_status { status = !status; body = clip body })
  else
    match Agent_protocol.decode_array body with
    | Error error -> Error (Protocol_violation { line = clip body; error })
    | Ok messages ->
        List.iter (fun m -> Queue.add m h.pending) messages;
        Ok ()

let recv_http (h : http) =
  match Queue.take_opt h.pending with
  | Some m -> Ok m
  | None -> Error No_message

let pid = function Process p -> Some (Eio.Process.pid p.proc) | Http _ -> None

let send t msg =
  match t with Process p -> send_process p msg | Http h -> send_http h msg

let recv = function Process p -> recv_process p | Http h -> recv_http h

let with_http_agent ~net ~clock ?(connector = Upstream.plain) config f =
  let clock = (clock :> float Eio.Time.clock_ty Eio.Resource.t) in
  f
    (Http
       {
         net :> [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
         http_clock = clock;
         connector;
         http_config = config;
         http_deadline = Eio.Time.now clock +. config.total_timeout;
         pending = Queue.create ();
       })
