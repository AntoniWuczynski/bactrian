module Trace = Bactrian_core.Trace
module Checker_protocol = Bactrian_core.Checker_protocol

type result = Verdict of Checker_protocol.verdict | Checker_error of string

let max_output = 1 lsl 20
let inherited = [ "PATH"; "HOME"; "LANG"; "TZ" ]

let run_id entries =
  List.find_map
    (fun ({ event; _ } : Trace.entry) ->
      match event with
      | Run_start { run_id; _ } -> Some run_id
      | Turn_start _ | Model_call _ | Tool_call _ | Tool_result _ | Http_call _
      | Agent_event _ | Reply _ | Sandbox_violation _ | Done ->
          None)
    entries

let scrubbed_env entries =
  let inherited =
    List.filter_map
      (fun k -> Option.map (fun v -> (k, v)) (Sys.getenv_opt k))
      inherited
  in
  Option.fold ~none:[]
    ~some:(fun id -> [ ("BACTRIAN_RUN_ID", id) ])
    (run_id entries)
  @ inherited

let kill_group clock pgid =
  ignore (Process_group.signal pgid Sys.sigkill : bool);
  ignore (Process_group.wait_gone ~clock ~timeout:5. pgid : bool)

let describe_status = function
  | `Exited code -> Printf.sprintf "exited with status %d" code
  | `Signaled signal -> Printf.sprintf "was killed by signal %d" signal

type outcome = Output of string | Too_long | Timed_out

let exchange ~clock ~timeout ~stdin ~stdout input =
  let read () =
    match
      Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:max_output stdout)
    with
    | text -> Output text
    | exception Eio.Buf_read.Buffer_limit_exceeded -> Too_long
  in
  (* A checker that never reads its stdin must not block the reader. *)
  let write () =
    (try Eio.Flow.copy_string input stdin with Eio.Io _ -> ());
    Eio.Resource.close stdin
  in
  let both () = fst (Eio.Fiber.pair read write) in
  match Eio.Time.with_timeout clock timeout (fun () -> Ok (both ())) with
  | Ok outcome -> outcome
  | Error `Timeout -> Timed_out

let spawn ~sw ~process_mgr ~cwd ~env ~argv ~stdin_r ~stdout_w =
  let fds =
    [
      (0, Eio_unix.Resource.fd stdin_r, `Blocking);
      (1, Eio_unix.Resource.fd stdout_w, `Blocking);
    ]
  in
  Eio_unix.Process.spawn_unix ~sw process_mgr ~cwd ~pgid:0 ~fds
    ~env:(Eio.Process.Env.of_bindings env)
    argv

let interpret ~timeout status = function
  | Timed_out ->
      Checker_error (Printf.sprintf "checker timed out after %gs" timeout)
  | Too_long -> Checker_error "checker wrote more than 1 MiB to stdout"
  | Output text -> (
      match status with
      | `Exited 0 -> (
          match Checker_protocol.decode_verdict text with
          | Ok verdict -> Verdict verdict
          | Error reason ->
              Checker_error ("checker printed an invalid verdict: " ^ reason))
      | (`Exited _ | `Signaled _) as s ->
          Checker_error ("checker " ^ describe_status s))

let run ~process_mgr ~clock ~timeout ~argv ~cwd entries =
  let clock = (clock :> float Eio.Time.clock_ty Eio.Resource.t) in
  Eio.Switch.run @@ fun sw ->
  match
    let stdin_r, stdin_w = Eio_unix.pipe sw in
    let stdout_r, stdout_w = Eio_unix.pipe sw in
    let proc =
      spawn ~sw ~process_mgr ~cwd ~env:(scrubbed_env entries) ~argv ~stdin_r
        ~stdout_w
    in
    (* The parent must drop the child's ends, or stdout never ends. *)
    Eio.Resource.close stdin_r;
    Eio.Resource.close stdout_w;
    (proc, stdin_w, stdout_r)
  with
  | exception (Eio.Io _ as e) ->
      Checker_error ("could not start checker: " ^ Printexc.to_string e)
  | exception (Unix.Unix_error _ as e) ->
      Checker_error ("could not start checker: " ^ Printexc.to_string e)
  | proc, stdin, stdout ->
      let pgid = Eio.Process.pid proc in
      let outcome =
        Fun.protect
          ~finally:(fun () ->
            Eio.Cancel.protect (fun () -> kill_group clock pgid))
          (fun () ->
            exchange ~clock ~timeout ~stdin ~stdout
              (Checker_protocol.trace_json entries))
      in
      Eio.Cancel.protect @@ fun () ->
      interpret ~timeout (Eio.Process.await proc) outcome
