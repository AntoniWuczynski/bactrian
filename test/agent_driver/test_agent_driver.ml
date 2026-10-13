open Bactrian_core
module D = Bactrian_runtime.Agent_driver

let () = Mirage_crypto_rng_unix.use_default ()
let path_env = ("PATH", Sys.getenv "PATH")

let with_env fn =
  Eio_main.run @@ fun env ->
  let dir = Filename.temp_dir "agent_driver" "" in
  Fun.protect
    ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / dir))
    (fun () -> fn env dir)

let run_agent ?(env_vars = []) ?(turn_timeout = 5.) ?(total_timeout = 30.)
    ?(exit_grace = 0.3) ?(kill_grace = 0.3) ?(use_cwd = false) env dir argv f =
  let fs = Eio.Stdenv.fs env in
  let config : D.config =
    {
      argv;
      env = path_env :: env_vars;
      cwd = (if use_cwd then Some Eio.Path.(fs / dir) else None);
      stderr_path = Eio.Path.(fs / dir / "stderr.txt");
      turn_timeout;
      total_timeout;
      exit_grace;
      kill_grace;
    }
  in
  D.with_agent
    ~process_mgr:(Eio.Stdenv.process_mgr env)
    ~clock:(Eio.Stdenv.clock env) config f

let start =
  Agent_protocol.Start
    {
      run_id = "r";
      scenario = "s";
      variant = "v";
      sim_now = "2026-01-01T00:00:00Z";
      state_dir = "/tmp";
    }

let pp_error ppf (e : D.error) =
  match e with
  | Spawn_failed s -> Format.fprintf ppf "Spawn_failed %S" s
  | Write_failed s -> Format.fprintf ppf "Write_failed %S" s
  | Protocol_violation { line; error = _ } ->
      Format.fprintf ppf "Protocol_violation %S" line
  | Line_too_long -> Format.pp_print_string ppf "Line_too_long"
  | Turn_timeout -> Format.pp_print_string ppf "Turn_timeout"
  | Total_timeout -> Format.pp_print_string ppf "Total_timeout"
  | Agent_exited None -> Format.pp_print_string ppf "Agent_exited"
  | Agent_exited (Some (`Exited n)) ->
      Format.fprintf ppf "Agent_exited (Exited %d)" n
  | Agent_exited (Some (`Signaled n)) ->
      Format.fprintf ppf "Agent_exited (Signaled %d)" n

let show_msg : Agent_protocol.from_agent -> string = function
  | Reply { turn; content } -> Printf.sprintf "Reply %d %S" turn content
  | Event { turn; name; data = _ } -> Printf.sprintf "Event %d %s" turn name
  | Agent_error { turn; message } ->
      Printf.sprintf "Agent_error %d %S" turn message

let show r =
  match r with
  | Ok msg -> show_msg msg
  | Error e -> Format.asprintf "%a" pp_error e

let fail_error what e = Alcotest.failf "%s: %a" what pp_error e
let ok what = function Ok v -> v | Error e -> fail_error what e

let alive pid =
  match Unix.kill pid 0 with
  | () -> true
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false

let gone_within clock pid seconds =
  let rec poll n =
    if not (alive pid) then true
    else if n = 0 then false
    else (
      Eio.Time.sleep clock 0.05;
      poll (n - 1))
  in
  poll (int_of_float (seconds /. 0.05))

let python_agent =
  {|
import sys, json
for line in sys.stdin:
    m = json.loads(line)
    if m["type"] == "turn":
        print(json.dumps({"v": 1, "type": "reply", "turn": m["turn"], "content": "echo " + m["content"]}), flush=True)
    elif m["type"] == "end":
        break
|}

let test_two_turns () =
  with_env @@ fun env dir ->
  let turn t (d : D.t) content =
    ok "send" (D.send d (Agent_protocol.Turn { turn = t; content }));
    ok "recv" (D.recv d)
  in
  let (r1, r2), status =
    ok "run"
      (run_agent env dir [ "python3"; "-I"; "-c"; python_agent ] (fun d ->
           ok "send" (D.send d start);
           let r1 = turn 1 d "hello" in
           let r2 = turn 2 d "again" in
           ok "send" (D.send d Agent_protocol.End);
           (r1, r2)))
  in
  Alcotest.(check string) "turn 1" {|Reply 1 "echo hello"|} (show_msg r1);
  Alcotest.(check string) "turn 2" {|Reply 2 "echo again"|} (show_msg r2);
  Alcotest.(check bool) "clean exit" true (status = `Exited 0)

let test_garbage () =
  with_env @@ fun env dir ->
  let r, _ =
    ok "run"
      (run_agent env dir
         [ "sh"; "-c"; "echo this is not json; sleep 300" ]
         D.recv)
  in
  Alcotest.(check string)
    "offending line" {|Protocol_violation "this is not json"|} (show r)

let test_early_exit () =
  with_env @@ fun env dir ->
  let r, _ = ok "run" (run_agent env dir [ "sh"; "-c"; "exit 3" ] D.recv) in
  Alcotest.(check string) "exit seen" "Agent_exited (Exited 3)" (show r)

let test_timeouts () =
  with_env @@ fun env dir ->
  let silent = [ "sh"; "-c"; "sleep 300" ] in
  let r, _ = ok "run" (run_agent ~turn_timeout:0.3 env dir silent D.recv) in
  Alcotest.(check string) "turn" "Turn_timeout" (show r);
  let r, _ =
    ok "run"
      (run_agent ~turn_timeout:10. ~total_timeout:0.3 env dir silent D.recv)
  in
  Alcotest.(check string) "total" "Total_timeout" (show r)

(* The agent ignores TERM and end, and leaves a TERM-ignoring child behind, so only
   SIGKILL to the whole group removes both. *)
let stubborn =
  {|trap '' TERM; sleep 300 & echo $! > "$DIR/child.pid"; echo $$ > "$DIR/leader.pid"; while :; do sleep 1; done|}

let read_pid dir name =
  In_channel.with_open_text (Filename.concat dir name) (fun ic ->
      int_of_string (String.trim (input_line ic)))

let test_kills_group () =
  with_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let pids, _ =
    ok "run"
      (run_agent
         ~env_vars:[ ("DIR", dir) ]
         env dir [ "sh"; "-c"; stubborn ]
         (fun d ->
           let rec wait n =
             if
               Sys.file_exists (Filename.concat dir "leader.pid")
               && Sys.file_exists (Filename.concat dir "child.pid")
             then ()
             else if n = 0 then Alcotest.fail "agent never started"
             else (
               Eio.Time.sleep clock 0.05;
               wait (n - 1))
           in
           wait 100;
           Eio.Time.sleep clock 0.1;
           ok "send" (D.send d Agent_protocol.End);
           (D.pid d, read_pid dir "child.pid")))
  in
  let leader, child = (fst pids, snd pids) in
  Alcotest.(check bool) "leader is gone" false (alive leader);
  Alcotest.(check bool) "child is gone" true (gone_within clock child 2.)

let test_stderr_env_cwd () =
  with_env @@ fun env dir ->
  let _ =
    ok "run"
      (run_agent
         ~env_vars:[ ("FOO", "bar") ]
         ~use_cwd:true env dir
         [ "sh"; "-c"; "echo \"err $FOO [$HOME]\" >&2; echo x > marker" ]
         (fun d -> ignore (D.recv d)))
  in
  let stderr_text =
    In_channel.with_open_text
      (Filename.concat dir "stderr.txt")
      In_channel.input_all
  in
  Alcotest.(check string)
    "stderr captured, env not inherited" "err bar []\n" stderr_text;
  Alcotest.(check bool)
    "cwd honoured" true
    (Sys.file_exists (Filename.concat dir "marker"))

let test_spawn_failure () =
  with_env @@ fun env dir ->
  let r = run_agent env dir [ "/nonexistent/agent" ] (fun _ -> ()) in
  let prefix =
    match r with
    | Ok _ -> "Ok"
    | Error e -> String.sub (Format.asprintf "%a" pp_error e) 0 13
  in
  Alcotest.(check string) "spawn error" "Spawn_failed " prefix

let () =
  Alcotest.run "agent_driver"
    [
      ( "driver",
        [
          Alcotest.test_case "two turns" `Quick test_two_turns;
          Alcotest.test_case "garbage output" `Quick test_garbage;
          Alcotest.test_case "early exit" `Quick test_early_exit;
          Alcotest.test_case "timeouts" `Quick test_timeouts;
          Alcotest.test_case "kills the process group" `Quick test_kills_group;
          Alcotest.test_case "stderr, env, cwd" `Quick test_stderr_env_cwd;
          Alcotest.test_case "spawn failure" `Quick test_spawn_failure;
        ] );
    ]
