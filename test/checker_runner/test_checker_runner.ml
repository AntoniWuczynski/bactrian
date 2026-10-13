open Bactrian_core
module R = Bactrian_runtime.Checker_runner

let entries : Trace.entry list =
  [
    {
      seq = 0;
      t_ms = 0;
      event =
        Run_start
          {
            run_id = "run-42";
            scenario = "s";
            variant = "v";
            sim_now = "2026-01-01T00:00:00Z";
            clock = Contract_only;
          };
    };
    { seq = 1; t_ms = 5; event = Turn_start { turn = 0; content = "hi" } };
    { seq = 2; t_ms = 9; event = Done };
  ]

let pp_result ppf = function
  | R.Verdict { pass; score; message } ->
      Format.fprintf ppf "Verdict(%b,%s,%s)" pass
        (Option.fold ~none:"-" ~some:string_of_float score)
        (Option.value ~default:"-" message)
  | R.Checker_error m -> Format.fprintf ppf "Checker_error %S" m

let is_error_containing needle = function
  | R.Checker_error m ->
      let n = String.length needle and l = String.length m in
      let rec at i = i + n <= l && (String.sub m i n = needle || at (i + 1)) in
      at 0
  | R.Verdict _ -> false

let scripts =
  [
    ( "pass.py",
      {|import json, sys
t = json.load(sys.stdin)
ok = len(t) == 3 and t[0]["type"] == "run_start"
print(json.dumps({"pass": ok}))|}
    );
    ( "fail.py",
      {|import json
print(json.dumps({"pass": False, "score": 0.25, "message": "double booked"}))|}
    );
    ( "hang.py",
      {|import os, time, subprocess, sys
child = subprocess.Popen(["sleep", "300"])
open(sys.argv[1], "w").write(str(child.pid))
time.sleep(300)|}
    );
    ("exit1.py", {|import sys
sys.stderr.write("boom")
sys.exit(3)|});
    ("garbage.py", {|print("this is not json")|});
    ("unknown_field.py", {|print('{"pass": true, "extra": 1}')|});
    ("big.py", {|import sys
sys.stdout.write("x" * (2 * 1024 * 1024))|});
    ( "env.py",
      {|import os, json
# LC_CTYPE and __CF_USER_TEXT_ENCODING are added by the interpreter itself
allowed = {"PATH", "HOME", "LANG", "TZ", "BACTRIAN_RUN_ID", "LC_CTYPE", "__CF_USER_TEXT_ENCODING"}
extra = sorted(set(os.environ) - allowed)
ok = "OPENAI_API_KEY" not in os.environ and not extra and os.environ.get("BACTRIAN_RUN_ID") == "run-42"
print(json.dumps({"pass": ok, "message": ",".join(extra)}))|}
    );
    ( "no_read.py",
      {|import time
print('{"pass": true}', flush=True)
time.sleep(0.1)|} );
  ]

let with_dir fn =
  Eio_main.run @@ fun env ->
  let dir = Filename.temp_dir "checker_runner" "" in
  List.iter
    (fun (name, body) ->
      Out_channel.with_open_text (Filename.concat dir name) (fun oc ->
          output_string oc body))
    scripts;
  Fun.protect
    ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / dir))
    (fun () -> fn env dir)

let run ?(timeout = 10.) ?(trace = entries) env dir script extra =
  R.run
    ~process_mgr:(Eio.Stdenv.process_mgr env)
    ~clock:(Eio.Stdenv.clock env) ~timeout
    ~argv:([ "python3"; "-I"; Filename.concat dir script ] @ extra)
    ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
    trace

let check_verdict expected r =
  match r with
  | R.Verdict v -> Alcotest.(check bool) "verdict" true (v = expected)
  | R.Checker_error _ -> Alcotest.failf "unexpected %a" pp_result r

let check_error needle r =
  Alcotest.(check bool)
    (Format.asprintf "error containing %S, got %a" needle pp_result r)
    true
    (is_error_containing needle r)

let test_pass () =
  with_dir @@ fun env dir ->
  check_verdict
    { pass = true; score = None; message = None }
    (run env dir "pass.py" [])

let test_fail () =
  with_dir @@ fun env dir ->
  check_verdict
    { pass = false; score = Some 0.25; message = Some "double booked" }
    (run env dir "fail.py" [])

let pid_gone pid =
  match Unix.kill pid 0 with
  | () -> false
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> true

let test_hang () =
  with_dir @@ fun env dir ->
  let pidfile = Filename.concat dir "child.pid" in
  let started = Unix.gettimeofday () in
  let r = run ~timeout:1.5 env dir "hang.py" [ pidfile ] in
  check_error "timed out" r;
  Alcotest.(check bool)
    "returned promptly" true
    (Unix.gettimeofday () -. started < 8.);
  let child =
    int_of_string (In_channel.with_open_text pidfile In_channel.input_all)
  in
  Alcotest.(check bool) "grandchild killed" true (pid_gone child)

let test_exit_nonzero () =
  with_dir @@ fun env dir -> check_error "status 3" (run env dir "exit1.py" [])

let test_garbage () =
  with_dir @@ fun env dir ->
  check_error "invalid verdict" (run env dir "garbage.py" []);
  check_error "unknown field" (run env dir "unknown_field.py" [])

let test_too_much () =
  with_dir @@ fun env dir -> check_error "1 MiB" (run env dir "big.py" [])

let test_env () =
  Unix.putenv "OPENAI_API_KEY" "sk-secret";
  Unix.putenv "HTTPS_PROXY" "http://127.0.0.1:1";
  with_dir @@ fun env dir ->
  check_verdict
    { pass = true; score = None; message = Some "" }
    (run env dir "env.py" [])

let test_missing_command () =
  with_dir @@ fun env dir ->
  let r =
    R.run
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~clock:(Eio.Stdenv.clock env) ~timeout:5. ~argv:[ "/nonexistent/checker" ]
      ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
      entries
  in
  match r with
  | R.Checker_error _ -> ()
  | R.Verdict _ -> Alcotest.fail "expected an error"

let test_checker_ignores_stdin () =
  with_dir @@ fun env dir ->
  let big =
    List.init 20000 (fun i : Trace.entry ->
        {
          seq = i;
          t_ms = i;
          event = Reply { turn = 0; content = String.make 100 'a' };
        })
  in
  check_verdict
    { pass = true; score = None; message = None }
    (run ~trace:(entries @ big) env dir "no_read.py" [])

let () =
  Alcotest.run "checker_runner"
    [
      ( "run",
        [
          Alcotest.test_case "passing checker" `Quick test_pass;
          Alcotest.test_case "failing checker" `Quick test_fail;
          Alcotest.test_case "hang is killed with its group" `Quick test_hang;
          Alcotest.test_case "non-zero exit" `Quick test_exit_nonzero;
          Alcotest.test_case "garbage output" `Quick test_garbage;
          Alcotest.test_case "too much output" `Quick test_too_much;
          Alcotest.test_case "scrubbed environment" `Quick test_env;
          Alcotest.test_case "missing command" `Quick test_missing_command;
          Alcotest.test_case "checker ignoring stdin" `Quick
            test_checker_ignores_stdin;
        ] );
    ]
