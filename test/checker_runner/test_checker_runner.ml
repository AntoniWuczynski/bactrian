open Bactrian_core
module R = Bactrian_runtime.Checker_runner
module Sandbox = Bactrian_runtime.Sandbox

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
allowed = {"PATH", "HOME", "TMPDIR", "LANG", "TZ", "BACTRIAN_RUN_ID", "LC_CTYPE", "__CF_USER_TEXT_ENCODING"}
extra = sorted(set(os.environ) - allowed)
ok = ("OPENAI_API_KEY" not in os.environ and not extra
      and os.environ.get("BACTRIAN_RUN_ID") == "run-42"
      and os.environ["HOME"] == os.environ["TMPDIR"] != os.environ.get("REAL_HOME"))
print(json.dumps({"pass": ok, "message": ",".join(extra)}))|}
    );
    ( "background.py",
      {|import subprocess
print('{"pass": true}', flush=True)
subprocess.Popen(["sleep", "30"])|}
    );
    ( "noisy.py",
      {|import sys
sys.stderr.write("first line\n" + "x" * 200000 + "\nTraceback: it broke\n")
sys.exit(2)|}
    );
    ( "no_read.py",
      {|import time
print('{"pass": true}', flush=True)
time.sleep(0.1)|} );
    (* Tries one thing and reports how it went: "ok", or the errno's name. *)
    ( "escape.py",
      {|import errno, json, os, socket, sys
def attempt(action, arg):
    try:
        if action == "connect":
            host, port = arg.rsplit(":", 1)
            socket.create_connection((host, int(port)), timeout=3).close()
        elif action == "write":
            with open(arg, "w") as f:
                f.write("x")
        elif action == "read":
            with open(arg) as f:
                f.read()
        elif action == "tmp":
            path = os.path.join(os.environ["TMPDIR"], "scratch")
            with open(path, "w") as f:
                f.write("x")
            return os.environ["TMPDIR"] + "|" + os.environ["HOME"]
        return "ok"
    except OSError as e:
        return errno.errorcode.get(e.errno, type(e).__name__) if e.errno else type(e).__name__
sys.stdin.read()
print(json.dumps({"pass": True, "message": attempt(sys.argv[1], sys.argv[2])}))|}
    );
  ]

(* Under the build directory, not the system's temporary directory, which the
   Linux sandbox replaces with an empty one. *)
let with_dir fn =
  Eio_main.run @@ fun env ->
  let dir = Filename.temp_dir ~temp_dir:(Sys.getcwd ()) "checker_runner" "" in
  List.iter
    (fun (name, body) ->
      Out_channel.with_open_text (Filename.concat dir name) (fun oc ->
          output_string oc body))
    scripts;
  Fun.protect
    ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / dir))
    (fun () -> fn env dir)

let run_with_stderr ?(timeout = 10.) ?(trace = entries) ?(sandbox = Sandbox.off)
    env dir script extra =
  R.run
    ~process_mgr:(Eio.Stdenv.process_mgr env)
    ~clock:(Eio.Stdenv.clock env) ~fs:(Eio.Stdenv.fs env) ~sandbox ~timeout
    ~argv:([ "python3"; "-I"; Filename.concat dir script ] @ extra)
    ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
    trace

let run ?timeout ?trace ?sandbox env dir script extra =
  fst (run_with_stderr ?timeout ?trace ?sandbox env dir script extra)

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
  (* Not passed on: the script compares HOME with it. *)
  Unix.putenv "REAL_HOME" (Sys.getenv "HOME");
  check_verdict
    { pass = true; score = None; message = Some "" }
    (run env dir "env.py" [])

let test_missing_command () =
  with_dir @@ fun env dir ->
  let r =
    R.run
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~clock:(Eio.Stdenv.clock env) ~fs:(Eio.Stdenv.fs env) ~sandbox:Sandbox.off
      ~timeout:5. ~argv:[ "/nonexistent/checker" ]
      ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
      entries
  in
  match fst r with
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

(* The leader printed its verdict and exited, but left a child holding
   stdout open. The group is killed when the leader exits, so the verdict
   arrives at once rather than when the child ends. *)
let test_background_child () =
  with_dir @@ fun env dir ->
  let started = Unix.gettimeofday () in
  check_verdict
    { pass = true; score = None; message = None }
    (run ~timeout:20. env dir "background.py" []);
  Alcotest.(check bool)
    "did not wait for the child" true
    (Unix.gettimeofday () -. started < 10.)

let test_stderr_captured () =
  with_dir @@ fun env dir ->
  let r, stderr = run_with_stderr env dir "noisy.py" [] in
  check_error "status 2 (stderr: Traceback: it broke)" r;
  Alcotest.(check int) "bounded to 64 KiB" (64 * 1024) (String.length stderr);
  Alcotest.(check bool)
    "keeps the end" true
    (String.ends_with ~suffix:"Traceback: it broke\n" stderr)

(* Inside the real sandbox (design §5.3), skipped on a host without one. *)

let macos = Sys.file_exists "/usr/bin/sandbox-exec"

let available =
  macos
  || List.exists
       (fun d -> Sys.file_exists (Filename.concat d "bwrap"))
       (String.split_on_char ':'
          (Option.value (Sys.getenv_opt "PATH") ~default:""))

let bactrian_bin = Filename.concat (Sys.getcwd ()) (Sys.getenv "BACTRIAN_BIN")

let strict env dir =
  Mirage_crypto_rng_unix.use_default ();
  match
    Sandbox.preflight ~env
      ~warn:(fun w -> Alcotest.failf "unexpected warning: %s" w)
      {
        mode = `Strict;
        allow_write = [];
        deny_read = [ Filename.concat dir "denied" ];
        bactrian_exe = bactrian_bin;
        bactrian_dir = Filename.concat dir ".bactrian";
      }
  with
  | Ok t -> t
  | Error m -> Alcotest.failf "preflight: %s" m

let message = function
  | R.Verdict { message = Some m; _ } -> m
  | (R.Verdict { message = None; _ } | R.Checker_error _) as r ->
      Alcotest.failf "no report: %a" pp_result r

let mkdirs path = ignore (Sys.command ("mkdir -p " ^ Filename.quote path) : int)

let write path text =
  mkdirs (Filename.dirname path);
  Out_channel.with_open_text path (fun oc -> output_string oc text)

(* Each attempt fails inside the sandbox with the error the platform gives,
   and does what it says without it. *)
let test_sandbox_denies () =
  if not available then Alcotest.skip ();
  with_dir @@ fun env dir ->
  let sandbox = strict env dir in
  let denied = Filename.concat dir "denied/secret"
  and other_run = Filename.concat dir ".bactrian/runs/other/trace.jsonl" in
  write denied "secret";
  write other_run "{}";
  let listener = Unix.socket PF_INET SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close listener) @@ fun () ->
  Unix.bind listener (ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen listener 8;
  let loopback =
    match Unix.getsockname listener with
    | ADDR_INET (_, port) -> Printf.sprintf "127.0.0.1:%d" port
    | ADDR_UNIX _ -> Alcotest.fail "the listener has no port"
  in
  let attempt ?sandbox action arg =
    message (run ?sandbox env dir "escape.py" [ action; arg ])
  in
  let either a b = if macos then a else b in
  let ok = String.equal "ok" in
  List.iter
    (fun (what, action, arg, inside, off) ->
      Alcotest.(check string)
        (what ^ ", sandboxed") inside
        (attempt ~sandbox action arg);
      let without = attempt action arg in
      Alcotest.(check bool) (what ^ ", off: " ^ without) true (off without))
    [
      (* Offline, an unsandboxed connection fails too, though never with
         Seatbelt's EPERM. *)
      ( "a public address",
        "connect",
        "1.1.1.1:443",
        either "EPERM" "ENETUNREACH",
        fun r -> not (macos && String.equal r "EPERM") );
      ( "a loopback listener",
        "connect",
        loopback,
        either "EPERM" "ECONNREFUSED",
        ok );
      ( "a write into the scenario directory",
        "write",
        Filename.concat dir "written",
        either "EPERM" "EROFS",
        ok );
      ("a deny_read path", "read", denied, either "EPERM" "ENOENT", ok);
      ("another run", "read", other_run, either "EPERM" "ENOENT", ok);
    ]

(* A checker that keeps to its temporary directory gives its verdict, and the
   directory is gone afterwards. *)
let test_sandbox_temp_dir () =
  if not available then Alcotest.skip ();
  with_dir @@ fun env dir ->
  let sandbox = strict env dir in
  match
    String.split_on_char '|'
      (message (run ~sandbox env dir "escape.py" [ "tmp"; "" ]))
  with
  | [ tmp; home ] ->
      Alcotest.(check string) "HOME is TMPDIR" tmp home;
      Alcotest.(check bool) "removed" false (Sys.file_exists tmp)
  | _ -> Alcotest.fail "expected TMPDIR|HOME"

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
          Alcotest.test_case "background child holding stdout" `Quick
            test_background_child;
          Alcotest.test_case "stderr captured and bounded" `Quick
            test_stderr_captured;
          Alcotest.test_case "checker ignoring stdin" `Quick
            test_checker_ignores_stdin;
        ] );
      ( "sandbox",
        [
          Alcotest.test_case "escapes fail" `Slow test_sandbox_denies;
          Alcotest.test_case "the temporary directory" `Slow
            test_sandbox_temp_dir;
        ] );
    ]
