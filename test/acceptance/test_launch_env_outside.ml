(* What another process of the same user can read: the launch environment of
   bactrian and of every process it starts, read from outside while a
   sandboxed run is live, with [ps -E] on macOS and /proc/PID/environ on Linux.
   A provider key bactrian holds (the sentinel) must be in none of them.

   The real binary is started as [bactrian replay] with the sentinel in its
   environment, which covers bactrian itself, the agent and, on Linux, the
   forwarder. An MCP stdio server and a checker are started by bactrian only in
   a live run, which this build cannot make without the TLS connector, so they
   come from a run through the library in this process, as in the other
   acceptance tests. A positive control shows that the method does see a
   sentinel in a process started with it. *)

open Bactrian_core
open Bactrian_runtime
module P = Model_proxy

let () = Mirage_crypto_rng_unix.use_default ()
let macos = Sys.file_exists "/usr/bin/sandbox-exec"

let executable variable =
  match Sys.getenv_opt variable with
  | Some p when Filename.is_relative p -> Filename.concat (Sys.getcwd ()) p
  | Some p -> p
  | None -> Alcotest.failf "%s is not set (run through dune)" variable

let sentinel = "sk-SENTINEL-" ^ Proc_util.hex (Mirage_crypto_rng.generate 12)

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let path_var = "PATH=" ^ Option.value (Sys.getenv_opt "PATH") ~default:"/bin"

(* Markers in argv tell the processes apart. *)
let agent_marker = "bactrian-outside-agent"
let mcp_marker = "bactrian-outside-mcp"
let checker_marker = "bactrian-outside-checker"

(* Replies once [GO_FILE] exists, after an MCP request when the run has a
   server and a [ready] file in its state directory. *)
let agent_script =
  {|
import json, os, sys, time, urllib.request
for line in sys.stdin:
    m = json.loads(line)
    if m["type"] == "turn":
        url = os.environ.get("BACTRIAN_MCP_URL")
        if url:
            body = json.dumps({"jsonrpc": "2.0", "id": 1, "method": "tools/list"})
            request = urllib.request.Request(url + "/probe", data=body.encode(),
                                             headers={"content-type": "application/json"})
            urllib.request.urlopen(request, timeout=20).read()
        open(os.path.join(os.environ["BACTRIAN_STATE_DIR"], "ready"), "w").close()
        while not os.path.exists(os.environ["GO_FILE"]):
            time.sleep(0.1)
        sys.stdout.write(json.dumps({"v": 1, "type": "reply", "turn": m["turn"],
                                     "content": "done"}) + "\n")
        sys.stdout.flush()
    elif m["type"] == "end":
        break
|}

let agent_argv = [ "python3"; "-I"; "-c"; agent_script; agent_marker ]

(* Answers every request with an empty tool list. *)
let mcp_script =
  {|
import json, sys
for line in sys.stdin:
    m = json.loads(line)
    if "id" in m:
        sys.stdout.write(json.dumps({"jsonrpc": "2.0", "id": m["id"],
                                     "result": {"tools": []}}) + "\n")
        sys.stdout.flush()
|}

(* Says it is ready, waits to be told to go, then passes. *)
let checker_script =
  {|
import os, sys, time
open(sys.argv[2], "w").close()
while not os.path.exists(sys.argv[3]):
    time.sleep(0.1)
sys.stdin.read()
print('{"pass": true}')
|}

(* Processes *)

type proc = { pid : int; ppid : int; command : string }

let processes mgr =
  Eio.Process.parse_out mgr ~env:[| path_var |] Eio.Buf_read.take_all
    [ "ps"; "-A"; "-ww"; "-o"; "pid="; "-o"; "ppid="; "-o"; "command=" ]
  |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
      match
        List.filter
          (fun w -> not (String.equal w ""))
          (String.split_on_char ' ' (String.trim line))
      with
      | pid :: ppid :: _ -> (
          match (int_of_string_opt pid, int_of_string_opt ppid) with
          | Some pid, Some ppid ->
              let rest = String.trim line in
              Some { pid; ppid; command = rest }
          | _ -> None)
      | _ -> None)

let rec descendants all pid =
  List.concat_map
    (fun p -> p :: descendants all p.pid)
    (List.filter (fun p -> p.ppid = pid) all)

(* The launch environment as the attacker sees it, or why it is unreadable. *)
let launch_env mgr pid =
  if macos then
    match
      Eio.Process.parse_out mgr ~env:[| path_var |] Eio.Buf_read.take_all
        [ "ps"; "-E"; "-ww"; "-p"; string_of_int pid; "-o"; "command=" ]
    with
    | text -> Ok text
    | exception Eio.Io _ -> Error "ps failed"
  else
    match
      In_channel.with_open_bin
        (Printf.sprintf "/proc/%d/environ" pid)
        In_channel.input_all
    with
    | text -> Ok text
    | exception Sys_error m -> Error m

let find what procs f =
  match List.filter f procs with
  | p :: _ -> p
  | [] -> Alcotest.failf "no %s among the processes" what

let has marker p = contains p.command marker
let forwarder p = has "__sandbox-init" p

(* Every process read must lack the sentinel. A role's process must be
   readable and hold [expect], which shows the read saw its environment. *)
let check_all mgr ~roles procs =
  List.iter
    (fun p ->
      match launch_env mgr p.pid with
      | Error why ->
          Printf.printf "pid %d unreadable (%s): %s\n" p.pid why p.command
      | Ok env ->
          Alcotest.(check bool)
            (Printf.sprintf "no sentinel in pid %d (%s)" p.pid p.command)
            false (contains env sentinel))
    procs;
  List.iter
    (fun (role, p, expect) ->
      match (launch_env mgr p.pid, expect) with
      | Ok env, Some needle ->
          Printf.printf "%s (pid %d): read, no sentinel\n" role p.pid;
          Alcotest.(check bool) (role ^ " was read") true (contains env needle)
      | Ok _, None ->
          Printf.printf "%s (pid %d): read, no sentinel\n" role p.pid
      | Error why, None ->
          Printf.printf "%s (pid %d): unreadable to the user (%s)\n" role p.pid
            why
      | Error why, Some _ -> Alcotest.failf "%s is unreadable: %s" role why)
    roles

let rec wait_for ~clock ~deadline what f =
  match f () with
  | Some v -> v
  | None ->
      if Eio.Time.now clock > deadline then Alcotest.failf "no %s in time" what
      else begin
        Eio.Time.sleep clock 0.1;
        wait_for ~clock ~deadline what f
      end

let ready_run ~except runs () =
  match Sys.readdir runs with
  | exception Sys_error _ -> None
  | ids ->
      Array.to_list ids
      |> List.find_opt (fun id ->
          (not (List.mem id except))
          && Sys.file_exists (String.concat "/" [ runs; id; "state"; "ready" ]))

let touch path = Out_channel.with_open_bin path (fun _ -> ())

(* The world *)

let with_dir name f =
  let dir = Filename.concat (Sys.getcwd ()) ("tmp-outside-" ^ name) in
  ignore (Sys.command ("rm -rf " ^ Filename.quote dir));
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command ("rm -rf " ^ Filename.quote dir)))
  @@ fun () -> f dir

let sandbox ~env ~dir =
  match
    Sandbox.preflight ~env
      ~warn:(fun w -> Alcotest.failf "warning: %s" w)
      {
        mode = `Strict;
        allow_write = [];
        deny_read = [];
        bactrian_exe = executable "BACTRIAN_EXE";
        bactrian_dir = Filename.concat dir ".bactrian";
      }
  with
  | Ok t -> t
  | Error m -> Alcotest.failf "sandbox preflight: %s" m

let config ~sandbox ~go ?(mcp = []) ?checks () : Run.config =
  {
    agent = Command agent_argv;
    cwd = ".";
    turns = [ "hello" ];
    scenario = "outside";
    variant = "base";
    sim_now = "";
    timezone = "UTC";
    replay = None;
    env_passthrough = [];
    variant_env = [ ("GO_FILE", go) ];
    fixtures = None;
    normalise = [];
    redact = [];
    pricing = Pricing.default;
    checks;
    unenforced = [];
    turn_timeout = 60.;
    total_timeout = 120.;
    mcp;
    sandbox;
    faketime = false;
  }

let execute ~env ~proxy ~store ~dir config =
  let ledger =
    Ledger.create
      ~path:
        Eio.Path.(
          Eio.Stdenv.fs env
          / Filename.concat dir ("ledger-" ^ config.Run.variant))
      ~caps:[ ("openai", 0.) ]
  in
  match
    Run.execute ~keys:[ (P.Openai, sentinel) ] ~env ~proxy ~store ~ledger config
  with
  | Ok r -> r
  | Error e -> Alcotest.failf "execute: %s" e

let outcome (r : Run.result) =
  Run.outcome_kind r.outcome
  ^ Option.fold ~none:""
      ~some:(fun s -> ": " ^ s)
      (Run.outcome_reason r.outcome)

(* The tests *)

let test_control () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  Eio.Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let proc =
    Eio.Process.spawn ~sw mgr ~stdin:r
      ~env:[| path_var; "CONTROL_KEY=" ^ sentinel |]
      [ "python3"; "-c"; "import sys; sys.stdin.read()" ]
  in
  Fun.protect ~finally:(fun () -> Eio.Resource.close w) @@ fun () ->
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.5;
  match launch_env mgr (Eio.Process.pid proc) with
  | Error why -> Alcotest.failf "the control is unreadable: %s" why
  | Ok text ->
      Alcotest.(check bool)
        "the method sees a sentinel that is there" true (contains text sentinel)

let skip_without_sandbox () =
  if
    not
      (macos
      || Option.is_some
           (Proc_util.find_executable ~path:(Sys.getenv_opt "PATH") ~cwd:"/"
              "bwrap"))
  then begin
    print_endline "no sandbox-exec or bwrap on this host: skipped";
    Alcotest.skip ()
  end

(* The real binary: bactrian replay, with the sentinel as OPENAI_API_KEY. *)
let test_bactrian () =
  skip_without_sandbox ();
  with_dir "cli" @@ fun dir ->
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let proxy =
    match P.start ~sw ~net:(Eio.Stdenv.net env) ~clock ~process_mgr:mgr with
    | Ok p -> p
    | Error e -> Alcotest.fail e
  in
  let store =
    Store.create ~root:Eio.Path.(Eio.Stdenv.fs env / dir / ".bactrian")
  in
  let early = Filename.concat dir "go-recording" in
  touch early;
  let recorded =
    execute ~env ~proxy ~store ~dir
      (config ~sandbox:(sandbox ~env ~dir) ~go:early ())
  in
  Alcotest.(check string) "the recording" "passed" (outcome recorded);
  let go = Filename.concat dir "go-replay" in
  let out = Filename.concat dir "replay.out" in
  let bactrian =
    Eio.Process.spawn ~sw mgr
      ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
      ~stdout:
        (Eio.Path.open_out ~sw ~create:(`Or_truncate 0o600)
           Eio.Path.(Eio.Stdenv.fs env / out))
      ~env:
        [|
          path_var;
          "HOME=" ^ Option.value (Sys.getenv_opt "HOME") ~default:"/";
          "OPENAI_API_KEY=" ^ sentinel;
          "GO_FILE=" ^ go;
        |]
      ([
         executable "BACTRIAN_EXE";
         "replay";
         recorded.run_id;
         "--sandbox";
         "strict";
         "--env";
         "GO_FILE";
         "--";
       ]
      @ agent_argv)
  in
  let pid = Eio.Process.pid bactrian in
  let runs = String.concat "/" [ dir; ".bactrian"; "runs" ] in
  ignore
    (wait_for ~clock
       ~deadline:(Eio.Time.now clock +. 60.)
       "replay agent"
       (ready_run ~except:[ recorded.run_id ] runs)
      : string);
  let all = processes mgr in
  let self = find "bactrian" all (fun p -> p.pid = pid) in
  let tree = self :: descendants all pid in
  let agent =
    find "agent" tree (fun p ->
        has agent_marker p
        && (not (forwarder p))
        && (not (has "bwrap" p))
        && not (has "replay" p))
  in
  let roles =
    [
      ("bactrian", self, Some "BACTRIAN_ENV_FD=");
      ("agent", agent, Some "BACTRIAN_RUN_ID=");
    ]
    @
    if macos then []
    else [ ("forwarder", find "forwarder" tree forwarder, None) ]
  in
  check_all mgr ~roles tree;
  Alcotest.(check bool)
    "bactrian's launch environment lost GO_FILE too" false
    (match launch_env mgr pid with
    | Ok e -> contains e "GO_FILE="
    | Error _ -> true);
  touch go;
  match Eio.Process.await bactrian with
  | `Exited 0 -> ()
  | `Exited n -> Alcotest.failf "bactrian replay exited %d" n
  | `Signaled n -> Alcotest.failf "bactrian replay killed by signal %d" n

(* An MCP stdio server and a checker, started by a run through the library. *)
let test_children () =
  skip_without_sandbox ();
  with_dir "children" @@ fun dir ->
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let proxy =
    match P.start ~sw ~net:(Eio.Stdenv.net env) ~clock ~process_mgr:mgr with
    | Ok p -> p
    | Error e -> Alcotest.fail e
  in
  let store =
    Store.create ~root:Eio.Path.(Eio.Stdenv.fs env / dir / ".bactrian")
  in
  let go = Filename.concat dir "go"
  and checker_ready = Filename.concat dir "checker-ready"
  and checker_go = Filename.concat dir "checker-go" in
  let mcp : Mcp_proxy.config list =
    [
      {
        name = "probe";
        server =
          Stdio
            {
              argv = [ "python3"; "-c"; mcp_script; mcp_marker ];
              env = [ ("MCP_MARK", "present") ];
              cwd = None;
            };
        policy = Passthrough;
      };
    ]
  and checks : Run.checks =
    {
      assertions = [];
      checkers =
        [
          [
            "python3";
            "-c";
            checker_script;
            checker_marker;
            checker_ready;
            checker_go;
          ];
        ];
      checks_cwd = dir;
      checker_timeout = 60.;
    }
  in
  let me = Unix.getpid () in
  let observe () =
    let runs = String.concat "/" [ dir; ".bactrian"; "runs" ] in
    ignore
      (wait_for ~clock
         ~deadline:(Eio.Time.now clock +. 60.)
         "agent"
         (ready_run ~except:[] runs)
        : string);
    let all = processes mgr in
    let tree = descendants all me in
    let roles =
      [
        ( "agent",
          find "agent" tree (fun p ->
              has agent_marker p && (not (forwarder p)) && not (has "bwrap" p)),
          Some "BACTRIAN_RUN_ID=" );
        ( "MCP server",
          find "MCP server" tree (has mcp_marker),
          Some "MCP_MARK=present" );
      ]
      @
      if macos then []
      else [ ("forwarder", find "forwarder" tree forwarder, None) ]
    in
    check_all mgr ~roles tree;
    touch go;
    wait_for ~clock
      ~deadline:(Eio.Time.now clock +. 60.)
      "checker"
      (fun () -> if Sys.file_exists checker_ready then Some () else None);
    let tree = descendants (processes mgr) me in
    check_all mgr
      ~roles:
        [
          ( "checker",
            find "checker" tree (has checker_marker),
            Some "BACTRIAN_RUN_ID=" );
        ]
      tree;
    touch checker_go
  in
  let result = ref None in
  Eio.Fiber.both
    (fun () ->
      result :=
        Some
          (execute ~env ~proxy ~store ~dir
             (config ~sandbox:(sandbox ~env ~dir) ~go ~mcp ~checks ())))
    observe;
  match !result with
  | Some r -> Alcotest.(check string) "the run" "passed" (outcome r)
  | None -> Alcotest.fail "the run did not finish"

let () =
  Alcotest.run "launch environment from outside"
    [
      ( "outside",
        [
          Alcotest.test_case "the method sees a sentinel" `Quick test_control;
          Alcotest.test_case "bactrian, its agent and forwarder" `Slow
            test_bactrian;
          Alcotest.test_case "an MCP server and a checker" `Slow test_children;
        ] );
    ]
