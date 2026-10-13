(* Runs inside the real operating-system sandbox (design §5.3): sandbox-exec on
   macOS, bubblewrap on Linux. Skipped, with a message, on a host that has
   neither. *)

open Bactrian_core
open Bactrian_runtime
open Run_support
module F = Fake_upstream

let on_path name =
  List.exists
    (fun dir ->
      (not (String.equal dir "")) && Sys.file_exists (Filename.concat dir name))
    (String.split_on_char ':'
       (Option.value (Sys.getenv_opt "PATH") ~default:""))

let macos = Sys.file_exists "/usr/bin/sandbox-exec"
let available = macos || on_path "bwrap"

let sandboxed name f () =
  if available then with_ctx name f ()
  else begin
    print_endline "no sandbox-exec or bwrap on this host: skipped";
    Alcotest.skip ()
  end

let root ctx = Filename.concat (Eio.Path.native_exn ctx.dir) ".bactrian"

let strict ?(deny_read = []) ctx =
  match
    Sandbox.preflight ~env:ctx.env
      ~warn:(fun w -> Alcotest.failf "unexpected warning: %s" w)
      {
        mode = `Strict;
        allow_write = [];
        deny_read;
        bactrian_exe = bactrian_bin;
        bactrian_dir = root ctx;
      }
  with
  | Ok s -> s
  | Error m -> Alcotest.failf "preflight: %s" m

let meta (r : Run.result) = Yojson.Safe.from_file (file r "meta.json")

let test_record_and_replay =
  sandboxed "sandbox-cycle" @@ fun ctx ->
  let sandbox = strict ctx in
  let recorded = ok (execute ctx { (config ()) with sandbox }) in
  Alcotest.(check string) "record outcome" "passed" (outcome_name recorded);
  Alcotest.(check string)
    "labelled" {|{"mode":"strict","applied":true}|}
    (Yojson.Safe.to_string (Yojson.Safe.Util.member "sandbox" (meta recorded)));
  let env = read (file recorded "state/env.json") in
  Alcotest.(check bool)
    "real key never inside" false
    (contains env real_openai_key);
  let replay : Run.replay =
    {
      from = recorded.run_id;
      matcher = Strict;
      allow_reorder = false;
      timing = Fast;
    }
  in
  let replayed =
    ok (execute ctx { (config ~replay ~turns:[] ()) with sandbox })
  in
  Alcotest.(check string) "replay outcome" "passed" (outcome_name replayed);
  Alcotest.(check int)
    "no divergences" 0
    (List.length replayed.findings.divergences);
  Alcotest.(check int) "upstream untouched" 2 (List.length (F.requests ctx.up))

(* An agent that tries to leave: each attempt's result is its reply. *)
let escape_agent =
  {|
import json, os, socket, sys

def send(m):
    sys.stdout.write(json.dumps(m) + "\n")
    sys.stdout.flush()

def attempt(f):
    try:
        f()
        return "ok"
    except OSError:
        return "denied"

def public():
    socket.create_connection(("1.1.1.1", 443), timeout=5).close()

def home():
    open(os.environ["REAL_HOME_FILE"], "w").close()

def secret():
    open(os.environ["SECRET_FILE"]).read()

def masked():
    open(os.path.join(os.path.dirname(os.environ["SECRET_FILE"]), "new"), "w").close()

def state():
    open(os.path.join(os.environ["BACTRIAN_STATE_DIR"], "ok"), "w").close()

def other():
    reached = []
    for port in os.environ["OTHER_PORTS"].split(","):
        try:
            socket.create_connection(("127.0.0.1", int(port)), timeout=5).close()
            reached.append(port)
        except OSError:
            pass
    if not reached:
        raise OSError("no other port reached")

for line in sys.stdin:
    m = json.loads(line)
    if m["type"] == "turn":
        result = {"public": attempt(public), "home": attempt(home),
                  "secret": attempt(secret), "masked": attempt(masked),
                  "state": attempt(state), "other": attempt(other)}
        send({"v": 1, "type": "reply", "turn": m["turn"],
              "content": json.dumps(result, sort_keys=True)})
    elif m["type"] == "end":
        break
|}

let real_home () =
  match Sys.getenv_opt "HOME" with Some h -> h | None -> Alcotest.fail "HOME"

let test_escapes_fail =
  sandboxed "sandbox-escape" @@ fun ctx ->
  let secret_dir =
    Filename.concat
      (Filename.concat (Sys.getcwd ()) (Eio.Path.native_exn ctx.dir))
      "secret"
  in
  Unix.mkdir secret_dir 0o700;
  let secret = Filename.concat secret_dir "key" in
  Out_channel.with_open_bin secret (fun oc -> output_string oc "s3cret");
  let home_file =
    Filename.concat (real_home ())
      (Printf.sprintf ".bactrian-sandbox-test-%d" (Unix.getpid ()))
  in
  Fun.protect ~finally:(fun () ->
      if Sys.file_exists home_file then Sys.remove home_file)
  @@ fun () ->
  let sandbox = strict ~deny_read:[ secret_dir ] ctx in
  Eio.Switch.run @@ fun sw ->
  (* Another run's own port, and on macOS the port every run shares, which
     answers any registered token. A token read from another run's launch
     environment is of no use if neither can be reached. *)
  let other_port =
    match
      P.listen_tcp ctx.proxy ~sw ~net:(Eio.Stdenv.net ctx.env)
        ~token:"another-run"
    with
    | Ok p -> p
    | Error m -> Alcotest.failf "listen: %s" m
  in
  let other_ports =
    other_port :: (if macos then [ P.port ctx.proxy ] else [])
  in
  let r =
    ok
      (execute ctx
         {
           (config ~agent:[ "python3"; "-I"; "-c"; escape_agent ] ()) with
           sandbox;
           variant_env =
             [
               ("REAL_HOME_FILE", home_file);
               ("SECRET_FILE", secret);
               ( "OTHER_PORTS",
                 String.concat "," (List.map string_of_int other_ports) );
             ];
         })
  in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  let reply =
    List.find_map
      (fun l ->
        let j = Yojson.Safe.from_string l in
        match Yojson.Safe.Util.(to_string (member "type" j)) with
        | "reply" -> Some Yojson.Safe.Util.(to_string (member "content" j))
        | _ -> None)
      (lines (file r "trace.jsonl"))
  in
  Alcotest.(check (option string))
    "every escape denied, the state dir writable"
    (Some
       {|{"home": "denied", "masked": "denied", "other": "denied", "public": "denied", "secret": "denied", "state": "ok"}|})
    reply;
  Alcotest.(check bool) "nothing written home" false (Sys.file_exists home_file);
  Alcotest.(check bool)
    "nothing written in the masked directory" false
    (Sys.file_exists (Filename.concat secret_dir "new"));
  match r.violations with
  | Not_collected m -> Alcotest.failf "violations not collected: %s" m
  | Collected { items = vs; coverage = Seatbelt_log; gaps } ->
      Alcotest.(check bool) "labelled a lower bound" true (gaps <> []);
      List.iter
        (fun (v : Sandbox_log.violation) ->
          Printf.printf "violation: %s %s\n" v.kind v.detail)
        vs;
      let seen kind needle =
        List.exists
          (fun (v : Sandbox_log.violation) ->
            String.equal v.kind kind && contains v.detail needle)
          vs
      in
      Alcotest.(check bool) "network" true (seen "network-outbound" ":443");
      Alcotest.(check bool)
        "home write" true
        (seen "file-write-create" (Filename.basename home_file));
      Alcotest.(check bool)
        "secret read" true
        (seen "file-read-data" "/secret/key");
      Alcotest.(check bool)
        "another run's port" true
        (seen "network-outbound" (Printf.sprintf ":%d" other_port));
      Alcotest.(check bool)
        "in the trace" true
        (List.mem "sandbox_violation" (trace_types r))
  | Collected { items = vs; coverage = Relay_failures_only; gaps = _ } ->
      (* Linux reports only relay failures, and these attempts make none. *)
      Alcotest.(check int) "no relay failures" 0 (List.length vs)

(* ~/.aws and the project's .env as symbolic links (a dotfile manager, a .env
   shared between projects): the sandbox denies what they lead to, read through
   the link or through the target. On Linux a masked file reads as empty. *)
let reader_agent =
  {|
import json, os, sys
names = os.environ["READ_NAMES"].split(",")
def read(path):
    try:
        with open(path) as f:
            return f.read()
    except OSError:
        return "denied"
for line in sys.stdin:
    m = json.loads(line)
    if m["type"] == "turn":
        result = {n: read(os.environ[n]) for n in names}
        sys.stdout.write(json.dumps({"v": 1, "type": "reply", "turn": m["turn"],
                                     "content": json.dumps(result)}) + "\n")
        sys.stdout.flush()
    elif m["type"] == "end":
        break
|}

let test_linked_credentials =
  sandboxed "sandbox-linked" @@ fun ctx ->
  let dir = Filename.concat (Sys.getcwd ()) (Eio.Path.native_exn ctx.dir) in
  let under = Filename.concat dir in
  let write path text =
    Out_channel.with_open_bin path (fun oc -> output_string oc text)
  in
  List.iter (fun d -> Unix.mkdir (under d) 0o700) [ "home"; "creds"; "shared" ];
  write (under "creds/key") "aws-s3cret";
  write (under "shared/app.env") "env-s3cret";
  (* An absolute link and a relative one. *)
  Unix.symlink (under "creds") (under "home/.aws");
  Unix.symlink "shared/app.env" (under ".env");
  let previous = Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" (under "home");
  let sandbox =
    Fun.protect
      ~finally:(fun () ->
        Unix.putenv "HOME" (Option.value previous ~default:""))
      (fun () -> strict ctx)
  in
  let paths =
    [
      ("AWS_LINK", under "home/.aws/key");
      ("AWS_TARGET", under "creds/key");
      ("ENV_LINK", under ".env");
      ("ENV_TARGET", under "shared/app.env");
    ]
  in
  let r =
    ok
      (execute ctx
         {
           (config ~agent:[ "python3"; "-I"; "-c"; reader_agent ] ()) with
           sandbox;
           variant_env =
             ("READ_NAMES", String.concat "," (List.map fst paths)) :: paths;
         })
  in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  let reply =
    List.find_map
      (fun l ->
        let j = Yojson.Safe.from_string l in
        match Yojson.Safe.Util.(to_string (member "type" j)) with
        | "reply" -> Some Yojson.Safe.Util.(to_string (member "content" j))
        | _ -> None)
      (lines (file r "trace.jsonl"))
  in
  match reply with
  | None -> Alcotest.fail "no reply"
  | Some reply ->
      print_endline reply;
      List.iter
        (fun (name, _) ->
          Alcotest.(check bool)
            (name ^ " is not readable")
            false
            (contains
               Yojson.Safe.Util.(
                 to_string (member name (Yojson.Safe.from_string reply)))
               "s3cret"))
        paths

(* On macOS the agent can read a stdio server's command line and environment,
   so a server that has either is named in a warning. *)
let test_mcp_warning =
  sandboxed "sandbox-mcp-warning" @@ fun ctx ->
  let sandbox = strict ctx in
  let server ?(env = []) ?(policy = Mcp_proxy.Record) name argv :
      Mcp_proxy.config =
    { name; server = Stdio { argv; env; cwd = None }; policy }
  in
  let warning servers = Sandbox.mcp_warning sandbox servers in
  Alcotest.(check (option string))
    "a bare command" None
    (warning [ server "plain" [ "server" ] ]);
  Alcotest.(check (option string))
    "unsandboxed" None
    (Sandbox.mcp_warning Sandbox.off
       [ server ~env:[ ("TOKEN", "t") ] "gh" [ "server" ] ]);
  Alcotest.(check (option string))
    "a server that only replays" None
    (warning
       [ server ~policy:Replay ~env:[ ("TOKEN", "t") ] "gh" [ "server" ] ]);
  let named =
    warning
      [
        server ~env:[ ("GITHUB_TOKEN", "t") ] "github" [ "server" ];
        server "files" [ "server"; "--root"; "/data" ];
      ]
  in
  if macos then
    Alcotest.(check bool)
      "names both servers" true
      (match named with
      | Some m -> contains m "servers github, files"
      | None -> false)
  else Alcotest.(check (option string)) "Linux" None named

(* The preload reaches the agent through bubblewrap and the forwarder, and
   faking the forwarder's clock harms nothing (design §5 step 2). *)
let test_clock =
  sandboxed "sandbox-clock" @@ fun ctx ->
  Clock_support.skip_unless
    (Clock_support.usable "python3")
    "python3 is not usable: skipped";
  Clock_support.skip_unless
    ((not Clock_support.linux) || Option.is_some Clock_support.library)
    "libfaketimeMT.so.1 is not installed: skipped";
  Clock_support.check_clock
    (Clock_support.run_agent ~sandbox:(strict ctx) ctx
       [ "python3"; "-I"; "-c"; Clock_support.python_agent ])

(* The sandbox mounts a fresh tmpfs over /tmp, /dev/shm and /run, so a library
   there is bound in at its own path. Linux only. *)
let with_library_under_tmp f =
  Clock_support.skip_unless Clock_support.linux "not Linux: skipped";
  Clock_support.skip_unless
    (Option.is_some Clock_support.library)
    "libfaketimeMT.so.1 is not installed: skipped";
  let dir = Filename.temp_dir ~temp_dir:"/tmp" "bactrian-faketime-" "" in
  let copy = Filename.concat dir "libfaketimeMT.so.1" in
  Out_channel.with_open_bin copy (fun oc ->
      output_string oc (read (Option.get Clock_support.library)));
  Unix.putenv Agent_clock.override_variable copy;
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv Agent_clock.override_variable "";
      Sys.remove copy;
      Unix.rmdir dir)
    (fun () -> f ~dir ~copy)

let test_clock_library_under_tmp =
  sandboxed "sandbox-clock-tmp" @@ fun ctx ->
  Clock_support.skip_unless
    (Clock_support.usable "python3")
    "python3 is not usable: skipped";
  with_library_under_tmp @@ fun ~dir:_ ~copy ->
  let r =
    Clock_support.run_agent ~sandbox:(strict ctx) ctx
      [ "python3"; "-I"; "-c"; Clock_support.python_agent ]
  in
  Clock_support.check_clock r;
  Alcotest.(check bool)
    "the library under /tmp is the one preloaded" true
    (contains
       Yojson.Safe.Util.(to_string (member "preload" (Clock_support.reply r)))
       copy)

(* A library under a path the sandbox masks cannot be seen by the agent, so
   the run does not claim an enforced clock. *)
let test_clock_library_hidden =
  sandboxed "sandbox-clock-hidden" @@ fun ctx ->
  Clock_support.skip_unless
    (Clock_support.usable "python3")
    "python3 is not usable: skipped";
  with_library_under_tmp @@ fun ~dir ~copy:_ ->
  let r =
    Clock_support.run_agent
      ~sandbox:(strict ~deny_read:[ dir ] ctx)
      ctx
      [ "python3"; "-I"; "-c"; Clock_support.python_agent ]
  in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  Alcotest.(check string)
    "label" {|{"mode":"contract_only","reason":"library-hidden"}|}
    (Clock_support.meta_clock r);
  Alcotest.(check string)
    "nothing is preloaded" ""
    Yojson.Safe.Util.(to_string (member "preload" (Clock_support.reply r)))

(* SIGINT to a sandboxed replay: bactrian exits 130 and nothing of the agent
   survives. The agent's own pid is no help, since on Linux it lives in another
   pid namespace, so its processes are found by a marker in their command
   line. *)
(* Eio's own signal handling interrupts a blocking waitpid, so poll. *)
let rec wait_exit pid =
  match Unix.waitpid [ WNOHANG ] pid with
  | 0, _ ->
      Unix.sleepf 0.05;
      wait_exit pid
  | _, status -> status
  | exception Unix.Unix_error (EINTR, _, _) -> wait_exit pid

let still_running marker =
  let devnull = Unix.openfile "/dev/null" [ O_RDWR ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close devnull) @@ fun () ->
  let pid =
    Unix.create_process "pgrep"
      [| "pgrep"; "-f"; marker |]
      devnull devnull devnull
  in
  match wait_exit pid with
  | WEXITED 0 -> true
  | WEXITED _ | WSIGNALED _ | WSTOPPED _ -> false

let test_interrupt =
  sandboxed "sandbox-interrupt" @@ fun ctx ->
  let recorded = ok (execute ctx (config ())) in
  let cwd = Filename.concat (Sys.getcwd ()) (Eio.Path.native_exn ctx.dir) in
  let runs = Filename.concat (Filename.concat cwd ".bactrian") "runs" in
  let marker = Printf.sprintf "bactrian-hung-agent-%d" (Unix.getpid ()) in
  let agent =
    String.concat " "
      (List.map Filename.quote
         [
           "sh";
           "-c";
           {|touch "$BACTRIAN_STATE_DIR/started"; while :; do sleep 1; done|};
           marker;
         ])
  in
  let pid =
    let devnull = Unix.openfile "/dev/null" [ O_RDWR ] 0 in
    Fun.protect ~finally:(fun () -> Unix.close devnull) @@ fun () ->
    Unix.create_process "sh"
      [|
        "sh";
        "-c";
        Printf.sprintf "cd %s && exec %s replay %s --sandbox strict -- %s"
          (Filename.quote cwd)
          (Filename.quote bactrian_bin)
          recorded.run_id agent;
      |]
      devnull devnull devnull
  in
  let deadline = Unix.gettimeofday () +. 20. in
  let rec started () =
    match
      List.find_opt
        (fun id ->
          Sys.file_exists (String.concat "/" [ runs; id; "state"; "started" ]))
        (Array.to_list (Sys.readdir runs))
    with
    | Some id -> id
    | None ->
        if Unix.gettimeofday () > deadline then begin
          Unix.kill pid Sys.sigkill;
          Alcotest.fail "the sandboxed agent did not start"
        end
        else begin
          Unix.sleepf 0.1;
          started ()
        end
  in
  let run_id = started () in
  Alcotest.(check bool) "the agent runs" true (still_running marker);
  Unix.kill pid Sys.sigint;
  (match wait_exit pid with
  | WEXITED n -> Alcotest.(check int) "exit status" 130 n
  | WSIGNALED n | WSTOPPED n -> Alcotest.failf "signal %d" n);
  let rec gone n =
    if not (still_running marker) then true
    else if n = 0 then false
    else begin
      Unix.sleepf 0.2;
      gone (n - 1)
    end
  in
  Alcotest.(check bool) "nothing of the agent survives" true (gone 30);
  let meta =
    Yojson.Safe.from_file (String.concat "/" [ runs; run_id; "meta.json" ])
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "reason" "interrupted"
    (to_string (member "reason" meta));
  Alcotest.(check bool)
    "sandboxed" true
    (to_bool (member "applied" (member "sandbox" meta)))

(* Preflight failures, on any host: a probe that cannot run is a sandbox that
   does not work. *)

let preflight ctx mode ~warn =
  Sandbox.preflight ~env:ctx.env ~warn
    {
      mode;
      allow_write = [];
      deny_read = [];
      bactrian_exe = "/usr/bin/false";
      bactrian_dir = root ctx;
    }

let test_strict_fails_closed =
  with_ctx "sandbox-strict" @@ fun ctx ->
  match
    preflight ctx `Strict ~warn:(fun w -> Alcotest.failf "warned: %s" w)
  with
  | Ok _ -> Alcotest.fail "a broken sandbox passed the strict preflight"
  | Error m ->
      Alcotest.(check bool)
        "says why" true
        (contains m "sandbox: strict, but the sandbox is unavailable")

let test_auto_falls_back =
  with_ctx "sandbox-auto" @@ fun ctx ->
  let warnings = ref [] in
  match preflight ctx `Auto ~warn:(fun w -> warnings := w :: !warnings) with
  | Error m -> Alcotest.failf "auto must not stop: %s" m
  | Ok t ->
      let l = Sandbox.label t in
      Alcotest.(check bool) "not applied" false l.applied;
      Alcotest.(check bool) "has a reason" true (Option.is_some l.reason);
      Alcotest.(check int) "warned once" 1 (List.length !warnings);
      let r = ok (execute ctx { (config ()) with sandbox = t }) in
      Alcotest.(check string) "the run still passes" "passed" (outcome_name r);
      Alcotest.(check bool)
        "labelled unsandboxed" false
        Yojson.Safe.Util.(
          to_bool (member "applied" (member "sandbox" (meta r))))

let test_off_probes_nothing =
  with_ctx "sandbox-off" @@ fun ctx ->
  match preflight ctx `Off ~warn:(fun w -> Alcotest.failf "warned: %s" w) with
  | Error m -> Alcotest.failf "off: %s" m
  | Ok t ->
      Alcotest.(check string)
        "label" {|{"mode":"off","applied":false,"reason":"sandbox: off"}|}
        (Yojson.Safe.to_string (Sandbox.label_json (Sandbox.label t)))

(* The probe run outside any sandbox must be judged a failure: the preflight
   cannot report a sandbox that does not enforce. *)
let test_unsandboxed_probe_is_rejected () =
  let dir = Filename.temp_dir "bactrian-probe" "" in
  let outside = Filename.concat dir "outside"
  and inside = Filename.concat dir "inside"
  and denied = Filename.concat dir "denied" in
  Out_channel.with_open_bin denied (fun oc -> output_string oc "secret");
  Fun.protect ~finally:(fun () ->
      List.iter
        (fun f -> if Sys.file_exists f then Sys.remove f)
        [ outside; inside; denied ];
      Unix.rmdir dir)
  @@ fun () ->
  let out = Filename.concat dir "report" in
  let code =
    Sys.command
      (Printf.sprintf
         "%s __sandbox-probe --port 9 --outside %s --inside %s --denied %s > %s"
         (Filename.quote bactrian_bin)
         (Filename.quote outside) (Filename.quote inside)
         (Filename.quote denied) (Filename.quote out))
  in
  let line = String.trim (read out) in
  Sys.remove out;
  Alcotest.(check int) "probe exit" 0 code;
  match Sandbox_probe.of_line line with
  | Error m -> Alcotest.failf "report: %s" m
  | Ok report -> (
      let platform : Sandbox_policy.platform = if macos then Macos else Linux in
      let host_netns =
        match Unix.readlink "/proc/self/ns/net" with
        | ns -> Some ns
        | exception Unix.Unix_error _ -> None
      in
      match Sandbox_probe.judge platform ~host_netns report with
      | Ok () -> Alcotest.fail "an unsandboxed probe was judged sandboxed"
      | Error wrong ->
          Alcotest.(check bool)
            "the outside write is named" true
            (List.mem "a write outside the state directory succeeded" wrong);
          Alcotest.(check bool)
            "the read is named" true
            (List.mem "a read of a denied file succeeded" wrong))

let report ?(public = Error "EPERM") ?netns () : Sandbox_probe.report =
  {
    public;
    listener = Ok ();
    outside_write = Error "EPERM";
    inside_write = Ok ();
    denied_read = Error "EPERM";
    netns;
  }

let test_judge () =
  let judged platform ?host r =
    Result.is_ok (Sandbox_probe.judge platform ~host_netns:host r)
  in
  Alcotest.(check bool) "macOS sandbox" true (judged Macos (report ()));
  Alcotest.(check bool)
    "an offline Mac is not a sandbox" false
    (judged Macos (report ~public:(Error "ENETUNREACH") ()));
  Alcotest.(check bool)
    "a connection that went through" false
    (judged Macos (report ~public:(Ok ()) ()));
  Alcotest.(check bool)
    "Linux in its own namespace" true
    (judged Linux ~host:"net:[1]"
       (report ~public:(Error "ENETUNREACH") ~netns:"net:[2]" ()));
  Alcotest.(check bool)
    "Linux in the host's namespace" false
    (judged Linux ~host:"net:[1]"
       (report ~public:(Error "ENETUNREACH") ~netns:"net:[1]" ()));
  Alcotest.(check bool)
    "Linux without a namespace reading" false
    (judged Linux (report ~public:(Error "ENETUNREACH") ()));
  Alcotest.(check bool)
    "a listener that cannot be reached" false
    (judged Macos { (report ()) with listener = Error "EPERM" });
  Alcotest.(check bool)
    "a state dir that cannot be written" false
    (judged Macos { (report ()) with inside_write = Error "EPERM" });
  Alcotest.(check bool)
    "a write outside that worked" false
    (judged Macos { (report ()) with outside_write = Ok () });
  Alcotest.(check bool)
    "a denied read that worked" false
    (judged Linux ~host:"net:[1]"
       {
         (report ~public:(Error "ENETUNREACH") ~netns:"net:[2]" ()) with
         denied_read = Ok ();
       })

let () =
  Alcotest.run "run_sandbox"
    [
      ( "preflight",
        [
          Alcotest.test_case "strict fails closed" `Quick
            test_strict_fails_closed;
          Alcotest.test_case "auto falls back, labelled" `Slow
            test_auto_falls_back;
          Alcotest.test_case "off probes nothing" `Quick test_off_probes_nothing;
          Alcotest.test_case "an unsandboxed probe is rejected" `Quick
            test_unsandboxed_probe_is_rejected;
          Alcotest.test_case "judging reports" `Quick test_judge;
        ] );
      ( "sandboxed",
        [
          Alcotest.test_case "record and replay inside the sandbox" `Slow
            test_record_and_replay;
          Alcotest.test_case "escapes fail and are reported" `Slow
            test_escapes_fail;
          Alcotest.test_case "linked credential paths are denied" `Slow
            test_linked_credentials;
          Alcotest.test_case "MCP servers readable by the agent" `Slow
            test_mcp_warning;
          Alcotest.test_case "SIGINT ends a sandboxed replay" `Slow
            test_interrupt;
          Alcotest.test_case "the clock reaches the sandboxed agent" `Slow
            test_clock;
          Alcotest.test_case "a library under /tmp is bound in" `Slow
            test_clock_library_under_tmp;
          Alcotest.test_case "a masked library is not claimed enforced" `Slow
            test_clock_library_hidden;
        ] );
    ]
