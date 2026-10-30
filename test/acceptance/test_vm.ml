(* The virtual machine sandbox (design §5.3) on a real container runtime: the
   example agents' cycle and the escape agents in a Linux container, no host
   process visible from inside it, and no container left after a run, an
   interrupt or a bactrian killed with SIGKILL.

   They run on docker, or on Apple's container when BACTRIAN_VM_RUNTIME is
   container. Without a working runtime the tests are skipped, unless
   BACTRIAN_REQUIRE_VM is 1 (continuous integration on Linux sets it), where
   they fail. The agents
   run from copies inside each world, because the virtual machine shows the
   project alone, and the Python agent from a Linux environment that uv builds
   in a container from the lock file. *)

open Bactrian_runtime
open Acceptance_world
open Vm_support

(* The agents, prepared in a world's directory. *)

type agent = { name : string; image : string; prepare : string -> string list }

let typescript =
  {
    name = "typescript";
    image = node_image;
    prepare =
      (fun dir ->
        let into = Filename.concat dir "ts" in
        List.iter
          (fun p -> copy ~into [ "examples/agents/typescript/" ^ p ])
          [ "package.json"; "dist"; "node_modules" ];
        [ "node"; Filename.concat into "dist/agent.js" ]);
  }

(* uv builds the environment in a container with the network, for the Linux
   wheels, at the path the agent's container sees it at. *)
let python =
  {
    name = "python";
    image = python_image;
    prepare =
      (fun dir ->
        let into = Filename.concat dir "py" in
        copy ~into
          (List.map
             (fun f -> "examples/agents/python/" ^ f)
             [ "agent.py"; "pyproject.toml"; "uv.lock" ]);
        let venv = Filename.concat into ".venv-linux" in
        run_or_fail
          (String.concat " "
             (List.map Filename.quote
                (run_prefix
                @ [
                    "--rm";
                    "--user";
                    Printf.sprintf "%d:%d" (Unix.getuid ()) (Unix.getgid ());
                    "-e";
                    "HOME=/tmp";
                    "-e";
                    "UV_CACHE_DIR=/tmp/uv-cache";
                    "-e";
                    "UV_PYTHON_DOWNLOADS=never";
                    "-e";
                    "UV_PROJECT_ENVIRONMENT=" ^ venv;
                    "--mount";
                    Printf.sprintf "type=bind,source=%s,target=%s" into into;
                    "--workdir";
                    into;
                    python_image;
                    "uv";
                    "sync";
                    "--locked";
                    "--no-dev";
                    "--quiet";
                  ])));
        [ Filename.concat venv "bin/python"; Filename.concat into "agent.py" ]);
  }

let cycle agent () =
  gate [ agent.image ];
  with_world ~vm:agent.image ("vm-" ^ agent.name) @@ fun w ->
  let argv = agent.prepare w.dir in
  Acceptance_checks.cycle w argv;
  List.iter
    (fun id ->
      Alcotest.(check bool)
        "no container left" false
        (container_exists ("bactrian-" ^ id)))
    (Array.to_list (Sys.readdir (Filename.concat w.dir ".bactrian/runs")))

let escapes agent () =
  gate [ agent.image ];
  let file, runtime =
    if String.equal agent.name "typescript" then ("escape.mts", [ "node" ])
    else ("escape.py", [ "python3"; "-I" ])
  in
  Acceptance_checks.escapes ~vm:agent.image ~name:("vm-" ^ agent.name)
    (fun ~dir ->
      copy ~into:dir [ "test/sandbox/" ^ file ];
      runtime @ [ Filename.concat dir file ])

(* Host processes. The agent looks for a sentinel held by a process of the
   user: through /proc where there is one, and through the process-arguments
   sysctl, which Seatbelt does not stop (decision 0010). It gets the sentinel
   reversed, so its own environment does not hold it. *)

let procs_agent =
  {|import ctypes, json, os, sys
sentinel = os.environ["SENTINEL_REVERSED"][::-1].encode()

def procargs(pid):
    # The process-arguments sysctl (PROCARGS2, names 1 and 49), as ps -E reads
    # it. The C library has no sysctl on Linux, which raises AttributeError.
    libc = ctypes.CDLL(None, use_errno=True)
    mib = (ctypes.c_int * 3)(1, 49, pid)
    size = ctypes.c_size_t(1 << 20)
    buf = ctypes.create_string_buffer(size.value)
    if libc.sysctl(mib, 3, buf, ctypes.byref(size), None, 0) != 0:
        raise OSError(ctypes.get_errno(), "sysctl")
    return buf.raw[: size.value]

for line in sys.stdin:
    m = json.loads(line)
    if m["type"] == "turn":
        found = []
        pids = [p for p in os.listdir("/proc") if p.isdigit()] if os.path.isdir("/proc") else []
        for p in pids:
            for f in ("environ", "cmdline"):
                try:
                    with open(f"/proc/{p}/{f}", "rb") as h:
                        if sentinel in h.read():
                            found.append(f"/proc/{p}/{f}")
                except OSError:
                    pass
        try:
            if sentinel in procargs(int(os.environ["HOST_PID"])):
                found.append("sysctl")
        except (AttributeError, OSError):
            pass
        reply = {"pids": len(pids), "found": found}
        sys.stdout.write(json.dumps({"v": 1, "type": "reply", "turn": m["turn"],
                                     "content": json.dumps(reply)}) + "\n")
        sys.stdout.flush()
    elif m["type"] == "end":
        break
|}

let rec wait_exit pid =
  match Unix.waitpid [ WNOHANG ] pid with
  | 0, _ ->
      Unix.sleepf 0.05;
      wait_exit pid
  | _, status -> status
  | exception Unix.Unix_error (EINTR, _, _) -> wait_exit pid

let rec until ~deadline what f =
  if f () then ()
  else if Unix.gettimeofday () > deadline then
    Alcotest.failf "timed out waiting until %s" what
  else begin
    Unix.sleepf 0.1;
    until ~deadline what f
  end

let with_sentinel_process f =
  let sentinel = "sk-SENTINEL-" ^ string_of_int (Unix.getpid ()) in
  let null = Unix.openfile "/dev/null" [ O_RDWR ] 0 in
  let r, w = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process_env "python3"
      [| "python3"; "-c"; "import sys; sys.stdin.read()" |]
      [|
        "PATH=" ^ Option.value (Sys.getenv_opt "PATH") ~default:"/usr/bin";
        "CONTROL_KEY=" ^ sentinel;
      |]
      r null null
  in
  Unix.close r;
  Unix.close null;
  Fun.protect ~finally:(fun () ->
      Unix.close w;
      ignore (wait_exit pid : Unix.process_status))
  @@ fun () ->
  (* Until the child has run exec, its arguments are this test's. *)
  if macos then
    until
      ~deadline:(Unix.gettimeofday () +. 10.)
      "the sentinel process runs"
      (fun () ->
        let out = Filename.temp_file "ps" ".txt" in
        Fun.protect ~finally:(fun () -> Sys.remove out) @@ fun () ->
        ignore
          (Sys.command
             (Printf.sprintf "ps -wwE -p %d -o command= > %s 2>/dev/null" pid
                (Filename.quote out)));
        contains (read out) sentinel);
  let reversed =
    String.init (String.length sentinel) (fun i ->
        sentinel.[String.length sentinel - 1 - i])
  in
  f ~pid [ ("SENTINEL_REVERSED", reversed); ("HOST_PID", string_of_int pid) ]

let procs_result (r : Run.result) =
  match entries "reply" r with
  | [ j ] ->
      Yojson.Safe.from_string Yojson.Safe.Util.(to_string (member "content" j))
  | _ -> Alcotest.fail "expected exactly one reply"

let found r =
  Yojson.Safe.Util.(
    List.map to_string (to_list (member "found" (procs_result r))))

let host_processes () =
  gate [ python_image ];
  with_sentinel_process @@ fun ~pid:_ variant_env ->
  with_world ~vm:python_image "vm-procs" @@ fun w ->
  let script = Filename.concat w.dir "procs.py" in
  Out_channel.with_open_bin script (fun oc -> output_string oc procs_agent);
  let r =
    w.execute ~agent:(Command [ "python3"; "-I"; script ]) ~variant_env ()
  in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  check_sandboxed w r;
  Alcotest.(check (list string)) "no host process seen" [] (found r);
  let pids = Yojson.Safe.Util.(to_int (member "pids" (procs_result r))) in
  Printf.printf "processes visible in the container: %d\n" pids;
  Alcotest.(check bool) "only the container's own processes" true (pids < 10);
  no_container r

(* The known hole, kept as a test: under Seatbelt the same agent reads the
   sentinel through the sysctl. *)
let seatbelt_control () =
  if not macos then begin
    print_endline "not macOS: no Seatbelt to compare with, skipped";
    Alcotest.skip ()
  end;
  with_sentinel_process @@ fun ~pid:_ variant_env ->
  with_world "seatbelt-procs" @@ fun w ->
  let script = Filename.concat w.dir "procs.py" in
  Out_channel.with_open_bin script (fun oc -> output_string oc procs_agent);
  let r =
    w.execute ~agent:(Command [ "python3"; "-I"; script ]) ~variant_env ()
  in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  check_sandboxed w r;
  Alcotest.(check (list string))
    "the sentinel is readable" [ "sysctl" ] (found r)

(* A hidden file's neighbours. The project's .env is not there under any
   spelling of its name (the host's file system ignores case), while the files
   and directories beside it are, read-only. Docker's replacement directory is
   a tmpfs, where only the exact name of a neighbour is there. container's is a
   staging directory on the host, so a neighbour's other spellings read it. *)

let neighbours () =
  let w, seen =
    probed "vm-neighbours"
      ~files:(fun w ->
        Unix.mkdir (Filename.concat w.dir "sub") 0o700;
        [
          (".env", "OPENAI_API_KEY=sk-masked-file\n");
          ("sibling.txt", "visible\n");
          ("sub/inner.txt", "inner\n");
        ])
      (fun w ->
        List.map (Filename.concat w.dir)
          [
            ".env";
            ".ENV";
            ".Env";
            "sibling.txt";
            "SIBLING.TXT";
            "sub/inner.txt";
          ]
        @ [ "w:" ^ Filename.concat w.dir "new.txt" ])
  in
  Alcotest.(check (list string))
    "what the agent saw"
    [
      "ENOENT";
      "ENOENT";
      "ENOENT";
      "visible";
      (match vm_runtime with `Docker -> "ENOENT" | `Container -> "visible");
      "inner";
      "EROFS";
    ]
    (List.map snd seen);
  Alcotest.(check bool)
    "nothing written beside it" false
    (Sys.file_exists (Filename.concat w.dir "new.txt"))

(* sandbox.allow_read: a directory outside the project is there, read-only,
   with its own .env hidden as the project's is, and its neighbour that the
   experiment does not name is not there at all. *)
let readable () =
  let dir = Filename.concat (Sys.getcwd ()) "tmp-vm-allow-read" in
  let other = dir ^ "-other" in
  let remove () =
    ignore
      (Sys.command
         ("rm -rf " ^ Filename.quote dir ^ " " ^ Filename.quote other))
  in
  remove ();
  Fun.protect ~finally:remove @@ fun () ->
  List.iter
    (fun d -> Unix.mkdir d 0o700)
    [ dir; Filename.concat dir "sub"; other ];
  List.iter
    (fun (p, text) ->
      Out_channel.with_open_bin p (fun oc -> output_string oc text))
    [
      (Filename.concat dir "data.txt", "shared\n");
      (Filename.concat dir ".env", "OPENAI_API_KEY=sk-allowed-dir\n");
      (Filename.concat dir "sub/inner.txt", "inner\n");
      (Filename.concat other "data.txt", "not allowed\n");
    ];
  let _, seen =
    probed ~allow_read:[ dir ] "vm-allow-read"
      ~files:(fun _ -> [])
      (fun _ ->
        List.map (Filename.concat dir)
          [ "data.txt"; "sub/inner.txt"; ".env"; ".ENV" ]
        @ [
            Filename.concat other "data.txt";
            "w:" ^ Filename.concat dir "new.txt";
          ])
  in
  Alcotest.(check (list string))
    "what the agent saw"
    [ "shared"; "inner"; "ENOENT"; "ENOENT"; "ENOENT"; "EROFS" ]
    (List.map snd seen);
  Alcotest.(check bool)
    "nothing written there" false
    (Sys.file_exists (Filename.concat dir "new.txt"))

(* The clock (decision 0021). The relay starts the agent's clock at SIM_NOW
   with libfaketime for the image's C library: musl in the node image, glibc in
   the Python one. An agent, and the program it starts, read SIM_NOW (give or
   take the seconds a run takes) instead of the host's clock. *)

let clock_musl () =
  ignore
    (clocked ~image:node_image ~name:"vm-clock-musl"
       ~files:[ ("clock.js", clock_node, 0o644) ]
       (fun w -> [ "node"; Filename.concat w.dir "clock.js" ])
      : Yojson.Safe.t)

let clock_glibc () =
  let reply =
    clocked ~image:python_image ~name:"vm-clock-glibc"
      ~files:[ ("clock.py", clock_python, 0o644) ]
      (fun w -> [ "python3"; "-I"; Filename.concat w.dir "clock.py" ])
  in
  (* Python 3.11 and later sleep on CLOCK_MONOTONIC (decision 0009). *)
  let slept = Yojson.Safe.Util.(to_float (member "slept" reply)) in
  Alcotest.(check bool)
    (Printf.sprintf "time.sleep still sleeps (%.1f s)" slept)
    true
    (slept >= 0.1 && slept < 5.)

(* A script is followed to its interpreter, in the image. *)
let clock_script () =
  ignore
    (clocked ~image:python_image ~name:"vm-clock-script"
       ~files:[ ("clock.py", "#!/usr/bin/env python3\n" ^ clock_python, 0o755) ]
       (fun w -> [ Filename.concat w.dir "clock.py" ])
      : Yojson.Safe.t)

(* An executable that cannot take a preload is not labelled enforced. *)
let clock_refused () =
  gate [ node_image ];
  with_world ~vm:node_image "vm-clock-refused" @@ fun w ->
  let r =
    w.execute ~agent:(Command [ "/bin/does-not-exist-in-the-image" ])
      ~sim_now:sim_now_text ()
  in
  Alcotest.(check string)
    "contract-only, with the reason"
    {|{"mode":"contract_only","reason":"unreadable-executable"}|}
    (Yojson.Safe.to_string (Bactrian_core.Clock_label.to_json r.clock));
  no_container r

(* Teardown from outside: bactrian record, through the command line, with an
   agent that never answers. SIGINT ends it with 130, SIGKILL leaves the bridge
   to remove the container, and with the bridge and the runtime's client killed
   too the relay stops the agent once the heartbeats stop, so the container
   goes with it. *)

let hung_agent =
  {|touch "$BACTRIAN_STATE_DIR/started"; while :; do sleep 1; done|}

let record_env =
  [|
    "PATH=" ^ Option.value (Sys.getenv_opt "PATH") ~default:"/usr/bin";
    "HOME=" ^ Option.value (Sys.getenv_opt "HOME") ~default:"/";
    "BACTRIAN_CAP_OPENAI=1";
    "OPENAI_API_KEY=not-a-real-key";
    "BACTRIAN_UPSTREAM_OPENAI=http://127.0.0.1:9";
  |]

(* bactrian record in the virtual machine, in a directory of its own. *)
let with_record ~name agent f =
  let dir = Filename.concat (Sys.getcwd ()) ("tmp-vm-" ^ name) in
  ignore (Sys.command ("rm -rf " ^ Filename.quote dir));
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command ("rm -rf " ^ Filename.quote dir)))
  @@ fun () ->
  (* A hidden file, so that container stages the project's directory. *)
  Out_channel.with_open_bin (Filename.concat dir ".env") (fun oc ->
      output_string oc "OPENAI_API_KEY=sk-masked-file\n");
  let null = Unix.openfile "/dev/null" [ O_RDWR ] 0 in
  let pid =
    Fun.protect ~finally:(fun () -> Unix.close null) @@ fun () ->
    Unix.create_process_env "sh"
      [|
        "sh";
        "-c";
        Printf.sprintf "cd %s && exec %s" (Filename.quote dir)
          (String.concat " "
             (List.map Filename.quote
                ([
                   executable "BACTRIAN_EXE";
                   "record";
                   "--turn";
                   "hi";
                   "--sandbox";
                   "strict";
                   "--sandbox-backend";
                   "vm";
                   "--sandbox-runtime";
                   runtime;
                   "--sandbox-image";
                   node_image;
                   "--";
                 ]
                @ agent)));
      |]
      record_env null null null
  in
  f ~dir ~pid

(* The run's container, once the hung agent has started in it. *)
let started ~dir =
  let runs = Filename.concat dir ".bactrian/runs" in
  let run_id = ref "" in
  until
    ~deadline:(Unix.gettimeofday () +. 120.)
    "the agent started"
    (fun () ->
      Sys.file_exists runs
      && List.exists
           (fun id ->
             if
               Sys.file_exists (String.concat "/" [ runs; id; "state/started" ])
             then begin
               run_id := id;
               true
             end
             else false)
           (Array.to_list (Sys.readdir runs)));
  let name = "bactrian-" ^ !run_id in
  Alcotest.(check bool) "the container runs" true (container_exists name);
  name

let gone ~within name =
  until
    ~deadline:(Unix.gettimeofday () +. within)
    "the container is gone"
    (fun () -> not (container_exists name))

let teardown ~signal () =
  gate [ node_image ];
  with_record
    ~name:(Printf.sprintf "teardown-%d" signal)
    [ "sh"; "-c"; hung_agent ]
  @@ fun ~dir ~pid ->
  let name = started ~dir in
  (* bactrian removes container's staging directory itself, or after a
     SIGKILL the bridge does. *)
  let staging =
    String.concat "/"
      [
        dir;
        ".bactrian/runs";
        String.sub name 9 (String.length name - 9);
        "staging";
      ]
  in
  Alcotest.(check bool)
    "staged" (vm_runtime = `Container) (Sys.file_exists staging);
  Unix.kill pid signal;
  (match wait_exit pid with
  | WEXITED n when signal = Sys.sigint ->
      Alcotest.(check int) "exit status" 130 n
  | WEXITED n -> Alcotest.failf "exit %d" n
  | WSIGNALED n when n = signal -> ()
  | WSIGNALED n | WSTOPPED n -> Alcotest.failf "signal %d" n);
  gone ~within:30. name;
  until
    ~deadline:(Unix.gettimeofday () +. 10.)
    "the staging directory is gone"
    (fun () -> not (Sys.file_exists staging))

(* The process ID of [pid]'s one child whose command holds [word]. *)
let child_of pid word =
  let out = Filename.temp_file "ps" ".txt" in
  Fun.protect ~finally:(fun () -> Sys.remove out) @@ fun () ->
  run_or_fail
    (Printf.sprintf "ps -A -o pid= -o ppid= -o command= > %s"
       (Filename.quote out));
  match
    List.filter_map
      (fun line ->
        match
          List.filter
            (fun w -> not (String.equal w ""))
            (String.split_on_char ' ' line)
        with
        | child :: parent :: command
          when int_of_string_opt parent = Some pid && List.mem word command ->
            int_of_string_opt child
        | _ -> None)
      (String.split_on_char '\n' (read out))
  with
  | [ child ] -> child
  | found ->
      Alcotest.failf "expected one child running %s, found %d" word
        (List.length found)

(* bactrian, its bridge and the runtime's client all killed, as
   pkill -9 -f bactrian would (the client's arguments name the container
   bactrian-RUN). The client alone keeps the container's input open: killing
   only the bridge closes it, which the relay already takes for an end. *)
let all_killed () =
  gate [ node_image ];
  with_record ~name:"all-killed" [ "sh"; "-c"; hung_agent ] @@ fun ~dir ~pid ->
  let name = started ~dir in
  let bridge = child_of pid "__vm-bridge" in
  let client =
    child_of bridge
      (match vm_runtime with `Docker -> "run" | `Container -> "start")
  in
  (* Back to back: the bridge checks its parent once a second, so it has no
     time to remove the container itself. *)
  List.iter (fun p -> Unix.kill p Sys.sigkill) [ client; bridge; pid ];
  ignore (wait_exit pid : Unix.process_status);
  let killed = Unix.gettimeofday () in
  gone ~within:(Bactrian_core.Vm_frame.liveness_timeout +. 20.) name;
  Printf.printf "the container went %.1f seconds after the kill\n"
    (Unix.gettimeofday () -. killed)

(* The relay alone, with its input open and silent, as when nothing of
   bactrian is left but the runtime's client: it stops the agent once
   [liveness_timeout] has passed without a heartbeat. *)
let relay_liveness () =
  gate [ node_image ];
  let relay =
    Eio_main.run @@ fun env ->
    let process_mgr = Eio.Stdenv.process_mgr env
    and clock = Eio.Stdenv.clock env in
    match
      Result.bind (Vm_runtime.detect ~process_mgr ~clock (Some vm_runtime))
        (fun r -> Vm_runtime.relay ~process_mgr ~clock r)
    with
    | Ok path -> path
    | Error m -> Alcotest.failf "no relay: %s" m
  in
  let name = Printf.sprintf "bactrian-liveness-%d" (Unix.getpid ()) in
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command (remove_command name ^ " > /dev/null 2>&1")))
  @@ fun () ->
  let r, w = Unix.pipe ~cloexec:true () in
  let null = Unix.openfile "/dev/null" [ O_RDWR ] 0 in
  let started = Unix.gettimeofday () in
  (* container mounts directories only. *)
  let relay_args =
    match vm_runtime with
    | `Docker ->
        [
          "--log-driver";
          "none";
          "--entrypoint";
          "/bactrian/relay";
          "--mount";
          Printf.sprintf "type=bind,source=%s,target=/bactrian/relay,readonly"
            relay;
        ]
    | `Container ->
        [
          "--entrypoint";
          "/bactrian/" ^ Filename.basename relay;
          "--mount";
          Printf.sprintf "type=bind,source=%s,target=/bactrian,readonly"
            (Filename.dirname relay);
        ]
  in
  let pid =
    Unix.create_process runtime
      (Array.of_list
         (run_prefix
         @ [ "--rm"; "-i"; "--name"; name; "--network"; "none" ]
         @ relay_args
         @ [ "--"; node_image; "--"; "sleep"; "300" ]))
      r null null
  in
  Unix.close r;
  Unix.close null;
  Fun.protect ~finally:(fun () -> Unix.close w) @@ fun () ->
  let status = wait_exit pid in
  let took = Unix.gettimeofday () -. started in
  Printf.printf "the relay stopped after %.1f seconds\n" took;
  (match status with
  | WEXITED 70 -> ()
  | WEXITED n -> Alcotest.failf "exit %d" n
  | WSIGNALED n | WSTOPPED n -> Alcotest.failf "signal %d" n);
  let timeout = Bactrian_core.Vm_frame.liveness_timeout in
  Alcotest.(check bool) "not before the timeout" true (took >= timeout -. 0.5);
  Alcotest.(check bool) "soon after it" true (took < timeout +. 20.);
  gone ~within:10. name

(* A relay cache that someone else could have written to is refused, before
   anything is built. *)
let relay_cache () =
  gate [];
  let base = Filename.concat (Sys.getcwd ()) "tmp-vm-relay-cache" in
  ignore (Sys.command ("rm -rf " ^ Filename.quote base));
  let previous = Sys.getenv_opt "XDG_CACHE_HOME" in
  Unix.putenv "XDG_CACHE_HOME" base;
  Fun.protect ~finally:(fun () ->
      Unix.putenv "XDG_CACHE_HOME" (Option.value previous ~default:"");
      ignore (Sys.command ("rm -rf " ^ Filename.quote base)))
  @@ fun () ->
  Eio_main.run @@ fun env ->
  let process_mgr = Eio.Stdenv.process_mgr env
  and clock = Eio.Stdenv.clock env in
  let runtime =
    match Vm_runtime.detect ~process_mgr ~clock (Some vm_runtime) with
    | Ok r -> r
    | Error m -> Alcotest.failf "no runtime: %s" m
  in
  let dir = Vm_runtime.relay_cache_dir () in
  let binary = Filename.concat dir (Vm_runtime.relay_name runtime) in
  let refused what needle =
    match Vm_runtime.relay ~process_mgr ~clock runtime with
    | Ok _ -> Alcotest.failf "%s: the relay was used" what
    | Error m ->
        Printf.printf "%s: %s\n" what m;
        Alcotest.(check bool) what true (contains m needle)
  in
  run_or_fail (Printf.sprintf "mkdir -p %s" (Filename.quote dir));
  Unix.chmod dir 0o777;
  refused "a directory others can write to" "writable by its group or by others";
  Unix.chmod dir 0o700;
  Unix.symlink "/bin/sh" binary;
  refused "a symbolic link" "is not a regular file";
  Unix.unlink binary;
  Out_channel.with_open_gen [ Open_wronly; Open_creat ] 0o600 binary ignore;
  Unix.chmod binary 0o775;
  refused "a binary its group can write to" "writable by its group or by others"

(* A container whose bactrian has gone, as the relay of an older bactrian
   would leave it, is removed by the next preflight. *)
let stale_removed () =
  gate [ node_image ];
  let dead =
    Unix.create_process "true" [| "true" |] Unix.stdin Unix.stdout Unix.stderr
  in
  ignore (wait_exit dead : Unix.process_status);
  let name = Printf.sprintf "bactrian-stale-%d" (Unix.getpid ()) in
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command (remove_command name ^ " > /dev/null 2>&1")))
  @@ fun () ->
  run_or_fail
    (String.concat " "
       (List.map Filename.quote
          (run_prefix
          @ [
              "-d";
              "--name";
              name;
              "--label";
              "bactrian.run=stale";
              "--label";
              "bactrian.pid=" ^ string_of_int dead;
              "--label";
              "bactrian.host=" ^ Unix.gethostname ();
              "--network";
              "none";
              "--";
              node_image;
              "sleep";
              "300";
            ]))
    ^ " > /dev/null");
  Alcotest.(check bool) "the stale container runs" true (container_exists name);
  with_record ~name:"stale" [ "true" ] @@ fun ~dir:_ ~pid ->
  ignore (wait_exit pid : Unix.process_status);
  Alcotest.(check bool) "removed by the preflight" false (container_exists name)

let () =
  Alcotest.run "vm sandbox"
    [
      ( "example agents",
        List.map
          (fun a -> Alcotest.test_case a.name `Slow (cycle a))
          [ typescript; python ] );
      ( "escapes",
        List.map
          (fun a -> Alcotest.test_case a.name `Slow (escapes a))
          [ typescript; python ] );
      ( "masks",
        [
          Alcotest.test_case "a hidden file's neighbours" `Slow neighbours;
          Alcotest.test_case "allow_read" `Slow readable;
        ] );
      ( "clock",
        [
          Alcotest.test_case "musl image" `Slow clock_musl;
          Alcotest.test_case "glibc image" `Slow clock_glibc;
          Alcotest.test_case "a script" `Slow clock_script;
          Alcotest.test_case "an executable the image lacks" `Slow clock_refused;
        ] );
      ( "host processes",
        [
          Alcotest.test_case "none visible in the virtual machine" `Slow
            host_processes;
          Alcotest.test_case "readable under Seatbelt" `Slow seatbelt_control;
        ] );
      ( "teardown",
        [
          Alcotest.test_case "SIGINT" `Slow (teardown ~signal:Sys.sigint);
          Alcotest.test_case "SIGKILL" `Slow (teardown ~signal:Sys.sigkill);
          Alcotest.test_case "bactrian, its bridge and the client killed" `Slow
            all_killed;
          Alcotest.test_case "the relay without heartbeats" `Slow relay_liveness;
          Alcotest.test_case "a stale container" `Slow stale_removed;
        ] );
      ( "relay cache",
        [ Alcotest.test_case "an untrusted one" `Slow relay_cache ] );
    ]
