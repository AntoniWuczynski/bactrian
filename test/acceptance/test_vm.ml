(* The virtual machine sandbox (design §5.3) on a real container runtime: the
   example agents' cycle and the escape agents in a Linux container, no host
   process visible from inside it, and no container left after a run, an
   interrupt or a bactrian killed with SIGKILL.

   Without a working runtime the tests are skipped, unless BACTRIAN_REQUIRE_VM
   is 1 (continuous integration on Linux sets it), where they fail. The agents
   run from copies inside each world, because the virtual machine shows the
   project alone, and the Python agent from a Linux environment that uv builds
   in a container from the lock file. *)

open Bactrian_runtime
open Acceptance_world

(* Pinned by digest: node 24 on Alpine 3.24 (2026-09-18), and uv 0.12.21 with
   Python 3.12 on Debian trixie (2026-09-29). *)
let node_image =
  "node:24-alpine@sha256:ebfe2f90462722a7a4de65e91990e97fe0d401c70e0e762c5b53302f905ec1c1"

let python_image =
  "ghcr.io/astral-sh/uv:0.12.21-python3.12-trixie-slim@sha256:5ae92e4d35b8d586d50ddf4aba6ecdd9f744237284d31c86bf45077e4e17e4bd"

let quiet cmd = Sys.command (cmd ^ " > /dev/null 2>&1") = 0
let macos = Sys.file_exists "/usr/bin/sandbox-exec"
let runtime_works = lazy (quiet "docker version --format '{{.Server.Arch}}'")

let required =
  match Sys.getenv_opt "BACTRIAN_REQUIRE_VM" with
  | Some "1" -> true
  | Some _ | None -> false

(* Pulled before the tests, which never pull: a run refuses a missing image. *)
let pulled = Hashtbl.create 2

let gate images =
  if not (Lazy.force runtime_works) then
    if required then
      Alcotest.fail "BACTRIAN_REQUIRE_VM is 1, but no container runtime works"
    else begin
      print_endline "no container runtime on this host: skipped";
      Alcotest.skip ()
    end;
  List.iter
    (fun image ->
      if not (Hashtbl.mem pulled image) then begin
        if
          not
            (quiet ("docker image inspect " ^ Filename.quote image)
            || quiet ("docker pull -q " ^ Filename.quote image))
        then Alcotest.failf "cannot pull %s" image;
        Hashtbl.replace pulled image ()
      end)
    images

let run_or_fail cmd =
  if Sys.command cmd <> 0 then Alcotest.failf "failed: %s" cmd

let copy ~into paths =
  run_or_fail
    (Printf.sprintf "mkdir -p %s && cp -R %s %s" (Filename.quote into)
       (String.concat " "
          (List.map (fun p -> Filename.quote (in_repo p)) paths))
       (Filename.quote into))

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
                [
                  "docker";
                  "run";
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
                ]));
        [ Filename.concat venv "bin/python"; Filename.concat into "agent.py" ]);
  }

let container_exists name =
  let out = Filename.temp_file "docker-ps" ".txt" in
  Fun.protect ~finally:(fun () -> Sys.remove out) @@ fun () ->
  run_or_fail
    (Printf.sprintf "docker ps -a --filter %s --format '{{.Names}}' > %s"
       (Filename.quote ("name=^" ^ name ^ "$"))
       (Filename.quote out));
  String.length (String.trim (read out)) > 0

let no_container (r : Run.result) =
  Alcotest.(check bool)
    "no container left" false
    (container_exists ("bactrian-" ^ r.run_id))

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

(* Teardown from outside: bactrian record, through the command line, with an
   agent that never answers. SIGINT ends it with 130, and SIGKILL leaves the
   bridge to remove the container. *)

let hung_agent =
  {|touch "$BACTRIAN_STATE_DIR/started"; while :; do sleep 1; done|}

let teardown ~signal () =
  gate [ node_image ];
  let dir =
    Filename.concat (Sys.getcwd ()) (Printf.sprintf "tmp-vm-teardown-%d" signal)
  in
  ignore (Sys.command ("rm -rf " ^ Filename.quote dir));
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command ("rm -rf " ^ Filename.quote dir)))
  @@ fun () ->
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
                [
                  executable "BACTRIAN_EXE";
                  "record";
                  "--turn";
                  "hi";
                  "--sandbox";
                  "strict";
                  "--sandbox-backend";
                  "vm";
                  "--sandbox-image";
                  node_image;
                  "--";
                  "sh";
                  "-c";
                  hung_agent;
                ]));
      |]
      [|
        "PATH=" ^ Option.value (Sys.getenv_opt "PATH") ~default:"/usr/bin";
        "HOME=" ^ Option.value (Sys.getenv_opt "HOME") ~default:"/";
        "BACTRIAN_CAP_OPENAI=1";
        "OPENAI_API_KEY=not-a-real-key";
        "BACTRIAN_UPSTREAM_OPENAI=http://127.0.0.1:9";
      |]
      null null null
  in
  let runs = Filename.concat dir ".bactrian/runs" in
  let deadline = Unix.gettimeofday () +. 120. in
  let run_id = ref "" in
  until ~deadline "the agent started" (fun () ->
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
  Unix.kill pid signal;
  (match wait_exit pid with
  | WEXITED n when signal = Sys.sigint ->
      Alcotest.(check int) "exit status" 130 n
  | WEXITED n -> Alcotest.failf "exit %d" n
  | WSIGNALED n when n = signal -> ()
  | WSIGNALED n | WSTOPPED n -> Alcotest.failf "signal %d" n);
  until
    ~deadline:(Unix.gettimeofday () +. 30.)
    "the container is gone"
    (fun () -> not (container_exists name))

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
        ] );
    ]
