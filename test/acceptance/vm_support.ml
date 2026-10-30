(* What the virtual machine acceptance tests share: the images, the gate that
   skips them without a runtime, and the checks that no container is left. *)

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
let runtime = vm_runtime_name

let runtime_works =
  lazy
    ( Eio_main.run @@ fun env ->
      Result.is_ok
        (Vm_runtime.detect
           ~process_mgr:(Eio.Stdenv.process_mgr env)
           ~clock:(Eio.Stdenv.clock env) (Some vm_runtime)) )

(* The runtime's own command for each thing the tests do beside bactrian. *)
let image_present image =
  match vm_runtime with
  | `Docker -> quiet ("docker image inspect " ^ Filename.quote image)
  | `Container -> quiet ("container image inspect " ^ Filename.quote image)

let pull image =
  match vm_runtime with
  | `Docker -> quiet ("docker pull -q " ^ Filename.quote image)
  | `Container ->
      quiet ("container image pull --progress none " ^ Filename.quote image)

let run_prefix =
  match vm_runtime with
  | `Docker -> [ "docker"; "run" ]
  | `Container -> [ "container"; "run"; "--progress"; "none" ]

let remove_command name =
  match vm_runtime with
  | `Docker -> "docker rm -f " ^ name
  | `Container -> "container delete --force " ^ name

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
        if not (image_present image || pull image) then
          Alcotest.failf "cannot pull %s" image;
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

let container_exists name =
  match vm_runtime with
  | `Container -> quiet ("container inspect " ^ Filename.quote name)
  | `Docker ->
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

(* Reads the files named in PROBES (a JSON list) and tries to create those
   that start with "w:", and answers with what each gave. *)
let probe_agent =
  {|import errno, json, os, sys

def attempt(probe):
    write = probe.startswith("w:")
    path = probe[2:] if write else probe
    try:
        if write:
            with open(path, "w"):
                return "written"
        with open(path) as h:
            return h.read().strip()
    except OSError as e:
        return errno.errorcode.get(e.errno, str(e.errno))

for line in sys.stdin:
    m = json.loads(line)
    if m["type"] == "turn":
        reply = {p: attempt(p) for p in json.loads(os.environ["PROBES"])}
        sys.stdout.write(json.dumps({"v": 1, "type": "reply", "turn": m["turn"],
                                     "content": json.dumps(reply)}) + "\n")
        sys.stdout.flush()
    elif m["type"] == "end":
        break
|}

(* What the probe agent saw, one answer per probe, in order. *)
let probed ?(allow_read = []) name ~files probes =
  gate [ python_image ];
  with_world ~vm:python_image ~allow_read name @@ fun w ->
  List.iter
    (fun (rel, text) ->
      Out_channel.with_open_bin (Filename.concat w.dir rel) (fun oc ->
          output_string oc text))
    (("probe.py", probe_agent) :: files w);
  let r =
    w.execute
      ~agent:(Command [ "python3"; "-I"; Filename.concat w.dir "probe.py" ])
      ~variant_env:
        [
          ( "PROBES",
            Yojson.Safe.to_string
              (`List (List.map (fun p -> `String p) (probes w))) );
        ]
      ()
  in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  check_sandboxed w r;
  let seen =
    match entries "reply" r with
    | [ j ] ->
        Yojson.Safe.Util.(
          List.map
            (fun (k, v) -> (k, to_string v))
            (to_assoc
               (Yojson.Safe.from_string (to_string (member "content" j)))))
    | _ -> Alcotest.fail "expected exactly one reply"
  in
  no_container r;
  (w, seen)

let sim_now_text = "2023-11-14T22:13:20Z"
let sim_now_epoch = 1700000000

(* Reads its own clock and a child's ([date +%s]). *)
let clock_python =
  {|import json, subprocess, sys, time
for line in sys.stdin:
    m = json.loads(line)
    if m["type"] == "turn":
        out = {"time": int(time.time()),
               "child": int(subprocess.check_output(["date", "+%s"])),
               "slept": time.time()}
        time.sleep(0.2)
        out["slept"] = round(time.time() - out["slept"], 1)
        sys.stdout.write(json.dumps({"v": 1, "type": "reply", "turn": m["turn"],
                                     "content": json.dumps(out)}) + "\n")
        sys.stdout.flush()
    elif m["type"] == "end":
        break
|}

let clock_node =
  {|const rl = require("readline").createInterface({ input: process.stdin });
rl.on("line", (line) => {
  const m = JSON.parse(line);
  if (m.type === "turn") {
    const out = {
      time: Math.floor(Date.now() / 1000),
      child: Number(require("child_process").execSync("date +%s")),
    };
    process.stdout.write(JSON.stringify({ v: 1, type: "reply", turn: m.turn,
      content: JSON.stringify(out) }) + "\n");
  } else if (m.type === "end") process.exit(0);
});|}

let clocked ~image ~name ~files agent =
  gate [ image ];
  with_world ~vm:image name @@ fun w ->
  List.iter
    (fun (rel, text, perm) ->
      let path = Filename.concat w.dir rel in
      Out_channel.with_open_bin path (fun oc -> output_string oc text);
      Unix.chmod path perm)
    files;
  let r = w.execute ~agent:(Command (agent w)) ~sim_now:sim_now_text () in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  check_sandboxed w r;
  Alcotest.(check string)
    "the clock is enforced" {|{"mode":"enforced"}|}
    (Yojson.Safe.to_string (Bactrian_core.Clock_label.to_json r.clock));
  let reply =
    match entries "reply" r with
    | [ j ] ->
        Yojson.Safe.Util.(
          Yojson.Safe.from_string (to_string (member "content" j)))
    | _ -> Alcotest.fail "expected exactly one reply"
  in
  let read k = Yojson.Safe.Util.(to_int (member k reply)) in
  List.iter
    (fun k ->
      let t = read k in
      Alcotest.(check bool)
        (Printf.sprintf "%s reads SIM_NOW (got %d)" k t)
        true
        (t >= sim_now_epoch && t < sim_now_epoch + 60))
    [ "time"; "child" ];
  no_container r;
  reply
