(* What the tests that drive [bactrian serve-mcp] share. *)

open Bactrian_core
open Bactrian_runtime
open Acceptance_world
open Cli_world

(* The tests run agents unsandboxed and pass them variables, which the server
   allows only with both flags. *)
let trusting = [ "--allow-unsandboxed"; "--trust-project" ]

let check_exit what expected = function
  | Unix.WEXITED n -> Alcotest.(check int) what expected n
  | Unix.WSIGNALED n | Unix.WSTOPPED n ->
      Alcotest.failf "%s: the server was stopped by signal %d" what n

let hang_script =
  {|
import os, subprocess, sys
child = subprocess.Popen(["sleep", "600"])
with open("child.pid", "w") as f:
    f.write(str(child.pid))
with open("agent.pid", "w") as f:
    f.write(str(os.getpid()))
sys.stdin.read()
|}

let hung_arguments run =
  [
    ("run", Canon.string run);
    ("agent", strings [ "python3"; "-I"; "-c"; hang_script ]);
    ("sandbox", Canon.string "off");
    ("turn_timeout", Canon.int 100);
    ("total_timeout", Canon.int 200);
  ]

type hung = { agent_pid : int; child_pid : int }

let with_recording name f =
  let agent = List.hd agents in
  with_world name @@ fun w ->
  let recorded = w.execute ~agent:(Run.Command (agent.argv ())) () in
  Alcotest.(check string) "record outcome" "passed" (outcome_name recorded);
  f w recorded.run_id

let await_hung dir =
  let agent_file = Filename.concat dir "agent.pid" in
  wait_until ~what:"the agent to start" ~seconds:30. (fun () ->
      Sys.file_exists agent_file && String.trim (read agent_file) <> "");
  let agent_pid = read_pid agent_file
  and child_pid = read_pid (Filename.concat dir "child.pid") in
  Alcotest.(check bool) "the agent is running" true (alive agent_pid);
  Alcotest.(check bool) "its child is running" true (alive child_pid);
  { agent_pid; child_pid }

let gone h =
  wait_gone "the agent" h.agent_pid;
  wait_gone "the agent's child" h.child_pid

let run_flags = [ "run"; "exp.json"; "--sandbox"; "off"; "--json" ]
let upstream_variable = "BACTRIAN_UPSTREAM_OPENAI"

let run_args ?resume () =
  [ ("experiment", Canon.string "exp.json"); ("sandbox", Canon.string "off") ]
  @ match resume with Some id -> [ ("resume", Canon.string id) ] | None -> []

let run_statuses j =
  List.map (string_field "status") (get "runs" (Canon.list_member "runs" j))

(* The command line and the server each started an experiment of their own, so
   each has a new experiment ID and new run IDs. Nothing else differs, and
   nothing else is normalised. *)
let summary_normalised j =
  let runs =
    List.map
      (Canon.remove_member "run_id")
      (get "runs" (Canon.list_member "runs" j))
  in
  Canon.set_member "runs" (Canon.list runs)
    (Canon.remove_member "experiment_id" j)
