(* [bactrian serve-mcp] from the outside: the real binary, a scripted stdio
   client in each protocol era, the real example agents and no network. The
   experiment and the recordings come from the library against the fake
   upstream. The server then reads them, replays them offline and answers, and
   each tool's JSON must equal what the command line prints with --json. *)

open Bactrian_core
open Bactrian_runtime
open Acceptance_world
open Cli_world
module C = Mcp_client
module E = Executor

let era_name = function C.Modern -> "modern" | C.Legacy -> "legacy"
let eras = [ C.Modern; C.Legacy ]

(* A client that has been told what it must not leak. *)
type conn = { c : C.t; seen : Buffer.t }

let connect ?(env = key_env) ~dir era =
  let c = C.start ~env ~exe:(exe ()) ~dir era in
  { c; seen = Buffer.create 1024 }

let note conn c = Buffer.add_string conn.seen (Canon.to_string c)

let ok_call conn name args =
  match C.call_tool conn.c name (obj args) with
  | Structured s ->
      note conn s;
      s
  | Tool_error e -> Alcotest.failf "%s: tool error %s" name (Canon.to_string e)

let error_call conn name args =
  match C.call_tool conn.c name (obj args) with
  | Tool_error e ->
      note conn e;
      e
  | Structured s ->
      Alcotest.failf "%s: expected a tool error, got %s" name
        (Canon.to_string s)

let no_secret conn =
  Alcotest.(check bool)
    "no tool result holds the key" false
    (contains (Buffer.contents conn.seen) real_key)

let check_exit what expected = function
  | Unix.WEXITED n -> Alcotest.(check int) what expected n
  | Unix.WSIGNALED n | Unix.WSTOPPED n ->
      Alcotest.failf "%s: the server was stopped by signal %d" what n

let golden_check_text =
  {|{"version": 1, "id": "weather", "timezone": "UTC",
 "turns": [{"user": "What is the weather in Paris right now?"}],
 "assertions": [{"tool_sequence": {"mode": "exact", "tools": ["get_weather"]}}],
 "checkers": [], "golden": {"pass": ["golden/pass.jsonl"]}}|}

type experiment = {
  id : string;
  run : string;  (** A run of the [base] variant. *)
}

let run_experiment (w : world) argv =
  write_experiment w.dir argv;
  let summary =
    match
      E.run_experiment
        ~root:Eio.Path.(Eio.Stdenv.fs w.env / Filename.concat w.dir ".bactrian")
        ~keys:[ (Model_proxy.Openai, real_key) ]
        ~upstream:(fun _ -> Uri.of_string (F.base_url w.up))
        ~sandbox:`Off ~bactrian_exe:(exe ()) ~env:w.env
        ~experiment_file:(Filename.concat w.dir "exp.json")
        ~resume:None ()
    with
    | Ok s -> s
    | Error (E.Configuration _) -> Alcotest.fail "experiment: configuration"
    | Error (E.Environment m | E.Usage m) -> Alcotest.failf "experiment: %s" m
  in
  Alcotest.(check (list string))
    "every experiment run passes"
    (List.map (fun _ -> "passed") summary.runs)
    (List.map
       (fun (_, st, _) -> Bactrian_app.Run_command.status_kind st)
       summary.runs);
  let base =
    List.find_map
      (fun ((p : E.planned), _, id) ->
        if String.equal p.variant "base" then id else None)
      summary.runs
  in
  write (Filename.concat w.dir "checks/weather.json") golden_check_text;
  write
    (Filename.concat w.dir "checks/golden/pass.jsonl")
    (read
       (Filename.concat w.dir
          (Printf.sprintf ".bactrian/runs/%s/trace.jsonl" (get "a run" base))));
  { id = summary.experiment_id; run = get "a run" base }

(* Replaying with a changed prompt: the server's environment holds it, and the
   tool argument [env] passes it to the agent. *)
let replay_arguments ~argv ~run =
  [
    ("run", Canon.string run);
    ("agent", strings argv);
    ("env", strings [ prompt_variable ]);
    ("sandbox", Canon.string "off");
    ("scenario", Canon.string "weather");
    ("variant", Canon.string "base");
  ]

let replay_flags ~argv ~run =
  [
    "replay";
    run;
    "--json";
    "--sandbox";
    "off";
    "--scenario";
    "weather";
    "--variant";
    "base";
    "--env";
    prompt_variable;
    "--";
  ]
  @ argv

let number f =
  match Canon.float f with
  | Ok c -> c
  | Error e -> Alcotest.failf "number: %a" Canon.pp_error e

(* A tool, its arguments, and the command line that must print the same. *)
type pair = {
  label : string;
  tool : string;
  arguments : (string * Canon.t) list;
  command : string list;
}

let pairs exp =
  let plan_arguments =
    [
      ("threshold", number 0.1);
      ("max_repeats", Canon.int 20);
      ("simulations", Canon.int 40);
      ("seed", Canon.int 1);
    ]
  and plan_flags =
    [ "--threshold"; "0.1"; "--max-repeats"; "20"; "--simulations"; "40" ]
    @ [ "--seed"; "1" ]
  in
  [
    {
      label = "compare";
      tool = "compare";
      arguments = [ ("experiment_id", Canon.string exp.id) ];
      command = [ "compare"; exp.id; "--json" ];
    };
    {
      label = "report";
      tool = "report";
      arguments =
        [
          ("experiment_id", Canon.string exp.id);
          ("output", Canon.string "report.html");
        ];
      command = [ "report"; exp.id; "--output"; "report.html"; "--json" ];
    };
    {
      label = "plan from rates";
      tool = "plan";
      arguments =
        ("baseline_rates", Canon.list [ number 0.8 ]) :: plan_arguments;
      command = "plan" :: "--json" :: "--baseline-rate" :: "0.8" :: plan_flags;
    };
    {
      label = "plan from the experiment";
      tool = "plan";
      arguments = ("from", Canon.string exp.id) :: plan_arguments;
      command = "plan" :: "--json" :: "--from" :: exp.id :: plan_flags;
    };
    {
      label = "check";
      tool = "check";
      arguments = [ ("scenarios", strings [ "checks/weather.json" ]) ];
      command = [ "check"; "--json"; "checks/weather.json" ];
    };
    {
      label = "doctor";
      tool = "doctor";
      arguments = [];
      command = [ "doctor"; "--json" ];
    };
  ]

(* Fields that legitimately differ between the server's replay and the command
   line's over one recording. Both are new runs, so each has a fresh random ID
   and directory:
   - [run_id]
   - [run_dir]
   Nothing else is normalised, in this or any other comparison here. *)
let replay_normalised j =
  List.fold_left
    (fun j key -> Canon.remove_member key j)
    j [ "run_id"; "run_dir" ]

let e2e agent era () =
  let argv = agent.argv () in
  with_world ~rounds:4 (agent.name ^ "-mcp-client-" ^ era_name era) @@ fun w ->
  let exp = run_experiment w argv in
  let env = key_env @ [ (prompt_variable, terse) ] in
  let conn = connect ~env ~dir:w.dir era in
  let answers =
    List.map (fun p -> (p, ok_call conn p.tool p.arguments)) (pairs exp)
  in
  (* A replay with the changed prompt diverges. *)
  let job =
    string_field "job_id"
      (ok_call conn "replay_start" (replay_arguments ~argv ~run:exp.run))
  in
  let status = C.poll_job conn.c job in
  Alcotest.(check string)
    "the job succeeded" "succeeded"
    (string_field "state" status);
  Alcotest.(check string) "its kind" "replay" (string_field "kind" status);
  Alcotest.(check (option int))
    "the replay diverged" (Some 1)
    (Canon.int_member "exit" status);
  let replayed = field "result" status in
  let divergences =
    ok_call conn "divergences"
      [ ("run", Canon.string (string_field "run_id" replayed)) ]
  in
  check_exit "the server exits at end of input" 0 (C.close conn.c);
  no_secret conn;
  (* The same on the command line, once the server is gone. *)
  List.iter
    (fun (p, answer) ->
      let code, printed = cli ~dir:w.dir ~env p.command in
      Alcotest.(check int)
        (p.label ^ ": the exit status")
        code
        (Option.value (Canon.int_member "exit" answer) ~default:code);
      Alcotest.check json (p.label ^ " equals the CLI's") printed answer)
    answers;
  let code, printed = cli ~dir:w.dir ~env (replay_flags ~argv ~run:exp.run) in
  Alcotest.(check int) "replay: the exit status" code 1;
  Alcotest.check json "the job's result equals replay --json"
    (replay_normalised printed)
    (replay_normalised replayed);
  Alcotest.check json "divergences equals the replay's divergences"
    (field "divergences" printed)
    (field "divergences" divergences);
  Alcotest.(check string)
    "divergences names the run"
    (string_field "run_id" replayed)
    (string_field "run_id" divergences);
  (match Canon.list_member "divergences" divergences with
  | Some (first :: _ as all) ->
      Alcotest.(check (list string))
        "a missing request, and the two recorded exchanges left unconsumed"
        [ "missing_request"; "unconsumed"; "unconsumed" ]
        (List.map (string_field "kind") all);
      Alcotest.(check bool)
        "the missing request differs in the system message" true
        (contains (Canon.to_string first) "messages[0].content")
  | Some [] | None -> Alcotest.fail "expected divergences");
  Alcotest.(check bool)
    "the report is written" true
    (contains (read (Filename.concat w.dir "report.html")) "<html")

(* Protocol errors *)

let version_key = "io.modelcontextprotocol/protocolVersion"
let capabilities_key = "io.modelcontextprotocol/clientCapabilities"

let meta_of version =
  obj
    [
      ( "_meta",
        obj
          [
            (version_key, Canon.string version);
            (capabilities_key, Canon.empty_object);
          ] );
    ]

let rpc_error what = function
  | Error (e : C.error) -> e
  | Ok r ->
      Alcotest.failf "%s: expected an error, got %s" what (Canon.to_string r)

let supported (e : C.error) =
  List.filter_map Canon.to_string_opt
    (get "data.supported" (Option.bind e.data (Canon.list_member "supported")))

let with_server ?era name f =
  with_project name @@ fun ~parent:_ ~project ->
  let c = C.spawn ~exe:(exe ()) ~dir:project () in
  Fun.protect ~finally:(fun () -> C.dispose c) @@ fun () ->
  Option.iter (fun e -> ignore (C.connect c e : Canon.t)) era;
  f c;
  check_exit "the server exits at end of input" 0 (C.close c)

(* The version in [_meta] only counts in the modern era, where it is the only
   way to name one, so a legacy connection ignores it. *)
let unknown_version ~connected () =
  with_server
    ?era:(if connected then Some Modern else None)
    (if connected then "version-connected" else "version-first")
  @@ fun c ->
  let e =
    rpc_error "an unknown version"
      (C.raw_request c "tools/list" (meta_of "1999-01-01"))
  in
  Alcotest.(check int) "UnsupportedProtocolVersionError" (-32022) e.code;
  Alcotest.(check (list string)) "supported" [ "2026-07-28" ] (supported e);
  Alcotest.(check (option string))
    "requested" (Some "1999-01-01")
    (Option.bind e.data (Canon.str_member "requested"))

let request_before_era () =
  with_server "before-era" @@ fun c ->
  let e =
    rpc_error "tools/list before the era is known"
      (C.raw_request c "tools/list" Canon.empty_object)
  in
  Alcotest.(check int) "invalid request" (-32600) e.code;
  Alcotest.(check (list string))
    "both ways to open are named"
    [ "2026-07-28"; "2025-11-25"; "2025-06-18"; "2025-03-26" ]
    (supported e);
  let called =
    rpc_error "tools/call before the era is known"
      (C.raw_request c "tools/call" (obj [ ("name", Canon.string "doctor") ]))
  in
  Alcotest.(check int) "tools/call too" (-32600) called.code;
  (match C.raw_request c "ping" Canon.empty_object with
  | Ok r -> Alcotest.check json "ping is answered" Canon.empty_object r
  | Error e -> Alcotest.failf "ping: %s" e.message);
  (* The refusals did not fix an era. *)
  ignore (C.connect c Modern : Canon.t)

let unknown_tool era () =
  with_server ~era ("unknown-tool-" ^ era_name era) @@ fun c ->
  let e =
    rpc_error "an unknown tool"
      (C.request c "tools/call" (obj [ ("name", Canon.string "nonexistent") ]))
  in
  Alcotest.(check int) "invalid params" (-32602) e.code

let opening era () =
  with_server ("opening-" ^ era_name era) @@ fun c ->
  let answer = C.connect c era in
  match era with
  | Modern ->
      Alcotest.(check (option string))
        "resultType" (Some "complete")
        (Canon.str_member "resultType" answer);
      Alcotest.(check (option (list string)))
        "supported versions" (Some [ "2026-07-28" ])
        (Option.map
           (List.filter_map Canon.to_string_opt)
           (Canon.list_member "supportedVersions" answer))
  | Legacy ->
      Alcotest.(check (option string))
        "the revision answered" (Some "2025-11-25")
        (Canon.str_member "protocolVersion" answer);
      Alcotest.(check (option string))
        "the server's name" (Some "bactrian")
        (Option.bind
           (Canon.member "serverInfo" answer)
           (Canon.str_member "name"))

let tool_names list =
  List.filter_map (Canon.str_member "name")
    (get "tools" (Canon.list_member "tools" list))

let tools_list_is_deterministic () =
  let lists =
    List.map
      (fun era ->
        with_project ("tools-" ^ era_name era) @@ fun ~parent:_ ~project ->
        let conn = connect ~dir:project era in
        let first = C.tools_list conn.c in
        let second = C.tools_list conn.c in
        Alcotest.check json
          (era_name era ^ ": two calls give one list")
          first second;
        note conn first;
        no_secret conn;
        check_exit "exit" 0 (C.close conn.c);
        first)
      eras
  in
  let modern, legacy =
    match lists with [ m; l ] -> (m, l) | _ -> Alcotest.fail "two eras"
  in
  Alcotest.(check (list string))
    "the tools, in their fixed order"
    [
      "run_start";
      "record_start";
      "replay_start";
      "job_status";
      "job_cancel";
      "compare";
      "plan";
      "report";
      "check";
      "doctor";
      "divergences";
    ]
    (tool_names modern);
  Alcotest.check json "both eras list the same tools" (field "tools" modern)
    (field "tools" legacy);
  Alcotest.(check (option int))
    "only the modern list carries a time to live" None
    (Canon.int_member "ttlMs" legacy);
  Alcotest.(check (option int))
    "modern: ttlMs" (Some 3_600_000)
    (Canon.int_member "ttlMs" modern)

(* The client's own guard: a line that is not a reply fails the test. *)

let stray_output () =
  with_project "stray" @@ fun ~parent ~project ->
  List.iter
    (fun (what, line) ->
      let script = Filename.concat parent "stray.sh" in
      write script (Printf.sprintf "#!/bin/sh\nread request\necho '%s'\n" line);
      Unix.chmod script 0o755;
      let c = C.spawn ~exe:script ~dir:project () in
      Fun.protect ~finally:(fun () -> C.dispose c) @@ fun () ->
      match C.raw_request c "tools/list" Canon.empty_object with
      | _ -> Alcotest.failf "%s was accepted" what
      | exception C.Protocol_violation _ -> ())
    [
      ("a line of text", "starting up");
      ("a notification", {|{"jsonrpc":"2.0","method":"notifications/message"}|});
      ("a reply to another request", {|{"jsonrpc":"2.0","id":99,"result":{}}|});
    ]

(* Root confinement *)

let message_has needle e =
  Alcotest.(check bool)
    (Printf.sprintf "%s in %s" needle (Canon.to_string e))
    true
    (contains (string_field "error" e) needle)

let confinement era () =
  with_project ("confine-" ^ era_name era) @@ fun ~parent ~project ->
  let outside = Filename.concat parent "outside" in
  write (Filename.concat outside "scenario.json") scenario_text;
  write (Filename.concat outside "exp.json") "{}";
  write (Filename.concat outside "divergences.json") {|{"divergences": []}|};
  Unix.symlink outside (Filename.concat project "link");
  let conn = connect ~dir:project era in
  let refused name args needle =
    let e = error_call conn name args in
    message_has needle e;
    Alcotest.(check (option int))
      (name ^ ": a usage error") (Some 64)
      (Canon.int_member "exit" e)
  in
  let outside_root = "outside the server's working directory" in
  let str = Canon.string in
  refused "check"
    [ ("scenarios", strings [ "../outside/scenario.json" ]) ]
    outside_root;
  refused "check"
    [ ("scenarios", strings [ Filename.concat outside "scenario.json" ]) ]
    outside_root;
  refused "check" [ ("scenarios", strings [ "/etc/hosts" ]) ] outside_root;
  refused "doctor" [ ("experiment", str "link/exp.json") ] outside_root;
  refused "run_start" [ ("experiment", str "../outside/exp.json") ] outside_root;
  refused "report"
    [ ("experiment_id", str "x"); ("output", str "../outside/report.html") ]
    outside_root;
  refused "replay_start"
    [
      ("run", str "x"); ("agent", strings [ "true" ]); ("cwd", str "../outside");
    ]
    outside_root;
  (* A run ID that climbs out of .bactrian/runs. *)
  refused "divergences" [ ("run", str "../outside") ] outside_root;
  refused "divergences" [ ("run", str "./link") ] outside_root;
  refused "replay_start"
    [ ("run", str "../outside"); ("agent", strings [ "true" ]) ]
    outside_root;
  refused "divergences" [ ("run", str "..") ] "is not an ID";
  refused "compare" [ ("experiment_id", str "../outside") ] "is not an ID";
  refused "plan"
    [ ("from", str "../outside"); ("threshold", number 0.1) ]
    "is not an ID";
  refused "run_start"
    [ ("experiment", str "exp.json"); ("resume", str "../outside") ]
    "is not an ID";
  Alcotest.(check bool)
    "nothing was written outside" false
    (Sys.file_exists (Filename.concat outside "report.html"));
  message_has "no job"
    (error_call conn "job_status" [ ("job_id", str "job-none") ]);
  no_secret conn;
  check_exit "exit" 0 (C.close conn.c)

(* A replay whose agent never answers, and the processes it leaves. *)

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

type hung = { conn : conn; job : string; agent_pid : int; child_pid : int }

let with_hung_replay era f =
  let agent = List.hd agents in
  with_world ("hung-" ^ era_name era) @@ fun w ->
  let recorded = w.execute ~agent:(Run.Command (agent.argv ())) () in
  Alcotest.(check string) "record outcome" "passed" (outcome_name recorded);
  let conn = connect ~dir:w.dir era in
  Fun.protect ~finally:(fun () -> C.dispose conn.c) @@ fun () ->
  let job =
    string_field "job_id"
      (ok_call conn "replay_start"
         [
           ("run", Canon.string recorded.run_id);
           ("agent", strings [ "python3"; "-I"; "-c"; hang_script ]);
           ("sandbox", Canon.string "off");
           ("turn_timeout", Canon.int 100);
           ("total_timeout", Canon.int 200);
         ])
  in
  let agent_file = Filename.concat w.dir "agent.pid" in
  wait_until ~what:"the agent to start" ~seconds:30. (fun () ->
      Sys.file_exists agent_file && String.trim (read agent_file) <> "");
  let agent_pid = read_pid agent_file
  and child_pid = read_pid (Filename.concat w.dir "child.pid") in
  Alcotest.(check bool) "the agent is running" true (alive agent_pid);
  Alcotest.(check bool) "its child is running" true (alive child_pid);
  f { conn; job; agent_pid; child_pid }

let gone h =
  wait_gone "the agent" h.agent_pid;
  wait_gone "the agent's child" h.child_pid

let cancel era () =
  with_hung_replay era @@ fun h ->
  let status () =
    ok_call h.conn "job_status" [ ("job_id", Canon.string h.job) ]
  in
  Alcotest.(check string) "running" "running" (string_field "state" (status ()));
  let cancelled =
    ok_call h.conn "job_cancel" [ ("job_id", Canon.string h.job) ]
  in
  Alcotest.(check string)
    "cancelled" "cancelled"
    (string_field "state" cancelled);
  Alcotest.(check (option int))
    "as an interrupt ends a command" (Some 130)
    (Canon.int_member "exit" cancelled);
  Alcotest.(check string)
    "the signal is named" "interrupted by SIGINT"
    (string_field "error" (field "error" cancelled));
  gone h;
  Alcotest.check json "the job stays cancelled" cancelled (status ());
  no_secret h.conn;
  check_exit "exit" 0 (C.close h.conn.c)

let end_of_input era () =
  with_hung_replay era @@ fun h ->
  check_exit "the server exits at end of input" 0 (C.close h.conn.c);
  gone h

(* [run_start] against the fake upstream through the override, for each
   example agent in each era. *)

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

let run_start agent era () =
  let argv = agent.argv () in
  with_project ("run-start-" ^ agent.name ^ "-" ^ era_name era)
  @@ fun ~parent:_ ~project ->
  with_upstream ~dir:project @@ fun up ->
  write_experiment project argv;
  let env = (upstream_variable, upstream_url up) :: key_env in
  let conn = connect ~env ~dir:project era in
  Fun.protect ~finally:(fun () -> C.dispose conn.c) @@ fun () ->
  let job = string_field "job_id" (ok_call conn "run_start" (run_args ())) in
  let status = C.poll_job conn.c job in
  Alcotest.(check string)
    "the job succeeded" "succeeded"
    (string_field "state" status);
  Alcotest.(check string) "its kind" "run" (string_field "kind" status);
  let served = field "result" status in
  Alcotest.(check (list string))
    "every run passed"
    (List.init 4 (fun _ -> "passed"))
    (run_statuses served);
  let compared =
    ok_call conn "compare"
      [ ("experiment_id", Canon.string (string_field "experiment_id" served)) ]
  in
  check_exit "the server exits at end of input" 0 (C.close conn.c);
  no_secret conn;
  Alcotest.(check int)
    "the model calls all went to the fake upstream" 8
    (List.length (upstream_requests up));
  let code, printed = cli ~dir:project ~env run_flags in
  Alcotest.(check int) "run: the exit status" 0 code;
  Alcotest.check json "the job's result equals run --json"
    (summary_normalised printed)
    (summary_normalised served);
  let code, printed =
    cli ~dir:project ~env
      [ "compare"; string_field "experiment_id" served; "--json" ]
  in
  Alcotest.(check int)
    "compare: the exit status" code
    (Option.value (Canon.int_member "exit" compared) ~default:code);
  Alcotest.check json "compare equals the CLI's" printed compared

(* Cancelling an experiment while an agent is running, then resuming it. Every
   agent records its process ID before it replaces the shell that started it,
   so the file lists each agent that ever ran. *)

let agent_pids path = if Sys.file_exists path then lines path else []

let cancel_and_resume era () =
  let agent = List.hd agents in
  with_project ("run-resume-" ^ era_name era) @@ fun ~parent:_ ~project ->
  let hold = Filename.concat project "hold" in
  write hold "";
  with_upstream ~hold ~dir:project @@ fun up ->
  let pids = Filename.concat project "agent.pids" in
  write_experiment project
    ([ "sh"; "-c"; {|echo $$ >> agent.pids; exec "$@"|}; "sh" ] @ agent.argv ());
  let env = (upstream_variable, upstream_url up) :: key_env in
  let conn = connect ~env ~dir:project era in
  Fun.protect ~finally:(fun () -> C.dispose conn.c) @@ fun () ->
  let job = string_field "job_id" (ok_call conn "run_start" (run_args ())) in
  (* The fake upstream holds the agent's first model call. *)
  wait_until ~what:"an agent to call the model" ~seconds:30. (fun () ->
      agent_pids pids <> [] && upstream_requests up <> []);
  Alcotest.(check string)
    "the experiment is running" "running"
    (string_field "state"
       (ok_call conn "job_status" [ ("job_id", Canon.string job) ]));
  let cancelled = ok_call conn "job_cancel" [ ("job_id", Canon.string job) ] in
  Alcotest.(check string)
    "cancelled" "cancelled"
    (string_field "state" cancelled);
  let id = string_field "experiment_id" (field "error" cancelled) in
  let all_gone what =
    List.iter
      (fun pid ->
        wait_gone what
          (get "a process ID" (int_of_string_opt (String.trim pid))))
      (agent_pids pids)
  in
  all_gone "a cancelled agent";
  Alcotest.(check int) "one agent ran" 1 (List.length (agent_pids pids));
  (* Release the upstream and resume: the cancelled run starts again. *)
  Sys.remove hold;
  let resumed =
    string_field "job_id" (ok_call conn "run_start" (run_args ~resume:id ()))
  in
  let status = C.poll_job conn.c resumed in
  Alcotest.(check string)
    "the resumed job succeeded" "succeeded"
    (string_field "state" status);
  let result = field "result" status in
  Alcotest.(check string)
    "it is the same experiment" id
    (string_field "experiment_id" result);
  Alcotest.(check (list string))
    "every run passed"
    (List.init 4 (fun _ -> "passed"))
    (run_statuses result);
  Alcotest.(check int)
    "the cancelled run started again, and no other run twice" 5
    (List.length (agent_pids pids));
  all_gone "an agent";
  Alcotest.(check int)
    "the model was called twice by each of the four runs, and the cancelled \
     run's first call reached the server once"
    9
    (List.length (upstream_requests up));
  check_exit "the server exits at end of input" 0 (C.close conn.c);
  no_secret conn

(* An https upstream cannot be called in this build, so the job fails as
   [bactrian run] does. *)

let run_start_https_without_tls () =
  if Bactrian_app.Features.tls_available then Alcotest.skip ();
  with_project "run-start-https" @@ fun ~parent:_ ~project ->
  write_experiment project [ "python3"; "-I"; "-c"; "pass" ];
  let env = (upstream_variable, "https://gateway.invalid") :: key_env in
  let conn = connect ~env ~dir:project Modern in
  let job = string_field "job_id" (ok_call conn "run_start" (run_args ())) in
  let status = C.poll_job conn.c job in
  check_exit "exit" 0 (C.close conn.c);
  let code, printed = cli ~dir:project ~env run_flags in
  Alcotest.(check string) "failed" "failed" (string_field "state" status);
  Alcotest.(check int) "69, the environment is not ready" 69 code;
  Alcotest.(check (option int))
    "the job's exit status" (Some code)
    (Canon.int_member "exit" status);
  Alcotest.check json "the job's error equals run --json" printed
    (field "error" status)

let per_era test =
  List.map (fun e -> Alcotest.test_case (era_name e) `Slow (test e)) eras

let () =
  Alcotest.run "mcp client"
    [
      ( "tools equal the command line",
        List.concat_map
          (fun a ->
            List.map
              (fun e ->
                Alcotest.test_case (a.name ^ ", " ^ era_name e) `Slow (e2e a e))
              eras)
          agents );
      ( "an unknown protocol version",
        [
          Alcotest.test_case "as the first request" `Quick
            (unknown_version ~connected:false);
          Alcotest.test_case "on a modern connection" `Quick
            (unknown_version ~connected:true);
        ] );
      ( "a request before the era is known",
        [ Alcotest.test_case "ping and refusals" `Quick request_before_era ] );
      ("opening a connection", per_era opening);
      ("an unknown tool", per_era unknown_tool);
      ( "tools/list",
        [
          Alcotest.test_case "deterministic" `Quick tools_list_is_deterministic;
        ] );
      ( "stdout holds only replies",
        [ Alcotest.test_case "the client's guard" `Quick stray_output ] );
      ("root confinement", per_era confinement);
      ("job_cancel", per_era cancel);
      ("end of input with a job running", per_era end_of_input);
      ( "run_start through the override",
        List.concat_map
          (fun a ->
            List.map
              (fun e ->
                Alcotest.test_case
                  (a.name ^ ", " ^ era_name e)
                  `Slow (run_start a e))
              eras)
          agents );
      ("job_cancel, then resume", per_era cancel_and_resume);
      ( "run_start with an https upstream",
        [
          Alcotest.test_case "fails as run does" `Quick
            run_start_https_without_tls;
        ] );
    ]
