(* What a run writes about itself: verdicts, divergences, cost, and the
   patterns a replay needs. *)

open Bactrian_runtime
open Run_support
module P = Model_proxy
module J = Yojson.Safe.Util

let strict =
  { Run.from = ""; matcher = Strict; allow_reorder = false; timing = P.Fast }

let checks ?(assertions = []) ?(checkers = []) () : Run.checks option =
  Some { assertions; checkers; checks_cwd = "."; checker_timeout = 10. }

let json (r : Run.result) name = Yojson.Safe.from_file (file r name)
let failing_agent = [ "python3"; "-I"; "-c"; "import sys; sys.exit(3)" ]

let verdict_names (r : Run.result) =
  List.map
    (fun item -> J.(to_string (member "verdict" item)))
    J.(to_list (member "items" (json r "verdicts.json")))

(* A checker that cannot give a verdict is a fault outside the agent. *)
let test_checker_error_is_errored =
  with_ctx "checker-error" @@ fun ctx ->
  let broken = [ "python3"; "-I"; "-c"; "import sys; sys.exit(3)" ] in
  let r =
    ok
      (execute ctx { (config ()) with checks = checks ~checkers:[ broken ] () })
  in
  Alcotest.(check string) "kind" "errored" (Run.outcome_kind r.outcome);
  Alcotest.(check bool)
    "reason names the checker" true
    (String.starts_with ~prefix:"checker: "
       (Option.value ~default:"" (Run.outcome_reason r.outcome)));
  Alcotest.check strs "verdicts.json" [ "checker_failed" ] (verdict_names r)

let test_checker_fail_is_failed =
  with_ctx "checker-fail" @@ fun ctx ->
  let r =
    ok
      (execute ctx
         { (config ()) with checks = checks ~assertions:[ Max_steps 0 ] () })
  in
  Alcotest.(check string) "kind" "failed" (Run.outcome_kind r.outcome)

(* An agent failure still gets its assertions evaluated and written. *)
let test_failed_run_has_verdicts =
  with_ctx "failed-verdicts" @@ fun ctx ->
  let r =
    ok
      (execute ctx
         {
           (config ~agent:failing_agent ()) with
           checks = checks ~assertions:[ Max_steps 0; Max_steps 5 ] ();
         })
  in
  (match r.outcome with
  | Failed reason ->
      Alcotest.(check bool)
        "the outcome stays the agent's failure" true
        (contains reason "exit_nonzero" || contains reason "protocol_violation");
      Alcotest.(check bool)
        "assertions do not change the reason" false
        (contains reason "max_steps")
  | Passed | Diverged | Errored _ | Not_run _ ->
      Alcotest.failf "expected Failed, got %s" (outcome_name r));
  Alcotest.check strs "verdicts.json" [ "pass"; "pass" ] (verdict_names r)

(* Divergences are persisted with their diff paths. *)
let test_divergences_file =
  with_ctx "divergences-file" @@ fun ctx ->
  let recorded = record ctx in
  Alcotest.(check int)
    "a record run writes an empty list" 0
    (List.length
       J.(to_list (member "divergences" (json recorded "divergences.json"))));
  let changed =
    config
      ~agent:[ "python3"; "-I"; "-c"; python_agent; "You are terse." ]
      ~replay:{ strict with from = recorded.run_id }
      ~turns:[] ()
  in
  let r = ok (execute ctx changed) in
  let all = J.(to_list (member "divergences" (json r "divergences.json"))) in
  Alcotest.(check int)
    "as many as in the findings"
    (List.length r.findings.divergences)
    (List.length all);
  let missing =
    List.find
      (fun d -> String.equal J.(to_string (member "kind" d)) "missing_request")
      all
  in
  Alcotest.(check bool)
    "nearest sequence number" true
    (J.(to_int (member "seq" missing)) >= 0);
  Alcotest.(check bool)
    "similarity" true
    (let s = J.(to_float (member "similarity" missing)) in
     s > 0. && s < 1.);
  Alcotest.(check bool)
    "diff paths" true
    (not (List.is_empty J.(to_list (member "changes" missing))));
  Alcotest.(check bool)
    "no temporary file" false
    (Sys.file_exists (file r "divergences.json.tmp"))

(* Cost, latency and tokens are stored once, in meta.json. *)
let test_meta_totals =
  with_ctx "meta-totals" @@ fun ctx ->
  let r = record ctx in
  let meta = json r "meta.json" in
  Alcotest.(check bool) "cost" true (r.spend > 0.);
  Alcotest.(check (float 1e-12))
    "meta cost is the run's result" r.spend
    J.(to_float (member "cost_usd" meta));
  let trace_tokens =
    List.fold_left
      (fun acc line ->
        let j = Yojson.Safe.from_string line in
        match J.(member "gen_ai.usage.input_tokens" j) with
        | `Null -> acc
        | input ->
            acc + J.to_int input
            + J.(to_int (member "gen_ai.usage.output_tokens" j)))
      0
      (lines (file r "trace.jsonl"))
  in
  Alcotest.(check bool) "the trace has usage" true (trace_tokens > 0);
  Alcotest.(check int) "tokens" trace_tokens J.(to_int (member "tokens" meta));
  Alcotest.(check bool)
    "latency" true
    (J.(to_float (member "latency_ms" meta)) >= 0.)

(* The patterns a run recorded under are saved, and a replay uses them. *)
let replays_with_patterns name ~record_config ~key =
  with_ctx name @@ fun ctx ->
  let recorded = ok (execute ctx record_config) in
  Alcotest.(check string) "recorded" "passed" (outcome_name recorded);
  Alcotest.check strs
    ("meta.json lists the " ^ key)
    [ "Paris" ]
    (List.map J.to_string J.(to_list (member key (json recorded "meta.json"))));
  let r =
    ok
      (execute ctx
         (config ~turns:[] ~replay:{ strict with from = recorded.run_id } ()))
  in
  Alcotest.(check string) "strict replay" "passed" (outcome_name r);
  Alcotest.(check int) "no divergences" 0 (List.length r.findings.divergences);
  Alcotest.check strs
    ("the replay carries the " ^ key ^ " on")
    [ "Paris" ]
    (List.map J.to_string J.(to_list (member key (json r "meta.json"))))

let test_replay_normalise =
  replays_with_patterns "replay-normalise"
    ~record_config:{ (config ()) with normalise = [ "Paris" ] }
    ~key:"normalise"

let test_replay_redact =
  replays_with_patterns "replay-redact"
    ~record_config:{ (config ()) with redact = [ "Paris" ] }
    ~key:"redact"

let test_unenforced_in_meta =
  with_ctx "unenforced" @@ fun ctx ->
  let r =
    ok (execute ctx { (config ()) with unenforced = [ "egress"; "mcp" ] })
  in
  Alcotest.check strs "meta.json" [ "egress"; "mcp" ]
    (List.map J.to_string
       J.(to_list (member "unenforced" (json r "meta.json"))))

let () =
  Alcotest.run "run_checks"
    [
      ( "outcome",
        [
          Alcotest.test_case "a checker error is Errored" `Slow
            test_checker_error_is_errored;
          Alcotest.test_case "a failing assertion is Failed" `Slow
            test_checker_fail_is_failed;
          Alcotest.test_case "a failed run still has verdicts" `Slow
            test_failed_run_has_verdicts;
        ] );
      ( "files",
        [
          Alcotest.test_case "divergences.json" `Slow test_divergences_file;
          Alcotest.test_case "cost, latency and tokens in meta.json" `Slow
            test_meta_totals;
          Alcotest.test_case "unenforced settings in meta.json" `Slow
            test_unenforced_in_meta;
        ] );
      ( "replay",
        [
          Alcotest.test_case "normalise patterns reach the replay" `Slow
            test_replay_normalise;
          Alcotest.test_case "redact patterns reach the replay" `Slow
            test_replay_redact;
        ] );
    ]
