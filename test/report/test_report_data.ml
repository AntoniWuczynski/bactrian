open Bactrian_core
module R = Report_data

let sigs = Alcotest.(list (list string))

(* Signatures *)

let tool name seq : Trace.entry =
  {
    seq;
    t_ms = seq;
    event =
      Tool_call
        { turn = 1; index = seq; name; arguments = Canon.null; mcp = Joined };
  }

let test_signature () =
  Alcotest.(check (list string))
    "tool names in order" [ "a"; "b"; "a" ]
    (R.signature
       [
         tool "a" 0; { seq = 1; t_ms = 1; event = Done }; tool "b" 2; tool "a" 3;
       ]);
  Alcotest.(check (list string)) "no tools" [] (R.signature [])

let repeat n x = List.init n (fun _ -> x)

let test_new_behaviours () =
  let old = [ "a" ] and fresh = [ "a"; "b" ] in
  Alcotest.check sigs "exactly 10% counts" [ fresh ]
    (R.new_behaviours ~baseline:(repeat 10 old)
       ~candidate:(fresh :: repeat 9 old));
  Alcotest.check sigs "1 in 11 is under 10%" []
    (R.new_behaviours ~baseline:(repeat 10 old)
       ~candidate:(fresh :: repeat 10 old));
  Alcotest.check sigs "seen in the baseline is not new" []
    (R.new_behaviours ~baseline:(fresh :: repeat 9 old)
       ~candidate:(repeat 5 fresh @ repeat 5 old));
  Alcotest.check sigs "no baseline runs, nothing to compare" []
    (R.new_behaviours ~baseline:[] ~candidate:[ fresh ]);
  Alcotest.check sigs "no candidate runs" []
    (R.new_behaviours ~baseline:[ old ] ~candidate:[]);
  Alcotest.check sigs "the empty signature can be new" [ [] ]
    (R.new_behaviours ~baseline:(repeat 5 old)
       ~candidate:(repeat 5 old @ repeat 5 []));
  Alcotest.check sigs "commoner first, then by name"
    [ [ "z" ]; [ "b" ]; [ "c" ] ]
    (R.new_behaviours ~baseline:(repeat 10 old)
       ~candidate:(repeat 4 [ "z" ] @ repeat 3 [ "c" ] @ repeat 3 [ "b" ]))

(* Measures and mapping *)

let test_measures () =
  let tr = Fixture.trace ~tools:[ "t" ] ~ms:700 ~tokens:100 in
  Alcotest.(check (option (float 0.))) "latency" (Some 700.) (R.latency_ms tr);
  Alcotest.(check (option int)) "tokens" (Some 110) (R.tokens tr);
  Alcotest.(check (option (float 0.))) "no model call" None (R.latency_ms []);
  Alcotest.(check (option int)) "no usage" None (R.tokens [])

let test_compare_run () =
  let run status = { (List.hd Fixture.runs) with R.status } in
  let kind s =
    let r = R.compare_run (run s) in
    (r.passed, r.missing)
  in
  Alcotest.(check bool) "passed" true (kind Passed = (Some true, None));
  Alcotest.(check bool) "failed" true (kind Failed = (Some false, None));
  Alcotest.(check bool) "diverged" true (kind Diverged = (Some false, None));
  Alcotest.(check bool)
    "errored" true
    (kind Errored = (None, Some Compare.Errored));
  List.iter
    (fun s ->
      Alcotest.(check bool)
        "unfinished is not_run" true
        (kind s = (None, Some Compare.Not_run)))
    [ R.Not_run; Pending; Interrupted ]

(* The fixture experiment *)

let d = Fixture.data ()

let scenario name =
  List.find (fun (b : R.behaviour) -> b.scenario = name) d.behaviours

let test_behaviours () =
  let refund = scenario "refund" in
  Alcotest.check sigs "new behaviour"
    [ [ "lookup_order"; "refund"; "notify" ] ]
    refund.new_signatures;
  Alcotest.(check int) "traced baseline" 10 refund.baseline_traced;
  Alcotest.(check bool)
    "b distribution" true
    (refund.candidate_signatures
    = [
        { R.signature = [ "lookup_order"; "refund" ]; count = 7 };
        { signature = [ "lookup_order"; "refund"; "notify" ]; count = 3 };
      ]);
  Alcotest.check sigs "search has none" [] (scenario "search").new_signatures;
  let book = scenario "book" in
  Alcotest.(check int)
    "errored and not-run have no trace" 8 book.candidate_traced;
  Alcotest.(check (list (pair int int)))
    "steps"
    [ (1, 10) ]
    refund.baseline_steps

let test_failures () =
  let msgs =
    List.map
      (fun (f : R.failure) -> (f.run.variant, f.run.scenario, f.messages))
      d.failures
  in
  Alcotest.(check int) "3 refund b, 2 book a, 1 errored" 6 (List.length msgs);
  Alcotest.(check bool)
    "assertion message kept" true
    (List.mem
       ( "b",
         "refund",
         [
           "assertions[0] tool_sequence: expected lookup_order then refund, \
            saw an extra notify";
         ] )
       msgs);
  Alcotest.(check bool)
    "errored reason" true
    (List.mem ("b", "book", [ "proxy fault" ]) msgs);
  Alcotest.(check bool)
    "no not_run" false
    (List.exists (fun (f : R.failure) -> f.run.status = Not_run) d.failures);
  Alcotest.(check bool)
    "failures hold their trace" true
    (List.for_all
       (fun (f : R.failure) -> f.run.status = Errored || f.run.trace <> [])
       d.failures)

let test_header () =
  Alcotest.(check bool)
    "missing data per arm" true
    (d.missing = [ ("a", 30, 0, 0); ("b", 30, 1, 1) ]);
  Alcotest.(check bool) "gate" true (d.gate = [ ("pass_rate", 0.1) ]);
  Alcotest.(check (float 1e-12))
    "one gate metric: no adjustment" 0.95 d.gate_level;
  Alcotest.(check (list int))
    "execution order" (List.init 60 Fun.id)
    (List.map (fun (r : R.run) -> r.index) d.order);
  Alcotest.(check (list string))
    "metrics"
    [ "pass_rate"; "cost"; "latency_p50" ]
    (List.map (fun (m : R.metric_row) -> m.name) d.metrics);
  Alcotest.(check (list string))
    "scenario rows sorted"
    [ "book"; "refund"; "search" ]
    (List.map (fun (s : R.scenario_row) -> s.scenario) d.scenarios);
  let refund =
    List.find (fun (s : R.scenario_row) -> s.scenario = "refund") d.scenarios
  in
  Alcotest.(check (pair int int))
    "refund counts" (7, 10)
    (refund.candidate_passes, refund.candidate_runs);
  Alcotest.(check (float 1e-12)) "difference" (-0.3) refund.difference;
  let cost = List.find (fun (m : R.metric_row) -> m.name = "cost") d.metrics in
  Alcotest.(check bool)
    "cost back-transformed" true
    (match cost.relative with
    | Some (e, _) -> Float.abs (e -. Float.expm1 cost.result.estimate) < 1e-12
    | None -> false);
  let pr = List.hd d.metrics in
  Alcotest.(check bool) "pass rate has no relative" true (pr.relative = None);
  Alcotest.(check bool)
    "summaries: both arms" true
    (List.map (fun (s : R.summary) -> s.variant) d.summaries = [ "a"; "b" ])

let test_gate_family_level () =
  let settings =
    {
      (Fixture.settings ()) with
      gate = [ (Pass_rate, 0.1); (Cost, 0.2); (Latency_p50, 0.25) ];
      report_metrics = [];
    }
  in
  match Compare.run settings with
  | Error m -> Alcotest.fail m
  | Ok result ->
      let t =
        R.build ~experiment_id:"x" ~experiment:"x" ~settings ~result
          Fixture.runs
      in
      Alcotest.(check (float 1e-12))
        "Bonferroni over three"
        (1. -. (0.05 /. 3.))
        t.gate_level

(* Every inconclusive gate metric carries the simulator's answer, and no other
   metric does. *)
let test_extra_repeats () =
  let settings = Fixture.settings () in
  let pass_rate = List.hd d.metrics in
  Alcotest.(check bool)
    "pass rate is an inconclusive gate metric" true
    (match pass_rate.result.verdict with
    | Inconclusive _ -> pass_rate.result.in_gate
    | Regression | Improvement | No_change -> false);
  Alcotest.(check bool)
    "same as the simulator" true
    (pass_rate.extra_repeats
    = Some
        (Plan_sim.extra_runs_needed settings ~metric:Pass_rate
          :> R.extra_repeats));
  Alcotest.(check bool)
    "a number of repeats" true
    (match pass_rate.extra_repeats with
    | Some (`Repeats n) -> n > 0
    | Some (`Cannot_resolve | `Not_simulated) | None -> false);
  List.iter
    (fun (m : R.metric_row) ->
      Alcotest.(check bool)
        (m.name ^ ": outside the gate, none")
        true
        (m.result.in_gate || Option.is_none m.extra_repeats))
    d.metrics

(* JSON *)

let json ?seed () =
  Yojson.Safe.pretty_to_string (R.to_json (Fixture.data ?seed ()))

let test_json_deterministic () =
  Alcotest.(check string)
    "same seed, same bytes" (json ~seed:7 ()) (json ~seed:7 ());
  let j = R.to_json d in
  Alcotest.(check int)
    "schema version" 1
    Yojson.Safe.Util.(to_int (member "schema_version" j));
  Alcotest.(check (list string))
    "key order"
    [
      "schema_version";
      "experiment_id";
      "experiment";
      "baseline";
      "candidate";
      "estimand";
      "confidence";
      "gate";
      "seed";
      "resamples";
      "missing";
      "dropped_scenarios";
      "exit";
      "metrics";
      "scenarios";
      "summaries";
      "behaviours";
      "failures";
      "order";
    ]
    (Yojson.Safe.Util.keys j)

(* Everything a seeded simulation touches: the intervals, the verdicts read
   off them, the extra repeats and the exit status. *)
let strip (j : Yojson.Safe.t) : Yojson.Safe.t =
  match j with
  | `Assoc fields ->
      `Assoc
        (List.filter_map
           (fun (k, v) ->
             match (k, v) with
             | ("exit" | "seed"), _ -> None
             | "metrics", `List ms ->
                 Some
                   ( k,
                     `List
                       (List.map
                          (function
                            | `Assoc f ->
                                `Assoc
                                  (List.filter
                                     (fun (k, _) ->
                                       not
                                         (List.mem k
                                            [
                                              "interval";
                                              "relative";
                                              "result";
                                              "extra_repeats";
                                            ]))
                                     f)
                            | other -> other)
                          ms) )
             | _ -> Some (k, v))
           fields)
  | other -> other

let test_json_seed () =
  let a = R.to_json (Fixture.data ~seed:7 ())
  and b = R.to_json (Fixture.data ~seed:8 ()) in
  Alcotest.(check bool) "bootstrap intervals differ" true (a <> b);
  Alcotest.(check string)
    "nothing else does"
    (Yojson.Safe.pretty_to_string (strip a))
    (Yojson.Safe.pretty_to_string (strip b))

(* A run that failed for its own reason and also has verdicts (they are
   written for every run that finished) keeps both messages. *)
let test_agent_failure_keeps_its_reason () =
  let template = List.hd Fixture.runs in
  let run : R.run =
    {
      template with
      status = Failed;
      reason = Some "timeout: turn";
      checks =
        [
          {
            name = "assertions[0] max_steps";
            status = Check_fail;
            reason = Some "3 > 2";
          };
        ];
    }
  in
  let runs = run :: List.tl Fixture.runs in
  let settings = Fixture.settings () in
  let result = Result.get_ok (Compare.run settings) in
  let d =
    R.build ~experiment_id:"x" ~experiment:"demo" ~settings ~result runs
  in
  let f =
    List.find (fun (f : R.failure) -> f.run.index = template.index) d.failures
  in
  Alcotest.(check (list string))
    "both"
    [ "timeout: turn"; "assertions[0] max_steps: 3 > 2" ]
    f.messages

let () =
  Alcotest.run "report_data"
    [
      ( "behaviour",
        [
          Alcotest.test_case "signature" `Quick test_signature;
          Alcotest.test_case "new behaviours" `Quick test_new_behaviours;
        ] );
      ( "inputs",
        [
          Alcotest.test_case "measures" `Quick test_measures;
          Alcotest.test_case "compare_run" `Quick test_compare_run;
        ] );
      ( "fixture",
        [
          Alcotest.test_case "behaviours" `Quick test_behaviours;
          Alcotest.test_case "failures" `Quick test_failures;
          Alcotest.test_case "agent failure keeps its reason" `Quick
            test_agent_failure_keeps_its_reason;
          Alcotest.test_case "header and rows" `Quick test_header;
          Alcotest.test_case "gate family level" `Quick test_gate_family_level;
          Alcotest.test_case "extra repeats" `Quick test_extra_repeats;
        ] );
      ( "json",
        [
          Alcotest.test_case "deterministic" `Quick test_json_deterministic;
          Alcotest.test_case "seed" `Quick test_json_seed;
        ] );
    ]
