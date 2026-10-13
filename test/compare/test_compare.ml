open Bactrian_core
module C = Compare

let iv lower upper : Stats.interval = { lower; upper }

let verdict_t =
  Alcotest.testable
    (fun ppf (v : C.verdict) ->
      Fmt.string ppf
        (match v with
        | Regression -> "Regression"
        | Improvement -> "Improvement"
        | No_change -> "No_change"
        | Inconclusive { lean = `Worse } -> "Inconclusive/worse"
        | Inconclusive { lean = `Better } -> "Inconclusive/better"
        | Inconclusive { lean = `None } -> "Inconclusive/none"))
    ( = )

let inc lean : C.verdict = Inconclusive { lean }

(* All numbers below are exact binary fractions, so closed-interval
   comparisons against the threshold are exact. *)
let d = 0.25
let decide_lower i = C.decide ~threshold:d ~worse_is:`Lower i
let decide_higher i = C.decide ~threshold:d ~worse_is:`Higher i

let test_decide_lower () =
  let chk name e i = Alcotest.check verdict_t name e (decide_lower i) in
  chk "wholly below -d" Regression (iv (-0.75) (-0.5));
  chk "wholly above +d" Improvement (iv 0.5 0.75);
  chk "inside +-d" No_change (iv (-0.125) 0.125);
  chk "inside, touching both ends" No_change (iv (-0.25) 0.25);
  chk "straddles -d, leans worse" (inc `Worse) (iv (-0.5) 0.0625);
  chk "straddles +d, leans better" (inc `Better) (iv (-0.0625) 0.5);
  chk "straddles both, centred" (inc `None) (iv (-0.5) 0.5);
  chk "closed interval ending at -d is not wholly worse" (inc `Worse)
    (iv (-0.5) (-0.25));
  chk "closed interval starting at +d is not wholly better" (inc `Better)
    (iv 0.25 0.5);
  chk "just below -d" Regression (iv (-0.5) (Float.pred (-0.25)))

let test_decide_higher () =
  let chk name e i = Alcotest.check verdict_t name e (decide_higher i) in
  chk "wholly above +d is worse" Regression (iv 0.5 0.75);
  chk "wholly below -d is better" Improvement (iv (-0.75) (-0.5));
  chk "inside" No_change (iv (-0.125) 0.125);
  chk "starting at +d is not wholly worse" (inc `Worse) (iv 0.25 0.5);
  chk "ending at -d is not wholly better" (inc `Better) (iv (-0.5) (-0.25));
  chk "straddling leans worse" (inc `Worse) (iv (-0.0625) 0.5);
  chk "straddling leans better" (inc `Better) (iv (-0.5) 0.0625)

let run_of ?(missing = None) ?(cost = None) ?(latency = None) ?(tokens = None)
    scenario variant passed : C.run =
  { scenario; variant; passed; missing; cost; latency_ms = latency; tokens }

(* [n] runs of [variant] in [scenario], the first [k] pass. *)
let runs scenario variant ~n ~k =
  List.init n (fun i -> run_of scenario variant (Some (i < k)))

let scenarios n = List.init n (fun i -> Printf.sprintf "s%02d" i)

let suite_runs ?(sc = 3) ~n ~kb ~kc () =
  List.concat_map
    (fun s -> runs s "A" ~n ~k:kb @ runs s "B" ~n ~k:kc)
    (scenarios sc)

let input ?(population = `Suite) ?(gate = [ (C.Pass_rate, 0.1) ])
    ?(report_metrics = []) ?(seed = 7) ?(fail_on_inconclusive = false) runs :
    C.input =
  {
    runs;
    baseline = "A";
    candidate = "B";
    population;
    confidence = 0.95;
    gate;
    report_metrics;
    seed;
    resamples = 500;
    fail_on_inconclusive;
  }

let ok = function
  | Ok r -> r
  | Error e -> Alcotest.failf "unexpected Error %s" e

let metric (r : C.result) m =
  match List.find_opt (fun (x : C.metric_result) -> x.metric = m) r.metrics with
  | Some x -> x
  | None -> Alcotest.fail "metric missing"

let test_regression_exit () =
  let r = ok (C.run (input (suite_runs ~n:40 ~kb:40 ~kc:0 ()))) in
  let m = metric r C.Pass_rate in
  Alcotest.check verdict_t "regression" Regression m.verdict;
  Alcotest.(check (float 1e-12)) "estimate" (-1.0) m.estimate;
  Alcotest.(check bool) "in gate" true m.in_gate;
  Alcotest.(check int) "exit" 1 r.exit

let test_improvement_no_regression () =
  let r = ok (C.run (input (suite_runs ~n:40 ~kb:0 ~kc:40 ()))) in
  Alcotest.check verdict_t "improvement" Improvement
    (metric r C.Pass_rate).verdict;
  Alcotest.(check int) "exit" 0 r.exit

let test_no_change () =
  let r = ok (C.run (input (suite_runs ~n:40 ~kb:40 ~kc:40 ()))) in
  Alcotest.check verdict_t "no change" No_change (metric r C.Pass_rate).verdict;
  Alcotest.(check int) "exit" 0 r.exit

let test_inconclusive_exit () =
  let rs = suite_runs ~sc:2 ~n:4 ~kb:2 ~kc:2 () in
  let r = ok (C.run (input rs)) in
  (match (metric r C.Pass_rate).verdict with
  | Inconclusive _ -> ()
  | Regression | Improvement | No_change ->
      Alcotest.fail "expected inconclusive");
  Alcotest.(check int) "exit without flag" 0 r.exit;
  let r = ok (C.run (input ~fail_on_inconclusive:true rs)) in
  Alcotest.(check int) "exit with flag" 2 r.exit

let test_bonferroni () =
  let rs =
    List.concat_map
      (fun s ->
        List.init 20 (fun i ->
            run_of s "A"
              (Some (i mod 2 = 0))
              ~cost:(Some (1. +. float i))
              ~latency:(Some (100. +. float i))
              ~tokens:(Some (10 + i)))
        @ List.init 20 (fun i ->
            run_of s "B"
              (Some (i mod 2 = 0))
              ~cost:(Some (1. +. float i))
              ~latency:(Some (100. +. float i))
              ~tokens:(Some (10 + i))))
      (scenarios 3)
  in
  let r =
    ok
      (C.run
         (input
            ~gate:[ (C.Pass_rate, 0.1); (C.Cost, 0.2); (C.Latency_p50, 0.2) ]
            ~report_metrics:[ (C.Tokens, 0.2) ]
            rs))
  in
  let level m = (metric r m).level in
  let want = 1. -. (0.05 /. 3.) in
  Alcotest.(check (float 1e-12)) "pass rate" want (level C.Pass_rate);
  Alcotest.(check (float 1e-12)) "cost" want (level C.Cost);
  Alcotest.(check (float 1e-12)) "latency" want (level C.Latency_p50);
  Alcotest.(check (float 1e-12)) "non-gate unadjusted" 0.95 (level C.Tokens);
  Alcotest.(check bool) "tokens outside gate" false (metric r C.Tokens).in_gate;
  Alcotest.(check (float 1e-12))
    "cost threshold is log(1+d)" (log 1.2) (metric r C.Cost).threshold;
  Alcotest.(check (float 1e-12))
    "log ratio of identical" 0. (metric r C.Cost).estimate;
  let c = metric r C.Cost in
  Alcotest.(check bool)
    "interval of identical data contains 0" true
    (c.interval.lower <= 0. && 0. <= c.interval.upper)

let test_cost_regression () =
  let arm v c =
    List.init 30 (fun i ->
        run_of "s" v (Some true) ~cost:(Some (c +. (float i *. 0.001))))
  in
  let rs = arm "A" 1.0 @ arm "B" 2.0 in
  let r = ok (C.run (input ~gate:[ (C.Cost, 0.2) ] rs)) in
  let m = metric r C.Cost in
  Alcotest.check verdict_t "doubling cost regresses" Regression m.verdict;
  Alcotest.(check bool)
    "log ratio near log 2" true
    (Float.abs (m.estimate -. log 2.) < 0.02);
  Alcotest.(check int) "exit" 1 r.exit

let test_per_scenario_bonferroni () =
  let r = ok (C.run (input (suite_runs ~sc:4 ~n:10 ~kb:5 ~kc:5 ()))) in
  Alcotest.(check int) "four scenarios" 4 (List.length r.per_scenario);
  let level = 1. -. (0.05 /. 4.) in
  let want = Stats.newcombe_diff ~s1:5 ~n1:10 ~s2:5 ~n2:10 ~confidence:level in
  List.iter
    (fun (_, (i : Stats.interval), _) ->
      Alcotest.(check (float 1e-12)) "lower" want.lower i.lower;
      Alcotest.(check (float 1e-12)) "upper" want.upper i.upper)
    r.per_scenario

let test_scenarios_population () =
  let few =
    input ~population:`Scenarios (suite_runs ~sc:9 ~n:5 ~kb:3 ~kc:3 ())
  in
  (match C.run few with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "9 scenarios must be rejected");
  let one =
    input ~population:`Scenarios (suite_runs ~sc:1 ~n:5 ~kb:3 ~kc:3 ())
  in
  (match C.run one with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "1 scenario must be rejected");
  let enough =
    input ~population:`Scenarios (suite_runs ~sc:10 ~n:5 ~kb:3 ~kc:3 ())
  in
  ignore (ok (C.run enough));
  ignore (ok (C.run (input (suite_runs ~sc:1 ~n:5 ~kb:3 ~kc:3 ()))))

let missing_runs ~errored ~not_run variant =
  let ok_runs = runs "s" variant ~n:(100 - errored - not_run) ~k:50 in
  ok_runs
  @ List.init errored (fun _ ->
      run_of "s" variant None ~missing:(Some C.Errored))
  @ List.init not_run (fun _ ->
      run_of "s" variant None ~missing:(Some C.Not_run))

let counts (r : C.result) = r.missing

let test_missing_counts_and_exit () =
  let rs e n =
    missing_runs ~errored:e ~not_run:n "A"
    @ missing_runs ~errored:e ~not_run:n "B"
  in
  let r = ok (C.run (input (rs 3 4))) in
  Alcotest.(check (list (triple string int int)))
    "counted"
    [ ("A", 3, 4); ("B", 3, 4) ]
    (counts r);
  Alcotest.(check int) "within limits" 0 r.exit;
  (* exactly 5% errored is not "more than 5%" *)
  let r = ok (C.run (input (rs 5 0))) in
  Alcotest.(check int) "exactly 5% errored" 0 r.exit;
  let r = ok (C.run (input (rs 6 0))) in
  Alcotest.(check int) "6% errored" 3 r.exit;
  (* one arm only *)
  let r =
    ok
      (C.run
         (input
            (missing_runs ~errored:0 ~not_run:0 "A"
            @ missing_runs ~errored:6 ~not_run:0 "B")))
  in
  Alcotest.(check int) "6% errored in candidate only" 3 r.exit;
  (* not_run share differs by 6 points, errored below 5% *)
  let r =
    ok
      (C.run
         (input
            (missing_runs ~errored:0 ~not_run:0 "A"
            @ missing_runs ~errored:0 ~not_run:6 "B")))
  in
  Alcotest.(check int) "share differs by 6 points" 3 r.exit;
  let r =
    ok
      (C.run
         (input
            (missing_runs ~errored:0 ~not_run:0 "A"
            @ missing_runs ~errored:0 ~not_run:5 "B")))
  in
  Alcotest.(check int) "share differs by exactly 5 points" 0 r.exit;
  (* regression outranks the data-quality code *)
  let r =
    ok
      (C.run
         (input
            (suite_runs ~n:40 ~kb:40 ~kc:0 ()
            @ missing_runs ~errored:50 ~not_run:0 "B")))
  in
  Alcotest.(check int) "regression wins" 1 r.exit

let test_missing_excluded () =
  (* A: 10 pass, 10 errored. Errored runs must not count as failures. *)
  let rs =
    runs "s" "A" ~n:10 ~k:10
    @ List.init 10 (fun _ -> run_of "s" "A" None ~missing:(Some C.Errored))
    @ runs "s" "B" ~n:10 ~k:10
  in
  let r = ok (C.run (input rs)) in
  Alcotest.(check (float 1e-12)) "estimate" 0. (metric r C.Pass_rate).estimate

let test_determinism () =
  let rs = suite_runs ~n:10 ~kb:6 ~kc:5 () in
  let a = ok (C.run (input ~seed:3 rs)) in
  let b = ok (C.run (input ~seed:3 rs)) in
  Alcotest.(check bool) "same seed, same result" true (a = b);
  let c = ok (C.run (input ~seed:4 rs)) in
  let ma = metric a C.Pass_rate and mc = metric c C.Pass_rate in
  Alcotest.(check (float 0.))
    "estimate independent of seed" ma.estimate mc.estimate;
  Alcotest.(check bool)
    "per-scenario independent of seed" true
    (a.per_scenario = c.per_scenario)

let test_undefined_median () =
  let arm v c =
    List.init 10 (fun _ -> run_of "s" v (Some true) ~cost:(Some c))
  in
  let r =
    ok (C.run (input ~gate:[ (C.Cost, 0.2) ] (arm "A" 0. @ arm "B" 1.)))
  in
  let m = metric r C.Cost in
  Alcotest.(check bool) "undefined estimate" true (Float.is_nan m.estimate);
  Alcotest.(check bool) "cannot gate" false m.in_gate;
  Alcotest.(check int) "exit" 0 r.exit

let test_swap_negates () =
  let rs = suite_runs ~n:10 ~kb:8 ~kc:3 () in
  let fwd = ok (C.run (input rs)) in
  let swapped =
    ok (C.run { (input rs) with baseline = "B"; candidate = "A" })
  in
  Alcotest.(check (float 1e-12))
    "estimate negated"
    (-.(metric fwd C.Pass_rate).estimate)
    (metric swapped C.Pass_rate).estimate

let test_no_variant_runs () =
  match C.run (input (runs "s" "A" ~n:5 ~k:5)) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "candidate without runs must be an Error"

let () =
  Alcotest.run "compare"
    [
      ( "decide",
        [
          Alcotest.test_case "worse is lower" `Quick test_decide_lower;
          Alcotest.test_case "worse is higher" `Quick test_decide_higher;
        ] );
      ( "run",
        [
          Alcotest.test_case "regression exits 1" `Quick test_regression_exit;
          Alcotest.test_case "improvement exits 0" `Quick
            test_improvement_no_regression;
          Alcotest.test_case "no change" `Quick test_no_change;
          Alcotest.test_case "inconclusive exits 2" `Quick
            test_inconclusive_exit;
          Alcotest.test_case "bonferroni" `Quick test_bonferroni;
          Alcotest.test_case "cost regression" `Quick test_cost_regression;
          Alcotest.test_case "per-scenario bonferroni" `Quick
            test_per_scenario_bonferroni;
          Alcotest.test_case "scenarios needs 10" `Quick
            test_scenarios_population;
          Alcotest.test_case "missing data" `Quick test_missing_counts_and_exit;
          Alcotest.test_case "missing excluded" `Quick test_missing_excluded;
          Alcotest.test_case "determinism" `Quick test_determinism;
          Alcotest.test_case "undefined median" `Quick test_undefined_median;
          Alcotest.test_case "swap negates" `Quick test_swap_negates;
          Alcotest.test_case "missing variant" `Quick test_no_variant_runs;
        ] );
    ]
