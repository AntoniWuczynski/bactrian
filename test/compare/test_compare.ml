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
  (* A declared gate metric that is undefined stays in the gate as
     inconclusive, so it cannot pass silently. *)
  Alcotest.(check bool) "still in gate" true m.in_gate;
  Alcotest.check verdict_t "inconclusive" (inc `None) m.verdict;
  Alcotest.(check int) "exit without flag" 0 r.exit;
  let r =
    ok
      (C.run
         (input ~fail_on_inconclusive:true
            ~gate:[ (C.Cost, 0.2) ]
            (arm "A" 0. @ arm "B" 1.)))
  in
  Alcotest.(check int) "exit with flag" 2 r.exit

let test_undefined_without_values () =
  (* Gate [pass_rate; cost] with no cost data at all. *)
  let r =
    ok
      (C.run
         (input ~fail_on_inconclusive:true
            ~gate:[ (C.Pass_rate, 0.1); (C.Cost, 0.2) ]
            (suite_runs ~n:40 ~kb:40 ~kc:40 ())))
  in
  Alcotest.check verdict_t "pass rate" No_change (metric r C.Pass_rate).verdict;
  Alcotest.(check bool) "cost in gate" true (metric r C.Cost).in_gate;
  Alcotest.(check int) "inconclusive cost gate exits 2" 2 r.exit

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

let width (i : Stats.interval) = i.upper -. i.lower

let tallies k ~n ~kb ~kc =
  List.init k (fun _ -> (kb, n)) |> fun b -> (b, List.init k (fun _ -> (kc, n)))

let test_suite_is_stratified_newcombe () =
  (* The suite interval is the stratified Newcombe interval at the gate
     level, independent of the seed. *)
  List.iter
    (fun (k, n, kb, kc) ->
      let r = ok (C.run (input (suite_runs ~sc:k ~n ~kb ~kc ()))) in
      let m = metric r C.Pass_rate in
      let baseline, candidate = tallies k ~n ~kb ~kc in
      let want =
        Stats.stratified_newcombe_diff ~baseline ~candidate ~confidence:0.95
      in
      Alcotest.(check (float 0.)) "lower" want.lower m.interval.lower;
      Alcotest.(check (float 0.)) "upper" want.upper m.interval.upper)
    [ (1, 3, 3, 3); (3, 5, 5, 4); (3, 10, 6, 5); (10, 3, 3, 3) ]

let test_unanimous_not_certain () =
  (* A percentile bootstrap gives unanimous arms the interval [0, 0] and a
     confident No_change. *)
  List.iter
    (fun (population, k, n, too_few) ->
      let r =
        ok
          (C.run
             (input ~population
                ~gate:[ (C.Pass_rate, 0.05) ]
                (suite_runs ~sc:k ~n ~kb:n ~kc:n ())))
      in
      let m = metric r C.Pass_rate in
      let label = Printf.sprintf "K=%d n=%d" k n in
      Alcotest.(check bool)
        (label ^ " not zero width")
        true
        (width m.interval > 0.);
      Alcotest.(check bool)
        (label ^ " No_change only with enough runs")
        (not too_few)
        (match m.verdict with
        | No_change -> true
        | Regression | Improvement | Inconclusive _ -> false))
    (* 200 passing runs per arm do support No_change at ±0.05: the interval
       is ±0.027. *)
    [
      (`Suite, 1, 3, true);
      (`Suite, 1, 10, true);
      (`Suite, 3, 5, true);
      (`Suite, 10, 3, true);
      (`Scenarios, 10, 3, true);
      (`Scenarios, 10, 20, false);
    ];
  (* A total flip is still a regression, but not a point. *)
  let r = ok (C.run (input (suite_runs ~sc:1 ~n:3 ~kb:3 ~kc:0 ()))) in
  let m = metric r C.Pass_rate in
  Alcotest.check verdict_t "3/3 -> 0/3" Regression m.verdict;
  Alcotest.(check bool) "not zero width" true (width m.interval > 0.)

let test_scenarios_hull () =
  (* M1: under [scenarios] the interval is the hull of the paired
     hierarchical bootstrap and the stratified Newcombe interval. *)
  let k = 12 and n = 6 in
  let rs =
    List.concat_map
      (fun s ->
        let i = int_of_string (String.sub s 1 2) in
        runs s "A" ~n ~k:(i mod 7) @ runs s "B" ~n ~k:((i + 2) mod 7))
      (scenarios k)
  in
  let r = ok (C.run (input ~population:`Scenarios rs)) in
  let m = metric r C.Pass_rate in
  (* Values in reverse run order, the order [Compare] collects them in, so the
     bootstrap below makes the same draws. *)
  let arm v s : Bootstrap.arm =
    {
      scenario = s;
      values =
        Array.of_list
          (List.rev_map Fun.id
          @@ List.filter_map
               (fun (x : C.run) ->
                 if String.equal x.variant v && String.equal x.scenario s then
                   Option.map (fun b -> if b then 1. else 0.) x.passed
                 else None)
               rs);
    }
  in
  let hier =
    Bootstrap.hierarchical_diff
      ~rng:(Random.State.make [| 7; 0 |])
      ~resamples:500 ~confidence:0.95
      ~baseline:(List.map (arm "A") (scenarios k))
      ~candidate:(List.map (arm "B") (scenarios k))
  in
  let tally v s =
    let a = arm v s in
    ( Array.fold_left (fun acc x -> if x > 0.5 then acc + 1 else acc) 0 a.values,
      Array.length a.values )
  in
  let sn =
    Stats.stratified_newcombe_diff
      ~baseline:(List.map (tally "A") (scenarios k))
      ~candidate:(List.map (tally "B") (scenarios k))
      ~confidence:0.95
  in
  Alcotest.(check (float 0.))
    "lower"
    (Float.min hier.lower sn.lower)
    m.interval.lower;
  Alcotest.(check (float 0.))
    "upper"
    (Float.max hier.upper sn.upper)
    m.interval.upper

let test_scenarios_swap () =
  let rs = suite_runs ~sc:10 ~n:6 ~kb:5 ~kc:2 () in
  let fwd = metric (ok (C.run (input ~population:`Scenarios rs))) C.Pass_rate in
  let bwd =
    metric
      (ok
         (C.run
            {
              (input ~population:`Scenarios rs) with
              baseline = "B";
              candidate = "A";
            }))
      C.Pass_rate
  in
  Alcotest.(check (float 0.)) "lower" (-.bwd.interval.upper) fwd.interval.lower;
  Alcotest.(check (float 0.)) "upper" (-.bwd.interval.lower) fwd.interval.upper

(* Cost runs: [n] per scenario, value [scale *. (1 + i / 100)] for run i. *)
let cost_runs ~n variant scale s =
  List.init n (fun i ->
      run_of s variant (Some true)
        ~cost:(Some (scale *. (1. +. (float_of_int i /. 100.)))))

let test_cost_needs_five_values () =
  (* A common scenario with fewer than 5 values in an arm makes the metric
     undefined. *)
  let rs n =
    cost_runs ~n:10 "A" 1. "s00"
    @ cost_runs ~n:10 "B" 1.2 "s00"
    @ cost_runs ~n:10 "A" 3. "s01"
    @ cost_runs ~n "B" 3.6 "s01"
  in
  let defined n =
    let r = ok (C.run (input ~gate:[ (C.Cost, 0.2) ] (rs n))) in
    not (Float.is_nan (metric r C.Cost).estimate)
  in
  Alcotest.(check bool) "5 values" true (defined 5);
  Alcotest.(check bool) "4 values" false (defined 4)

let test_cost_common_scenarios_only () =
  (* Values from a scenario the other arm lacks are not pooled. *)
  let common =
    List.concat_map
      (fun (s, scale) ->
        cost_runs ~n:10 "A" scale s @ cost_runs ~n:10 "B" (2. *. scale) s)
      [ ("s00", 1.); ("s01", 4.) ]
  in
  let extra = cost_runs ~n:10 "A" 1000. "s02" in
  let est rs =
    (metric (ok (C.run (input ~gate:[ (C.Cost, 0.2) ] rs))) C.Cost).estimate
  in
  Alcotest.(check (float 1e-12)) "log 2" (log 2.) (est common);
  Alcotest.(check (float 1e-12))
    "extra scenario ignored" (log 2.)
    (est (common @ extra))

let test_cost_stratified_interval () =
  (* Scenario costs differing by 100x no longer widen the interval: every
     resample keeps each scenario's run count. *)
  let rs =
    List.concat_map
      (fun (s, scale) ->
        cost_runs ~n:20 "A" scale s @ cost_runs ~n:20 "B" (1.5 *. scale) s)
      [ ("s00", 1.); ("s01", 10.); ("s02", 100.) ]
  in
  let m = metric (ok (C.run (input ~gate:[ (C.Cost, 0.2) ] rs))) C.Cost in
  Alcotest.(check (float 1e-12)) "estimate" (log 1.5) m.estimate;
  Alcotest.check verdict_t "regression" Regression m.verdict

let test_dropped_scenario () =
  (* 30 scenarios x 10 runs, the candidate errors on every run of
     one scenario (3.3% of its runs, under the 5% trip-wire). *)
  let sc = scenarios 30 in
  let lost = List.nth sc 29 in
  let rs =
    List.concat_map
      (fun s ->
        runs s "A" ~n:10 ~k:10
        @
        if String.equal s lost then
          List.init 10 (fun _ -> run_of s "B" None ~missing:(Some C.Errored))
        else runs s "B" ~n:10 ~k:10)
      sc
  in
  let r = ok (C.run (input rs)) in
  Alcotest.(check (list string)) "listed" [ lost ] r.dropped;
  Alcotest.(check int) "exit 3" 3 r.exit;
  Alcotest.(check int) "29 compared" 29 (List.length r.per_scenario);
  (* A scenario the candidate never ran at all counts the same way. *)
  let rs =
    List.concat_map
      (fun s ->
        runs s "A" ~n:10 ~k:10
        @ if String.equal s lost then [] else runs s "B" ~n:10 ~k:10)
      sc
  in
  let r = ok (C.run (input rs)) in
  Alcotest.(check (list string)) "listed, never run" [ lost ] r.dropped;
  Alcotest.(check int) "exit 3, never run" 3 r.exit;
  let r = ok (C.run (input (suite_runs ~n:10 ~kb:10 ~kc:10 ()))) in
  Alcotest.(check (list string)) "none dropped" [] r.dropped

(* Seeded simulations at the boundary, where a percentile bootstrap
   collapses to zero width. Each cell draws
   [datasets] experiments with K scenarios and n runs per scenario and arm,
   true rates [p_base] and [p_cand] in every scenario, and runs the real gate
   at 0.95 with a 0.05 threshold. The true difference is 2δ, so any No_change
   is a false all-clear. *)
let boundary_datasets = 2000

let boundary_cell ?(population = `Suite) ~p_base ~p_cand ~n ~k () =
  let st = Random.State.make [| 20261013; n; k; truncate (p_base *. 10.) |] in
  let truth = p_cand -. p_base in
  let covered = ref 0 and no_change = ref 0 in
  for _ = 1 to boundary_datasets do
    let arm variant p s =
      List.init n (fun _ ->
          run_of s variant (Some (Random.State.float st 1. < p)))
    in
    let rs =
      List.concat_map
        (fun s -> arm "A" p_base s @ arm "B" p_cand s)
        (scenarios k)
    in
    let r =
      ok
        (C.run
           (input ~population
              ~gate:[ (C.Pass_rate, 0.05) ]
              ~seed:(Random.State.bits st) rs))
    in
    let m = metric r C.Pass_rate in
    if m.interval.lower <= truth && truth <= m.interval.upper then incr covered;
    match m.verdict with
    | No_change -> incr no_change
    | Regression | Improvement | Inconclusive _ -> ()
  done;
  let rate x = float_of_int x /. float_of_int boundary_datasets in
  (rate !covered, rate !no_change)

let test_boundary_coverage () =
  let se = sqrt (0.05 *. 0.95 /. float_of_int boundary_datasets) in
  let no_change_limit = 0.05 +. (3. *. se) in
  let cells =
    List.concat_map
      (fun (p_base, p_cand) ->
        List.concat_map
          (fun n -> List.map (fun k -> (`Suite, p_base, p_cand, n, k)) [ 1; 3 ])
          [ 3; 5 ])
      [ (1.0, 0.9); (0.0, 0.1) ]
    (* M1: the hierarchical bootstrap alone said No_change 4.4% of the time
       here. *)
    @ [ (`Scenarios, 1.0, 0.9, 3, 10) ]
  in
  let failures =
    List.filter_map
      (fun (population, p_base, p_cand, n, k) ->
        let coverage, no_change =
          boundary_cell ~population ~p_base ~p_cand ~n ~k ()
        in
        let line =
          Printf.sprintf
            "%s %.1f -> %.1f n=%d K=%d: coverage %.4f, false no-change %.4f"
            (match population with
            | `Suite -> "suite"
            | `Scenarios -> "scenarios")
            p_base p_cand n k coverage no_change
        in
        print_endline line;
        if coverage >= 0.93 && no_change <= no_change_limit then None
        else Some line)
      cells
  in
  match failures with
  | [] -> ()
  | _ :: _ ->
      Alcotest.failf "coverage below 0.93 or false no-change above %.4f:\n%s"
        no_change_limit
        (String.concat "\n" failures)

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
          Alcotest.test_case "undefined without values" `Quick
            test_undefined_without_values;
          Alcotest.test_case "suite is stratified newcombe" `Quick
            test_suite_is_stratified_newcombe;
          Alcotest.test_case "unanimous arms not certain" `Quick
            test_unanimous_not_certain;
          Alcotest.test_case "scenarios hull" `Quick test_scenarios_hull;
          Alcotest.test_case "scenarios swap" `Quick test_scenarios_swap;
          Alcotest.test_case "cost needs 5 values" `Quick
            test_cost_needs_five_values;
          Alcotest.test_case "cost common scenarios" `Quick
            test_cost_common_scenarios_only;
          Alcotest.test_case "cost stratified" `Quick
            test_cost_stratified_interval;
          Alcotest.test_case "dropped scenario" `Quick test_dropped_scenario;
          Alcotest.test_case "swap negates" `Quick test_swap_negates;
          Alcotest.test_case "missing variant" `Quick test_no_variant_runs;
        ] );
      ( "coverage",
        [
          Alcotest.test_case "boundary rates, n = 3 and 5, K = 1, 3 and 10"
            `Slow test_boundary_coverage;
        ] );
    ]
