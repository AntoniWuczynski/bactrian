open Bactrian_core
module P = Plan_sim

let scenarios k = List.init k (fun i -> (Printf.sprintf "s%02d" i, 0.8))

let base : P.assumption =
  {
    baseline_rates = scenarios 3;
    true_effect = 0.3;
    threshold = 0.1;
    between_sd = None;
    population = `Suite;
    confidence = 0.95;
    power = 0.8;
    max_repeats = 100;
    seed = 7;
    simulations = 100;
  }

let repeats = function
  | P.Repeats { n; _ } -> n
  | P.Unreachable _ -> Alcotest.fail "unexpected Unreachable"

let test_power_increases () =
  let at n = P.power_at base ~repeats:n in
  let p5 = at 5 and p20 = at 20 and p60 = at 60 in
  Alcotest.(check bool) "5 < 20" true (p5 < p20);
  Alcotest.(check bool) "20 < 60" true (p20 < p60);
  Alcotest.(check bool) "60 is high" true (p60 > 0.8)

let test_higher_power_needs_more () =
  let n_low = repeats (P.plan { base with power = 0.5 }) in
  let n_high = repeats (P.plan { base with power = 0.9 }) in
  Alcotest.(check bool) "n(0.5) <= n(0.9)" true (n_low <= n_high)

let test_found_n_reaches_power () =
  (* The reported power is a fresh estimate at the returned n, independent of
     the simulations that chose n, so it is unbiased but can fall below the
     target by Monte Carlo error (it used to be the selecting estimate, which
     always cleared the target and ran 0.01 to 0.02 high). *)
  match P.plan base with
  | P.Repeats { n; achieved_power; mc_se } ->
      Alcotest.(check bool)
        "power reached within Monte Carlo error" true
        (achieved_power >= 0.8 -. (3. *. mc_se));
      Alcotest.(check (float 0.0))
        "same as power_at" achieved_power
        (P.power_at base ~repeats:n)
  | P.Unreachable _ -> Alcotest.fail "unexpected Unreachable"

let test_no_change_at_ceiling () =
  (* A zero-width interval would let one run per arm "prove" no change
     between two 100% arms. The stratified Newcombe interval of n/n against
     n/n is +-z^2/(n + z^2), inside +-0.05 from n = 73 at 0.95. *)
  let a =
    {
      base with
      baseline_rates = [ ("only", 1.0) ];
      true_effect = 0.0;
      threshold = 0.05;
      max_repeats = 200;
      simulations = 50;
    }
  in
  match P.plan a with
  | P.Repeats { n; achieved_power; _ } ->
      Alcotest.(check bool) (Printf.sprintf "n = %d >= 10" n) true (n >= 10);
      Alcotest.(check (float 0.)) "certain at n" 1.0 achieved_power;
      Alcotest.(check (float 0.))
        "one fewer is not enough" 0.0
        (P.power_at a ~repeats:(n - 1))
  | P.Unreachable _ -> Alcotest.fail "unexpected Unreachable"

let test_effect_at_threshold_unreachable () =
  match
    P.plan { base with true_effect = 0.1; max_repeats = 60; simulations = 100 }
  with
  | P.Unreachable { best_power } ->
      Alcotest.(check bool) "near or below 0.5" true (best_power <= 0.65)
  | P.Repeats _ -> Alcotest.fail "an effect equal to the threshold is reachable"

let test_large_effect_few_repeats () =
  let n =
    repeats
      (P.plan
         {
           base with
           baseline_rates = [ ("a", 0.95); ("b", 0.95) ];
           true_effect = 0.6;
         })
  in
  Alcotest.(check bool) "few repeats" true (n <= 8)

let test_deterministic () =
  Alcotest.(check bool) "same seed, same answer" true (P.plan base = P.plan base);
  Alcotest.(check (float 0.0))
    "power_at"
    (P.power_at base ~repeats:10)
    (P.power_at base ~repeats:10)

let test_mc_se () =
  match P.plan base with
  | P.Repeats { achieved_power = p; mc_se; _ } ->
      Alcotest.(check (float 1e-12))
        "sqrt(p(1-p)/sims)"
        (sqrt (p *. (1.0 -. p) /. 100.0))
        mc_se
  | P.Unreachable _ -> Alcotest.fail "unexpected Unreachable"

let test_runtime () =
  let t0 = Sys.time () in
  ignore (P.plan base);
  let dt = Sys.time () -. t0 in
  Alcotest.(check bool) (Printf.sprintf "%.2fs under 10s" dt) true (dt < 10.0)

let test_scenarios_between_sd () =
  let a : P.assumption =
    {
      base with
      baseline_rates = scenarios 12;
      population = `Scenarios;
      between_sd = Some 0.02;
      simulations = 40;
      max_repeats = 40;
    }
  in
  let a_wide = { a with between_sd = Some 0.5 } in
  let p_narrow = P.power_at a ~repeats:30 in
  let p_wide = P.power_at a_wide ~repeats:30 in
  Alcotest.(check bool) "narrow spread has power" true (p_narrow > 0.5);
  Alcotest.(check bool) "wide spread loses power" true (p_wide < p_narrow)

let test_scenarios_too_few () =
  Alcotest.(check bool)
    "raises" true
    (match P.plan { base with population = `Scenarios; simulations = 5 } with
    | exception Invalid_argument _ -> true
    | _ -> false)

let test_invalid () =
  let raises f =
    match f () with exception Invalid_argument _ -> true | _ -> false
  in
  Alcotest.(check bool)
    "empty" true
    (raises (fun () -> P.plan { base with baseline_rates = [] }));
  Alcotest.(check bool)
    "max_repeats" true
    (raises (fun () -> P.plan { base with max_repeats = 0 }));
  Alcotest.(check bool)
    "simulations" true
    (raises (fun () -> P.plan { base with simulations = 0 }))

let runs ~scenario ~variant ~passes ~total : Compare.run list =
  List.init total (fun i ->
      {
        Compare.scenario;
        variant;
        passed = Some (i < passes);
        missing = None;
        cost = None;
        latency_ms = None;
        tokens = None;
      })

let input ~a ~b ~n : Compare.input =
  {
    runs =
      runs ~scenario:"s1" ~variant:"A" ~passes:a ~total:n
      @ runs ~scenario:"s1" ~variant:"B" ~passes:b ~total:n;
    baseline = "A";
    candidate = "B";
    population = `Suite;
    confidence = 0.95;
    gate = [ (Pass_rate, 0.1) ];
    report_metrics = [];
    seed = 3;
    resamples = 400;
    fail_on_inconclusive = false;
  }

let extra =
  Alcotest.(
    testable
      (fun ppf -> function
        | `Repeats n -> Fmt.pf ppf "Repeats %d" n
        | `Cannot_resolve -> Fmt.string ppf "Cannot_resolve")
      ( = ))

let test_extra_conclusive () =
  Alcotest.check extra "regression already clear" (`Repeats 0)
    (P.extra_runs_needed (input ~a:50 ~b:10 ~n:50) ~metric:Pass_rate)

let test_extra_needed () =
  (* Observed drop of 0.4 with 10 runs: inconclusive, resolvable. *)
  match P.extra_runs_needed (input ~a:8 ~b:4 ~n:10) ~metric:Pass_rate with
  | `Repeats n -> Alcotest.(check bool) "positive" true (n > 0)
  | `Cannot_resolve -> Alcotest.fail "should be resolvable"

let test_extra_on_threshold () =
  (* Observed drop of exactly the threshold: never resolvable. *)
  Alcotest.check extra "cannot resolve" `Cannot_resolve
    (P.extra_runs_needed (input ~a:9 ~b:8 ~n:10) ~metric:Pass_rate)

let test_extra_expensive () =
  (* A single scenario with a 0.2 drop needs a few hundred repeats. This used
     to be [`Cannot_resolve] because the search stopped at 200; expensive is
     not unresolvable, so the estimate is now returned. *)
  match P.extra_runs_needed (input ~a:8 ~b:6 ~n:10) ~metric:Pass_rate with
  | `Repeats n ->
      Alcotest.(check bool) (Printf.sprintf "%d beyond 190" n) true (n > 190);
      Alcotest.(check bool) (Printf.sprintf "%d within 2,000" n) true (n <= 2000)
  | `Cannot_resolve -> Alcotest.fail "expensive, not unresolvable"

let test_extra_other_metric () =
  Alcotest.check extra "cost" `Cannot_resolve
    (P.extra_runs_needed (input ~a:8 ~b:6 ~n:10) ~metric:Cost)

let () =
  Alcotest.run "plan_sim"
    [
      ( "plan",
        [
          Alcotest.test_case "power increases with n" `Quick
            test_power_increases;
          Alcotest.test_case "higher power needs more" `Quick
            test_higher_power_needs_more;
          Alcotest.test_case "found n reaches power" `Quick
            test_found_n_reaches_power;
          Alcotest.test_case "no change at the ceiling" `Quick
            test_no_change_at_ceiling;
          Alcotest.test_case "effect at threshold unreachable" `Quick
            test_effect_at_threshold_unreachable;
          Alcotest.test_case "large effect needs few" `Quick
            test_large_effect_few_repeats;
          Alcotest.test_case "deterministic" `Quick test_deterministic;
          Alcotest.test_case "mc_se" `Quick test_mc_se;
          Alcotest.test_case "runtime" `Quick test_runtime;
          Alcotest.test_case "scenarios between_sd" `Quick
            test_scenarios_between_sd;
          Alcotest.test_case "scenarios too few" `Quick test_scenarios_too_few;
          Alcotest.test_case "invalid input" `Quick test_invalid;
        ] );
      ( "extra_runs_needed",
        [
          Alcotest.test_case "conclusive" `Quick test_extra_conclusive;
          Alcotest.test_case "needed" `Quick test_extra_needed;
          Alcotest.test_case "on threshold" `Quick test_extra_on_threshold;
          Alcotest.test_case "expensive" `Quick test_extra_expensive;
          Alcotest.test_case "other metric" `Quick test_extra_other_metric;
        ] );
    ]
