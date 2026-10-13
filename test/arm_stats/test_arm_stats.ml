open Bactrian_core
module A = Arm_stats

let run ?cost ?tokens ?latency ?(ttft = []) outcome : A.run =
  { outcome; cost; tokens; latency_ms = latency; ttft_ms = ttft }

let compute runs =
  A.compute
    ~rng:(Random.State.make [| 1 |])
    ~resamples:500 ~confidence:0.95 runs

(* 7 of 10 finished runs pass, one errored and two not run. *)
let ten =
  List.init 10 (fun i ->
      run
        ~cost:(float_of_int (i + 1))
        ~tokens:(100 * (i + 1))
        ~latency:(1000. +. float_of_int i)
        ~ttft:[ 100. +. float_of_int i; 200. ]
        (if i < 7 then Passed else Failed))
  @ [ run ~cost:1000. Errored; run Not_run; run Not_run ]

let test_counts () =
  let t = compute ten in
  Alcotest.(check (list int))
    "finished, passes, errored, not run" [ 10; 7; 1; 2 ]
    [ t.finished; t.passes; t.errored; t.not_run ]

let test_wilson () =
  let t = compute ten in
  match t.wilson with
  | Some i ->
      let want = Stats.wilson ~successes:7 ~trials:10 ~confidence:0.95 in
      Alcotest.(check (pair (float 0.) (float 0.)))
        "Wilson 7/10" (want.lower, want.upper) (i.lower, i.upper)
  | None -> Alcotest.fail "expected an interval"

let test_pass_hat_k () =
  (* C(7, k) / C(10, k) for k = 1 to 10. *)
  let want =
    [ 7. /. 10.; 21. /. 45.; 35. /. 120.; 35. /. 210.; 21. /. 252.; 7. /. 210. ]
    @ [ 1. /. 120.; 0.; 0.; 0. ]
  in
  Alcotest.(check (list (float 1e-15)))
    "every k up to n" want (compute ten).pass_hat_k

let measure what = function
  | Some (m : A.measure) -> m
  | None -> Alcotest.failf "%s: expected a measure" what

let test_measures () =
  let t = compute ten in
  (* Finished runs only: the errored run's cost of 1000 is left out. Type 7
     quantiles of 1..10: median 5.5, 90th percentile 9.1. *)
  let cost = measure "cost" t.cost in
  Alcotest.(check (pair (float 1e-12) (float 1e-12)))
    "cost median, p90" (5.5, 9.1) (cost.median, cost.p90);
  Alcotest.(check (pair int int)) "cost counts" (10, 10) (cost.runs, cost.values);
  let tokens = measure "tokens" t.tokens in
  Alcotest.(check (float 1e-12)) "tokens median" 550. tokens.median;
  let latency = measure "latency" t.latency_ms in
  Alcotest.(check (float 1e-12)) "latency median" 1004.5 latency.median;
  (* Two calls per run: 100..109 and ten 200s. *)
  let ttft = measure "ttft" t.ttft_ms in
  Alcotest.(check (pair int int)) "ttft counts" (10, 20) (ttft.runs, ttft.values);
  Alcotest.(check (float 1e-12)) "ttft median" 154.5 ttft.median;
  List.iter
    (fun (what, (m : A.measure)) ->
      match (m.median_interval, m.p90_interval) with
      | Some mi, Some pi ->
          Alcotest.(check bool)
            (what ^ ": intervals contain the estimates")
            true
            (mi.lower <= m.median && m.median <= mi.upper && pi.lower <= m.p90
           && m.p90 <= pi.upper)
      | None, _ | _, None -> Alcotest.failf "%s: expected intervals" what)
    [ ("cost", cost); ("tokens", tokens); ("latency", latency); ("ttft", ttft) ]

let test_few_runs () =
  let four = List.init 4 (fun i -> run ~cost:(float_of_int i) Passed) in
  let cost = measure "cost" (compute four).cost in
  Alcotest.(check bool)
    "no interval below 5 runs" true
    (Option.is_none cost.median_interval && Option.is_none cost.p90_interval);
  let t = compute [ run Errored ] in
  Alcotest.(check bool)
    "no finished run, no Wilson" true (Option.is_none t.wilson);
  Alcotest.(check bool) "no cost" true (Option.is_none t.cost);
  Alcotest.(check (list (float 0.))) "no pass^k" [] t.pass_hat_k

let test_clusters_stay_together () =
  (* Two runs: one with a single fast call, one with three slow calls. Every
     resample pools whole runs, so the pooled median is 1 (the fast run drawn
     twice) or 2 (any slow run drawn), never a value between. Resampling calls
     one by one would give 1.5 as well. *)
  let lo, _ =
    Bootstrap.median_p90_intervals
      ~rng:(Random.State.make [| 3 |])
      ~resamples:200 ~confidence:0.9
      [ [| 1. |]; [| 2.; 2.; 2. |] ]
  in
  Alcotest.(check bool)
    "medians from whole runs" true
    (List.mem lo.lower [ 1.; 2. ] && List.mem lo.upper [ 1.; 2. ])

let test_deterministic () =
  Alcotest.(check bool)
    "same draws, same result" true
    (compute ten = compute ten)

let () =
  Alcotest.run "arm_stats"
    [
      ( "arm",
        [
          Alcotest.test_case "counts" `Quick test_counts;
          Alcotest.test_case "wilson" `Quick test_wilson;
          Alcotest.test_case "pass^k" `Quick test_pass_hat_k;
          Alcotest.test_case "measures" `Quick test_measures;
          Alcotest.test_case "few runs" `Quick test_few_runs;
          Alcotest.test_case "clusters stay together" `Quick
            test_clusters_stay_together;
          Alcotest.test_case "deterministic" `Quick test_deterministic;
        ] );
    ]
