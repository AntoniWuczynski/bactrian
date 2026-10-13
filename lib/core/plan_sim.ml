type assumption = {
  baseline_rates : (string * float) list;
  true_effect : float;
  threshold : float;
  between_sd : float option;
  population : [ `Suite | `Scenarios ];
  confidence : float;
  gate_metrics : int;
  power : float;
  max_repeats : int;
  seed : int;
  simulations : int;
}

type answer =
  | Repeats of { n : int; achieved_power : float; mc_se : float }
  | Unreachable of { best_power : float }

type target = Want_regression | Want_improvement | Want_no_change

(* Resamples of the hierarchical bootstrap inside one simulated [`Scenarios]
   experiment. Far below the 10,000 of a real comparison: planning runs
   [simulations] experiments per candidate repeat count, and the Monte Carlo
   error already dominates. *)
let sim_resamples = 400
let baseline_name = "baseline"
let candidate_name = "candidate"

type setup = {
  scenarios : (string * float * float) list;
      (* scenario, true baseline rate, true drop in pass rate *)
  effect_sd : float; (* spread of the per-scenario drop, 0 for none *)
  target : target;
  threshold : float;
  population : [ `Suite | `Scenarios ];
  level : float;
      (* the level the pass-rate interval is computed at, Bonferroni-adjusted
         for the gate family *)
  seed : int;
  simulations : int;
}

let clamp01 x = Float.min 1.0 (Float.max 0.0 x)

let classify ~threshold drop =
  if drop >= threshold -. 1e-12 then Want_regression
  else if drop <= -.threshold +. 1e-12 then Want_improvement
  else Want_no_change

let bernoulli rng p = Random.State.float rng 1.0 < p

(* Box-Muller. [1 - u] keeps the argument of [log] in (0, 1]. *)
let normal rng =
  let u1 = 1.0 -. Random.State.float rng 1.0 in
  let u2 = Random.State.float rng 1.0 in
  sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2)

let outcomes rng ~scenario ~variant ~rate n : Compare.run list =
  List.init n (fun _ ->
      {
        Compare.scenario;
        variant;
        passed = Some (bernoulli rng rate);
        missing = None;
        cost = None;
        latency_ms = None;
        tokens = None;
      })

let simulated_runs rng setup n =
  List.concat_map
    (fun (scenario, base, drop) ->
      let drop =
        if setup.effect_sd > 0.0 then drop +. (setup.effect_sd *. normal rng)
        else drop
      in
      let baseline =
        outcomes rng ~scenario ~variant:baseline_name ~rate:base n
      in
      let candidate =
        outcomes rng ~scenario ~variant:candidate_name
          ~rate:(clamp01 (base -. drop))
          n
      in
      baseline @ candidate)
    setup.scenarios

let pass_rate_verdict (result : Compare.result) =
  List.find_map
    (fun (m : Compare.metric_result) ->
      match m.metric with
      | Pass_rate -> Some m.verdict
      | Cost | Latency_p50 | Tokens -> None)
    result.metrics

let hit target (verdict : Compare.verdict) =
  match (verdict, target) with
  | Regression, Want_regression
  | Improvement, Want_improvement
  | No_change, Want_no_change ->
      true
  | Regression, (Want_improvement | Want_no_change)
  | Improvement, (Want_regression | Want_no_change)
  | No_change, (Want_regression | Want_improvement)
  | Inconclusive _, (Want_regression | Want_improvement | Want_no_change) ->
      false

(* Two independent streams of simulated experiments: [Search] chooses the
   repeat count and [Report] estimates the power of the count chosen, so the
   reported power is not biased upwards by the choice. *)
type stream = Search | Report

let one_simulation setup ~stream ~n ~index =
  let tag = match stream with Search -> 0 | Report -> 1 in
  let rng = Random.State.make [| setup.seed; n; index; tag |] in
  let runs = simulated_runs rng setup n in
  let input : Compare.input =
    {
      runs;
      baseline = baseline_name;
      candidate = candidate_name;
      population = setup.population;
      (* A gate family of one leaves the level as given, so the simulated
         comparison runs the pass rate at the real gate's level. *)
      confidence = setup.level;
      gate = [ (Pass_rate, setup.threshold) ];
      report_metrics = [];
      seed = Random.State.bits rng;
      resamples = sim_resamples;
      fail_on_inconclusive = false;
    }
  in
  match Compare.run input with
  | Error message -> invalid_arg ("Plan_sim: " ^ message)
  | Ok result -> (
      match pass_rate_verdict result with
      | Some verdict -> hit setup.target verdict
      | None -> invalid_arg "Plan_sim: no pass rate verdict")

let power_of ~check setup ~stream n =
  let hits = ref 0 in
  for index = 0 to setup.simulations - 1 do
    check ();
    if one_simulation setup ~stream ~n ~index then incr hits
  done;
  float_of_int !hits /. float_of_int setup.simulations

let mc_se ~power ~simulations =
  sqrt (power *. (1.0 -. power) /. float_of_int simulations)

(* Smallest [n] in [1, max_repeats] whose power reaches [required]: doubling
   until one does, then bisection between the last failure and that success.
   Powers are memoised, and each is a pure function of [n], so the result does
   not depend on the order of evaluation. *)
let search ~check setup ~required ~max_repeats =
  let cache = Hashtbl.create 16 in
  let power n =
    match Hashtbl.find_opt cache n with
    | Some p -> p
    | None ->
        let p = power_of ~check setup ~stream:Search n in
        Hashtbl.add cache n p;
        p
  in
  let ok n = power n >= required in
  let rec bisect lo hi =
    if hi - lo <= 1 then hi
    else
      let mid = lo + ((hi - lo) / 2) in
      if ok mid then bisect lo mid else bisect mid hi
  in
  let rec double lo n =
    if ok n then Some (bisect lo n)
    else if n >= max_repeats then None
    else double n (min max_repeats (2 * n))
  in
  match double 0 1 with
  | Some n ->
      let p = power_of ~check setup ~stream:Report n in
      Repeats
        {
          n;
          achieved_power = p;
          mc_se = mc_se ~power:p ~simulations:setup.simulations;
        }
  | None ->
      Unreachable
        {
          best_power = Hashtbl.fold (fun _ p best -> Float.max p best) cache 0.0;
        }

let check_positive name value =
  if value < 1 then
    invalid_arg (Printf.sprintf "Plan_sim: %s must be at least 1" name)

let setup_of_assumption (a : assumption) =
  (match a.baseline_rates with
  | [] -> invalid_arg "Plan_sim: no baseline rates"
  | _ :: _ -> ());
  check_positive "simulations" a.simulations;
  check_positive "gate_metrics" a.gate_metrics;
  if not (a.threshold > 0. && a.threshold < 1.) then
    invalid_arg "Plan_sim: threshold outside (0, 1)";
  {
    scenarios =
      List.map
        (fun (name, rate) -> (name, clamp01 rate, a.true_effect))
        a.baseline_rates;
    effect_sd =
      (match (a.population, a.between_sd) with
      | `Scenarios, Some sd -> sd
      | `Suite, _ | `Scenarios, None -> 0.0);
    target = classify ~threshold:a.threshold a.true_effect;
    threshold = a.threshold;
    population = a.population;
    level =
      Stats.bonferroni_level ~confidence:a.confidence ~family:a.gate_metrics;
    seed = a.seed;
    simulations = a.simulations;
  }

let target (a : assumption) : Compare.verdict =
  match classify ~threshold:a.threshold a.true_effect with
  | Want_regression -> Regression
  | Want_improvement -> Improvement
  | Want_no_change -> No_change

let plan ?(check = ignore) (a : assumption) =
  check_positive "max_repeats" a.max_repeats;
  search ~check (setup_of_assumption a) ~required:a.power
    ~max_repeats:a.max_repeats

let power_at a ~repeats =
  check_positive "repeats" repeats;
  power_of ~check:ignore (setup_of_assumption a) ~stream:Report repeats

let extra_power = 0.8
let extra_simulations = 200
let extra_max_repeats = 2000

module Smap = Map.Make (String)

(* Observed (successes, trials) of each scenario for one variant. *)
let tally (input : Compare.input) variant =
  List.fold_left
    (fun acc (r : Compare.run) ->
      match r.passed with
      | Some passed when String.equal r.variant variant ->
          let s, t =
            Option.value ~default:(0, 0) (Smap.find_opt r.scenario acc)
          in
          Smap.add r.scenario ((if passed then s + 1 else s), t + 1) acc
      | Some _ | None -> acc)
    Smap.empty input.runs

let ratio (s, t) = float_of_int s /. float_of_int t

let observed_setup (input : Compare.input) ~threshold ~level =
  let base = tally input input.baseline
  and cand = tally input input.candidate in
  let scenarios =
    Smap.fold
      (fun name b acc ->
        match Smap.find_opt name cand with
        | Some c -> (name, ratio b, ratio b -. ratio c) :: acc
        | None -> acc)
      base []
  in
  let drops = List.map (fun (_, _, d) -> d) scenarios in
  let mean_drop =
    List.fold_left ( +. ) 0.0 drops /. float_of_int (List.length drops)
  in
  let current =
    Smap.fold (fun _ (_, t) m -> max t m) base 0
    |> Smap.fold (fun _ (_, t) m -> max t m) cand
  in
  ( scenarios,
    current,
    {
      scenarios;
      effect_sd = 0.0;
      target = classify ~threshold mean_drop;
      threshold;
      population = input.population;
      level;
      seed = input.seed;
      simulations = extra_simulations;
    } )

let extra_runs_needed ?(check = ignore) (input : Compare.input) ~metric =
  let declared = Compare.threshold_of metric in
  (* The level [Compare] judges the metric at: Bonferroni across the gate
     family for a gate metric, the confidence itself otherwise. *)
  let threshold, level =
    match declared input.gate with
    | Some t ->
        ( Some t,
          Stats.bonferroni_level ~confidence:input.confidence
            ~family:(List.length input.gate) )
    | None -> (declared input.report_metrics, input.confidence)
  in
  match (metric, threshold, Compare.run input) with
  | (Cost | Latency_p50 | Tokens), _, _ -> `Not_simulated
  | Pass_rate, None, _ | Pass_rate, _, Error _ -> `Cannot_resolve
  | Pass_rate, Some threshold, Ok result -> (
      match pass_rate_verdict result with
      | None -> `Cannot_resolve
      | Some (Regression | Improvement | No_change) -> `Repeats 0
      | Some (Inconclusive _) -> (
          match observed_setup input ~threshold ~level with
          | [], _, _ -> `Cannot_resolve
          | _, current, setup -> (
              match
                search ~check setup ~required:extra_power
                  ~max_repeats:extra_max_repeats
              with
              | Unreachable _ -> `Cannot_resolve
              | Repeats { n; _ } -> `Repeats (max 1 (n - current)))))
