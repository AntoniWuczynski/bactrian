type metric = Pass_rate | Cost | Latency_p50 | Tokens

type verdict =
  | Regression
  | Improvement
  | No_change
  | Inconclusive of { lean : [ `Worse | `Better | `None ] }

type metric_result = {
  metric : metric;
  estimate : float;
  interval : Stats.interval;
  threshold : float;
  level : float;
  verdict : verdict;
  in_gate : bool;
  undefined : string option;
}

type missing_reason = Errored | Not_run

type run = {
  scenario : string;
  variant : string;
  passed : bool option;
  missing : missing_reason option;
  cost : float option;
  latency_ms : float option;
  tokens : int option;
}

type input = {
  runs : run list;
  baseline : string;
  candidate : string;
  population : [ `Suite | `Scenarios ];
  confidence : float;
  gate : (metric * float) list;
  report_metrics : (metric * float) list;
  seed : int;
  resamples : int;
  fail_on_inconclusive : bool;
}

type result = {
  metrics : metric_result list;
  per_scenario : (string * Stats.interval * verdict) list;
  missing : (string * int * int) list;
  dropped : string list;
  exit : int;
}

module Smap = Map.Make (String)

let metric_equal a b =
  match (a, b) with
  | Pass_rate, Pass_rate
  | Cost, Cost
  | Latency_p50, Latency_p50
  | Tokens, Tokens ->
      true
  | (Pass_rate | Cost | Latency_p50 | Tokens), _ -> false

let threshold_of metric l =
  List.find_map (fun (m, t) -> if metric_equal m metric then Some t else None) l

let decide ~threshold ~worse_is (i : Stats.interval) =
  let below = i.upper < -.threshold and above = i.lower > threshold in
  let worse, better =
    match worse_is with `Lower -> (below, above) | `Higher -> (above, below)
  in
  if worse then Regression
  else if better then Improvement
  else if i.lower >= -.threshold && i.upper <= threshold then No_change
  else
    (* The sum is NaN for (-inf, inf), which falls through to [`None]. *)
    let mid = i.lower +. i.upper in
    let low_side, high_side =
      match worse_is with
      | `Lower -> (`Worse, `Better)
      | `Higher -> (`Better, `Worse)
    in
    if mid < 0. then Inconclusive { lean = low_side }
    else if mid > 0. then Inconclusive { lean = high_side }
    else Inconclusive { lean = `None }

let min_scenarios = 10

let metric_worse_is = function
  | Pass_rate -> `Lower
  | Cost | Latency_p50 | Tokens -> `Higher

(* Pass rate thresholds are absolute, the rest are relative changes. *)
let scale_threshold metric delta =
  match metric with
  | Pass_rate -> delta
  | Cost | Latency_p50 | Tokens -> Float.log1p delta

let is_missing (r : run) = Option.is_none r.passed

(* Per scenario, the values [f] gives for the variant's runs that have an
   outcome. *)
let by_scenario runs variant f =
  List.fold_left
    (fun acc (r : run) ->
      match f r with
      | Some v when String.equal r.variant variant && not (is_missing r) ->
          Smap.update r.scenario
            (fun cur -> Some (v :: Option.value cur ~default:[]))
            acc
      | Some _ | None -> acc)
    Smap.empty runs

(* Per scenario, the 1.0/0.0 outcomes of the variant's runs that have one. *)
let outcomes_by_scenario runs variant =
  by_scenario runs variant (fun r ->
      Option.map (fun b -> if b then 1.0 else 0.0) r.passed)

let count_missing runs variant =
  List.fold_left
    (fun (total, errored, not_run) (r : run) ->
      if not (String.equal r.variant variant) then (total, errored, not_run)
      else
        match (r.passed, r.missing) with
        | Some _, _ -> (total + 1, errored, not_run)
        | None, Some Errored -> (total + 1, errored + 1, not_run)
        | None, (Some Not_run | None) -> (total + 1, errored, not_run + 1))
    (0, 0, 0) runs

(* Spec §8: more than 5% of an arm errored, or the errored plus not-run share
   differing by more than 5 points. Exact integer arithmetic, so a share of
   exactly 5% does not trip it. *)
let biased (t1, e1, n1) (t2, e2, n2) =
  let m1 = e1 + n1 and m2 = e2 + n2 in
  e1 * 20 > t1 || e2 * 20 > t2 || abs ((m1 * t2) - (m2 * t1)) * 20 > t1 * t2

(* Below 5 values a scenario's median has too coarse a bootstrap
   distribution: with 3 values, a nominal 0.95 interval covered the truth only
   about 90% of the time in simulation. *)
let min_values = 5

let value_estimate (input : input) ~rng ~common metric ~level =
  let pick =
    match metric with
    | Pass_rate -> fun _ -> None
    | Cost -> fun (r : run) -> r.cost
    | Latency_p50 -> fun r -> r.latency_ms
    | Tokens -> fun r -> Option.map float_of_int r.tokens
  in
  let arms variant =
    let m = by_scenario input.runs variant pick in
    List.map
      (fun s : Bootstrap.arm ->
        {
          scenario = s;
          values = Array.of_list (Option.value (Smap.find_opt s m) ~default:[]);
        })
      common
  in
  let short variant arms =
    List.find_map
      (fun (a : Bootstrap.arm) ->
        let n = Array.length a.values in
        if n < min_values then
          Some
            (Printf.sprintf "scenario %s has %d value%s in %s, fewer than %d"
               a.scenario n
               (if n = 1 then "" else "s")
               variant min_values)
        else None)
      arms
  in
  let b = arms input.baseline and c = arms input.candidate in
  match (short input.baseline b, short input.candidate c) with
  | Some reason, _ | None, Some reason -> Error reason
  | None, None -> (
      let statistic, zero =
        match metric with
        | Latency_p50 ->
            ( Bootstrap.mean_log_ratio_of_medians,
              "a scenario's median is zero in one variant" )
        | Pass_rate | Cost | Tokens ->
            (Bootstrap.log_ratio_of_means, "the mean is zero in one variant")
      in
      match
        statistic ~rng ~resamples:input.resamples ~confidence:level ~baseline:b
          ~candidate:c
      with
      | Some x -> Ok x
      | None -> Error (zero ^ ", so the ratio is undefined"))

let undefined metric ~threshold ~level ~in_gate ~reason : metric_result =
  {
    metric;
    estimate = Float.nan;
    interval = { lower = Float.nan; upper = Float.nan };
    threshold;
    level;
    verdict = Inconclusive { lean = `None };
    in_gate;
    undefined = Some reason;
  }

let tally xs = (List.length (List.filter (fun x -> x > 0.5) xs), List.length xs)
let rate (s, n) = float_of_int s /. float_of_int n

let pass_rate_estimate (input : input) ~rng ~level paired =
  let tb = List.map (fun (_, xs, _) -> tally xs) paired
  and tc = List.map (fun (_, _, ys) -> tally ys) paired in
  let sn =
    Stats.stratified_newcombe_diff ~baseline:tb ~candidate:tc ~confidence:level
  in
  let interval : Stats.interval =
    match input.population with
    | `Suite -> sn
    | `Scenarios ->
        let arms pick =
          List.map
            (fun ((s, _, _) as p) : Bootstrap.arm ->
              { scenario = s; values = Array.of_list (pick p) })
            paired
        in
        let h =
          Bootstrap.hierarchical_diff ~rng ~resamples:input.resamples
            ~confidence:level
            ~baseline:(arms (fun (_, xs, _) -> xs))
            ~candidate:(arms (fun (_, _, ys) -> ys))
        in
        {
          lower = Float.min h.lower sn.lower;
          upper = Float.max h.upper sn.upper;
        }
  in
  (* The same sum, in the same order, as the centre of the interval. *)
  let mean_rate ts =
    List.fold_left (fun acc t -> acc +. rate t) 0. ts
    /. float_of_int (List.length ts)
  in
  (mean_rate tc -. mean_rate tb, interval)

let run (input : input) =
  let open Result.Syntax in
  let ((tb, _, _) as cb) = count_missing input.runs input.baseline in
  let ((tc, _, _) as cc) = count_missing input.runs input.candidate in
  let* () =
    if tb = 0 then Error ("no runs for baseline " ^ input.baseline)
    else if tc = 0 then Error ("no runs for candidate " ^ input.candidate)
    else Ok ()
  in
  let b = outcomes_by_scenario input.runs input.baseline in
  let c = outcomes_by_scenario input.runs input.candidate in
  let paired =
    Smap.bindings b
    |> List.filter_map (fun (s, xs) ->
        Option.map (fun ys -> (s, xs, ys)) (Smap.find_opt s c))
  in
  let common = List.map (fun (s, _, _) -> s) paired in
  let dropped =
    List.filter_map
      (fun (r : run) ->
        if
          (String.equal r.variant input.baseline
          || String.equal r.variant input.candidate)
          && not (List.exists (String.equal r.scenario) common)
        then Some r.scenario
        else None)
      input.runs
    |> List.sort_uniq String.compare
  in
  let n_common = List.length common in
  let* () =
    if n_common = 0 then Error "no scenario has finished runs in both variants"
    else
      match input.population with
      | `Scenarios when n_common < min_scenarios ->
          Error
            (Printf.sprintf
               "population scenarios needs at least %d scenarios with runs in \
                both variants, found %d"
               min_scenarios n_common)
      | `Scenarios | `Suite -> Ok ()
  in
  let family = List.length input.gate in
  let gate_level =
    if family = 0 then input.confidence
    else Stats.bonferroni_level ~confidence:input.confidence ~family
  in
  let declared =
    List.map (fun (m, d) -> (m, d, true)) input.gate
    @ List.map (fun (m, d) -> (m, d, false)) input.report_metrics
  in
  let metrics =
    List.mapi
      (fun idx (metric, delta, in_gate) ->
        let rng = Random.State.make [| input.seed; idx |] in
        let level = if in_gate then gate_level else input.confidence in
        let threshold = scale_threshold metric delta in
        let judged estimate interval : metric_result =
          let verdict =
            decide ~threshold ~worse_is:(metric_worse_is metric) interval
          in
          {
            metric;
            estimate;
            interval;
            threshold;
            level;
            verdict;
            in_gate;
            undefined = None;
          }
        in
        match metric with
        | Pass_rate ->
            let est, interval = pass_rate_estimate input ~rng ~level paired in
            judged est interval
        | Cost | Latency_p50 | Tokens -> (
            match value_estimate input ~rng ~common metric ~level with
            | Ok (est, interval) -> judged est interval
            | Error reason ->
                undefined metric ~threshold ~level ~in_gate ~reason))
      declared
  in
  let pass_threshold =
    List.find_map
      (fun (m, d, _) ->
        match m with Pass_rate -> Some d | Cost | Latency_p50 | Tokens -> None)
      declared
    |> Option.value ~default:0.
  in
  let scenario_level =
    Stats.bonferroni_level ~confidence:input.confidence ~family:n_common
  in
  let per_scenario =
    List.map
      (fun (s, xs, ys) ->
        let s1, n1 = tally xs and s2, n2 = tally ys in
        let i =
          Stats.newcombe_diff ~s1 ~n1 ~s2 ~n2 ~confidence:scenario_level
        in
        (s, i, decide ~threshold:pass_threshold ~worse_is:`Lower i))
      paired
  in
  let gated pred =
    List.exists (fun (m : metric_result) -> m.in_gate && pred m.verdict) metrics
  in
  let regression =
    gated (function
      | Regression -> true
      | Improvement | No_change | Inconclusive _ -> false)
  in
  let inconclusive =
    gated (function
      | Inconclusive _ -> true
      | Regression | Improvement | No_change -> false)
  in
  let exit =
    if regression then 1
    else if biased cb cc || not (List.is_empty dropped) then 3
    else if inconclusive && input.fail_on_inconclusive then 2
    else 0
  in
  let missing_of variant (_, e, n) = (variant, e, n) in
  Ok
    {
      metrics;
      per_scenario;
      missing = [ missing_of input.baseline cb; missing_of input.candidate cc ];
      dropped;
      exit;
    }
