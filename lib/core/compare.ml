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
  exit : int;
}

module Smap = Map.Make (String)

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
let mean xs = Array.fold_left ( +. ) 0. xs /. float_of_int (Array.length xs)

(* Per scenario, the 1.0/0.0 outcomes of the variant's runs that have one. *)
let outcomes_by_scenario runs variant =
  List.fold_left
    (fun acc (r : run) ->
      match r.passed with
      | Some b when String.equal r.variant variant ->
          let v = if b then 1.0 else 0.0 in
          Smap.update r.scenario
            (fun cur -> Some (v :: Option.value cur ~default:[]))
            acc
      | Some _ | None -> acc)
    Smap.empty runs

(* Observed values of [f] over the variant's runs that have an outcome. *)
let values runs variant f =
  List.filter_map
    (fun (r : run) ->
      if String.equal r.variant variant && not (is_missing r) then f r else None)
    runs
  |> Array.of_list

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

let value_estimate runs (input : input) ~rng metric ~level =
  let get f variant = values runs variant f in
  let pick =
    match metric with
    | Pass_rate -> fun _ -> None
    | Cost -> fun (r : run) -> r.cost
    | Latency_p50 -> fun r -> r.latency_ms
    | Tokens -> fun r -> Option.map float_of_int r.tokens
  in
  let b = get pick input.baseline and c = get pick input.candidate in
  if Array.length b = 0 || Array.length c = 0 then None
  else
    Bootstrap.log_ratio_of_medians ~rng ~resamples:input.resamples
      ~confidence:level ~baseline:b ~candidate:c

let undefined metric ~threshold ~level : metric_result =
  {
    metric;
    estimate = Float.nan;
    interval = { lower = Float.nan; upper = Float.nan };
    threshold;
    level;
    verdict = Inconclusive { lean = `None };
    in_gate = false;
  }

let pass_rate_estimate (input : input) ~rng ~level ~common b c =
  let arm_of m s : Bootstrap.arm =
    { scenario = s; values = Array.of_list (Smap.find s m) }
  in
  let barms = List.map (arm_of b) common
  and carms = List.map (arm_of c) common in
  let diff =
    match input.population with
    | `Suite -> Bootstrap.stratified_diff
    | `Scenarios -> Bootstrap.hierarchical_diff
  in
  let interval =
    diff ~rng ~resamples:input.resamples ~confidence:level ~baseline:barms
      ~candidate:carms
  in
  let est =
    mean
      (Array.of_list
         (List.map2
            (fun (x : Bootstrap.arm) (y : Bootstrap.arm) ->
              mean y.values -. mean x.values)
            barms carms))
  in
  (est, interval)

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
  let common =
    Smap.bindings b |> List.map fst |> List.filter (fun s -> Smap.mem s c)
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
          { metric; estimate; interval; threshold; level; verdict; in_gate }
        in
        match metric with
        | Pass_rate ->
            let est, interval =
              pass_rate_estimate input ~rng ~level ~common b c
            in
            judged est interval
        | Cost | Latency_p50 | Tokens -> (
            match value_estimate input.runs input ~rng metric ~level with
            | Some (est, interval) -> judged est interval
            | None -> undefined metric ~threshold ~level))
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
  let tally m s =
    let xs = Smap.find s m in
    (List.length (List.filter (fun x -> x > 0.5) xs), List.length xs)
  in
  let per_scenario =
    List.map
      (fun s ->
        let s1, n1 = tally b s and s2, n2 = tally c s in
        let i =
          Stats.newcombe_diff ~s1 ~n1 ~s2 ~n2 ~confidence:scenario_level
        in
        (s, i, decide ~threshold:pass_threshold ~worse_is:`Lower i))
      common
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
    else if biased cb cc then 3
    else if inconclusive && input.fail_on_inconclusive then 2
    else 0
  in
  let missing_of variant (_, e, n) = (variant, e, n) in
  Ok
    {
      metrics;
      per_scenario;
      missing = [ missing_of input.baseline cb; missing_of input.candidate cc ];
      exit;
    }
