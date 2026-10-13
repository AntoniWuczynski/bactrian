let schema_version = 1

type status =
  | Passed
  | Failed
  | Diverged
  | Errored
  | Not_run
  | Pending
  | Interrupted

type check_status = Check_pass | Check_fail | Check_error
type check = { name : string; status : check_status; reason : string option }
type divergence = { description : string; changes : Diff.change list }

type run = {
  index : int;
  repeat : int;
  scenario : string;
  variant : string;
  run_id : string option;
  status : status;
  reason : string option;
  checks : check list;
  trace : Trace.entry list;
  divergences : divergence list;
  cost : float option;
  latency_ms : float option;
  tokens : int option;
}

let model_calls trace =
  List.filter_map
    (fun (e : Trace.entry) ->
      match e.event with
      | Model_call { duration_ms; usage; _ } -> Some (duration_ms, usage)
      | Run_start _ | Turn_start _ | Tool_call _ | Tool_result _ | Http_call _
      | Agent_event _ | Reply _ | Sandbox_violation _ | Done ->
          None)
    trace

let latency_ms trace =
  match model_calls trace with
  | [] -> None
  | calls ->
      Some (float_of_int (List.fold_left (fun a (d, _) -> a + d) 0 calls))

let tokens trace =
  match List.filter_map snd (model_calls trace) with
  | [] -> None
  | usages ->
      Some
        (List.fold_left
           (fun a (u : Trace.usage) -> a + u.input_tokens + u.output_tokens)
           0 usages)

let compare_run (r : run) : Compare.run =
  let passed, missing =
    match r.status with
    | Passed -> (Some true, None)
    | Failed | Diverged -> (Some false, None)
    | Errored -> (None, Some Compare.Errored)
    | Not_run | Pending | Interrupted -> (None, Some Compare.Not_run)
  in
  {
    scenario = r.scenario;
    variant = r.variant;
    passed;
    missing;
    cost = r.cost;
    latency_ms = r.latency_ms;
    tokens = r.tokens;
  }

let metric_name : Compare.metric -> string = function
  | Pass_rate -> "pass_rate"
  | Cost -> "cost"
  | Latency_p50 -> "latency_p50"
  | Tokens -> "tokens"

let metric_of_name = function
  | "pass_rate" -> Some Compare.Pass_rate
  | "cost" -> Some Cost
  | "latency_p50" -> Some Latency_p50
  | "tokens" -> Some Tokens
  | _ -> None

let verdict_name : Compare.verdict -> string = function
  | Regression -> "regression"
  | Improvement -> "improvement"
  | No_change -> "no_change"
  | Inconclusive _ -> "inconclusive"

let lean_name : Compare.verdict -> string option = function
  | Inconclusive { lean = `Worse } -> Some "worse"
  | Inconclusive { lean = `Better } -> Some "better"
  | Inconclusive { lean = `None } -> Some "none"
  | Regression | Improvement | No_change -> None

(* Behaviour *)

let signature trace =
  List.filter_map
    (fun (e : Trace.entry) ->
      match e.event with
      | Tool_call { name; _ } -> Some name
      | Run_start _ | Turn_start _ | Model_call _ | Tool_result _ | Http_call _
      | Agent_event _ | Reply _ | Sandbox_violation _ | Done ->
          None)
    trace

let compare_signatures = List.compare String.compare

let tally compare_key xs =
  List.sort compare_key xs
  |> List.fold_left
       (fun acc x ->
         match acc with
         | (y, n) :: rest when compare_key x y = 0 -> (y, n + 1) :: rest
         | _ -> (x, 1) :: acc)
       []
  |> List.rev

let new_behaviours ~baseline ~candidate =
  let nb = List.length baseline and nc = List.length candidate in
  if nb = 0 || nc = 0 then []
  else
    tally compare_signatures candidate
    |> List.filter (fun (s, n) ->
        n * 10 >= nc
        && not (List.exists (fun b -> compare_signatures b s = 0) baseline))
    |> List.stable_sort (fun (a, na) (b, nb) ->
        match Int.compare nb na with 0 -> compare_signatures a b | c -> c)
    |> List.map fst

type dist = { n : int; median : float; p90 : float }

type metric_row = {
  name : string;
  result : Compare.metric_result;
  delta : float;
  relative : (float * Stats.interval) option;
}

type scenario_row = {
  scenario : string;
  baseline_passes : int;
  baseline_runs : int;
  candidate_passes : int;
  candidate_runs : int;
  difference : float;
  interval : Stats.interval;
  verdict : Compare.verdict;
}

type summary = { variant : string; cost : dist option; latency : dist option }
type signature_count = { signature : string list; count : int }

type behaviour = {
  scenario : string;
  baseline_signatures : signature_count list;
  baseline_traced : int;
  candidate_signatures : signature_count list;
  candidate_traced : int;
  new_signatures : string list list;
  baseline_steps : (int * int) list;
  candidate_steps : (int * int) list;
}

type failure = { run : run; messages : string list }

type t = {
  experiment_id : string;
  experiment : string;
  baseline : string;
  candidate : string;
  population : [ `Suite | `Scenarios ];
  confidence : float;
  gate : (string * float) list;
  gate_level : float;
  seed : int;
  resamples : int;
  missing : (string * int * int * int) list;
  exit : int;
  metrics : metric_row list;
  scenarios : scenario_row list;
  summaries : summary list;
  behaviours : behaviour list;
  failures : failure list;
  order : run list;
}

(* Building *)

let count_signatures signatures =
  tally compare_signatures signatures
  |> List.map (fun (signature, count) -> { signature; count })
  |> List.stable_sort (fun a b ->
      match Int.compare b.count a.count with
      | 0 -> compare_signatures a.signature b.signature
      | c -> c)

let dist = function
  | [] -> None
  | xs ->
      let a = Array.of_list xs in
      Some
        {
          n = Array.length a;
          median = Stats.median a;
          p90 = Stats.quantile a 0.9;
        }

let of_variant variant runs =
  List.filter (fun (r : run) -> String.equal r.variant variant) runs

let finished (r : run) =
  match r.status with
  | Passed | Failed | Diverged -> true
  | Errored | Not_run | Pending | Interrupted -> false

let summary runs variant =
  let mine = List.filter finished (of_variant variant runs) in
  {
    variant;
    cost = dist (List.filter_map (fun (r : run) -> r.cost) mine);
    latency = dist (List.filter_map (fun (r : run) -> r.latency_ms) mine);
  }

let metric_row (settings : Compare.input) (m : Compare.metric_result) =
  let delta =
    List.assoc_opt m.metric (settings.gate @ settings.report_metrics)
    |> Option.value ~default:0.
  in
  let relative =
    match m.metric with
    | Pass_rate -> None
    | Cost | Latency_p50 | Tokens ->
        if Float.is_nan m.estimate then None
        else
          Some
            ( Float.expm1 m.estimate,
              {
                Stats.lower = Float.expm1 m.interval.lower;
                upper = Float.expm1 m.interval.upper;
              } )
  in
  { name = metric_name m.metric; result = m; delta; relative }

let traced variant scenario runs =
  List.filter
    (fun (r : run) ->
      String.equal r.variant variant
      && String.equal r.scenario scenario
      && not (List.is_empty r.trace))
    runs

let steps traced_runs =
  tally Int.compare
    (List.map
       (fun (r : run) ->
         List.length
           (List.filter
              (fun (e : Trace.entry) ->
                match e.event with
                | Model_call _ -> true
                | Run_start _ | Turn_start _ | Tool_call _ | Tool_result _
                | Http_call _ | Agent_event _ | Reply _ | Sandbox_violation _
                | Done ->
                    false)
              r.trace))
       traced_runs)

let behaviour (settings : Compare.input) runs scenario =
  let b = traced settings.baseline scenario runs
  and c = traced settings.candidate scenario runs in
  let sigs rs = List.map (fun (r : run) -> signature r.trace) rs in
  {
    scenario;
    baseline_signatures = count_signatures (sigs b);
    baseline_traced = List.length b;
    candidate_signatures = count_signatures (sigs c);
    candidate_traced = List.length c;
    new_signatures = new_behaviours ~baseline:(sigs b) ~candidate:(sigs c);
    baseline_steps = steps b;
    candidate_steps = steps c;
  }

let check_message (c : check) =
  match c.status with
  | Check_pass -> None
  | Check_fail | Check_error ->
      let what =
        match c.status with
        | Check_error -> "checker failed to give a verdict"
        | Check_pass | Check_fail -> "failed"
      in
      Some
        (match c.reason with
        | Some r -> Printf.sprintf "%s: %s" c.name r
        | None -> Printf.sprintf "%s: %s" c.name what)

let failure (r : run) =
  let items = List.filter_map check_message r.checks in
  (* The reason of a run judged by its checks only repeats them. *)
  let reason = if List.is_empty items then Option.to_list r.reason else [] in
  let divs = List.map (fun d -> "divergence: " ^ d.description) r.divergences in
  { run = r; messages = reason @ items @ divs }

let is_failure (r : run) =
  match r.status with
  | Failed | Diverged | Errored -> true
  | Passed | Not_run | Pending | Interrupted -> false

let scenario_row (settings : Compare.input) runs (scenario, interval, verdict) :
    scenario_row =
  let tally variant =
    let rs =
      List.filter
        (fun (r : run) -> String.equal r.scenario scenario && finished r)
        (of_variant variant runs)
    in
    ( List.length (List.filter (fun (r : run) -> r.status = Passed) rs),
      List.length rs )
  in
  let bp, bn = tally settings.baseline and cp, cn = tally settings.candidate in
  let rate p n =
    if n = 0 then Float.nan else float_of_int p /. float_of_int n
  in
  {
    scenario;
    baseline_passes = bp;
    baseline_runs = bn;
    candidate_passes = cp;
    candidate_runs = cn;
    difference = rate cp cn -. rate bp bn;
    interval;
    verdict;
  }

let build ~experiment_id ~experiment ~(settings : Compare.input)
    ~(result : Compare.result) runs =
  let order =
    List.stable_sort (fun (a : run) b -> Int.compare a.index b.index) runs
  in
  let family = List.length settings.gate in
  let compared = [ settings.baseline; settings.candidate ] in
  let scenario_names =
    List.filter_map
      (fun (r : run) ->
        if List.mem r.variant compared then Some r.scenario else None)
      runs
    |> List.sort_uniq String.compare
  in
  {
    experiment_id;
    experiment;
    baseline = settings.baseline;
    candidate = settings.candidate;
    population = settings.population;
    confidence = settings.confidence;
    gate = List.map (fun (m, d) -> (metric_name m, d)) settings.gate;
    gate_level =
      (if family = 0 then settings.confidence
       else Stats.bonferroni_level ~confidence:settings.confidence ~family);
    seed = settings.seed;
    resamples = settings.resamples;
    missing =
      List.map
        (fun (variant, errored, not_run) ->
          (variant, List.length (of_variant variant runs), errored, not_run))
        result.missing;
    exit = result.exit;
    metrics = List.map (metric_row settings) result.metrics;
    scenarios = List.map (scenario_row settings runs) result.per_scenario;
    summaries = List.map (summary runs) compared;
    behaviours = List.map (behaviour settings runs) scenario_names;
    failures =
      List.filter
        (fun (r : run) -> List.mem r.variant compared && is_failure r)
        order
      |> List.map failure;
    order;
  }

(* JSON *)

let float_json f : Yojson.Safe.t = if Float.is_finite f then `Float f else `Null
let opt f : _ -> Yojson.Safe.t = function Some x -> f x | None -> `Null

let interval_json (i : Stats.interval) : Yojson.Safe.t =
  `Assoc [ ("lower", float_json i.lower); ("upper", float_json i.upper) ]

let verdict_json v : Yojson.Safe.t =
  `Assoc
    [
      ("verdict", `String (verdict_name v));
      ("lean", opt (fun s -> `String s) (lean_name v));
    ]

let status_name = function
  | Passed -> "passed"
  | Failed -> "failed"
  | Diverged -> "diverged"
  | Errored -> "errored"
  | Not_run -> "not_run"
  | Pending -> "pending"
  | Interrupted -> "interrupted"

let metric_json (m : metric_row) : Yojson.Safe.t =
  let r = m.result in
  `Assoc
    ([
       ("metric", `String m.name);
       ("in_gate", `Bool r.in_gate);
       ("estimate", float_json r.estimate);
       ("interval", interval_json r.interval);
       ("threshold", float_json m.delta);
       ("threshold_scaled", float_json r.threshold);
       ("level", float_json r.level);
     ]
    @ (match m.relative with
      | Some (e, i) ->
          [
            ( "relative",
              `Assoc
                [ ("estimate", float_json e); ("interval", interval_json i) ] );
          ]
      | None -> [])
    @ [ ("result", verdict_json r.verdict) ])

let scenario_json (s : scenario_row) : Yojson.Safe.t =
  `Assoc
    [
      ("scenario", `String s.scenario);
      ("baseline_passes", `Int s.baseline_passes);
      ("baseline_runs", `Int s.baseline_runs);
      ("candidate_passes", `Int s.candidate_passes);
      ("candidate_runs", `Int s.candidate_runs);
      ("difference", float_json s.difference);
      ("interval", interval_json s.interval);
      ("result", verdict_json s.verdict);
    ]

let dist_json : dist option -> Yojson.Safe.t =
  opt (fun d ->
      `Assoc
        [
          ("n", `Int d.n);
          ("median", float_json d.median);
          ("p90", float_json d.p90);
        ])

let strings l : Yojson.Safe.t = `List (List.map (fun s -> `String s) l)

let signatures_json l : Yojson.Safe.t =
  `List
    (List.map
       (fun s ->
         `Assoc [ ("signature", strings s.signature); ("count", `Int s.count) ])
       l)

let steps_json l : Yojson.Safe.t =
  `List
    (List.map (fun (s, n) -> `Assoc [ ("steps", `Int s); ("runs", `Int n) ]) l)

let behaviour_json (b : behaviour) : Yojson.Safe.t =
  `Assoc
    [
      ("scenario", `String b.scenario);
      ("baseline_traced", `Int b.baseline_traced);
      ("baseline_signatures", signatures_json b.baseline_signatures);
      ("candidate_traced", `Int b.candidate_traced);
      ("candidate_signatures", signatures_json b.candidate_signatures);
      ("new_behaviours", `List (List.map strings b.new_signatures));
      ("baseline_steps", steps_json b.baseline_steps);
      ("candidate_steps", steps_json b.candidate_steps);
    ]

let change_json (c : Diff.change) : Yojson.Safe.t =
  `Assoc
    [
      ("path", `String c.path);
      ( "kind",
        `String
          (match c.kind with
          | Changed -> "changed"
          | Added -> "added"
          | Removed -> "removed") );
    ]

let run_json ~detail (r : run) : Yojson.Safe.t =
  `Assoc
    ([
       ("index", `Int r.index);
       ("scenario", `String r.scenario);
       ("variant", `String r.variant);
       ("repeat", `Int r.repeat);
       ("run_id", opt (fun s -> `String s) r.run_id);
       ("status", `String (status_name r.status));
       ("reason", opt (fun s -> `String s) r.reason);
     ]
    @
    if detail then
      [
        ("cost_usd", opt float_json r.cost);
        ("latency_ms", opt float_json r.latency_ms);
        ("tokens", opt (fun n -> `Int n) r.tokens);
        ( "divergences",
          `List
            (List.map
               (fun d ->
                 `Assoc
                   [
                     ("description", `String d.description);
                     ("changes", `List (List.map change_json d.changes));
                   ])
               r.divergences) );
        ( "trace",
          `List
            (List.map
               (fun e -> Yojson.Safe.from_string (Trace.to_line e))
               r.trace) );
      ]
    else [])

let failure_json (f : failure) : Yojson.Safe.t =
  match run_json ~detail:true f.run with
  | `Assoc fields -> `Assoc (fields @ [ ("messages", strings f.messages) ])
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _) as
    other ->
      other

let summary_fields (t : t) =
  [
    ("schema_version", `Int schema_version);
    ("experiment_id", `String t.experiment_id);
    ("experiment", `String t.experiment);
    ("baseline", `String t.baseline);
    ("candidate", `String t.candidate);
    ( "estimand",
      `String
        (match t.population with
        | `Suite -> "suite"
        | `Scenarios -> "scenarios") );
    ("confidence", float_json t.confidence);
    ( "gate",
      `Assoc
        [
          ( "metrics",
            `List
              (List.map
                 (fun (m, d) ->
                   `Assoc [ ("metric", `String m); ("threshold", float_json d) ])
                 t.gate) );
          ("level", float_json t.gate_level);
        ] );
    ("seed", `Int t.seed);
    ("resamples", `Int t.resamples);
    ( "missing",
      `List
        (List.map
           (fun (v, total, errored, not_run) ->
             `Assoc
               [
                 ("variant", `String v);
                 ("runs", `Int total);
                 ("errored", `Int errored);
                 ("not_run", `Int not_run);
               ])
           t.missing) );
    ("exit", `Int t.exit);
    ("metrics", `List (List.map metric_json t.metrics));
    ("scenarios", `List (List.map scenario_json t.scenarios));
    ( "summaries",
      `List
        (List.map
           (fun s ->
             `Assoc
               [
                 ("variant", `String s.variant);
                 ("cost_usd", dist_json s.cost);
                 ("latency_ms", dist_json s.latency);
               ])
           t.summaries) );
  ]

let summary_json (t : t) : Yojson.Safe.t = `Assoc (summary_fields t)

let to_json (t : t) : Yojson.Safe.t =
  `Assoc
    (summary_fields t
    @ [
        ("behaviours", `List (List.map behaviour_json t.behaviours));
        ("failures", `List (List.map failure_json t.failures));
        ("order", `List (List.map (run_json ~detail:false) t.order));
      ])
