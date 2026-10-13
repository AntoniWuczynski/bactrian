open Bactrian_core
open Bactrian_runtime

let ( let* ) = Result.bind

type request = {
  rates : float list;
  scenarios : int option;
  from : string option;
  variant : string option;
  effect_ : float option;
  threshold : float;
  population : [ `Suite | `Scenarios ];
  between_sd : float option;
  gate_metrics : int;
  power : float;
  confidence : float;
  max_repeats : int;
  simulations : int;
  seed : int;
}

let populations = [ ("suite", `Suite); ("scenarios", `Scenarios) ]
let default_power = 0.8
let default_confidence = 0.95
let default_max_repeats = 200
let default_simulations = 500
let default_seed = 0
let default_gate_metrics = 1

type t = { assumption : Plan_sim.assumption; answer : Plan_sim.answer }

module Smap = Map.Make (String)

(* Per-scenario pass rates of a variant over its finished runs in an earlier
   experiment, sorted by scenario. The variant defaults to the experiment's
   compare.baseline. *)
let rates_from ~files ~env ~experiment_id ~variant =
  let* loaded =
    Results.load ~fs:files
      ~root:Eio.Path.(Eio.Stdenv.cwd env / ".bactrian")
      ~experiment_id
    |> Result.map_error (Format.asprintf "%a" Results.pp_error)
  in
  let* variant =
    match variant with
    | Some v -> Ok v
    | None -> (
        match
          Option.bind loaded.experiment (fun (e : Experiment.t) -> e.compare)
        with
        | Some c -> Ok c.baseline
        | None ->
            Error
              "--variant is required: the experiment file names no \
               compare.baseline")
  in
  let tallies =
    List.fold_left
      (fun acc (r : Report_data.run) ->
        let passed =
          match r.status with
          | Passed -> Some true
          | Failed | Diverged -> Some false
          | Errored | Not_run | Pending | Interrupted -> None
        in
        match passed with
        | Some p when String.equal r.variant variant ->
            Smap.update r.scenario
              (fun cur ->
                let s, n = Option.value cur ~default:(0, 0) in
                Some ((if p then s + 1 else s), n + 1))
              acc
        | Some _ | None -> acc)
      Smap.empty loaded.runs
  in
  match Smap.bindings tallies with
  | [] ->
      Error
        (Printf.sprintf "variant %S has no finished runs in experiment %s"
           variant experiment_id)
  | bindings ->
      Ok
        (List.map
           (fun (s, (passes, n)) -> (s, float_of_int passes /. float_of_int n))
           bindings)

let max_scenarios = 10_000
let max_work = 100_000_000

(* The scenarios and their baseline rates, from the flags or from [--from]. *)
let baseline_rates ~files ~env ~rates ~scenarios ~from ~variant =
  match from with
  | Some experiment_id -> (
      match (rates, scenarios) with
      | [], None -> rates_from ~files ~env ~experiment_id ~variant
      | _ :: _, _ | [], Some _ ->
          Error
            "--from gives the scenarios and their rates, so it takes no \
             --baseline-rate or --scenarios")
  | None ->
      let count = Option.value scenarios ~default:(List.length rates) in
      let* () =
        match variant with
        | None -> Ok ()
        | Some _ -> Error "--variant only applies with --from"
      in
      let* () =
        match rates with
        | [] -> Error "plan needs at least one --baseline-rate, or --from"
        | _ :: _ ->
            if
              List.for_all
                (fun r -> Float.is_finite r && r >= 0. && r <= 1.)
                rates
            then Ok ()
            else Error "--baseline-rate must be between 0 and 1"
      in
      let* () =
        if count < 1 then Error "--scenarios must be at least 1"
        else if count > max_scenarios then
          Error (Printf.sprintf "--scenarios must be at most %d" max_scenarios)
        else if
          List.compare_length_with rates 1 > 0
          && List.compare_length_with rates count <> 0
        then
          Error
            "give one --baseline-rate for every scenario, or a single one for \
             all of them"
        else Ok ()
      in
      let rate i =
        match rates with
        | [ r ] -> r
        | _ -> Option.value (List.nth_opt rates i) ~default:0.
      in
      Ok
        (List.init count (fun i ->
             (Printf.sprintf "scenario-%d" (i + 1), rate i)))

let max_gate_metrics = 4

let validate_assumption (a : Plan_sim.assumption) =
  let count = List.length a.baseline_rates in
  let in_unit name v =
    if Float.is_finite v && v > 0. && v < 1. then Ok ()
    else Error (Printf.sprintf "%s must be strictly between 0 and 1" name)
  in
  let non_negative name v =
    if Float.is_finite v && v >= 0. then Ok ()
    else Error (Printf.sprintf "%s must be a non-negative number" name)
  in
  let* () = in_unit "--threshold" a.threshold in
  let* () =
    if Float.is_finite a.true_effect then Ok ()
    else Error "--effect must be a number"
  in
  let* () = in_unit "--power" a.power in
  let* () = in_unit "--confidence" a.confidence in
  let* () =
    if a.max_repeats < 1 || a.simulations < 1 then
      Error "--max-repeats and --simulations must be at least 1"
    else Ok ()
  in
  let* () =
    (* Each repeat count tried simulates every scenario's runs [simulations]
       times. In floating point, as the product of three ints can overflow. *)
    let work =
      Float.of_int count *. Float.of_int a.simulations
      *. Float.of_int a.max_repeats
    in
    if work > Float.of_int max_work then
      Error
        (Printf.sprintf
           "scenarios x --simulations x --max-repeats is %.0f, more than the \
            %d a plan may simulate: lower --simulations or --max-repeats"
           work max_work)
    else Ok ()
  in
  let* () =
    if a.gate_metrics < 1 || a.gate_metrics > max_gate_metrics then
      Error
        (Printf.sprintf "--gate-metrics must be from 1 to %d" max_gate_metrics)
    else Ok ()
  in
  match (a.population, a.between_sd) with
  | `Suite, _ -> Ok ()
  | `Scenarios, None -> Error "--population scenarios needs --between-sd"
  | `Scenarios, Some sd ->
      if count < 10 then
        Error "--population scenarios needs at least 10 --scenarios"
      else non_negative "--between-sd" sd

let plan ~compute ~files ~env r =
  let* baseline_rates =
    baseline_rates ~files ~env ~rates:r.rates ~scenarios:r.scenarios
      ~from:r.from ~variant:r.variant
  in
  let assumption : Plan_sim.assumption =
    {
      baseline_rates;
      true_effect = Option.value r.effect_ ~default:(2. *. r.threshold);
      threshold = r.threshold;
      between_sd = r.between_sd;
      population = r.population;
      confidence = r.confidence;
      gate_metrics = r.gate_metrics;
      power = r.power;
      max_repeats = r.max_repeats;
      seed = r.seed;
      simulations = r.simulations;
    }
  in
  let* () = validate_assumption assumption in
  Ok
    {
      assumption;
      answer =
        compute.Compute.run (fun ~check -> Plan_sim.plan ~check assumption);
    }

let run ?(compute = Compute.inline) ?files ~env r =
  let files = Option.value files ~default:(Eio.Stdenv.fs env) in
  Result.map_error
    (fun m -> Command_error.Usage m)
    (plan ~compute ~files ~env r)

let exit_code (_ : t) = Exit_code.ok
let population_name = function `Suite -> "suite" | `Scenarios -> "scenarios"
let target_name a = Report_data.verdict_name (Plan_sim.target a)

let to_json { assumption = a; answer } =
  let float = Json_float.to_json in
  `Assoc
    [
      ("schema_version", `Int Report_data.schema_version);
      ( "assumptions",
        `Assoc
          [
            ( "baseline_rates",
              `List
                (List.map
                   (fun (s, r) ->
                     `Assoc [ ("scenario", `String s); ("rate", float r) ])
                   a.baseline_rates) );
            ("effect", float a.true_effect);
            ("threshold", float a.threshold);
            ( "between_sd",
              match a.between_sd with Some s -> float s | None -> `Null );
            ("population", `String (population_name a.population));
            ("confidence", float a.confidence);
            ("gate_metrics", `Int a.gate_metrics);
            ("power", float a.power);
            ("max_repeats", `Int a.max_repeats);
            ("seed", `Int a.seed);
            ("simulations", `Int a.simulations);
          ] );
      ("target", `String (target_name a));
      ( "answer",
        match answer with
        | Plan_sim.Repeats { n; achieved_power; mc_se } ->
            `Assoc
              [
                ("result", `String "repeats");
                ("repeats", `Int n);
                ("achieved_power", float achieved_power);
                ("mc_se", float mc_se);
              ]
        | Unreachable { best_power } ->
            `Assoc
              [
                ("result", `String "unreachable");
                ("best_power", float best_power);
              ] );
    ]
