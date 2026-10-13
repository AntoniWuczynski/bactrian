open Cmdliner
open Bactrian_core
open Bactrian_runtime

let ( let* ) = Result.bind

module Smap = Map.Make (String)

(* Per-scenario pass rates of a variant over its finished runs in an earlier
   experiment, sorted by scenario. The variant defaults to the experiment's
   compare.baseline. *)
let rates_from ~experiment_id ~variant =
  Eio_main.run @@ fun env ->
  let* loaded =
    Results.load ~fs:(Eio.Stdenv.fs env)
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

(* The scenarios and their baseline rates, from the flags or from [--from]. *)
let baseline_rates ~rates ~scenarios ~from ~variant =
  match from with
  | Some experiment_id -> (
      match (rates, scenarios) with
      | [], None -> rates_from ~experiment_id ~variant
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

let plan_command ~json ~baseline_rates ~true_effect ~threshold ~population
    ~between_sd ~gate_metrics ~power ~confidence ~max_repeats ~simulations ~seed
    =
  let count = List.length baseline_rates in
  let in_unit name v =
    if Float.is_finite v && v > 0. && v < 1. then Ok ()
    else Error (Printf.sprintf "%s must be strictly between 0 and 1" name)
  in
  let non_negative name v =
    if Float.is_finite v && v >= 0. then Ok ()
    else Error (Printf.sprintf "%s must be a non-negative number" name)
  in
  let* () = in_unit "--threshold" threshold in
  let* () =
    if Float.is_finite true_effect then Ok ()
    else Error "--effect must be a number"
  in
  let* () = in_unit "--power" power in
  let* () = in_unit "--confidence" confidence in
  let* () =
    if max_repeats < 1 || simulations < 1 then
      Error "--max-repeats and --simulations must be at least 1"
    else Ok ()
  in
  let* () =
    if gate_metrics < 1 || gate_metrics > max_gate_metrics then
      Error
        (Printf.sprintf "--gate-metrics must be from 1 to %d" max_gate_metrics)
    else Ok ()
  in
  let* () =
    match (population, between_sd) with
    | `Suite, _ -> Ok ()
    | `Scenarios, None -> Error "--population scenarios needs --between-sd"
    | `Scenarios, Some sd ->
        if count < 10 then
          Error "--population scenarios needs at least 10 --scenarios"
        else non_negative "--between-sd" sd
  in
  let assumption : Plan_sim.assumption =
    {
      baseline_rates;
      true_effect;
      threshold;
      between_sd;
      population;
      confidence;
      gate_metrics;
      power;
      max_repeats;
      seed;
      simulations;
    }
  in
  let answer = Plan_sim.plan assumption in
  print_string
    (if json then Stats_output.render_plan_json assumption answer
     else Stats_output.render_plan_text assumption answer);
  Ok Output.exit_ok

let cmd =
  let rates =
    Arg.(
      value & opt_all float []
      & info [ "baseline-rate" ] ~docv:"R"
          ~doc:
            "The baseline pass rate of a scenario, from 0 to 1. Repeat it for \
             several scenarios, or give one for all of them.")
  in
  let scenarios =
    Arg.(
      value
      & opt (some int) None
      & info [ "scenarios" ] ~docv:"N"
          ~doc:"The number of scenarios (default: the number of rates).")
  in
  let effect_flag =
    Arg.(
      value
      & opt (some float) None
      & info [ "effect" ] ~docv:"E"
          ~doc:
            "The assumed true drop in pass rate of the candidate (default \
             twice the threshold).")
  in
  let threshold =
    Arg.(
      required
      & opt (some float) None
      & info [ "threshold" ] ~docv:"T" ~doc:"The regression threshold.")
  in
  let population =
    Arg.(
      value
      & opt (enum [ ("suite", `Suite); ("scenarios", `Scenarios) ]) `Suite
      & info [ "population" ] ~docv:"POP"
          ~doc:"The estimand, $(b,suite) or $(b,scenarios).")
  in
  let between_sd =
    Arg.(
      value
      & opt (some float) None
      & info [ "between-sd" ] ~docv:"S"
          ~doc:
            "The standard deviation of the effect between scenarios (required \
             for $(b,--population scenarios)).")
  in
  let power =
    Arg.(
      value & opt float 0.8
      & info [ "power" ] ~docv:"P"
          ~doc:"The probability of the correct verdict to reach.")
  in
  let confidence =
    Arg.(
      value & opt float 0.95
      & info [ "confidence" ] ~docv:"C" ~doc:"The confidence level.")
  in
  let max_repeats =
    Arg.(
      value & opt int 200
      & info [ "max-repeats" ] ~docv:"N" ~doc:"The largest repeat count to try.")
  in
  let simulations =
    Arg.(
      value & opt int 500
      & info [ "simulations" ] ~docv:"N"
          ~doc:"Simulated experiments per repeat count.")
  in
  let seed =
    Arg.(value & opt int 0 & info [ "seed" ] ~docv:"N" ~doc:"The random seed.")
  in
  let gate_metrics =
    Arg.(
      value & opt int 1
      & info [ "gate-metrics" ] ~docv:"N"
          ~doc:
            "The number of metrics in the experiment's gate. The pass rate is \
             then judged at the Bonferroni level, as $(b,compare) judges it, \
             which needs more repeats.")
  in
  let from =
    Arg.(
      value
      & opt (some string) None
      & info [ "from" ] ~docv:"EXPERIMENT-ID"
          ~doc:
            "Take the scenarios and their baseline pass rates from the \
             finished runs of an earlier experiment under \
             ./.bactrian/experiments, instead of $(b,--baseline-rate) and \
             $(b,--scenarios).")
  in
  let variant =
    Arg.(
      value
      & opt (some string) None
      & info [ "variant" ] ~docv:"VARIANT"
          ~doc:
            "With $(b,--from), the variant whose rates to take. Defaults to \
             $(b,compare.baseline) in that experiment's file.")
  in
  let run json rates scenarios from variant effect_opt threshold population
      between_sd gate_metrics power confidence max_repeats simulations seed =
    let true_effect = Option.value effect_opt ~default:(2. *. threshold) in
    match
      let* baseline_rates = baseline_rates ~rates ~scenarios ~from ~variant in
      plan_command ~json ~baseline_rates ~true_effect ~threshold ~population
        ~between_sd ~gate_metrics ~power ~confidence ~max_repeats ~simulations
        ~seed
    with
    | Ok code -> `Ok code
    | Error m when json -> `Ok (Cli.fail ~json Output.exit_usage m)
    | Error m -> `Error (false, m)
  in
  let info =
    Cmd.info "plan" ~exits:Cli.exits
      ~doc:"Say how many repeats an experiment needs"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Simulates the exact decision rule of $(b,compare) and prints the \
             smallest repeat count per scenario and variant that reaches the \
             correct verdict with the requested power, with the Monte Carlo \
             standard error. It says $(b,unreachable) when no count up to \
             $(b,--max-repeats) gets there. The correct verdict, which it \
             prints first, is a regression when the assumed drop is at least \
             the threshold, an improvement when it is at most minus the \
             threshold, and no change otherwise.";
        ]
  in
  Cmd.v info
    Term.(
      ret
        (const run $ Cli.json $ rates $ scenarios $ from $ variant $ effect_flag
       $ threshold $ population $ between_sd $ gate_metrics $ power $ confidence
       $ max_repeats $ simulations $ seed))
