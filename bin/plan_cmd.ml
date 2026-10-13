open Cmdliner
open Bactrian_core

let ( let* ) = Result.bind

let plan_command ~json ~rates ~scenarios ~true_effect ~threshold ~population
    ~between_sd ~power ~confidence ~max_repeats ~simulations ~seed =
  let count = Option.value scenarios ~default:(List.length rates) in
  let in_unit name v =
    if Float.is_finite v && v > 0. && v < 1. then Ok ()
    else Error (Printf.sprintf "%s must be strictly between 0 and 1" name)
  in
  let non_negative name v =
    if Float.is_finite v && v >= 0. then Ok ()
    else Error (Printf.sprintf "%s must be a non-negative number" name)
  in
  let* () =
    match rates with
    | [] -> Error "plan needs at least one --baseline-rate"
    | _ :: _ ->
        if List.for_all (fun r -> Float.is_finite r && r >= 0. && r <= 1.) rates
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
        "give one --baseline-rate for every scenario, or a single one for all \
         of them"
    else Ok ()
  in
  let* () = non_negative "--threshold" threshold in
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
    match (population, between_sd) with
    | `Suite, _ -> Ok ()
    | `Scenarios, None -> Error "--population scenarios needs --between-sd"
    | `Scenarios, Some sd ->
        if count < 10 then
          Error "--population scenarios needs at least 10 --scenarios"
        else non_negative "--between-sd" sd
  in
  let rate i =
    match rates with
    | [ r ] -> r
    | _ -> Option.value (List.nth_opt rates i) ~default:0.
  in
  let assumption : Plan_sim.assumption =
    {
      baseline_rates =
        List.init count (fun i ->
            (Printf.sprintf "scenario-%d" (i + 1), rate i));
      true_effect;
      threshold;
      between_sd;
      population;
      confidence;
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
  let run json rates scenarios effect_opt threshold population between_sd power
      confidence max_repeats simulations seed =
    let true_effect = Option.value effect_opt ~default:(2. *. threshold) in
    match
      plan_command ~json ~rates ~scenarios ~true_effect ~threshold ~population
        ~between_sd ~power ~confidence ~max_repeats ~simulations ~seed
    with
    | Ok code -> `Ok code
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
             $(b,--max-repeats) gets there.";
        ]
  in
  Cmd.v info
    Term.(
      ret
        (const run $ Cli.json $ rates $ scenarios $ effect_flag $ threshold
       $ population $ between_sd $ power $ confidence $ max_repeats
       $ simulations $ seed))
