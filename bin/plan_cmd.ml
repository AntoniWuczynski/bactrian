open Cmdliner
open Bactrian_app

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
      & opt (enum Plan_command.populations) `Suite
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
      value
      & opt float Plan_command.default_power
      & info [ "power" ] ~docv:"P"
          ~doc:"The probability of the correct verdict to reach.")
  in
  let confidence =
    Arg.(
      value
      & opt float Plan_command.default_confidence
      & info [ "confidence" ] ~docv:"C" ~doc:"The confidence level.")
  in
  let max_repeats =
    Arg.(
      value
      & opt int Plan_command.default_max_repeats
      & info [ "max-repeats" ] ~docv:"N" ~doc:"The largest repeat count to try.")
  in
  let simulations =
    Arg.(
      value
      & opt int Plan_command.default_simulations
      & info [ "simulations" ] ~docv:"N"
          ~doc:"Simulated experiments per repeat count.")
  in
  let seed =
    Arg.(
      value
      & opt int Plan_command.default_seed
      & info [ "seed" ] ~docv:"N" ~doc:"The random seed.")
  in
  let gate_metrics =
    Arg.(
      value
      & opt int Plan_command.default_gate_metrics
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
  let run json rates scenarios from variant effect_ threshold population
      between_sd gate_metrics power confidence max_repeats simulations seed =
    Eio_main.run @@ fun env ->
    match
      Plan_command.run ~env
        {
          rates;
          scenarios;
          from;
          variant;
          effect_;
          threshold;
          population;
          between_sd;
          gate_metrics;
          power;
          confidence;
          max_repeats;
          simulations;
          seed;
        }
    with
    | Ok plan ->
        print_string
          (if json then Yojson.Safe.to_string (Plan_command.to_json plan) ^ "\n"
           else Stats_output.render_plan_text plan);
        `Ok (Plan_command.exit_code plan)
    | Error e when json -> `Ok (Cli.fail_with ~json e)
    | Error e -> `Error (false, Command_error.message e)
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
