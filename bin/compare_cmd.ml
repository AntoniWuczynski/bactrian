open Cmdliner

let exits =
  Output.
    [
      Cmd.Exit.info exit_ok
        ~doc:
          "on success, or when $(b,--gate) is not given and the comparison ran.";
      Cmd.Exit.info 1
        ~doc:"with $(b,--gate), when a gate metric is a regression.";
      Cmd.Exit.info 2
        ~doc:
          "with $(b,--gate) and $(b,--fail-on-inconclusive), when a gate \
           metric is inconclusive.";
      Cmd.Exit.info 3
        ~doc:
          "with $(b,--gate), when more than 5% of the runs in an arm errored, \
           or the share of errored and unrun runs differs between the arms by \
           more than 5 percentage points.";
      Cmd.Exit.info exit_usage
        ~doc:"on a usage or configuration error, or an unknown experiment.";
      Cmd.Exit.info exit_internal
        ~doc:"on an internal error, such as unreadable results.";
    ]

let compare_command ~json ~gate ~fail_on_inconclusive ~baseline ~candidate
    experiment_id =
  Eio_main.run @@ fun env ->
  match
    Analysis.analyse ~json ~baseline ~candidate ~fail_on_inconclusive
      ~experiment_id env
  with
  | Error code -> code
  | Ok data ->
      print_string
        (if json then Stats_output.render_compare_json data
         else
           Stats_output.render_compare_text ~colour:(Lazy.force Output.colour)
             data);
      if gate then data.exit else Output.exit_ok

let cmd =
  let gate =
    Arg.(
      value & flag
      & info [ "gate" ]
          ~doc:"Exit with the gate's status (see EXIT STATUS), not 0.")
  in
  let fail_on_inconclusive =
    Arg.(
      value & flag
      & info [ "fail-on-inconclusive" ]
          ~doc:"Make an inconclusive gate metric exit 2 under $(b,--gate).")
  in
  let run json gate fail_on_inconclusive baseline candidate id =
    compare_command ~json ~gate ~fail_on_inconclusive ~baseline ~candidate id
  in
  let info =
    Cmd.info "compare" ~exits
      ~doc:"Compare two variants of an experiment with confidence intervals"
      ~man:Analysis.comparison_man
  in
  Cmd.v info
    Term.(
      const run $ Cli.json $ gate $ fail_on_inconclusive
      $ Analysis.baseline_flag $ Analysis.candidate_flag
      $ Analysis.experiment_id)
