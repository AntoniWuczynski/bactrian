open Cmdliner
open Bactrian_app

let report_command ~json ~baseline ~candidate ~output experiment_id =
  Eio_main.run @@ fun env ->
  match
    Report_command.run ~env { experiment_id; baseline; candidate; output }
  with
  | Error e -> Cli.fail_with ~json e
  | Ok report ->
      print_endline
        (if json then Yojson.Safe.to_string (Report_command.to_json report)
         else "wrote " ^ report.output);
      Exit_code.ok

let cmd =
  let output =
    Arg.(
      required
      & opt (some string) None
      & info [ "o"; "output" ] ~docv:"FILE"
          ~doc:"Where to write the HTML report.")
  in
  let run json baseline candidate output id =
    report_command ~json ~baseline ~candidate ~output id
  in
  let info =
    Cmd.info "report" ~exits:Cli.exits
      ~doc:"Write a self-contained HTML report of an experiment"
      ~man:
        (Analysis.comparison_man
        @ [
            `P
              "The report is one file with inline styles and charts, no \
               scripts and no external assets, and works offline.";
          ])
  in
  Cmd.v info
    Term.(
      const run $ Cli.json $ Analysis.baseline_flag $ Analysis.candidate_flag
      $ output $ Analysis.experiment_id)
