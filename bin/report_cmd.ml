open Cmdliner
open Bactrian_core
open Bactrian_runtime

let report_command ~baseline ~candidate ~output experiment_id =
  Eio_main.run @@ fun env ->
  match
    Analysis.analyse ~json:false ~baseline ~candidate
      ~fail_on_inconclusive:false ~experiment_id env
  with
  | Error code -> code
  | Ok data -> (
      match
        Store.write_atomic
          Eio.Path.(Eio.Stdenv.fs env / output)
          (Report_html.render data)
      with
      | () ->
          print_endline ("wrote " ^ output);
          Output.exit_ok
      | exception Eio.Io _ ->
          Cli.fail ~json:false Output.exit_environment
            (Printf.sprintf "cannot write %s" output))

let cmd =
  let output =
    Arg.(
      required
      & opt (some string) None
      & info [ "o"; "output" ] ~docv:"FILE"
          ~doc:"Where to write the HTML report.")
  in
  let run baseline candidate output id =
    report_command ~baseline ~candidate ~output id
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
      const run $ Analysis.baseline_flag $ Analysis.candidate_flag $ output
      $ Analysis.experiment_id)
