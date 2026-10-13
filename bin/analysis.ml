open Cmdliner
open Bactrian_core
open Bactrian_runtime

let ( let* ) = Result.bind

let results_error ~json (e : Results.error) =
  match e with
  | Bad_experiment errors -> Cli.fail_located ~json errors
  | No_such_experiment _ ->
      Cli.fail ~json Output.exit_usage (Format.asprintf "%a" Results.pp_error e)
  | Bad_manifest _ | Bad_run _ ->
      Cli.fail ~json Output.exit_internal
        (Format.asprintf "%a" Results.pp_error e)

let analyse ~json ~baseline ~candidate ~fail_on_inconclusive ~experiment_id env
    =
  let* loaded =
    Results.load ~fs:(Eio.Stdenv.fs env)
      ~root:Eio.Path.(Eio.Stdenv.cwd env / ".bactrian")
      ~experiment_id
    |> Result.map_error (results_error ~json)
  in
  let* settings =
    Results.settings loaded ~baseline ~candidate ~fail_on_inconclusive
    |> Result.map_error (Cli.fail ~json Output.exit_usage)
  in
  let* result =
    Compare.run settings |> Result.map_error (Cli.fail ~json Output.exit_usage)
  in
  Ok
    (Report_data.build ~experiment_id ~experiment:loaded.manifest.name ~settings
       ~result loaded.runs)

let experiment_id =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"EXPERIMENT-ID"
        ~doc:"An experiment under ./.bactrian/experiments.")

let baseline_flag =
  Arg.(
    value
    & opt (some string) None
    & info [ "baseline" ] ~docv:"VARIANT"
        ~doc:
          "The baseline variant. Defaults to $(b,compare.baseline) in the \
           experiment file the manifest records.")

let candidate_flag =
  Arg.(
    value
    & opt (some string) None
    & info [ "candidate" ] ~docv:"VARIANT"
        ~doc:
          "The candidate variant. Defaults to the other variant when the \
           experiment has exactly two.")

let comparison_man =
  [
    `S Manpage.s_description;
    `P
      "Compares the candidate with the baseline over the runs of \
       $(i,EXPERIMENT-ID). The estimand, confidence, gate and thresholds come \
       from the $(b,compare) section of the experiment file that the manifest \
       records. Every gate metric needs a threshold there, under \
       $(b,compare.thresholds): none is ever assumed, and a missing one is a \
       usage error naming the metric. The file must therefore still exist and \
       have a $(b,compare) section. $(b,--baseline) and $(b,--candidate) \
       override its baseline and pick the candidate.";
  ]
