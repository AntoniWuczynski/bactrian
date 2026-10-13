open Bactrian_core
open Bactrian_runtime

let ( let* ) = Result.bind

let results_error (e : Results.error) : Command_error.t =
  match e with
  | Bad_experiment errors -> Configuration errors
  | No_such_experiment _ -> Usage (Format.asprintf "%a" Results.pp_error e)
  | Bad_manifest _ | Bad_run _ ->
      Internal (Format.asprintf "%a" Results.pp_error e)

let analyse ~(compute : Compute.t) ~files ~env ~experiment_id ~baseline
    ~candidate ~fail_on_inconclusive =
  let* loaded =
    Results.load ~fs:files
      ~root:Eio.Path.(Eio.Stdenv.cwd env / ".bactrian")
      ~experiment_id
    |> Result.map_error results_error
  in
  let* settings =
    Results.settings loaded ~baseline ~candidate ~fail_on_inconclusive
    |> Result.map_error (fun m -> Command_error.Usage m)
  in
  compute.run @@ fun ~check ->
  let* result =
    Compare.run settings |> Result.map_error (fun m -> Command_error.Usage m)
  in
  Ok
    (Report_data.build ~check ~experiment_id ~experiment:loaded.manifest.name
       ~settings ~result loaded.runs)
