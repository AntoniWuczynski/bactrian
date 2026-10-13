open Bactrian_runtime

let ( let* ) = Result.bind

type request = {
  experiment_id : string;
  baseline : string option;
  candidate : string option;
  output : string;
}

type t = { output : string; data : Bactrian_core.Report_data.t }

let run ?(compute = Compute.inline) ?files ~env (r : request) =
  let files = Option.value files ~default:(Eio.Stdenv.fs env) in
  let* data =
    Comparison.analyse ~compute ~files ~env ~experiment_id:r.experiment_id
      ~baseline:r.baseline ~candidate:r.candidate ~fail_on_inconclusive:false
  in
  let html =
    compute.run (fun ~check:_ -> Bactrian_core.Report_html.render data)
  in
  match Store.write_atomic Eio.Path.(files / r.output) html with
  | () -> Ok { output = r.output; data }
  | exception Eio.Io _ ->
      Error
        (Command_error.Environment (Printf.sprintf "cannot write %s" r.output))

let to_json t =
  `Assoc
    [
      ("output", `String t.output);
      ("summary", Bactrian_core.Report_data.summary_json t.data);
    ]
