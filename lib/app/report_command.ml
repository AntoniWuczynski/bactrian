open Bactrian_runtime

let ( let* ) = Result.bind

type request = {
  experiment_id : string;
  baseline : string option;
  candidate : string option;
  output : string;
}

type t = { output : string; data : Bactrian_core.Report_data.t }

let run ~env (r : request) =
  let* data =
    Comparison.analyse ~env ~experiment_id:r.experiment_id ~baseline:r.baseline
      ~candidate:r.candidate ~fail_on_inconclusive:false
  in
  match
    Store.write_atomic
      Eio.Path.(Eio.Stdenv.fs env / r.output)
      (Bactrian_core.Report_html.render data)
  with
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
