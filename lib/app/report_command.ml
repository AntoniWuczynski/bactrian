open Bactrian_runtime

let ( let* ) = Result.bind

type request = {
  experiment_id : string;
  baseline : string option;
  candidate : string option;
  output : string;
}

type t = { output : string }

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
  | () -> Ok { output = r.output }
  | exception Eio.Io _ ->
      Error
        (Command_error.Environment (Printf.sprintf "cannot write %s" r.output))
