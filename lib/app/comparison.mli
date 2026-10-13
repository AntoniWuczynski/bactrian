(** Loading an experiment's results and comparing two of its variants, which
    [compare] and [report] share. *)

val analyse :
  env:Eio_unix.Stdenv.base ->
  experiment_id:string ->
  baseline:string option ->
  candidate:string option ->
  fail_on_inconclusive:bool ->
  (Bactrian_core.Report_data.t, Command_error.t) result
(** Reads [./.bactrian/experiments/<experiment_id>] and compares [candidate]
    with [baseline] (see {!Bactrian_runtime.Results.settings} for the defaults).
    An invalid experiment file is [Configuration], an unknown experiment or
    unusable settings are [Usage], and unreadable results are [Internal]. *)
