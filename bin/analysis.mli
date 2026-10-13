(** What [compare] and [report] share: loading an experiment's results,
    comparing them and the arguments that pick the variants. *)

open Cmdliner
open Bactrian_core

val analyse :
  json:bool ->
  baseline:string option ->
  candidate:string option ->
  fail_on_inconclusive:bool ->
  experiment_id:string ->
  Eio_unix.Stdenv.base ->
  (Report_data.t, int) result
(** The report data, or the exit status of a failure already printed. *)

val experiment_id : string Term.t
val baseline_flag : string option Term.t
val candidate_flag : string option Term.t

val comparison_man : Manpage.block list
(** The description shared by the [compare] and [report] man pages. *)
