(** The arguments and man-page text that [compare] and [report] share. *)

open Cmdliner

val experiment_id : string Term.t
val baseline_flag : string option Term.t
val candidate_flag : string option Term.t

val comparison_man : Manpage.block list
(** The description shared by the [compare] and [report] man pages. *)
