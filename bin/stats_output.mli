(** Terminal output of [compare] and [plan]. *)

open Bactrian_core

val render_compare_text : colour:bool -> Report_data.t -> string
(** The header, the verdict table, the per-scenario table, missing data and the
    gate's exit status. *)

val render_plan_text : Bactrian_app.Plan_command.t -> string
(** The target verdict and the smallest repeat count that reaches the power, or
    that none does. *)
