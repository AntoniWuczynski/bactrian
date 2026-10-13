(** Terminal and JSON output of [compare] and [plan]. *)

open Bactrian_core

val render_compare_text : colour:bool -> Report_data.t -> string
(** The header, the verdict table, the per-scenario table, missing data and the
    gate's exit status. *)

val render_compare_json : Report_data.t -> string
(** {!Report_data.summary_json} on one line, ending in a newline. *)

val render_plan_text : Plan_sim.assumption -> Plan_sim.answer -> string

val render_plan_json : Plan_sim.assumption -> Plan_sim.answer -> string
(** One JSON object with a schema version, the assumptions and the answer. *)
