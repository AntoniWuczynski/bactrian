(** Exit codes and the rendering of a finished run. *)

val exit_ok : int
val exit_failed : int
val exit_not_run : int
val exit_usage : int
val exit_environment : int
val exit_internal : int

val exit_code : Bactrian_runtime.Run.outcome -> int
(** Passed 0, Failed and Diverged 1, Not_run 2, Errored 70. *)

val render_text : Bactrian_runtime.Run.result -> string

val render_json : Bactrian_runtime.Run.result -> string
(** One JSON object: run ID, directory, outcome with its reason, divergences
    with their kinds and nearest-exchange diff paths, findings and spend. *)
