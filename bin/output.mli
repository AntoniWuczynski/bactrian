(** Exit codes and the rendering of a finished run. *)

val exit_ok : int
val exit_failed : int
val exit_not_run : int
val exit_usage : int
val exit_environment : int
val exit_internal : int

val exit_code : Bactrian_runtime.Run.outcome -> int
(** Passed 0, Failed and Diverged 1, Not_run 2, Errored 70. *)

val summary_exit_code : Bactrian_runtime.Executor.summary -> int
(** 70 if any run errored, else 1 if any failed or diverged, else 2 if any run
    was not run, is pending or was interrupted, else 0. *)

val colour : bool Lazy.t
(** Whether to colour output: stdout is a terminal and [NO_COLOR] is unset. *)

val render_summary_text :
  colour:bool -> Bactrian_runtime.Executor.summary -> string
(** One line per planned run, then a count per outcome. *)

val render_summary_json : Bactrian_runtime.Executor.summary -> string
(** One JSON object: experiment ID and every run with its status, outcome,
    reason and run ID. *)

val render_golden_text : colour:bool -> Bactrian_runtime.Golden.report -> string
(** [ok FILE], or [FAIL FILE], then one line per error of the golden set and per
    offending golden trace, then one [warning:] line per warning (also under
    [ok]). *)

val render_text : Bactrian_runtime.Run.result -> string

val render_json : Bactrian_runtime.Run.result -> string
(** One JSON object: run ID, directory, outcome with its reason, divergences
    with their kinds and nearest-exchange diff paths, findings and spend. *)
