(** The exit statuses of the commands and how outcomes map onto them. *)

val ok : int
(** 0. *)

val failed : int
(** 1: a run failed or diverged. *)

val not_run : int
(** 2: a run did not happen because a spend cap was reached. *)

val usage : int
(** 64: a usage or configuration error. *)

val environment : int
(** 69: the environment is not ready. *)

val internal : int
(** 70: an internal error. *)

val of_outcome : Bactrian_runtime.Run.outcome -> int
(** Passed 0, Failed and Diverged 1, Not_run 2, Errored 70. *)

val of_summary : Bactrian_runtime.Executor.summary -> int
(** 70 if any run errored, else 1 if any failed or diverged, else 2 if any run
    was not run, is pending or was interrupted, else 0. *)
