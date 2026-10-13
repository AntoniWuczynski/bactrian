(** Long commands ([run], [record], [replay]) running in the background of the
    server, each behind a job ID that tool calls name (decision record 0011).
    Jobs live in the server's switch. A finished job stays queryable for an
    hour, and only the newest hundred finished jobs are kept. *)

open Bactrian_core

type state =
  | Running
  | Succeeded  (** The command produced its result, whatever its outcome. *)
  | Failed  (** The command stopped with an error. *)
  | Cancelled  (** Stopped as SIGINT stops it, so a [run] can be resumed. *)

val state_name : state -> string
(** ["running"], ["succeeded"], ["failed"] or ["cancelled"]. *)

type progress = {
  done_ : int;  (** Runs finished. *)
  total : int option;  (** Runs planned, once known. *)
}

type outcome = (Yojson.Safe.t * int, Bactrian_app.Command_error.t) result
(** What a command gives: its [--json] output with its exit status, or its
    error. *)

type t
type job

val create :
  ?now:(unit -> float) -> ?log:(string -> unit) -> sw:Eio.Switch.t -> unit -> t
(** Job IDs are random, so the generator must be seeded. [now] (the clock, in
    seconds) decides when finished jobs expire. [log] gets the exception and
    backtrace of a job that raised. *)

val start :
  t ->
  kind:string ->
  progress:(finished:bool -> progress) ->
  (Bactrian_runtime.Interrupt.trigger -> outcome) ->
  string
(** Forks the command into the switch and returns the new job's ID at once. The
    command must stop, as on SIGINT, when the trigger fires. An exception from
    it, other than cancellation, fails the job with an [Internal] error.
    [progress] is asked on every {!status}. *)

val find : t -> string -> job option
val state : job -> state

val running : t -> int
(** The number of jobs still running. *)

val status : job -> Canon.t
(** [{"job_id", "kind", "state", "progress": {"done", "total"}}], with the
    ["exit"] status of a finished job and ["result"] (the command's [--json]
    output) once it has succeeded or ["error"] ({!Bactrian_app.Command_error} as
    [--json]) once it has failed or been cancelled. [total] is [null] until
    known. *)

val cancel : job -> unit
(** Stops a running job and returns once its command has finished unwinding: its
    agents are gone and what it wrote is consistent. Does nothing to a finished
    job. *)

val cancel_all : t -> unit
(** {!cancel} on every job, concurrently. *)
