(** [bactrian run]: run an experiment, its scenarios, variants and repeats. *)

open Bactrian_runtime

type request = {
  experiment : string;  (** The experiment file (YAML or JSON). *)
  resume : string option;
      (** The ID of an experiment under [./.bactrian/experiments] to continue:
          runs that finished are not repeated. *)
  sandbox : Sandbox.mode option;
      (** Overrides the experiment's [sandbox] setting. *)
}

type t = {
  summary : Executor.summary;
  sandbox : Sandbox.label option;
      (** The label every run carries; [None] when the experiment stopped before
          its sandbox preflight. *)
}

val run :
  ?stop:Interrupt.trigger ->
  ?files:Eio.Fs.dir_ty Eio.Path.t ->
  ?vet:(Executor.contents -> (unit, string) result) ->
  on_progress:(Progress.t -> unit) ->
  bactrian_exe:string ->
  env:Eio_unix.Stdenv.base ->
  request ->
  (t, Command_error.t) result
(** Runs the experiment (see {!Bactrian_runtime.Executor.run_experiment}, which
    [files] and [vet] are passed to), calling each provider at its
    {!Upstream_env} URL. An invalid [BACTRIAN_UPSTREAM_<PROVIDER>] variable is
    [Usage]. [on_progress] gets {!Progress.Experiment_started} once the ID is
    known and a {!Progress.Warning} for each setting that is accepted but not
    enforced. SIGINT and SIGTERM, or [stop] when given (then no signal handler
    is installed), stop the experiment and give [Interrupted] with the
    experiment's ID if it had started, so it can be resumed. Seeds the random
    generator, and must be called inside [Eio_main.run], after
    {!Bactrian_runtime.Launch_env.scrub}. *)

val exit_code : t -> int
(** {!Exit_code.of_summary}. *)

val to_json : t -> Yojson.Safe.t
(** The experiment ID, the sandbox label and every run with its index, scenario,
    variant, repeat, status, reason and run ID. *)

val status_kind : Executor.status -> string
(** ["pending"], ["interrupted"], or the outcome's kind
    ({!Bactrian_runtime.Run.outcome_kind}). *)

val status_reason : Executor.status -> string option
(** The outcome's reason, if the run is done and has one. *)
