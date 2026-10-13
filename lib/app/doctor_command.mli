(** [bactrian doctor]: check that this host is ready to record and run. *)

type request = {
  experiment : string option;
      (** An experiment file whose needs to check: the providers its spend caps
          call, its agent, its sandbox mode and its clock setting. Without it,
          the host and every provider key. *)
}

val run :
  ?files:Eio.Fs.dir_ty Eio.Path.t ->
  bactrian_exe:string ->
  env:Eio_unix.Stdenv.base ->
  request ->
  (Bactrian_runtime.Doctor.report, Command_error.t) result
(** Runs the checks; no agent is started and no URL is contacted. The experiment
    file is read through [files] (default the whole filesystem). An invalid
    experiment file is [Configuration], and an invalid
    [BACTRIAN_UPSTREAM_<PROVIDER>] variable ({!Upstream_env}) is [Usage]. Seeds
    the random generator. *)

val exit_code : Bactrian_runtime.Doctor.report -> int
(** {!Exit_code.ok}, or {!Exit_code.environment} when a check failed. *)

val to_json : Bactrian_runtime.Doctor.report -> Yojson.Safe.t
(** {!Bactrian_runtime.Doctor.to_json}. *)
