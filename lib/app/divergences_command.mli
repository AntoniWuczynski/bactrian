(** The replay divergences of one run, read back from its [divergences.json]. *)

type request = {
  run : string;
      (** A run ID under [./.bactrian/runs], or that run's directory
          [.bactrian/runs/<ID>]. Any other directory is a [Usage] error. *)
}

type t = { run_id : string; divergences : Bactrian_runtime.Divergences.t list }

val run : env:Eio_unix.Stdenv.base -> request -> (t, Command_error.t) result
(** [Usage] for an invalid run ID or a run that does not exist, [Internal] for a
    [divergences.json] that cannot be read or decoded. A run without one, such
    as a recording, has no divergences. *)

val to_json : t -> Yojson.Safe.t
(** [{"run_id", "divergences": […]}], each divergence as the [--json] output of
    [replay] gives it ({!Bactrian_runtime.Divergences.to_json}). *)
