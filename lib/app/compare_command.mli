(** [bactrian compare]: compare two variants of an experiment with confidence
    intervals. *)

type request = {
  experiment_id : string;  (** An experiment under [./.bactrian/experiments]. *)
  baseline : string option;
  candidate : string option;
  fail_on_inconclusive : bool;
      (** An inconclusive gate metric makes the gate's status 2. *)
}

val run :
  env:Eio_unix.Stdenv.base ->
  request ->
  (Bactrian_core.Report_data.t, Command_error.t) result
(** {!Comparison.analyse}. *)

val to_json : Bactrian_core.Report_data.t -> Yojson.Safe.t
(** {!Bactrian_core.Report_data.summary_json}. *)
