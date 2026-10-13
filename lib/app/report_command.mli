(** [bactrian report]: write a self-contained HTML report of an experiment. *)

type request = {
  experiment_id : string;  (** An experiment under [./.bactrian/experiments]. *)
  baseline : string option;
  candidate : string option;
  output : string;  (** Where to write the HTML. *)
}

type t = { output : string  (** The file written. *) }

val run : env:Eio_unix.Stdenv.base -> request -> (t, Command_error.t) result
(** Compares the variants as {!Comparison.analyse} does (an inconclusive gate
    metric is not a failure here) and writes the report atomically. A file that
    cannot be written is [Environment]. *)
