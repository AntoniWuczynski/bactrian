(** [bactrian report]: write a self-contained HTML report of an experiment. *)

type request = {
  experiment_id : string;  (** An experiment under [./.bactrian/experiments]. *)
  baseline : string option;
  candidate : string option;
  output : string;  (** Where to write the HTML. *)
}

type t = {
  output : string;  (** The file written. *)
  data : Bactrian_core.Report_data.t;  (** What the report shows. *)
}

val run :
  ?compute:Compute.t ->
  env:Eio_unix.Stdenv.base ->
  request ->
  (t, Command_error.t) result
(** Compares the variants as {!Comparison.analyse} does, and renders the HTML,
    through [compute] (an inconclusive gate metric is not a failure here) and
    writes the report atomically. A file that cannot be written is
    [Environment]. *)

val to_json : t -> Yojson.Safe.t
(** [{"output": FILE, "summary": …}], the summary being what
    {!Compare_command.to_json} gives for the same comparison. *)
