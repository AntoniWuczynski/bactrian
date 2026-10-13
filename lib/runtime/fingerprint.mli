(** What an experiment was started from, recorded in its manifest so that a
    resume refuses to mix two configurations under one experiment ID (spec §9).

    Everything is a digest of the content, never of the path alone: the
    experiment document (without the agent, [caps_usd] and [concurrency]), the
    agent section, each scenario file, and the price file. Raising a cap or
    changing the concurrency between a run and its resume is allowed on purpose,
    because the usual reason to resume is to continue after a cap was reached.
*)

type t = {
  experiment : string;
  agent : string;
  pricing : string option;  (** [None] without a price file. *)
  scenarios : (string * string) list;  (** Path as planned, digest. *)
}

val compute :
  base:_ Eio.Path.t ->
  experiment:Bactrian_core.Canon.t ->
  pricing_file:string option ->
  scenario_files:string list ->
  (t, string) result
(** [experiment] is the experiment document as parsed. [base] resolves the price
    file and the scenario files, which are relative to the experiment file.
    [Error] names a file that could not be read. *)

val changes : recorded:t -> t -> string list
(** What differs from the recorded fingerprint, one phrase per difference, such
    as ["the experiment changed (anything but caps_usd and concurrency)"],
    ["the agent changed"], ["the price file changed"] or
    ["scenario scenarios/a.yaml changed"]. Empty when nothing did. *)

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
