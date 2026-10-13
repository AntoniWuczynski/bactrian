(** Masking of an experiment's [normalise] patterns in a canonical request
    before it is keyed (spec §7). *)

val marker : string
(** What every match is replaced with: ["<normalised>"]. *)

val text : Re.re list -> string -> int * string
(** The patterns applied in order to the spans of the text outside [<secret:N>]
    placeholders, with the number of matches replaced. *)

val canon : Re.re list -> Canon.t -> int * Canon.t
(** Replaces every match of each pattern, in order, in every string value (not
    object keys), and counts the matches replaced. Patterns are compiled with
    {!Regex.compile}. *)
