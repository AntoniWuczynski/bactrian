(** RFC 3339 timestamps, as scenarios give them for [SIM_NOW]. *)

val epoch : string -> int option
(** [epoch s] is the Unix time in whole seconds of [s], [YYYY-MM-DDThh:mm:ss]
    with an optional fraction (dropped) and a [Z] or [+hh:mm] / [-hh:mm] offset.
    [None] when [s] is anything else, a date that does not exist, or a second of
    60. *)
