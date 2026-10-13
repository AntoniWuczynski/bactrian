(** Text from an agent, made safe to print on a terminal. *)

val escape : string -> string
(** [escape s] is [s] with every C0 control character and DEL written as [\xNN],
    and every C1 control character (U+0080 to U+009F, encoded in UTF-8) as
    [\uNNNN], so a denied path or a protocol line the agent chose cannot move
    the cursor, recolour the output or send a terminal command. Everything else
    is unchanged. *)
