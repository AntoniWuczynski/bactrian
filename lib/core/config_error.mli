type error = { path : string; message : string }
(** A mistake in a configuration document. [path] is the document path, e.g.
    ["assertions[2].tool_args.tool"], empty for the document itself. *)

val pp : Format.formatter -> error -> unit
(** [path: message], or just [message] when the path is empty. *)
