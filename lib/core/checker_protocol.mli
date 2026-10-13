(** The wire format between bactrian and an external checker (spec §6). The
    checker reads the trace as JSON on stdin and prints one verdict object. *)

type verdict = { pass : bool; score : float option; message : string option }

val trace_json : Trace.entry list -> string
(** The checker's stdin: a JSON array of entries, each as {!Trace.to_line}. *)

val decode_verdict : string -> (verdict, string) result
(** Strict: one JSON object (surrounding whitespace allowed) with a boolean
    ["pass"], and optionally a number ["score"] and a string ["message"] (each
    may be [null]). Unknown fields, other types and trailing text are errors.
    The error is a human-readable reason. *)
