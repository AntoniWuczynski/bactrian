(** The one compiler for user-supplied regular expressions. *)

val compile : ?allow_empty_match:bool -> string -> (Re.re, string) result
(** Perl syntax ([Re.Perl]), plus a leading [(?i)] meaning case-insensitive. Any
    other inline flag is rejected. Unless [allow_empty_match] (default [false])
    is set, a pattern that matches the empty string is rejected. The error is a
    short reason, without the pattern. *)
