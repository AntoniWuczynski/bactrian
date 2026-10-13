(** Replacing secrets with numbered placeholders such as [<secret:3>]. Within
    one [state], the same secret always gets the same placeholder and different
    secrets never share one. Text that is already a placeholder is never
    rewritten, so scrubbing twice equals scrubbing once. *)

type pattern

type error =
  | Invalid_pattern of { source : string; reason : string }
      (** Not valid Perl-style syntax, or it matches the empty string. *)

val pattern : string -> (pattern, error) result
(** Compile a Perl-style regular expression (the [re] library's [Re.Perl]). *)

val provider_key_patterns : pattern list
(** Key formats of Anthropic ([sk-ant-]), OpenAI ([sk-proj-], [sk-svcacct-] and
    bare [sk-]), OpenRouter ([sk-or-]) and Google ([AIza] followed by at least
    35 characters). The distinctive prefixes match anywhere, even inside a word.
    Bare [sk-] needs at least 32 key characters and a left boundary: start of
    text, a character outside [[A-Za-z0-9_-]], a JSON or C escape (backslash
    then [n], [t], [r], [/] or a double quote) or a URL-encoded byte ([%3D]). *)

type state

val empty : state

val scrub :
  patterns:pattern list ->
  known:string list ->
  state ->
  string ->
  state * string
(** Replace every occurrence of a [known] literal (longest first, empty strings
    ignored), then every match of each pattern in order, outside existing
    placeholders. The passes repeat until the text stops changing, so scrubbing
    twice equals scrubbing once. Placeholders are numbered from 1 in order of
    first replacement within [state] (literals, then each pattern in order), not
    by position in the text. *)

val scrub_json_strings :
  patterns:pattern list -> state -> string -> state * string
(** Like {!scrub} without literals, but only inside the string literals of the
    JSON [text], each decoded and scrubbed on its own and written back only if
    it changed. A pattern such as [Bearer .+] therefore stops at the end of its
    string instead of swallowing the closing quote and brace, and the result is
    valid JSON whenever the input was. Everything outside string literals, and
    every literal that no pattern touched, keeps its bytes. *)

val scrub_structured :
  patterns:pattern list -> state -> string -> state * string
(** For user patterns on a request or response body, or one piece of a stream.
    JSON goes through {!scrub_json_strings}. Otherwise each line is scrubbed on
    its own, except that the payload of a [data:] line holding JSON (a
    server-sent event) is treated as JSON. Anything else is plain text. *)

val fold_plain : ('a -> string -> 'a * string) -> 'a -> string -> 'a * string
(** Applies the function to each span of the text outside existing placeholders,
    in order, and puts the results back between the untouched placeholders. *)

val placeholder : int -> string
(** [placeholder 3] is ["<secret:3>"]. *)

val count : state -> int
(** Distinct secrets seen so far. *)

val pp_error : Format.formatter -> error -> unit
