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

val placeholder : int -> string
(** [placeholder 3] is ["<secret:3>"]. *)

val count : state -> int
(** Distinct secrets seen so far. *)

val pp_error : Format.formatter -> error -> unit
