type located_error = {
  file : string;
  line : int;
  column : int;
  path : string;
  message : string;
}
(** [line] and [column] start at 1. [path] is the document path, e.g.
    ["assertions[2].tool_args.tool"], empty for the document root. *)

val load :
  _ Eio.Path.t ->
  ( Bactrian_core.Canon.t * (string -> (int * int) option),
    located_error list )
  result
(** YAML (or JSON, which YAML accepts) to [Canon.t], plus a lookup from document
    path to line and column. The lookup gives the position of the key (or list
    item) at that path, and falls back to the nearest ancestor path that has
    one. Syntax errors, aliases, non-scalar keys and duplicate keys are reported
    with their position. Only the first document of a stream is read. *)

val load_scenario :
  _ Eio.Path.t -> (Bactrian_core.Scenario.t, located_error list) result

val load_experiment :
  _ Eio.Path.t -> (Bactrian_core.Experiment.t, located_error list) result

val expand_globs :
  base:_ Eio.Path.t -> string list -> (string list, located_error list) result
(** Files matching each pattern, as paths relative to [base], sorted and without
    duplicates. A glob matching nothing is an error whose [path] is the pattern.
    Supports [*] and [?] within a path segment and [**] across directories (zero
    or more). *)
