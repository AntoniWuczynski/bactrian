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
    with their position. A file that cannot be read is reported as
    [no such file] (or [permission denied]) at 1:1. A tab or another character
    that cannot start a token is reported where it is. Only the first document
    of a stream is read.

    The loaders below ({!load_scenario}, {!load_pricing}, {!load_experiment})
    carry on after a duplicate key (the first value is kept) or an alias (read
    as null), so those mistakes are reported together with the decoding errors,
    all sorted by position. *)

val sort_errors : located_error list -> located_error list
(** Errors grouped by file, files in order of first appearance, each file's
    errors by line and column. *)

val load_scenario :
  _ Eio.Path.t -> (Bactrian_core.Scenario.t, located_error list) result
(** Also an error at path [fixtures] when the directory it names (relative to
    the scenario file) does not exist. *)

val load_pricing :
  _ Eio.Path.t -> (Bactrian_core.Pricing.table, located_error list) result
(** A price file ({!Bactrian_core.Price_file}) applied over the built-in table.
*)

val load_experiment :
  _ Eio.Path.t -> (Bactrian_core.Experiment.t, located_error list) result

val expand_globs :
  base:_ Eio.Path.t -> string list -> (string list, located_error list) result
(** Files matching each pattern, as paths relative to [base], sorted and without
    duplicates. A glob matching nothing is an error whose [path] is the pattern.
    Supports [*] and [?] within a path segment and [**] across directories (zero
    or more). A wildcard does not match a name starting with a dot unless the
    pattern segment starts with one, and [**] does not enter directories that
    start with a dot, [node_modules] or [_build]. *)
