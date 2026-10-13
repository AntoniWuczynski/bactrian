(** A documented subset of JSON Schema: [type] (a name or a list of names from
    object, array, string, number, integer, boolean, null), [properties],
    [required], [enum], [items] (one schema), [additionalProperties] (a
    boolean), and the annotations [title] and [description]. Any other keyword
    is rejected by {!check} rather than silently ignored. *)

val check : Canon.t -> Config_error.error list
(** Problems with the schema itself. Paths are relative to the schema, e.g.
    ["properties.a.type"]. *)

val validate : schema:Canon.t -> Canon.t -> string list
(** Violations, each prefixed by the instance path
    (["$.a[0]: expected string, got number"]). Empty means valid. A schema that
    fails {!check} yields one violation per problem. *)
