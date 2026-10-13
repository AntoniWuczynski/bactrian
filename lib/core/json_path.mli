(** The JSONPath subset used by [output] assertions: [$], [.key], [[n]] and
    [["key"]]. No wildcards, filters, slices or recursive descent. *)

type segment = Key of string | Index of int

val parse : string -> (segment list, string) result
(** [Error] carries a message naming the offending position. *)

val eval : segment list -> Canon.t -> (Canon.t, string) result
(** [Error] says which segment found nothing, e.g. ["$.a[2]: no element 2"]. *)
