(** Structural differences between two canonical JSON documents. *)

type kind = Changed | Added | Removed

type change = {
  path : string;  (** e.g. ["messages[3].content"], or [""] for the root. *)
  kind : kind;
}

val changes : Canon.t -> Canon.t -> change list
(** From the first document to the second, in document order. Lists are compared
    position by position. Empty when the documents are equal. *)

val similarity : Canon.t -> Canon.t -> float
(** 1.0 for equal documents, 0.0 for nothing in common. Defined as the share of
    leaf values (strings, numbers, booleans, nulls, empty containers) of the
    larger document that are equal at the same path in the other. *)
