(** Canonical JSON. Object keys are sorted and unique, and every number has one
    textual form, so two documents with the same meaning serialise to the same
    bytes and hash to the same key. *)

type t = private
  | Null
  | Bool of bool
  | Int of int
  | Big_int of string
      (** An integer literal outside the native [int] range, digits as written.
      *)
  | Float of float
      (** Finite and not integral. Integral floats that fit an [int] become
          [Int]. *)
  | String of string
  | List of t list
  | Object of (string * t) list
      (** Keys strictly increasing under [String.compare]. *)

type error =
  | Syntax of string  (** The text is not JSON. Carries the parser's message. *)
  | Duplicate_key of string
  | Non_finite_float  (** NaN or an infinity, which JSON does not allow. *)

val null : t
val bool : bool -> t
val int : int -> t
val string : string -> t
val list : t list -> t

val float : float -> (t, error) result
(** [Error Non_finite_float] for NaN and infinities. *)

val obj : (string * t) list -> (t, error) result
(** Sorts the fields. [Error (Duplicate_key k)] if [k] appears twice. *)

val of_string : string -> (t, error) result
val of_yojson : Yojson.Safe.t -> (t, error) result
val to_yojson : t -> Yojson.Safe.t

val to_string : t -> string
(** Compact form: no whitespace, keys in order, floats printed with [%.17g],
    strings escaped as Yojson does. [of_string (to_string v)] equals [v]. *)

val equal : t -> t -> bool
val compare : t -> t -> int

val digest : t -> string
(** BLAKE2b-256 of [to_string], as 64 lowercase hex characters. *)

val pp_error : Format.formatter -> error -> unit

(** {1 Reading}

    Total accessors: each returns [None] when the value has another shape, so
    callers never match on [t] with a wildcard. *)

val to_string_opt : t -> string option
val to_int_opt : t -> int option
val to_bool_opt : t -> bool option
val to_list_opt : t -> t list option
val to_fields_opt : t -> (string * t) list option

val member : string -> t -> t option
(** [member k v] is the value of field [k] when [v] is an object that has it. *)

(** {1 Editing}

    Every edit returns a value that still satisfies the invariant. *)

val set_member : string -> t -> t -> t
(** [set_member k x v] sets field [k] of object [v] to [x], adding it in key
    order if absent. Returns [v] unchanged when [v] is not an object. *)

val remove_member : string -> t -> t
(** Returns [v] unchanged when [v] is not an object or lacks the field. *)

val map_fields : (string -> t -> t) -> t -> t
(** Rewrite every field value of an object, keys unchanged. Identity on other
    shapes. *)

val map_items : (t -> t) -> t -> t
(** Rewrite every element of a list. Identity on other shapes. *)

(** {1 Paths}

    A path names fields to remove from a document. Written as dotted keys, with
    [[]] after a key meaning "every element of that list", e.g.
    ["stream_options.include_obfuscation"] or
    ["messages[].content[].signature"]. *)

type segment = Key of string | Each

val path_of_string : string -> segment list
(** Splits on ['.'] and turns a [k[]] component into [Key k; Each]. *)

val remove_at : segment list -> t -> t
(** Remove the field the path names, wherever it exists. An object that the
    removal leaves empty is removed from its parent as well, so a request that
    only differed by an ignored nested field becomes equal to one that never had
    its parent. Paths that do not match are a no-op. *)
