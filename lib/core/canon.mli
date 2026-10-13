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
