(** Decoders that accumulate every error with its document path, for the
    scenario and experiment formats. Objects are strict: a field no decoder
    asked for is an error. *)

type 'a decoded = ('a, Config_error.error list) result
type 'a decoder = path:string -> Canon.t -> 'a decoded

val fail : path:string -> string -> 'a decoded
val ( let+ ) : 'a decoded -> ('a -> 'b) -> 'b decoded

val ( and+ ) : 'a decoded -> 'b decoded -> ('a * 'b) decoded
(** Keeps the errors of both sides, left first. *)

val ( let* ) : 'a decoded -> ('a -> 'b decoded) -> 'b decoded
(** Stops at the first failing side: for checks that need the decoded values. *)

val nest : string -> string -> string
(** [nest "a" "b.c"] is ["a.b.c"], [nest "a" "[2]"] is ["a[2]"], and an empty
    side yields the other. *)

val field_path : string -> string -> string
(** [field_path "a" "k"] is ["a.k"]. A key with characters outside [A-Za-z0-9_-]
    is written [a["k.x"]]. *)

val index_path : string -> int -> string

val collect : 'a decoded list -> 'a list decoded
(** All values, or the errors of every failing element in order. *)

(** {1 Values} *)

val string : string decoder
val non_empty_string : string decoder
val int : int decoder
val int_at_least : int -> int decoder

val float : float decoder
(** An integer or a decimal number. *)

val float_at_least : float -> float decoder
val bool : bool decoder

val duration : float decoder
(** Seconds, from a positive ["120s"], ["2m"] or ["1.5s"]. *)

val regex : string decoder
(** A pattern {!Redact.pattern} accepts. *)

val enum : what:string -> (string * 'a) list -> 'a decoder

val version : int -> int decoder
(** The [version] field: the given number, nothing else. *)

val list : 'a decoder -> 'a list decoder
val non_empty_list : 'a decoder -> 'a list decoder

val assoc : 'a decoder -> (string * 'a) list decoder
(** An object with arbitrary keys, in key order. *)

(** {1 Objects} *)

type reader

val object_ : path:string -> Canon.t -> (reader -> 'a decoded) -> 'a decoded
(** Runs the function on an object, then reports every field it did not read as
    unknown. *)

val path : reader -> string
val required : reader -> string -> 'a decoder -> 'a decoded
val optional : reader -> string -> 'a decoder -> 'a option decoded
val default : reader -> string -> 'a -> 'a decoder -> 'a decoded

val ignore_rest : reader -> unit
(** Suppresses the unknown-field report, after an earlier error made the
    remaining fields meaningless. *)
