(** Helpers shared by the protocol adapter tests. Failures raise through
    Alcotest, so tests need no error plumbing. *)

open Bactrian_core

val fixture : protocol:string -> string -> string -> string
(** [fixture ~protocol name file] reads
    [test/fixtures/protocols/<protocol>/<name>/<file>]. *)

val json : string -> Canon.t

val get : string -> Canon.t -> Canon.t
(** Field of an object. Fails when absent. *)

val string_exn : Canon.t -> string
val int_exn : Canon.t -> int
val list_exn : Canon.t -> Canon.t list

val str : string -> Canon.t -> string
(** [str k v] is [string_exn (get k v)]. *)

val ok_exn : ('a, Protocol.error) result -> 'a

val sse_items : string list -> Sse.item list
(** Feed the pieces to the SSE parser in order. Fails if the stream ends
    mid-event. *)

val recorded_pieces : raw:string -> chunks:string -> string list
(** Cut [raw] where [chunks] (the text of chunks.json) says the capture
    delivered it. *)

val single_bytes : string -> string list
val canon_t : Canon.t Alcotest.testable
val summary_t : Protocol.summary Alcotest.testable
val usage_equal : Trace.usage -> Trace.usage -> bool
val obj : (string * Canon.t) list -> Canon.t

val set_path : string -> Canon.t -> Canon.t -> Canon.t
(** [set_path path value v] sets the field named by the dotted [path] (with [[]]
    for every list element, as in [Canon.path_of_string]), creating missing
    objects on the way. *)

val contains : sub:string -> string -> bool
val replace_all : sub:string -> by:string -> string -> string

val first_half_body : Protocol.body -> Protocol.body
(** The first half of a stream's items. A whole [Json] body is unchanged. *)

val check_usage_relations : string -> Protocol.summary -> unit
(** [cached + cache_write <= input] and [reasoning <= output]. *)

val check_truncation :
  string ->
  (Protocol.body -> (Protocol.summary, Protocol.error) result) ->
  Protocol.body ->
  unit
(** The full stream summarises without an error, and its first half either fails
    to summarise (a cut-off tool call) or carries [summary.error]. *)
