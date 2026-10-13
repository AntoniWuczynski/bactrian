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
