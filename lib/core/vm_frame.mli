(** The frames bactrian and the relay of a virtual machine sandbox exchange over
    the container's standard input and output (design §5.3, [vm/relay.c]). Pure.

    A frame is [[channel u32 BE][kind u8][length u32 BE][payload]]. Channel 0
    carries the agent's standard input (to the relay) and standard output (from
    it). Each connection the agent opens to a forwarded port gets a new odd
    channel. *)

type kind =
  | Data
  | Eof  (** No more data in this direction. Empty payload. *)
  | Open  (** Relay to bactrian: a new connection. Payload: the port, u16 BE. *)
  | Close  (** The connection is gone, in both directions. Empty payload. *)
  | Exit
      (** Relay to bactrian, last: the agent's status, u32 BE (128 plus the
          signal number when a signal ended it). *)
  | Log  (** Relay to bactrian: one JSON line of a relay failure. *)

type t = private { channel : int; kind : kind; payload : string }

val header_size : int
(** 9. *)

val max_payload : int
(** 65536. A longer frame is refused. *)

val data : channel:int -> string -> t list
(** The data, split into frames of at most {!max_payload} bytes, none for an
    empty string. *)

val eof : channel:int -> t
val close : channel:int -> t

val open_ : channel:int -> port:int -> t
(** [port] from 1 to 65535. *)

val exit : int -> t
(** An exit status from 0 to 255. *)

val log : string -> t
(** On channel 0, cut to {!max_payload} bytes. *)

val port : t -> int option
(** The port of an {!Open} frame whose payload is two bytes. *)

val status : t -> int option
(** The status of an {!Exit} frame whose payload is four bytes. *)

val encode : t -> string

type error =
  | Unknown_kind of int
  | Too_long of int  (** The length a header declared. *)
  | Truncated of int  (** Bytes of an unfinished frame at end of input. *)

val describe_error : error -> string

type decoder
(** Holds the bytes of an unfinished frame between calls to {!feed}. *)

val decoder : unit -> decoder

val feed : decoder -> string -> (t list, error) result
(** The frames the bytes complete, in order, however the input is split. After
    an error the decoder is spent and keeps giving that error. *)

val finish : decoder -> (unit, error) result
(** At end of input: [Truncated] when part of a frame is left over. *)
