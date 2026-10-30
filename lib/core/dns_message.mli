(** A Domain Name System (DNS) query as the Linux sandbox's forwarder reads it
    (design §5.3): the forwarder answers every query with [REFUSED] and records
    the name asked about as a violation. Pure, and total on any bytes, because
    the agent writes them. The message is the one carried by a User Datagram
    Protocol (UDP) datagram, or one TCP message without its two-byte length
    prefix. *)

type question = {
  id : int;  (** The query's ID, from 0 to 65535. *)
  name : string;
      (** The name asked about, its labels joined with ['.'] (["."] for the
          root): letters, digits, ['-'] and ['_'] as they are, every other byte
          of a label as [\DDD], its decimal value (RFC 1035, section 5.1), so a
          name of the agent's choosing can neither forge a dot nor carry a
          control character. *)
  qtype : int;  (** The type asked for, such as 1 for [A]. *)
}

type error =
  | Truncated  (** The header or the question runs past the end. *)
  | Not_a_query  (** The response bit is set. *)
  | Question_count of int  (** Not exactly one question. *)
  | Compressed_name  (** The question's name uses a pointer. *)
  | Bad_label of int
      (** A label length byte whose two top bits are [01] or [10]. *)
  | Name_too_long  (** Over 255 bytes, as RFC 1035 limits a name. *)

val parse : string -> (question, error) result
(** The question of a query: the 12-byte header, then exactly one question. What
    follows the question is ignored. *)

val describe_error : error -> string

val type_name : int -> string
(** ["A"], ["AAAA"], ["HTTPS"] and the other common types by name, any other as
    [TYPE<n>] (RFC 3597). *)

val refused : string -> string option
(** The [REFUSED] response to a query: its ID, opcode and recursion-desired bit,
    the response bit set and response code 5, the question copied when {!parse}
    reads it (otherwise none), and no records. [None] for a message shorter than
    the header or one with the response bit set, which gets no answer. *)
