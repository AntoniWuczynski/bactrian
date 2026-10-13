(** Server-Sent Events, parsed incrementally under the WHATWG HTML event-stream
    rules, with two deliberate differences: comment lines are returned as items
    rather than dropped, and [id] is the value seen in that event's own block
    rather than the persistent last-event ID. Parsing gives the same items
    however the input bytes are split. *)

type event = {
  event : string option;  (** The [event:] field, if present. *)
  data : string;  (** [data:] lines joined with ['\n']. *)
  id : string option;
  retry : int option;  (** Only when the value is all ASCII digits. *)
}

type item = Event of event | Comment of string
type state

val init : state
(** Nothing received yet. A UTF-8 byte order mark at the very start is skipped.
*)

val feed : state -> string -> state * item list
(** Consume the next chunk. Returns every item completed by it, in order. A
    block with no [data:] line completes no item, as the standard requires. *)

val finish : state -> [ `Clean | `Truncated of int ]
(** End of stream. [`Truncated n] when [n] bytes of an incomplete line or event
    were pending and are discarded, as the standard requires. *)

val serialise : item -> string
(** Wire form: [event:], one [data:] line per ['\n']-separated piece, [id:],
    [retry:], then a blank line. A comment is [": text\n"].
    @raise Invalid_argument
      if [event] or [id] contains ['\r'] or ['\n'], [id] contains a NUL, [retry]
      is negative, [data] or a comment contains ['\r'], or a comment contains
      ['\n']. These values would not parse back unchanged. *)

val equal_item : item -> item -> bool
