(** A conformance transcript (examples/transcripts): the messages of a run, one
    per line, as they cross the wire. ["> <json>"] is a message from bactrian to
    the agent and ["< <json>"] one from the agent. Blank lines and lines
    starting with ["#"] are comments. *)

type exchange = {
  request : string;  (** A [>] line's JSON text, as bactrian writes it. *)
  replies : string list;  (** The [<] lines that follow it, in order. *)
}

val parse : string -> (exchange list, string) result
(** Fails on a line that starts with neither marker, and on a [<] line before
    the first [>] line. *)

val load : string -> (exchange list, string) result
