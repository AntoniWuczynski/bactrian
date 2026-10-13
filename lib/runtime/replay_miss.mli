(** How a refused replay request is described to the agent, shared by the model
    and MCP routes. *)

open Bactrian_core

val describe : Matcher.divergence -> string
(** One sentence, e.g.
    ["the request is not in the recording (nearest: seq 3, similarity 0.800)"].
*)

val json : Matcher.divergence -> Canon.t
(** [{"kind": ...}] with the sequence numbers involved and, for a missing
    request, the diff paths and similarity of the nearest candidate. *)
