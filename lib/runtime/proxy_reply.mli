(** Responses written by the model proxy and the egress proxy. Bodies are
    written chunk by chunk by hand, so that each upstream piece reaches the
    agent as its own flushed HTTP chunk, and a dropped upstream can drop the
    agent's connection before the terminal chunk. *)

open Bactrian_core

type item = Chunk of string | Finished | Dropped

exception Upstream_dropped
(** Raised by a response whose source said [Dropped], to drop the connection. *)

exception Attributed of exn
(** A failure of the agent's connection, already noted in its run's errors. *)

type response = Http.Response.t * (Eio.Buf_read.t -> Eio.Buf_write.t -> unit)
(** What a cohttp-eio expert callback returns. *)

val respond :
  ?on_exn:(exn -> unit) ->
  status:int ->
  headers:(string * string) list ->
  (unit -> item) ->
  response
(** A chunked response whose body is the items [next] gives until [Finished] or
    [Dropped]. [on_exn] notes a failure to write to the agent, which is then
    raised as [Attributed]. *)

val of_list : string list -> unit -> item
(** The pieces, then [Finished]. *)

val error_response :
  ?extra:(string * Canon.t) list ->
  ?headers:(string * string) list ->
  status:int ->
  type_:string ->
  string ->
  response
(** A JSON body [{"error": {"type", "message", ...extra}}], with [headers] after
    its [content-type]. *)
