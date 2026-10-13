(** [bactrian serve-mcp]: the MCP server over stdio (decision record 0011). One
    JSON-RPC message per line in each direction. Replies go out through one
    writer, so lines never interleave, and the server never sends a request.
    Each [tools/call] runs in its own fibre, so calls are answered as they
    finish, and [notifications/cancelled] stops the call it names, which then
    gets no reply. *)

open Bactrian_core

type ended =
  | Input_closed  (** End of input: the client is done. *)
  | Stopped of int  (** The stop trigger fired with this signal. *)

val serve :
  server:Mcp_dispatch.server ->
  call:(string -> Canon.t option -> Canon.t) ->
  stop:Bactrian_runtime.Interrupt.trigger ->
  log:(string -> unit) ->
  input:_ Eio.Flow.source ->
  output:_ Eio.Flow.sink ->
  ended
(** Answers the messages on [input] with [call] for the tools until the end of
    input or until [stop] fires, then cancels the calls still in flight (they
    get no reply) and returns once they have unwound. [log] gets a line for
    standard error when a message is too long to read. *)

type config = {
  bactrian_exe : string;
  on_progress : Bactrian_app.Progress.t -> unit;
  log : string -> unit;
}

val run :
  env:Eio_unix.Stdenv.base ->
  root:Confine.t ->
  stop:Bactrian_runtime.Interrupt.trigger ->
  config ->
  input:_ Eio.Flow.source ->
  output:_ Eio.Flow.sink ->
  ended
(** {!serve} with the tools of {!Mcp_tools} and a job table, which ends with it:
    when {!serve} returns, every running job is cancelled as SIGINT cancels its
    command (a [run] stays resumable) and [run] returns once all of them have
    unwound, their agents gone. Seeds the random generator, and must be called
    inside [Eio_main.run], after {!Bactrian_runtime.Launch_env.scrub}. *)
