(** A scripted client for [bactrian serve-mcp] over stdio, for tests. It starts
    the real binary in a project directory, speaks either protocol era and reads
    newline-delimited replies with a timeout. Every line the server writes must
    be a JSON-RPC response to the request in flight: anything else raises
    {!Protocol_violation}, so a stray print on standard output fails the test.
    The client sends one request at a time, with increasing ids. *)

open Bactrian_core

exception Protocol_violation of string
(** The server wrote something other than a response to the request in flight:
    text that is not JSON, a JSON value that is not a JSON-RPC message, a
    message that is not a response, a response to another id, or a tool result
    whose text block differs from its [structuredContent]. *)

exception Timeout of string
(** A reply, a job or the server's exit took longer than allowed. The server is
    killed by its PID before this is raised. *)

type era =
  | Modern  (** 2026-07-28: [_meta] on every request, [server/discover]. *)
  | Legacy  (** 2025-11-25: [initialize], then [notifications/initialized]. *)

type error = { code : int; message : string; data : Canon.t option }
(** A JSON-RPC error response. *)

type t

val spawn :
  ?env:(string * string) list ->
  ?timeout:float ->
  exe:string ->
  dir:string ->
  unit ->
  t
(** Starts [exe serve-mcp] with [dir] as its working directory and the test's
    environment plus [env] (which wins). [timeout] (default 60 seconds) bounds
    each reply. The connection has no era yet. *)

val connect : t -> era -> Canon.t
(** Opens the connection in that era and returns the server's answer, the result
    of [server/discover] or [initialize]. *)

val start :
  ?env:(string * string) list ->
  ?timeout:float ->
  exe:string ->
  dir:string ->
  era ->
  t
(** {!spawn} then {!connect}. *)

val pid : t -> int
(** The server's process ID. *)

val era : t -> era option
(** The era {!connect} opened, if any. *)

val raw_request : t -> string -> Canon.t -> (Canon.t, error) result
(** Sends a request with exactly these [params] and returns its result or its
    error. *)

val request : t -> string -> Canon.t -> (Canon.t, error) result
(** {!raw_request} with the [_meta] of a [Modern] connection added to [params]
    (the protocol version and empty client capabilities). *)

val notify : t -> string -> Canon.t -> unit
(** Sends a notification. *)

val tools_list : t -> Canon.t
(** The result of [tools/list]. *)

type outcome =
  | Structured of Canon.t  (** The tool's [structuredContent]. *)
  | Tool_error of Canon.t
      (** The same, from a result with [isError] true: a failure of the tool
          that the model can read. *)

val call_tool : t -> string -> Canon.t -> outcome
(** [tools/call]. A JSON-RPC error raises {!Protocol_violation} (use {!request}
    to see one). The result's text block must hold the [structuredContent]
    serialised, and in the [Modern] era [resultType] must be ["complete"]. *)

val poll_job : ?timeout:float -> ?interval:float -> t -> string -> Canon.t
(** [job_status] every [interval] seconds (default 0.1) until the job's state is
    no longer ["running"], and returns that status. [Timeout] after [timeout]
    seconds (default 120) altogether. *)

val close : ?timeout:float -> t -> Unix.process_status
(** Closes the server's standard input, reads what is left of its standard
    output (which must be nothing) and waits for it to exit, at most [timeout]
    seconds (default 30). When that runs out, the server is killed by its PID
    and {!Timeout} is raised. *)

val dispose : t -> unit
(** Kills the server by its PID if it is still running. For [Fun.protect]. *)
