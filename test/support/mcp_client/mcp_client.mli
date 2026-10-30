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

val tasks_extension : string
(** ["io.modelcontextprotocol/tasks"]. *)

type t

val spawn :
  ?env:(string * string) list ->
  ?args:string list ->
  ?timeout:float ->
  exe:string ->
  dir:string ->
  unit ->
  t
(** Starts [exe serve-mcp] with [dir] as its working directory and the test's
    environment plus [env] (which wins), and [args] after [serve-mcp]. [timeout]
    (default 60 seconds) bounds each reply. The connection has no era yet. *)

val connect : t -> era -> Canon.t
(** Opens the connection in that era and returns the server's answer, the result
    of [server/discover] or [initialize]. *)

val start :
  ?env:(string * string) list ->
  ?args:string list ->
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

val request : ?tasks:bool -> t -> string -> Canon.t -> (Canon.t, error) result
(** {!raw_request} with the [_meta] of a [Modern] connection added to [params]
    (the protocol version and client capabilities, empty or, with [tasks],
    declaring the Tasks extension). *)

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

val call_task : t -> string -> Canon.t -> Canon.t
(** [tools/call] from a request that declares the Tasks extension, which must be
    answered with a task: [resultType] ["task"] and a [taskId]. *)

val poll_task : ?timeout:float -> ?interval:float -> t -> string -> Canon.t
(** [tasks/get] every [interval] seconds (default 0.1) until the task is
    [completed], [failed] or [cancelled], and returns it. [Timeout] after
    [timeout] seconds (default 120) altogether. *)

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

(** The same server over Streamable HTTP ([serve-mcp --http 0]), one POST per
    message. *)
module Http : sig
  type response = {
    status : int;
    headers : (string * string) list;
    body : string;  (** Without the chunked framing. *)
  }

  val header : response -> string -> string option
  (** A header of the answer, by a name in any case. *)

  val send :
    ?meth:string ->
    ?path:string ->
    ?timeout:float ->
    port:int ->
    (string * string) list ->
    string ->
    response
  (** One request to [127.0.0.1:port] (default [POST /mcp]) with these headers
      and body, [Host] added unless given, on a connection of its own. [Timeout]
      when no answer comes within [timeout] (default 60) seconds. *)

  val hang_up :
    ?timeout:float ->
    ?linger:float ->
    port:int ->
    (string * string) list ->
    string ->
    unit
  (** Sends a POST, waits for the head of the answer and [linger] seconds more
      (default 0.2), and closes the connection before its body. *)

  val events : string -> string list
  (** The [data] of each event of an SSE body. *)

  val message : response -> Mcp_msg.message
  (** The JSON-RPC message of an answer, from a JSON body or the single event of
      an SSE one. *)

  val outcome : response -> (Canon.t, error) result
  (** The result or error of {!message}. *)

  type server

  val test_token : string
  (** The bearer token {!start} gives the server by default. *)

  val start :
    ?env:(string * string) list ->
    ?args:string list ->
    ?token:string option ->
    ?timeout:float ->
    exe:string ->
    dir:string ->
    unit ->
    server
  (** Starts [exe serve-mcp --http 0 args] in [dir] with [BACTRIAN_MCP_TOKEN]
      set to [token] (default {!test_token}), or unset for [None], and the
      test's environment plus [env]. Waits until it prints the port it listens
      on and, without a token of the test's, the token it made. Its standard
      error goes to a file, {!log}. *)

  val port : server -> int
  val token : server -> string
  val pid : server -> int

  val log : server -> string
  (** What the server wrote to standard error so far. *)

  val stop : ?timeout:float -> server -> Unix.process_status
  (** SIGTERM, then waits for the server to exit, at most [timeout] seconds
      (default 30), killing it by its PID when that runs out. *)

  val dispose : server -> unit
  (** Kills the server by its PID if it is still running. *)

  val auth : server -> string * string
  (** The [Authorization] header with the server's token. *)

  val rpc_headers :
    ?accept:string ->
    token:string ->
    string ->
    Canon.t ->
    (string * string) list
  (** What a 2026-07-28 request with this method and params carries: the token,
      [Content-Type], [Accept] (default both JSON and SSE),
      [MCP-Protocol-Version], [Mcp-Method] and, for [tools/call] and the task
      methods, [Mcp-Name]. *)

  val with_meta : tasks:bool -> Canon.t -> Canon.t
  (** [params] with the [_meta] of {!Mcp_client.request}. *)

  val body : ?id:int -> string -> Canon.t -> string
  (** A JSON-RPC request with this method and params, with the next id unless
      given. *)

  val rpc :
    ?tasks:bool ->
    ?accept:string ->
    ?headers:((string * string) list -> (string * string) list) ->
    server ->
    string ->
    Canon.t ->
    response
  (** A 2026-07-28 request: the [_meta] of {!request} added to [params], with
      {!rpc_headers} passed through [headers] (default unchanged). *)

  val must_ok : string -> response -> Canon.t
  (** The result of an answer with status 200. Anything else raises
      {!Protocol_violation}, naming the method. *)

  val call_tool : ?accept:string -> server -> string -> Canon.t -> outcome
  (** {!Mcp_client.call_tool} over HTTP, which must answer with status 200. *)

  val call_task : server -> string -> Canon.t -> Canon.t
  (** {!Mcp_client.call_task} over HTTP. *)

  val task : server -> string -> string -> (Canon.t, error) result
  (** [tasks/get] or [tasks/cancel] for this task, from a request that declares
      the extension. *)

  val poll_task :
    ?timeout:float -> ?interval:float -> server -> string -> Canon.t
  (** {!Mcp_client.poll_task} over HTTP. *)
end
