(** One run's Model Context Protocol (MCP) traffic, for stdio servers reached
    through [bactrian mcp-shim] and for Streamable HTTP servers (decision 0006).

    The model proxy routes [/r/<token>/mcp/<name>] to {!stdio} and
    [/r/<token>/mcp-http/<name>/...] to {!http}, and shares the run's state
    through {!shared}. Each JSON-RPC request and its response become one
    {!Bactrian_core.Cassette.exchange} on channel [Mcp], with protocol ["mcp"],
    method [POST], path [/mcp/<name>] for either transport, and the key
    {!val:Bactrian_core.Matcher.key} over that method and path and
    {!Bactrian_core.Mcp_msg.canonical_request} of the redacted request (masked
    with the experiment's [normalise] patterns). The response is stored with the
    agent's id. Notifications are never recorded. *)

open Bactrian_core

type server =
  | Stdio of {
      argv : string list;
          (** Looked up on bactrian's own [PATH], like the agent's. *)
      env : (string * string) list;
          (** The experiment's declared variables. The server's whole
              environment is bactrian's [PATH], [HOME] and [LANG] (where set)
              followed by these, a later entry replacing an earlier one with the
              same name. Nothing else is inherited: no provider keys, no
              placeholder keys, no proxy or bactrian variables. *)
      cwd : Eio.Fs.dir_ty Eio.Path.t option;  (** Default: bactrian's own. *)
    }
  | Http of { url : Uri.t }
      (** The server's real endpoint. A request to [.../mcp-http/<name>/x] goes
          to [url]'s path followed by [/x], with the agent's query. *)

type policy =
  | Record
      (** Follows the run: live and recorded in a recording run, answered from
          the recording in a replay, live and unrecorded in a passthrough run.
      *)
  | Replay
      (** Answered from the recording. A run without one refuses every request,
          as a strict miss would, without contacting the server. *)
  | Passthrough  (** Always live, never recorded or matched. *)
  | Block
      (** Every request is refused: JSON-RPC error [-32601] on the stdio route,
          HTTP 403 on the HTTP route. Notifications are dropped. *)

type config = { name : string; server : server; policy : policy }

val url_variable : string
(** ["BACTRIAN_MCP_URL"]: the variable that tells [mcp-shim] where to post. *)

val http_variable : string -> string
(** [http_variable name] is ["BACTRIAN_MCP_HTTP_"] followed by [name]
    uppercased, with every character outside [A-Z0-9] written as ['_']. *)

(** {1 What the model proxy shares} *)

type serving =
  | Live of { record : bool }
  | Replaying of {
      decide : t_ms:int -> Matcher.request -> Matcher.decision;
          (** Runs the run's matcher (under the run's lock), keeps its findings
              and refusals, and on [Serve] counts the exchange and appends a
              copy with this run's [seq] and the given [t_ms]. *)
    }

type shared = {
  serving : serving;
  store : Store.t;
  connector : Upstream.connector;
  t_ms : unit -> int;  (** Milliseconds since the run started. *)
  scrub : string -> string;  (** The run's redaction, provider keys included. *)
  mask : Canon.t -> Canon.t;
      (** The run's [normalise] patterns, counted in its findings. *)
  occurrence : string -> int;
      (** Earlier requests with this key in the run, counting this one from then
          on. *)
  exchange : record:bool -> Cassette.exchange -> unit;
      (** Counts a live exchange and, when [record] holds and the run still has
          an open cassette, appends it with the run's next [seq]. *)
  failed : string -> unit;
      (** Counts a transport error and adds it to the run's errors. *)
  note : string -> unit;  (** Adds to the run's errors. *)
}

type env = {
  sw : Eio.Switch.t;  (** Outlives every run: the proxy's switch. *)
  process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  call :
    sw:Eio.Switch.t ->
    Upstream.connector ->
    Upstream.request ->
    on_head:(Upstream.head -> unit) ->
    on_chunk:(dt_ms:int -> string -> unit) ->
    (unit, Upstream.error) result;
}

(** {1 Serving} *)

type t
(** One run's MCP servers. *)

val create : env -> shared -> config list -> t
(** Starts nothing: a stdio server is spawned on the run's first live request or
    notification for it, in its own process group, and supervised until {!stop}.
*)

type reply = {
  status : int;
  headers : (string * string) list;
  body : string;  (** Empty for 202 and 405. *)
}

val stdio : t -> name:string -> string -> reply
(** One JSON-RPC message from the shim. A request gets 200 and its JSON-RPC
    response, always with the agent's id: the server's own answer, the recorded
    one, or a bactrian error. A crashed or unresponsive server gives error
    [-32000] (data [type] ["bactrian_mcp_server_error"]) and is recorded as a
    [Transport_error]. A replay miss gives error [-32000] (data [type]
    ["bactrian_replay_miss"] and [divergence]), naming the handshake when the
    request is [initialize] or [server/discover]. A notification or response
    gets 202 and an empty body. A body that is not one JSON-RPC message gets 400
    with error [-32700], an unknown server 404. *)

val http :
  t ->
  name:string ->
  meth:string ->
  rest:string ->
  query:string option ->
  headers:(string * string) list ->
  string ->
  reply
(** One Streamable HTTP request. Only POST is served (405 with [Allow: POST]
    otherwise, which both eras permit). Forwarded headers are [content-type],
    [accept], [mcp-protocol-version], [mcp-method], [mcp-name], [mcp-session-id]
    and [mcp-param-*], never the agent's key headers. The reply carries the
    server's status, [content-type] and [mcp-session-id]. In replay the recorded
    reply is served with the live id, in a JSON body or in each [data:] line of
    an event stream, and the agent's [mcp-session-id] is accepted whatever it
    is. A transport failure is 502 with a JSON-RPC error, a replay miss 409 with
    one. *)

val stop : t -> unit
(** Kills the process group of every stdio server the run started (SIGTERM, then
    SIGKILL after two seconds) and waits for it to go. Requests after this are
    refused with a server error. Not cancellable. *)
