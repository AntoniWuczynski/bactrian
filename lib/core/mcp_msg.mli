(** JSON-RPC 2.0 messages as the Model Context Protocol (MCP) uses them, with
    the little protocol knowledge the proxy needs: which protocol era a request
    speaks, the canonical form that keys a request for replay, and the tool call
    a request carries. Pure: no IO.

    Covers MCP revisions 2025-11-25 and earlier (with [initialize]) and
    2026-07-28 (stateless, every request carries [_meta]). *)

type id = Int_id of int | String_id of string

type message =
  | Request of { id : id; method_ : string; params : Canon.t option }
  | Notification of { method_ : string; params : Canon.t option }
  | Response of { id : id; result : Canon.t }
  | Error_response of {
      id : id option;
      code : int;
      message : string;
      data : Canon.t option;
    }
      (** [id] is [None] when the peer could not tell which request failed
          (JSON-RPC sends [null]). *)

type era =
  | Modern  (** 2026-07-28: no handshake, [_meta] on every request. *)
  | Legacy  (** 2025-11-25 and earlier: [initialize] opens a session. *)

type error =
  | Invalid_json of string
      (** The text is not JSON, or has a duplicate key or a non-finite number.
          Carries the reason. *)
  | Not_jsonrpc of string
      (** Valid JSON that is not a single JSON-RPC 2.0 message: a batch (an
          array, removed from MCP in 2026-07-28), a missing or wrong [jsonrpc],
          [method], [id], [result] or [error], a member outside the JSON-RPC
          set, or a [params] that is neither an object nor an array. Carries the
          reason. *)

val decode : string -> (message, error) result
(** A request has [method] and a non-null [id], a notification has [method] and
    no [id], a response has [id] and [result], an error response has [error]
    with an integer [code] and a string [message]. Ids are integers or strings.
    A message with both [result] and [error], or with a member other than
    [jsonrpc], [id], [method], [params], [result] and [error], is rejected. *)

val encode : message -> string
(** One line, no newline, keys in canonical (sorted) order, so
    [decode (encode m)] is [Ok m]. *)

val era_of_request : message -> era option
(** [Legacy] for an [initialize] request, [Modern] when [params._meta] carries
    [io.modelcontextprotocol/protocolVersion], [None] otherwise and for every
    message that is not a request. *)

val canonical_request : message -> Canon.t option
(** Requests only ([None] for the other kinds, since notifications and responses
    are never matched). The key source: an object with [method] and, when
    non-empty, [params], where [params] has lost the JSON-RPC id (it is not in
    [params] anyway) and the [_meta] entries that vary per client or run:
    [io.modelcontextprotocol/clientInfo], [traceparent], [tracestate], [baggage]
    and [progressToken]. A [_meta] left empty is removed, and so is a [params]
    left empty, so a request without params equals one whose only params were
    dropped entries. The protocol version and client capabilities stay. For
    [initialize], [params.clientInfo] is dropped too, since it names the client
    just as the modern [_meta] entry does. *)

val tool_call : message -> (string * Canon.t) option
(** For a [tools/call] request: the tool name and its [arguments] ([{}] when
    absent). [None] for anything else, including a [tools/call] without a string
    [name]. For the trace join. *)

val with_id : id -> message -> message
(** Replay rewrites a recorded response to the live request's id. Requests,
    responses and error responses take the new id (an error response with no id
    gains one). Notifications have none and are unchanged. *)
