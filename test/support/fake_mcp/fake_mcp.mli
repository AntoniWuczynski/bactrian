(** A fake MCP tool server for tests. It offers one tool, [get_weather], and
    speaks either protocol era: [Modern] is 2026-07-28 (stateless, answers
    [server/discover], rejects [initialize]), [Legacy] is 2025-11-25 (answers
    [initialize], rejects [server/discover]). The same responder serves
    newline-delimited stdio and an in-process Streamable HTTP server. *)

type era = Modern | Legacy

val protocol_version : era -> string
(** ["2026-07-28"] or ["2025-11-25"]. *)

val session_id : string
(** The [Mcp-Session-Id] a [Legacy] HTTP server returns for [initialize] and
    requires on every later request. *)

(** {1 Stdio} *)

type stdio_options = {
  crash_on : string option;
      (** A [tools/call] for this city is logged and then the server returns at
          once without answering, as a server that crashed mid-call would. *)
  hang_on : string list;
      (** A [tools/call] for one of these cities is logged and never answered,
          and the server goes on serving. *)
  null_id_on : string option;
      (** A [tools/call] for this city is answered with error [-32603] and a
          [null] id, as a server that could not tell which request failed. *)
  reverse_pairs : bool;
      (** Each [tools/call] waits for the next one, and the two are answered in
          reverse order (an unanswered call still counts as one of the pair).
          Other messages are answered at once. *)
  ask_client : bool;
      (** Before answering a [tools/call], the server sends the client a
          [roots/list] request with id ["fake-ask"] and reads lines until its
          answer arrives. The call's text then says what came back:
          ["roots/list: error <code>"] or ["roots/list: answered"]. *)
}

val stdio_defaults : stdio_options
(** None of the above. *)

val serve_stdio :
  ?options:stdio_options ->
  era:era ->
  log:string option ->
  In_channel.t ->
  Out_channel.t ->
  unit
(** Answers one JSON-RPC message per input line until end of input, flushing
    after every reply. Notifications get no reply, a batch gets [-32600], text
    that is not JSON gets [-32700]. A [Legacy] server refuses [tools/list] and
    [tools/call] with [-32600] until the client has sent
    [notifications/initialized], as the SDK servers do. With [log], each raw
    input line is appended to that file before it is answered. May raise
    [Sys_error] if the log cannot be written. *)

(** {1 Streamable HTTP}

    One endpoint, any path. [Modern]: POST only ([405] with [Allow: POST]
    otherwise), [MCP-Protocol-Version] and [Mcp-Method] required, [Mcp-Name]
    required for [tools/call], any mismatch with the body is [400] with
    [-32020]; an unsupported version is [400] with [-32022]; an unknown method
    is [404] with [-32601]. [Legacy]: [initialize] returns the [Mcp-Session-Id]
    header, later requests without it get [400] and with another value [404],
    [tools/list] and [tools/call] get [-32600] until a
    [notifications/initialized] has arrived, GET and DELETE get [405]. A
    notification is [202] with no body. *)

type request = {
  meth : string;
  path : string;
  query : string option;  (** As sent, without the [?]. *)
  headers : (string * string) list;  (** Names lower-cased. *)
  body : string;
}

type t

val start : sw:Eio.Switch.t -> net:_ Eio.Net.t -> era -> t
(** Listens on an ephemeral 127.0.0.1 port until [sw] ends. *)

val port : t -> int

val base_url : t -> string
(** ["http://127.0.0.1:<port>"], without a trailing slash. *)

val requests : t -> request list
(** Every request received so far, oldest first, including rejected ones. *)
