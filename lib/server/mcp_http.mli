(** [bactrian serve-mcp --http]: the MCP server over Streamable HTTP (decision
    record 0021), on [127.0.0.1] only, at the endpoint [/mcp].

    Each POST carries one JSON-RPC message and is answered on its own: there are
    no sessions. Before anything else a request must name this server in its
    [Host] header, come from no [Origin] or an allowed one (403 otherwise, so a
    page in a browser cannot drive the server through DNS rebinding), be a POST
    to [/mcp] (404, and 405 for another method), carry the bearer token (401)
    and JSON (415) of at most 16 MiB (413). The body of a refused request is
    read and dropped, so the next request on the connection is parsed from its
    own start, and one longer than 64 KiB ends the connection after the answer,
    as a 413 does. A request with neither [Content-Length] nor
    [Transfer-Encoding] has no body (RFC 9112, section 6.3). One whose
    [Transfer-Encoding] does not end in [chunked], or whose [Content-Length] is
    not a number, is 400 once it is past the checks above, and one with both
    headers is answered as usual. Both end the connection after the answer.

    A request whose [_meta] names 2026-07-28 must repeat its protocol version in
    [MCP-Protocol-Version] and its method in [Mcp-Method], and a [tools/call]
    its tool and a task method its task ID in [Mcp-Name], plainly or as
    [=?base64?...?=]. A header that is missing, holds a character outside
    visible ASCII or differs from the body is [-32020] (HeaderMismatch) with
    status 400. A request without that [_meta] is [initialize] or follows the
    era its [MCP-Protocol-Version] names (2025-03-26 when there is none). *)

open Bactrian_core

val endpoint : string
(** ["/mcp"]. *)

type config = {
  port : int;  (** The port the server listens on. *)
  token : string;  (** The bearer token every request must carry. *)
  allowed_origins : string list;
      (** Origins allowed besides [http://127.0.0.1:PORT] and
          [http://localhost:PORT], as {!origin} gives them. *)
}

type early = { status : int; headers : (string * string) list; body : string }
(** An HTTP answer given before the message is read: a refusal, or the answer to
    a CORS preflight. *)

val origin : string -> (string, string) result
(** An origin a user allows, as [Origin] carries it: [http] or [https], [://]
    and a host, with a port or not, in lower case. [Error] says what is wrong
    with anything else, such as a path, a trailing slash or [*]. *)

val admit : config -> Http.Request.t -> (unit, early) result
(** The checks on [Host], [Origin], the path, the method, [Authorization] and
    [Content-Type], in that order. A refused [Host] or [Origin] is answered with
    a JSON-RPC error that has no id, a missing or wrong token with
    [WWW-Authenticate: Bearer]. An [OPTIONS] request with an allowed [Origin] is
    a CORS preflight, answered with 204, [Access-Control-Allow-Methods: POST]
    and the headers it asks for in [Access-Control-Allow-Headers]. *)

val header_value : string -> string option
(** A header value as the client meant it: the decoded UTF-8 of
    [=?base64?...?=], the value itself otherwise. [None] when it holds a
    character other than visible ASCII, space and tab, or a sentinel whose
    Base64 is broken. *)

val era_of_request :
  Http.Header.t -> Mcp_msg.message -> (Mcp_dispatch.era, Mcp_msg.message) result
(** The era to dispatch a request in, after checking its headers: [Modern] for a
    request whose [_meta] names a protocol version (or whose
    [MCP-Protocol-Version] is 2026-07-28), [Undecided] for [initialize] and
    [Legacy] for the rest. [Error] is the response to send with status 400: a
    [-32020] for a header that does not match, or a [-32600] for an
    [MCP-Protocol-Version] that names no revision this server knows. *)

val status : Mcp_dispatch.era -> Mcp_msg.message -> int
(** The HTTP status of a response sent in this era. In 2026-07-28 an error is
    404 for an unknown method ([-32601]), 500 for an internal error and 400
    otherwise. In the legacy era a malformed request ([-32700], [-32600]) is 400
    and another error, which the client reads as JSON-RPC, 200. A result is 200.
*)

val listen :
  sw:Eio.Switch.t ->
  net:[> [ `Generic | `Unix ] Eio.Net.ty ] Eio.Resource.t ->
  int ->
  ( [ `Generic | `Unix ] Eio.Net.listening_socket_ty Eio.Resource.t * int,
    string )
  result
(** A listening socket on [127.0.0.1] at this port (0 picks a free one), with
    the port it got. *)

val serve :
  server:Mcp_dispatch.server ->
  handlers:Mcp_dispatch.handlers ->
  stop:Bactrian_runtime.Interrupt.trigger ->
  log:(string -> unit) ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?head_timeout:float ->
  config ->
  [ `Generic | `Unix ] Eio.Net.listening_socket_ty Eio.Resource.t ->
  Mcp_server.ended
(** Answers requests on the socket until [stop] fires, then cancels the
    connections and the calls in flight and returns [Stopped] once they have
    unwound. A [tools/call] is answered with an SSE stream when the request's
    [Accept] lists [text/event-stream], with JSON otherwise, and the client
    closing the connection before the answer cancels the call. A notification is
    accepted with 202, a message that is not JSON-RPC is 400 and GET and DELETE
    are 405. Every answer to a request whose [Origin] is allowed carries it in
    [Access-Control-Allow-Origin], so a page from there can read it. [log] gets
    a line for each error of a connection.

    At most 64 connections are open at once, and a request's head holds at most
    64 KiB. Until a request is admitted, a connection has [head_timeout] seconds
    (default 30), counted from its start or from the answer to its last admitted
    request, and the bound holds for what the server writes to it as well as for
    what it reads. One that idles longer, sends a longer head or does not read
    the answers to its refused requests is closed without an answer. *)
