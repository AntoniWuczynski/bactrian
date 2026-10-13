(** One HTTP request to a model provider, with the response body delivered chunk
    by chunk as it arrives.

    Only plain TCP is built in. A TLS connector can be added from a separate
    library through {!of_https}. *)

type connector

val plain : connector
(** Plain TCP. An [https] URL is refused with [Connect_failed]. *)

type https =
  Uri.t ->
  [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r ->
  [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r
(** Wraps an established TCP connection in TLS for the given URL. *)

val of_https : https -> connector

val is_https : Uri.t -> bool
(** Whether the URL's scheme is [https], which needs a TLS connector. *)

type request = {
  meth : string;
  url : Uri.t;
  headers : (string * string) list;
  body : string;
}

type head = { status : int; headers : (string * string) list }
(** [headers] holds only [content-type], [retry-after], [x-ratelimit-*],
    [anthropic-ratelimit-*], [x-request-id] and [mcp-session-id] (the legacy MCP
    session, which the agent needs back), names lowercased, in response order.
    [content-length], [transfer-encoding], [set-cookie] and everything else
    never appear. *)

type error =
  | Connect_failed of string
      (** No byte of the request was written: refused, DNS failure, failed TLS
          handshake, or an [https] URL with no TLS connector. The provider never
          saw the request. *)
  | Failed_after_send of string
      (** The request was written but no usable head came back: reset or closed
          before the head, or an unreadable head. The provider may have run and
          billed the request. *)
  | Timed_out
  | Truncated of { bytes : int; reason : string }
      (** The head arrived but the body ended early. [bytes] is the number
          already passed to [on_chunk]. *)

val describe_error : error -> string
(** One line for a run's errors and recorded transport errors. *)

val call :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  connector ->
  request ->
  on_head:(head -> unit) ->
  on_chunk:(dt_ms:int -> string -> unit) ->
  (unit, error) result
(** Sends [request] with [Accept-Encoding: identity] (any caller-supplied
    [accept-encoding], [host], [content-length] or [transfer-encoding] header is
    dropped). [on_head] runs once, before any chunk. [on_chunk] runs for each
    piece as it is read, with the milliseconds since the previous event (the
    head for the first chunk). [timeout] bounds the whole call in seconds, body
    included. Connections are not reused. Exceptions raised by [on_head] or
    [on_chunk] propagate. *)
