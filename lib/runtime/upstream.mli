(** One HTTP request to a model provider, with the response body delivered chunk
    by chunk as it arrives.

    Plain TCP is built in. The TLS connector comes from a separate library
    ([bactrian.tls]) through {!of_https}, so this library builds without the TLS
    stack. *)

type connector

val plain : connector
(** Plain TCP. An [https] URL is refused with [Connect_failed]. *)

type flow = [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r

type extra_trust = {
  variable : string;  (** The variable that names the file. *)
  path : string;  (** The file. *)
  count : int;  (** Its certificates, trusted for every host. *)
  skipped : int;  (** Its PEM blocks that did not decode. *)
}
(** Certificates trusted for every host on top of the system's, and where they
    came from. *)

type trust = { roots : int; extra : extra_trust option }
(** What a connector trusts: [roots] certificates in all, [extra]'s included. *)

type https = {
  ready : unit -> (trust, string) result;
      (** Loads what the connector needs before its first connection (the
          trusted root certificates), once. [Error] says why HTTPS cannot work.
      *)
  wrap : Uri.t -> flow -> (flow, string) result;
      (** Wraps an established TCP connection in TLS for the given URL: the
          handshake, the certificate and the host name check. [Error] says why
          it failed. Used only for [https] URLs. *)
  failure : exn -> string option;
      (** The reason, for an exception that a wrapped flow raises on a read or
          write (a TLS alert or a bad record), so that a call ends with an
          {!error} rather than the exception. [None] for any other exception. *)
}

val of_https : https -> connector

val tls_ready : connector -> (trust, string) result
(** The connector's [ready], or [Error] for {!plain}. *)

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
