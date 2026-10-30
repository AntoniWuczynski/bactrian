(** HTTPS egress interception (design §5.3, decision record 0018).

    An agent's HTTPS client reaches its host through [HTTPS_PROXY], which names
    the model proxy's listener: the model proxy authenticates the [CONNECT]
    request (the run ID as the proxy user name, the run token as its password)
    and hands it to {!connect}. Each request on the tunnel becomes a
    {!Bactrian_core.Cassette.exchange} on channel [Http], with protocol
    ["http"], the request's method, path and normalised query, and the key
    {!val:Bactrian_core.Matcher.key} over those and
    {!Bactrian_core.Egress.canonical} of the redacted request (masked with the
    experiment's [normalise] patterns). Bodies are stored whole. *)

open Bactrian_core

type tls = {
  terminate : host:string -> Upstream.flow -> (Upstream.flow, string) result;
      (** The server side of TLS on the tunnel, with a certificate for [host]
          from the project CA: the handshake, or why it failed. *)
  failure : exn -> string option;
      (** The reason, for an exception the terminated flow raises (a TLS alert
          or a bad record from the agent), [None] for any other. *)
}

type interception = {
  tls : tls;
  bundle : string;
      (** The PEM certificates the agent trusts: the system's roots and the
          project CA. *)
}
(** What a run needs from the TLS stack to intercept egress. *)

type run = {
  policy : Egress.t;
  tls : tls;
  shared : Mcp_proxy.shared;  (** The run's state, as MCP gets it. *)
  call :
    sw:Eio.Switch.t ->
    Upstream.connector ->
    Upstream.request ->
    on_head:(Upstream.head -> unit) ->
    on_chunk:(dt_ms:int -> string -> unit) ->
    (unit, Upstream.error) result;
  max_body : int;  (** Larger request bodies get 413. *)
  logged : Trace_builder.http_call -> unit;
      (** Called for every request: refused at [CONNECT], refused, answered from
          the recording or forwarded. *)
}

exception Tunnel_closed
(** Raised once a tunnel ends, so that the listener closes the connection
    instead of reading HTTP from it again. *)

val connect :
  run ->
  conn:Eio.Switch.t * Eio.Net.Sockaddr.stream ->
  target:string ->
  Proxy_reply.response
(** Answers an authenticated [CONNECT] to [target] ([host:port]). A target that
    does not parse gets 400. A host whose policy is [block] gets 403 with
    [bactrian_egress_blocked], before any TLS work. Otherwise 200, then the TLS
    handshake on the same connection (bytes the agent sent after the [CONNECT]
    head are not lost) and HTTP/1.1 requests on it until either side closes:
    - [record] follows the run: forwarded and recorded when recording, served
      from the recording in a replay, forwarded unrecorded in a passthrough run;
    - [replay] is served from the recording, and refused like a strict miss when
      there is none;
    - [passthrough] is always forwarded and never recorded.

    A replay miss gets 409 with [bactrian_replay_miss] and the divergence, an
    upstream failure 502 with [bactrian_upstream_error], an [Upgrade] request (a
    WebSocket) 501 with [bactrian_unsupported]. Forwarded requests go to
    [https://<target><path>] through the run's TLS connector, without the hop by
    hop and proxy headers. *)
