(** The TLS connector for bactrian's own HTTPS calls: to providers through the
    model proxy, to HTTP-mode agents and to Streamable HTTP MCP servers
    (decision record 0017).

    Each connection verifies the server's certificate chain against the trusted
    roots, sends the URL's host as Server Name Indication and checks it against
    the certificate (a DNS name, or an IP address for an address host), and
    offers only [http/1.1] by ALPN, so bactrian's own HTTP/1.1 reader, which
    tells a cut-off body from a finished one, reads every response. *)

val make :
  trust:
    (unit ->
    (X509.Authenticator.t * Bactrian_runtime.Upstream.trust, string) result) ->
  Bactrian_runtime.Upstream.https
(** [trust] gives the authenticator and what it trusts. It is called by [ready]
    and before each handshake, and should be memoised by the caller. *)

val system :
  process_mgr:_ Eio.Process.mgr -> Bactrian_runtime.Upstream.connector
(** A connector trusting {!Trust.load}'s certificates, loaded on first use and
    then kept. Its [ready] gives {!Trust.load}'s error. *)
