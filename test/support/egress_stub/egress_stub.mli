(** An egress interception for tests whose agents make no HTTPS calls of their
    own and never read the trust bundle: runs get the proxy variables and a
    bundle that holds no certificate, and a handshake, were one tried, fails. An
    agent whose HTTP client loads [SSL_CERT_FILE] at start needs the real
    project CA instead. *)

val interception : Bactrian_runtime.Egress_proxy.interception
