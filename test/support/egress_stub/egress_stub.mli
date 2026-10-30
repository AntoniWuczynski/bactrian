(** An egress interception for tests whose agents make no HTTPS calls of their
    own: runs get the proxy variables and a bundle, and a handshake, were one
    tried, fails. *)

val interception : Bactrian_runtime.Egress_proxy.interception
