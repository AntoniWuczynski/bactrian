(** The root certificates bactrian trusts for its own HTTPS calls to providers
    and to HTTP-mode agents (decision record 0017).

    The sources follow the [ca-certs] library: on macOS the system root and
    system keychains, read with [/usr/bin/security], on Linux the first bundle
    that exists of the usual distribution paths. Unlike [ca-certs], bactrian
    starts [security] with an empty environment, because its own environment
    holds the provider keys (decision record 0010), and it never reads
    [SSL_CERT_FILE], [NIX_SSL_CERT_FILE] or [OCAML_EXTRA_CA_CERTS]: those are
    the variables bactrian gives agents, which name its own interception CA and
    must not replace its upstream trust. *)

val extra_variable : string
(** [BACTRIAN_EXTRA_CA_CERTS]: a file of PEM certificates trusted in addition to
    the system's, for a TLS-inspecting corporate proxy or a test server. Unset
    or empty adds nothing. *)

val linux_bundles : string list
(** The bundle paths tried on Linux, in order. *)

val macos_keychains : string list
(** The keychains read on macOS. *)

val load :
  process_mgr:_ Eio.Process.mgr -> (X509.Certificate.t list, string) result
(** The system's root certificates and those of {!extra_variable}. Certificates
    that do not decode are skipped. [Error] when no system root certificate is
    found, or when {!extra_variable} names a file that cannot be read or holds
    no certificate. *)
