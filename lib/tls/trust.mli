(** The root certificates bactrian trusts for its own HTTPS calls to providers
    and to HTTP-mode agents (decision record 0017).

    The sources follow the [ca-certs] library: on macOS the system root and
    system keychains, read with [/usr/bin/security], on Linux the first of the
    usual distribution bundles that holds a certificate. Unlike [ca-certs],
    bactrian starts [security] with an empty environment, because its own
    environment holds the provider keys (decision record 0010), and it never
    reads [SSL_CERT_FILE], [NIX_SSL_CERT_FILE] or [OCAML_EXTRA_CA_CERTS]: a user
    may have them set for other tools, and once egress is intercepted bactrian
    will give agents [SSL_CERT_FILE] naming its own interception CA, which must
    not replace its upstream trust. *)

val extra_variable : string
(** [BACTRIAN_EXTRA_CA_CERTS]: a file of PEM certificates trusted in addition to
    the system's, for a TLS-inspecting corporate proxy or a test server. Unset
    or empty adds nothing. *)

val linux_bundles : string list
(** The bundle paths tried on Linux, in order. *)

val macos_roots_keychain : string
(** The system roots keychain, read on macOS. It must give a certificate. *)

val macos_admin_keychain : string
(** The keychain of certificates an administrator added, also read on macOS.
    Nothing is taken from it when [security] cannot read it. *)

type extra = {
  path : string;  (** The file {!extra_variable} names. *)
  certs : X509.Certificate.t list;
      (** Its certificates, trusted for every host. *)
  skipped : int;  (** How many PEM blocks in it did not decode. *)
}

type t = {
  system : X509.Certificate.t list;  (** The system's root certificates. *)
  extra : extra option;  (** [None] when {!extra_variable} is unset or empty. *)
}

val load : process_mgr:_ Eio.Process.mgr -> (t, string) result
(** The system's root certificates and those of {!extra_variable}. Certificates
    that do not decode are skipped. [Error] when no system root certificate is
    found, when the macOS system roots keychain cannot be read, or when
    {!extra_variable} names a file that cannot be read, holds no certificate,
    belongs to a user other than this one or root, or can be written by its
    group or by others. *)
