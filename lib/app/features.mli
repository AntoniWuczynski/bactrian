(** What this build can do. *)

val connector : _ Eio.Process.mgr -> Bactrian_runtime.Upstream.connector
(** The TLS connector for [https] upstreams and HTTP-mode agents
    ({!Bactrian_tls.Tls_connector.system}), which loads the trusted root
    certificates on first use. *)

val ca_dir : root:string -> string
(** The project CA's directory, [ca] in the project's [.bactrian] directory
    [root]. *)

val ca :
  root:string -> (Bactrian_tls.Ca.t * Bactrian_tls.Ca.origin, string) result
(** The project CA ({!Bactrian_tls.Ca.load_or_create}), made when missing. *)

val describe_ca : Bactrian_tls.Ca.t -> string
(** Where the CA's certificate is, until when it is valid and its SHA-256
    fingerprint. *)

val interception :
  _ Eio.Process.mgr ->
  root:string ->
  unit ->
  (Bactrian_runtime.Egress_proxy.interception, string) result
(** What runs need to intercept egress: the project CA's TLS and the agents'
    trust bundle, the system's root certificates followed by the CA. *)
