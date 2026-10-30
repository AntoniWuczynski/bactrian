(** The TLS side of egress interception: the server end of each tunnel, with a
    leaf certificate of the project CA for the [CONNECT] host (decision record
    0026). *)

val tls : Ca.t -> sim_now:string -> Bactrian_runtime.Egress_proxy.tls
(** A fresh single-certificate server configuration per tunnel (tls-eio has no
    Server Name Indication callback, so the certificate comes from the [CONNECT]
    host), offering only [http/1.1] by ALPN. The leaf is valid now and at
    [sim_now] (the run's [SIM_NOW], RFC 3339), so an agent whose clock
    libfaketime sets there accepts it too. *)

val interception :
  Ca.t ->
  roots:X509.Certificate.t list ->
  Bactrian_runtime.Egress_proxy.interception
(** {!tls}, and the agents' trust bundle: [roots] (the system's) followed by the
    CA's certificate. *)
