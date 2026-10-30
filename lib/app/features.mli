(** What this build can do. *)

val connector : _ Eio.Process.mgr -> Bactrian_runtime.Upstream.connector
(** The TLS connector for [https] upstreams and HTTP-mode agents
    ({!Bactrian_tls.Tls_connector.system}), which loads the trusted root
    certificates on first use. *)
