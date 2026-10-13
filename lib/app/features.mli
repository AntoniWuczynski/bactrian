(** What this build can do. *)

val tls_available : bool
(** Whether this build can call a provider over HTTPS. The TLS connector is a
    separate library that is not part of this build yet. *)
