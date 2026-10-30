(** The source of the relay ([vm/relay.c] and the [vm/probe.inc] it includes)
    that a virtual machine sandbox runs as the container's entrypoint, built
    into bactrian so an installed copy needs no source tree. *)

val text : string
(** [vm/relay.c]. *)

val probe : string
(** [vm/probe.inc], which [relay.c] includes. *)
