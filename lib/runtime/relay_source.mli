(** The sources of the relay ([vm/relay.c], [vm/relay_image.c] and
    [vm/relay_image.h]) that a virtual machine sandbox runs as the container's
    entrypoint, built into bactrian so an installed copy needs no source tree.
*)

val files : (string * string) list
(** Each file's name and contents. *)
