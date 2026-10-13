(** Turning SIGINT and SIGTERM into Eio cancellation (spec §9, Interrupt). *)

val signals : int list
(** The signals handled: [Sys.sigint] and [Sys.sigterm]. *)

val exit_status : int
(** 130, the status of a command ended by an interrupt. *)

val run : (unit -> 'a) -> ('a, int) result
(** [run f] runs [f] with handlers installed for {!signals}. When one arrives,
    [f]'s fibre is cancelled (so its switches end and its cleanup runs) and
    [Error signal] is returned once [f] has finished unwinding. Otherwise
    [Ok (f ())]. The previous handlers are restored on return. The handler only
    broadcasts an [Eio.Condition], the one operation that is safe in a signal
    handler. Must be called inside [Eio_main.run]. *)
