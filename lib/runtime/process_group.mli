(** Signalling and polling a process group whose leader we spawned with
    [~pgid:0], so the leader's pid is also the group id. *)

val signal : int -> int -> bool
(** [signal pgid s] sends signal [s] to the whole group. [false] means the group
    is gone: ESRCH, or EPERM, which macOS reports instead when only zombies are
    left (the group runs as our own user, so EPERM cannot mean a foreign
    process). Signal [0] probes without signalling. *)

val wait_gone : clock:_ Eio.Time.clock -> timeout:float -> int -> bool
(** [wait_gone ~clock ~timeout pgid] polls every 20 ms until the group is gone.
    [false] if it still exists after [timeout] seconds. *)
