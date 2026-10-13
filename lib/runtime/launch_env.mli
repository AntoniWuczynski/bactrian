(** A launch environment free of secrets for the commands that start agents
    (design §5.3, decision 0010).

    On macOS any process of the same user, sandboxed or not, can read another
    process's launch-time arguments and environment with the process-arguments
    [sysctl], and on Linux [/proc/PID/environ] shows the same. That copy is
    fixed at [exec] and does not follow later [putenv] calls. So before it
    starts anything, bactrian re-executes itself with only {!allowed} variables
    in its launch environment and hands the full environment to the new image
    through an unlinked temporary file, from which {!restore} puts it back with
    [Unix.putenv]. *)

val marker : string
(** [BACTRIAN_ENV_FD]: names the descriptor the full environment is read from.
*)

val allowed : string -> bool
(** Whether a variable may stay in the launch environment: [PATH], [HOME],
    [USER], [LOGNAME], [SHELL], [LANG], [LC_*], [TERM], [TZ] and [TMPDIR]. *)

val kept : string array -> string list
(** The [NAME=VALUE] entries of an environment that {!allowed} keeps, in order.
    Entries without [=] are dropped. *)

val encode : string array -> string
(** The entries, each followed by a NUL byte (an entry cannot hold one). *)

val decode : string -> (string * string) list
(** The [(name, value)] pairs of {!encode}'s output, split at each entry's first
    [=]. Entries without a name are dropped. *)

val scrub : exe:string -> (unit, string) result
(** Re-executes [exe] (resolved to an absolute path without symbolic links) with
    the same arguments and a launch environment of the {!kept} entries plus
    {!marker}, unless the launch environment already holds only {!kept} entries
    or {!restore} ran in this process. Does not return on success. [Error] when
    the environment cannot be written or the [exec] fails. *)

val restore : unit -> (unit, string) result
(** When {!marker} names a descriptor, reads the full environment from it,
    closes it, sets every variable with [Unix.putenv] and sets {!marker} to the
    empty string. [Ok ()] with nothing to do when {!marker} is unset or empty.
*)
