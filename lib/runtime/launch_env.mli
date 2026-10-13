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
(** [BACTRIAN_ENV_FD]: [N:NONCE], the descriptor the full environment is read
    from and a random nonce that the file must start with. *)

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
    or {!restore} ran in this process. The environment goes to a file created
    exclusively (mode 0600, a random name under the temporary directory) and
    unlinked at once, on a descriptor above 2, after a random nonce. Does not
    return on success. [Error] when the environment cannot be written or the
    [exec] fails, with the descriptor closed. *)

val restore : unit -> (unit, string) result
(** When {!marker} is set, reads the full environment from the descriptor it
    names, which must be above 2, a regular file, and start with its nonce,
    closes it, sets every variable with [Unix.putenv] and sets {!marker} to the
    empty string. Anything else is [Error], so a stray [BACTRIAN_ENV_FD] is not
    taken for an environment. [Ok ()] with nothing to do when {!marker} is unset
    or empty. *)

val close_inherited_on_exec : unit -> unit
(** Marks every open descriptor above 2 close-on-exec. bactrian's own
    descriptors already are, but one it inherited from whatever started it (a
    shell's [exec 5<file], a CI runner, a supervisor) is not, and would reach
    every child, through [bwrap] into the sandbox: an inherited directory
    descriptor leads out of any mount namespace. Called once at start, after
    {!restore}, so a child gets only the descriptors it is given explicitly. *)
