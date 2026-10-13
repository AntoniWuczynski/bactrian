(** Keeping tool arguments inside the server's working directory (decision
    record 0011). A coding agent drives the server, and a prompt-injected one
    should not turn it into a reader or writer of files elsewhere. *)

type t

val create : string -> (t, string) result
(** The directory, resolved to an absolute path without symbolic links. *)

val root : t -> string

val path : t -> field:string -> string -> (string, string) result
(** Accepts a path argument when it resolves under the root: relative paths are
    taken from the root, symbolic links are followed, and of a path that does
    not exist yet the longest existing prefix is resolved and the rest may not
    hold [..]. A symbolic link that leads nowhere is refused, since a write
    would follow it. Gives the resolved path relative to the root (["."] for the
    root itself), which a directory capability for the root accepts. The [Error]
    names [field]. *)

val id : field:string -> string -> (unit, string) result
(** Accepts a run or experiment ID: not empty, not [.] or [..], and without
    ['/'] or a NUL, so it names one entry of its directory. *)

val run : t -> field:string -> string -> (string, string) result
(** A run given as an ID ({!id}), which it gives back, or, when it holds a
    ['/'], as a run directory ({!path}). *)
