(** Small helpers shared by the modules that find and start processes. *)

val hex : string -> string
(** Two lowercase hex digits per byte. *)

val absolute : ?cwd:string -> string -> string
(** A relative path joined to [cwd] (default: the current directory); an
    absolute one unchanged. Symbolic links are not resolved. *)

val regular_file : string -> bool
(** Whether the path leads, through symbolic links, to a regular file. *)

val find_executable :
  path:string option -> cwd:string -> string -> string option
(** [find_executable ~path ~cwd name] is the executable regular file [name]
    names, as the system would find it: a name with a [/] is a file path
    (relative to [cwd]), any other is looked up in the directories of [path], a
    [PATH] value, in order. *)

val describe_exit : Eio.Process.exit_status -> string
(** ["exit status N"] or ["killed by signal N"]. *)
