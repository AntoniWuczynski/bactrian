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

val signal_name : int -> string
(** The name of a signal given by its OCaml number, as [Sys.sigint] and Eio's
    [`Signaled] carry it: ["SIGINT"] for [Sys.sigint]. A number that is none of
    OCaml's portable signals is a system number, and gives ["signal N"]. *)

val describe_exit : Eio.Process.exit_status -> string
(** ["exit status N"] or ["killed by SIGKILL"] (see {!signal_name}). *)

val path_var : unit -> string
(** [PATH=...] as bactrian has it, or [/usr/bin:/bin]. *)

val tool_env : unit -> string array
(** The whole environment of a helper command bactrian runs: {!path_var} alone,
    so that no provider key reaches a launch environment another process of the
    user can read (decision 0010). *)
