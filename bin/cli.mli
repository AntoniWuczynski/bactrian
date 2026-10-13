(** Pieces shared by every command: exit statuses, error and progress printing
    and common arguments. *)

open Cmdliner

val exits : Cmd.Exit.info list
(** The exit statuses of a single run, also the default for other commands. *)

val fail_with :
  json:bool -> ?resume:(string -> string) -> Bactrian_app.Command_error.t -> int
(** Prints the error and returns its exit status. Under [json] it is
    {!Bactrian_app.Command_error.to_json} on stdout, otherwise
    [bactrian: message] on stderr, or for configuration errors one
    [file:line:column] line each. An interruption is always printed on stderr,
    with the text [resume] gives for the experiment's ID appended, if one had
    started. *)

val print_progress : Bactrian_app.Progress.t -> unit
(** Prints a warning or the experiment's ID on stderr. *)

val json : bool Term.t
(** The [--json] flag. *)
