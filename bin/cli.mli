(** Pieces shared by every command: exit statuses, error printing and common
    arguments. *)

open Cmdliner

val exits : Cmd.Exit.info list
(** The exit statuses of a single run, also the default for other commands. *)

val fail : json:bool -> int -> string -> int
(** Prints the message, as [{"error": …, "exit": code}] under [json] or as
    [bactrian: message] on stderr otherwise, and returns the code. *)

val fail_located :
  json:bool -> Bactrian_runtime.Config_loader.located_error list -> int
(** Every configuration error with its location, then {!Output.exit_usage}. *)

val json : bool Term.t
(** The [--json] flag. *)
