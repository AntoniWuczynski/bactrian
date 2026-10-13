(** Why a command did not produce its result, with the exit status it maps to.
*)

type t =
  | Usage of string  (** Bad arguments, or a missing or unusable input. *)
  | Environment of string
      (** The host is not ready, for example a spend cap of 0, a missing API key
          or no TLS support for a live call. *)
  | Internal of string
      (** A fault inside bactrian, such as unreadable results. *)
  | Configuration of Bactrian_runtime.Config_loader.located_error list
      (** Every configuration error, with its location. *)
  | Interrupted of { signal : int; experiment_id : string option }
      (** SIGINT or SIGTERM ended the command. [experiment_id] is the
          experiment's ID when one had started, so it can be resumed. *)

val exit_code : t -> int
(** {!Exit_code.usage} for [Usage] and [Configuration],
    {!Exit_code.environment}, {!Exit_code.internal}, and
    {!Bactrian_runtime.Interrupt.exit_status}. *)

val message : t -> string
(** The text of the error. Configuration errors are one per line, as
    [file:line:column: …]. *)

val to_json : t -> Yojson.Safe.t
(** [{"error": message, "exit": code}], or for [Configuration]
    [{"errors": [{file, line, column, path, message}…], "exit": code}]. An
    interruption adds the ["experiment_id"] to resume, if one had started. *)
