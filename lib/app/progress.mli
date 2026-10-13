(** What a command reports while it works. Each command takes an [~on_progress]
    callback and calls it instead of printing. *)

type t =
  | Warning of string
      (** Something the command carries on past, such as an unsandboxed agent or
          an experiment setting that is accepted but not enforced. *)
  | Experiment_started of string
      (** The experiment's ID, known before its first run starts. *)
