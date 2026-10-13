type turn = { user : string }

type t = {
  id : string;
  now : string option;  (** RFC 3339. *)
  timezone : string option;
  fixtures : string option;  (** Directory, relative to the scenario file. *)
  turns : turn list;  (** At least one. *)
  assertions : Assertion.t list;
  checkers : string list list;  (** argv of each checker command. *)
  golden_pass : string list;
  golden_fail : string list;
}

type error = Config_error.error = { path : string; message : string }
(** [path] is the document path, e.g. ["assertions[2].tool_args.tool"]. *)

val decode : Canon.t -> (t, error list) result
(** Strict: unknown fields are errors. Collects every error. *)
