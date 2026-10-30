(** [bactrian ca]: the project certificate authority that egress interception
    signs its leaf certificates with ([.bactrian/ca], design §5.2). *)

type action =
  | Path  (** Where its certificate is, making the CA when it is missing. *)
  | Rotate  (** Replace it with a new one. *)

type t = {
  certificate : string;  (** The certificate's path. *)
  not_after : string;  (** RFC 3339, when it stops being valid. *)
  sha256 : string;  (** Its fingerprint, in lowercase hex. *)
  created : bool;  (** Whether this command made it. *)
}

val run : root:string -> action -> (t, Command_error.t) result
(** [root] is the project's [.bactrian] directory. A CA that cannot be loaded or
    made is an [Environment] error. *)

val to_json : t -> Yojson.Safe.t
(** [{"certificate", "not_after", "sha256", "created"}]. *)

val render_text : action -> t -> string
(** The path alone for [Path], so that it can be used in a shell, and a line
    saying what was made for [Rotate]. *)
