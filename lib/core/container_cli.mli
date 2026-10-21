(** The JSON that Apple's [container] command prints (version 1.5.0), decoded
    into what the virtual machine sandbox reads of it (design §5.3). Pure.

    bactrian does not own these shapes, so fields it does not read are ignored,
    and a field it reads that is missing or of another type is an error. *)

type status = {
  running : bool;  (** [status] is ["running"]. *)
  state : string;  (** [status] as given. *)
  architecture : string;  (** [host.architecture], such as ["arm64"]. *)
}

val status : string -> (status, string) result
(** [container system status --format json]. *)

val image_digest : string -> (string, string) result
(** [container image inspect REF]: [[0].configuration.descriptor.digest], the
    digest ([sha256:...]) of the image the reference names. *)

val container_digest : string -> (string, string) result
(** [container inspect NAME]: [[0].configuration.image.descriptor.digest], the
    digest of the image the container was created from. *)

type listed = {
  id : string;  (** The container's name. *)
  labels : (string * string) list;
}

val containers : string -> (listed list, string) result
(** [container list --all --format json]: each container's name
    ([configuration.id]) and labels ([configuration.labels]). *)
