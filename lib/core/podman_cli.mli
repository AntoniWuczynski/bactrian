(** What Podman's command line prints (version 4.9.3, Ubuntu 24.04's), read for
    the virtual machine sandbox (decision 0016). Pure. *)

type info = {
  arch : string;  (** [Host.Arch], such as ["arm64"]. *)
  rootless : bool;  (** [Host.Security.Rootless]. *)
}

val info_format : string
(** The [--format] template of [podman info] that {!val-info} reads. *)

val info : string -> (info, string) result
(** The output of [podman info --format {!info_format}]. The error completes
    "[podman info] ...". *)

val image_id : string -> (string, string) result
(** [podman image inspect --format '{{.Id}}']: 64 hexadecimal digits, which come
    back with [sha256:] before them, as Docker's image ID has, because [run] and
    [image inspect] read that form as the same image. The error completes
    "[podman image inspect] ...". *)

val ps_label : string -> string
(** The [podman ps --format] expression for the label with this key. Rows of
    version 4.9 have a map [.Labels] and no [.Label] method. *)
