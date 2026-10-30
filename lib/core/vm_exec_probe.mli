(** What the relay's [--probe-exec NAME] reports about an image (design §5.3,
    decision 0021): the C library its programs are linked against, and the start
    of the file a command name leads to. Pure. *)

type libc =
  | Musl  (** The image carries [ld-musl-ARCH.so.1], as Alpine does. *)
  | Glibc  (** The image carries [ld-linux-ARCH.so.N], as Debian does. *)

val libc_name : libc -> string
(** ["musl"] or ["glibc"], the names of the library directories. *)

type report = {
  libc : libc option;  (** [None] for an image with neither loader. *)
  executable : string option;
      (** The first {!Elf_probe.prefix_length} bytes of the regular executable
          file the name led to, [None] when none was found. *)
}

val of_line : string -> (report, string) result
(** Decodes the JSON line [{"libc","found","prefix"}], whose prefix is
    hexadecimal. [Error] describes what is wrong with it. *)
