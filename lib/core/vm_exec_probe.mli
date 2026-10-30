(** What the relay's [--probe-exec NAME] reports about an image (design §5.3,
    decision 0021): whether its glibc is new enough for libfaketime, and the
    start of the file a command name leads to. Pure. *)

type libc =
  | Musl  (** The executable's interpreter is [ld-musl-ARCH.so.1]. *)
  | Glibc  (** The executable's interpreter is [ld-linux-ARCH.so.N]. *)

val libc_name : libc -> string
(** ["musl"] or ["glibc"], the names of the library directories. *)

val glibc_floor : string
(** ["2.34"]: the oldest glibc libfaketime's glibc build loads in. It is built
    on Debian bookworm, whose glibc is 2.36, and needs only [GLIBC_2.34]. *)

type report = {
  glibc_2_34 : bool;
      (** A [libc.so.6] of the image defines [GLIBC_2.34], so is at least
          {!glibc_floor}. *)
  executable : string option;
      (** The first {!Elf_probe.prefix_length} bytes of the regular executable
          file the name led to, [None] when none was found. *)
}

val of_line : string -> (report, string) result
(** Decodes the JSON line [{"glibc_2_34","found","prefix"}], whose prefix is
    hexadecimal. [Error] describes what is wrong with it. *)

val libc : arch:string -> report -> libc option
(** The C library libfaketime is preloaded from for the executable of a report,
    on a runtime whose machines are [arch] (["arm64"] or ["amd64"]): the one its
    [PT_INTERP] names ([ld-musl-...] is musl, [ld-linux...] is glibc), and for
    glibc only when the image's is at least {!glibc_floor}. The image's other
    loaders play no part, because a library of the wrong C library, preloaded,
    stops the program from starting. The library is built for [arch], so an
    executable of another architecture (an amd64 image run under emulation on
    arm64) has none either. [None] for a static executable, one that is not ELF,
    another loader, another architecture or glibc older than the floor. *)
