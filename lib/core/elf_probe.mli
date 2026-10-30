(** Whether an executable can be given a preloaded library (design §11): a
    reader for the few fields of an ELF file that decide it.

    [LD_PRELOAD] reaches a program only through the dynamic loader, so a
    statically linked executable ignores it. A Go program reads the clock
    through the vDSO and its own system calls, so it ignores it even when it is
    dynamically linked. *)

type kind =
  | Dynamic
      (** Has a [PT_INTERP] segment: the dynamic loader starts it, and a
          preloaded library is loaded. A static-PIE executable has a
          [PT_DYNAMIC] segment but no [PT_INTERP], so it is {!Static}. *)
  | Static  (** An ELF file with no dynamic loader. *)
  | Go
      (** Carries the Go build ID note (a [PT_NOTE] segment holding a note named
          [Go] of type 4, the [.note.go.buildid] section), whatever its linking.
          A binary built without a build ID (as with [-ldflags=-buildid=]) is
          not recognised. *)
  | Not_elf
      (** Not a little-endian 32 or 64-bit ELF file, or its program headers are
          cut off. Every ELF target bactrian supports is little-endian. *)

val prefix_length : int
(** How much of the start of a file {!probe} needs: the program header table and
    the notes sit near the start of the file. A segment or note that lies beyond
    the prefix is not seen, which can only miss a Go build ID, never mistake a
    kind. *)

val probe : string -> kind
(** [probe prefix] classifies a file from its first {!prefix_length} bytes (or
    all of them, if it is shorter). *)

val interpreter : string -> string option
(** The path the [PT_INTERP] segment names, such as [/lib/ld-musl-aarch64.so.1]
    or [/lib64/ld-linux-x86-64.so.2], read from the first {!prefix_length} bytes
    of a file. [None] for a file with no such segment, one that is not a
    little-endian ELF file, or an interpreter cut off by the end of the prefix.
*)

val machine : string -> int option
(** The [e_machine] field of a little-endian ELF file: 62 for x86-64, 183 for
    AArch64. [None] for a file that is not one. *)
