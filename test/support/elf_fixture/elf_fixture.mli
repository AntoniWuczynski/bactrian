(** Minimal ELF files as byte strings: a header, a program header table and one
    body per segment. Enough for {!Bactrian_core.Elf_probe}, not for a loader.
*)

val pt_load : int
val pt_dynamic : int
val pt_interp : int
val pt_note : int

val elf : ?class64:bool -> ?machine:int -> (int * string) list -> string
(** [elf segments] is a little-endian ELF file with one program header per
    [(type, body)], whose body follows the table. 64-bit unless [class64] is
    [false]. Its [e_machine] is [machine], by default x86-64 (62) for 64-bit and
    i386 (3) for 32-bit. *)

val note : name:string -> kind:int -> desc:string -> string
(** One ELF note. [name] is written as given, so it carries its own NUL bytes.
*)

val interp : string
(** The body of a [PT_INTERP] segment. *)

val go_note : string
(** The Go build ID note: name ["Go\000\000"], type 4. *)

val gnu_note : string
(** A GNU build ID note: name ["GNU\000"], type 3. *)
