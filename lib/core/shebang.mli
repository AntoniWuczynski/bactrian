(** The program a script's [#!] line starts (design §11). *)

type t =
  | Interpreter of string  (** The path the line names. *)
  | Env_program of string
      (** [#!/usr/bin/env NAME]: the program [env] finds on [PATH], after
          skipping its options and [NAME=value] assignments. *)

val parse : string -> t option
(** [parse prefix] reads the first line of a file. [None] when the file does not
    start with [#!], or the line names nothing. *)
