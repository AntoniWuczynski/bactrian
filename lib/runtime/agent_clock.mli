(** The agent's clock (design §5 step 2, §11): [libfaketime] on Linux, decided
    once per run, and the {!Bactrian_core.Clock_label} that says what happened.
    macOS gets no injection in v1, so every run there is contract-only. *)

open Bactrian_core

val override_variable : string
(** [BACTRIAN_LIBFAKETIME]: the library to preload instead of searching. *)

val candidates : string list
(** Where a Linux package puts [libfaketimeMT.so.1] (the multi-thread build, the
    one that is safe to preload into a threaded runtime). *)

val locate :
  ?candidates:string list -> override:string option -> unit -> string option
(** The library to preload: [override] when it is set and non-empty (and [None]
    if that file does not exist, rather than a search that would ignore the
    setting), else the first of [candidates] (default {!candidates}) that is a
    regular file. *)

val linux : unit -> bool
(** Whether this host is Linux. *)

val probe_executable :
  path:string option -> cwd:string -> string -> Elf_probe.kind
(** [probe_executable ~path ~cwd argv0] finds [argv0] (a name looked up in the
    directories of [path], or a file path, relative to [cwd]), follows a [#!]
    line one level (including [#!/usr/bin/env NAME], looked up in [path]), and
    classifies the executable that is left. {!Bactrian_core.Elf_probe.Not_elf}
    when anything on the way is missing or unreadable. *)

type t
(** What a run does about the clock. *)

val label : t -> Clock_label.t

val decide :
  ?candidates:string list ->
  linux:bool ->
  faketime:bool ->
  library_override:string option ->
  env:(string * string) list ->
  cwd:string ->
  agent:string list ->
  sim_now:string ->
  unit ->
  t
(** The first reason that applies, in this order: not Linux, [faketime] off, no
    library, [sim_now] not RFC 3339, then the agent's executable. [env] is the
    agent's environment, whose [PATH] finds the executable. *)

val inject : t -> (string * string) list -> (string * string) list
(** [inject t env] is [env] with the variables that start the clock at [sim_now]
    and let it run: [LD_PRELOAD] (the library first, then whatever [env] already
    preloads, once), [FAKETIME] ([@] and the Unix time of [sim_now], so the
    agent's [TZ] plays no part), [FAKETIME_FMT=%s], [FAKETIME_DONT_RESET=1] and
    [FAKETIME_DONT_FAKE_MONOTONIC=1]. [env] unchanged when the clock is
    contract-only. *)
