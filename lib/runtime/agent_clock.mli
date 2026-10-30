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
    regular file. The path is absolute with symbolic links resolved, because the
    sandbox binds the library at the path [LD_PRELOAD] names. *)

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

val http_mode : t
(** The clock of an [http] agent, which bactrian does not start: contract-only
    with {!Bactrian_core.Clock_label.Http_mode}, and {!inject} adds nothing. *)

val library : t -> string option
(** The host file the run preloads, [None] when the clock is contract-only and
    in the virtual machine, where the relay preloads the image's own. *)

val epoch : t -> int option
(** The Unix time the clock starts at, [None] when it is contract-only. *)

(** What the virtual machine's relay found in the image (decision 0021). *)
type vm =
  | Vm_unprobed  (** Not looked at, because the clock is not wanted. *)
  | Vm_unavailable
      (** The image could not be probed, or libfaketime built for its C library.
      *)
  | Vm_image of { library : bool; executable : Elf_probe.kind }
      (** [library]: libfaketime is there for the image's C library.
          [executable] is what the agent's command leads to in the image. *)

val decide :
  ?candidates:string list ->
  ?vm:vm ->
  ?hidden:(string -> bool) ->
  ?stripped:bool ->
  linux:bool ->
  faketime:bool ->
  library_override:string option ->
  env:(string * string) list ->
  cwd:string ->
  agent:string list ->
  sim_now:string ->
  unit ->
  t
(** For a command agent, whose argv is [agent]: the first reason that applies,
    in this order: not Linux, [faketime] off, no library, [hidden] says the
    sandbox would hide the library (default: never), [stripped] says the sandbox
    drops [LD_PRELOAD] (default: no), [sim_now] not RFC 3339, then the agent's
    executable. [env] is the agent's environment, whose [PATH] finds the
    executable.

    With [vm], the agent runs in the virtual machine sandbox, on any host, and
    the host's own library, sandbox and executable play no part: [faketime] off,
    [sim_now] not RFC 3339, the image not probed ([Vm_unprobed],
    [Vm_unavailable]: reason [vm]), then the executable in the image and the
    library for its C library ([library-not-found] where there is none). *)

val inject : t -> (string * string) list -> (string * string) list
(** [inject t env] is [env] with the variables that start the clock at [sim_now]
    and let it run: [LD_PRELOAD] (the library first, then whatever [env] already
    preloads, once), [FAKETIME] ([@] and the Unix time of [sim_now], so the
    agent's [TZ] plays no part), [FAKETIME_FMT=%s] and [FAKETIME_DONT_RESET=1].
    The monotonic clock is faked along with the wall clock, as libfaketime does
    by default: libfaketime 0.9.10 breaks absolute sleeps on [CLOCK_MONOTONIC]
    (Python 3.11's [time.sleep]) when only the wall clock moves. An [env] that
    carries [FAKETIME_DONT_FAKE_MONOTONIC] keeps it, for a runtime that needs
    real monotonic time. [env] unchanged when the clock is contract-only, and in
    the virtual machine, where the relay sets the variables
    ([Vm_args.faketime]). *)
