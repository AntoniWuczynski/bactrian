(** Whether a run's clock was enforced (design §5 step 2, §11). Every run has
    [SIM_NOW] and [TZ] in its environment, which is the contract. The clock is
    [Enforced] when [libfaketime] was also injected and the agent's executable
    can take it, and [Contract_only] otherwise, with the reason. *)

type reason =
  | Macos  (** Not Linux: bactrian injects nothing there. *)
  | Faketime_off  (** The experiment says [clock: {faketime: off}]. *)
  | Library_not_found  (** No [libfaketimeMT.so.1] on this host. *)
  | Library_hidden
      (** The sandbox hides the library from the agent: it lies under a path the
          sandbox's deny-list masks. *)
  | Setuid_sandbox
      (** The Linux sandbox's [bwrap] is setuid, so the dynamic loader runs it
          in secure mode and drops [LD_PRELOAD] before the agent starts. *)
  | Invalid_sim_now  (** [SIM_NOW] is not an RFC 3339 time since 1970. *)
  | Static_binary  (** A statically linked executable ignores [LD_PRELOAD]. *)
  | Go_binary  (** Go reads the clock without the C library. *)
  | Unreadable_executable
      (** The agent's executable was not found, not readable, not an ELF file
          after following a [#!] line one level, or it is not little-endian. *)
  | Http_mode
      (** An [http] agent is not started by bactrian, so there is no process to
          inject [libfaketime] into and no executable to probe. *)
  | Vm
      (** The agent runs in a virtual machine sandbox, whose image has no
          [libfaketime] that bactrian knows of. *)

type t = Enforced | Contract_only of reason

val reason_name : reason -> string
(** [macos], [faketime-off], [library-not-found], [library-hidden],
    [setuid-sandbox], [invalid-sim-now], [static-binary], [go-binary],
    [unreadable-executable], [http-mode] or [vm]. *)

val of_kind : Elf_probe.kind -> t
(** The label of a run that injected the library, given the kind of the agent's
    executable. *)

val to_clock : t -> Trace.clock
(** What the trace and cassette headers record. *)

val to_json : t -> Yojson.Safe.t
(** [{"mode": "enforced"}] or
    [{"mode": "contract_only", "reason": "static-binary"}], as in [meta.json].
*)
