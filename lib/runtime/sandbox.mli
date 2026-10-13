(** The agent's operating-system sandbox at run time (design §5.3).
    {!Bactrian_core.Sandbox_policy} writes the Seatbelt profile or the
    bubblewrap command line. This module checks once that the sandbox works on
    this host ({!preflight}), wraps each agent's command in it ({!start}), and
    collects what it denied ({!collect}).

    Every path given to the policy has its symbolic links resolved first,
    because Seatbelt matches resolved paths ([/tmp] is [/private/tmp] on macOS).
    MCP stdio servers are not wrapped: they run outside the agent's sandbox, as
    part of the world. *)

type mode = [ `Auto | `Strict | `Off ]

val modes : (string * mode) list
(** [auto], [strict] and [off] with their names. *)

val mode_name : mode -> string

type label = {
  mode : mode;
  applied : bool;  (** Whether the run was sandboxed. *)
  reason : string option;  (** Why it was not. *)
}
(** What [meta.json] records under [sandbox]. *)

val label_json : label -> Yojson.Safe.t
(** [{"mode": "auto", "applied": false, "reason": "…"}], [reason] only when
    there is one. *)

type t
(** What the preflight decided for every run of a command or experiment. *)

val off : t
(** Unsandboxed by choice ([sandbox: off]). *)

val http_mode : mode -> t
(** Unsandboxed because bactrian does not start an HTTP-mode agent, so it cannot
    wrap it: no preflight is run. The label's reason is ["http mode"]. *)

val label : t -> label

type settings = {
  mode : mode;
  allow_write : string list;  (** [sandbox.allow_write], absolute. *)
  deny_read : string list;  (** [sandbox.deny_read], absolute. *)
  bactrian_exe : string;
      (** The bactrian executable, re-run inside the sandbox as
          [__sandbox-probe] and, on Linux, [__sandbox-init]. *)
  bactrian_dir : string;
      (** The [.bactrian] directory, created if missing. Its parent is the
          project root, whose [.env] files the agent may not read. *)
}

val preflight :
  env:Eio_unix.Stdenv.base ->
  warn:(string -> unit) ->
  settings ->
  (t, string) result
(** Under [`Off], {!off} without a probe. Otherwise runs
    [bactrian __sandbox-probe] inside the sandbox (see {!Sandbox_probe}) with a
    probe listener of its own. On Linux it first looks for [bwrap] on [PATH] and
    checks that it is 0.5 or later, and explains a failed probe by Ubuntu's
    AppArmor restriction when [kernel.apparmor_restrict_unprivileged_userns] is
    1, with both fixes (it never runs [sudo]). When the sandbox does not work,
    [`Auto] calls [warn] with the cause and the fix and gives an unsandboxed
    {!t} labelled with that reason, and [`Strict] gives [Error] with the same
    message (exit 69). *)

type run = {
  run_id : string;
  run_dir : string;
      (** Absolute. Holds [sandbox.log], the Linux forwarder's violation log,
          which no sandboxed process can write: only the forwarder holds a
          descriptor on it. *)
  state_dir : string;
  temp_dir : string;
  port : int;  (** The model proxy's loopback port. *)
  listen_unix : string -> (unit, string) result;
      (** On Linux, serves the model proxy for this run only on a Unix socket at
          the path given, until the run's switch ends. *)
  preload : string option;
      (** The library the agent's [LD_PRELOAD] names, as a resolved absolute
          path. On Linux it is bound read-only into the sandbox, so it is there
          even under [/tmp], [/dev/shm] or [/run]. *)
}

type session = {
  argv : string list;  (** The agent's command, wrapped in the sandbox. *)
  env : (string * string) list;  (** Variables to add to the agent's. *)
  fds : (int * Eio_unix.Fd.t) list;  (** Descriptors the command inherits. *)
}

type violations =
  | Collected of Bactrian_core.Sandbox_log.violation list
  | Not_collected of string  (** Why the denials could not be read. *)

type started

val start :
  sw:Eio.Switch.t ->
  t ->
  run ->
  string list ->
  (session * started, string) result
(** [start ~sw t run agent] wraps [agent] for one run. Unsandboxed, the session
    is [agent] unchanged. On Linux it also makes the run's Unix socket in a new
    directory under the system's temporary directory (short enough for the
    socket path limit), removed when [sw] ends. *)

val preload_hidden : t -> string -> bool
(** [preload_hidden t library] is whether the library, a resolved absolute path,
    would not be readable inside the sandbox of [t] although it is bound there:
    on Linux, when it lies under a path the deny-list masks after the bind
    ([sandbox.deny_read] among them). Never when unsandboxed or on macOS. *)

val collect : env:Eio_unix.Stdenv.base -> started -> violations
(** After the agent exited: on macOS, the Seatbelt denials logged with the run's
    tag since it started, read with [log show] until the end marker is there and
    two reads a second apart agree (at most about 30 seconds, see
    {!Bactrian_core.Sandbox_log.settle}); on Linux, the forwarder's relay
    failures from [sandbox.log]. An unsandboxed run has none. *)
