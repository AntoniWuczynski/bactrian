(** The operating-system sandbox of agents and checkers at run time (design
    §5.3). {!Bactrian_core.Sandbox_policy} writes the Seatbelt profile or the
    bubblewrap command line, and {!Sandbox_vm} runs an agent in a Linux
    container instead (the [vm] backend). This module checks once that the
    sandbox works on this host ({!preflight}), wraps each agent's command in it
    ({!start}) and each checker's ({!checker}), and collects what it denied the
    agent ({!collect}). Checkers always use the host's own sandbox (Seatbelt or
    bubblewrap), never the virtual machine.

    Every path given to the policy has its symbolic links resolved first,
    because Seatbelt matches resolved paths ([/tmp] is [/private/tmp] on macOS).
    MCP stdio servers are not wrapped: they run outside the agent's sandbox, as
    part of the world. *)

type mode = [ `Auto | `Strict | `Off ]

val modes : (string * mode) list
(** [auto], [strict] and [off] with their names. *)

val mode_name : mode -> string

type backend = Bactrian_core.Experiment.sandbox_backend

val backends : (string * backend) list
(** [auto], [vm], [seatbelt] and [bubblewrap] with their names. *)

val host_backend : unit -> [ `Seatbelt | `Bubblewrap ]
(** The host's own sandbox: [`Seatbelt] on macOS, [`Bubblewrap] elsewhere. *)

type applied_backend =
  | Seatbelt_backend
  | Bubblewrap_backend
  | Vm_backend of { runtime : string; image : string }

type label = {
  mode : mode;
  applied : bool;  (** Whether the run was sandboxed. *)
  reason : string option;  (** Why it was not. *)
  backend : applied_backend option;  (** What sandboxed it. *)
}
(** What [meta.json] records under [sandbox]. *)

val label_json : label -> Yojson.Safe.t
(** [{"mode": "auto", "applied": false, "reason": "…"}], [reason] only when
    there is one, and for a sandboxed run [backend] ([seatbelt], [bubblewrap] or
    [vm], with [{"vm": {"runtime", "image"}}]). *)

type t
(** What the preflight decided for every run of a command or experiment. *)

val off : t
(** Unsandboxed by choice ([sandbox: off]). *)

val http_mode : mode -> t
(** Unsandboxed because bactrian does not start an HTTP-mode agent, so it cannot
    wrap it: no preflight is run. The label's reason is ["http mode"]. *)

val label : t -> label

val vm : t -> bool
(** Whether agents run in the virtual machine sandbox. *)

type settings = {
  mode : mode;
  backend : backend;
  image : string option;  (** The image of the [vm] backend. *)
  runtime : Bactrian_core.Experiment.sandbox_runtime option;
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
(** Under [`Off], {!off} without a probe. Otherwise it picks the backend and
    probes it once.

    [backend: auto] is bubblewrap on Linux. On macOS it is the virtual machine
    when [image] is set and the runtime works ([`Strict] stops when either is
    missing, naming [backend: seatbelt] and what it exposes as the alternative),
    and otherwise, under [`Auto], Seatbelt, with a warning that the agent can
    read the launch environment of the user's other processes (decision 0010).
    Under [`Auto] a [vm] backend that does not work gives way to the host's own
    sandbox, with a warning. [seatbelt] off macOS and [bubblewrap] off Linux are
    errors.

    Seatbelt and bubblewrap run [bactrian __sandbox-probe] inside the sandbox
    (see {!Sandbox_probe}) with a probe listener of its own. On Linux it first
    looks for [bwrap] on [PATH] and checks that it is 0.5 or later, and explains
    a failed probe by Ubuntu's AppArmor restriction when
    [kernel.apparmor_restrict_unprivileged_userns] is 1, with both fixes (it
    never runs [sudo]). The virtual machine runs the relay's probe in a
    container ({!Sandbox_vm.probe}), after checking the runtime and the image
    and building the relay if needed. When the sandbox does not work, [`Auto]
    calls [warn] with the cause and the fix and gives an unsandboxed {!t}
    labelled with that reason, and [`Strict] gives [Error] with the same message
    (exit 69). *)

val backend_available : backend -> (unit, string) result
(** [Error] for [seatbelt] off macOS and [bubblewrap] on macOS: a configuration
    error (exit 64). *)

val checker_settings : settings -> settings
(** The settings with the backend checkers use: the host's own sandbox, unless
    [seatbelt] or [bubblewrap] is named. *)

type run = {
  run_id : string;
  run_dir : string;
      (** Absolute. Holds [sandbox.log], the Linux forwarder's violation log,
          which bactrian writes from a pipe whose write end only the forwarder
          holds (see {!Sandbox_init.log_variable}). *)
  state_dir : string;
  temp_dir : string;
  port : int;
      (** The model proxy's loopback port for this run: its own on macOS (see
          {!own_port}), the shared one's number on Linux. *)
  listen_unix : string -> (unit, string) result;
      (** On Linux, serves the model proxy for this run only on a Unix socket at
          the path given, until the run's switch ends. *)
  preload : string option;
      (** The library the agent's [LD_PRELOAD] names, as a resolved absolute
          path. On Linux it is bound read-only into the sandbox, so it is there
          even under [/tmp], [/dev/shm] or [/run]. *)
  cwd : string;  (** The agent's working directory. *)
  agent_env : (string * string) list;
      (** The agent's environment, which the virtual machine passes to the
          container through a file, since its command is the bridge's. *)
}

val strips_preload : t -> bool
(** Whether the sandbox drops the agent's [LD_PRELOAD]: on Linux when [bwrap] is
    setuid, because the dynamic loader then runs it in secure mode and removes
    [LD_PRELOAD] (and [TMPDIR], among others) from the environment it passes on.
    The clock is then contract-only ({!Agent_clock.decide}). *)

val own_port : t -> bool
(** Whether each run needs a loopback port of the model proxy that serves it
    alone ({!Model_proxy.listen_tcp}), given as {!type-run}'s [port]: on macOS,
    where the profile allows the agent that port and no other, so a token read
    from another run's launch environment cannot be used. On Linux the agent's
    network namespace reaches only its own forwarder, so the shared port's
    number serves. *)

val mcp_warning : t -> Mcp_proxy.config list -> string option
(** On macOS, where an agent can read the launch-time arguments and environment
    of any process of the same user that is not an Apple platform binary
    (decision 0010), the warning to print once when stdio servers that will be
    started have arguments or [env]: they may hold credentials, and the agent
    can read them. [None] elsewhere and for servers that only replay or block.
*)

type session = {
  argv : string list;  (** The agent's command, wrapped in the sandbox. *)
  env : (string * string) list;
      (** Variables to add to the agent's, or with [replaces_env] the command's
          whole environment. *)
  replaces_env : bool;
      (** In the virtual machine: the command is [bactrian __vm-bridge], which
          gets the runtime client's environment, and the agent's own reaches the
          container through [agent.env]. *)
  fds : (int * Eio_unix.Fd.t) list;  (** Descriptors the command inherits. *)
}

(** What kinds of denial a run's violations can include. *)
type coverage = Sandbox_violations.coverage =
  | Seatbelt_log
      (** macOS: every kind Seatbelt logs, read from the system log, which can
          drop denials under load. *)
  | Relay_failures_only
      (** Linux: only the connections the forwarder could not relay. Denied
          connections, writes and reads fail inside the agent, unrecorded. *)

val coverage_name : coverage -> string
(** ["seatbelt-log"] or ["relay-failures-only"]. *)

val coverage_of_name : string -> coverage option

type violations = Sandbox_violations.violations =
  | Collected of {
      items : Bactrian_core.Sandbox_log.violation list;
      coverage : coverage;
      gaps : string list;
          (** Why [items] may miss some, empty when they are complete for their
              [coverage]. On macOS always at least that the system log can drop
              denials, which makes [items] a lower bound. *)
    }
  | Not_collected of string  (** Why the denials could not be read. *)

val unsandboxed_violations : violations
(** [Not_collected "the run was not sandboxed"]. *)

val violations_json : violations -> Yojson.Safe.t
(** [{"collected": true, "coverage": "seatbelt-log", "complete": false,
     "reason": "…", "items": [{"kind", "detail"}]}]: [complete] is whether
    [gaps] is empty, and [reason] joins the gaps. Not collected:
    [{"collected": false, "reason": "…", "items": []}]. *)

val describe_violations : violations -> string
(** One line for people, such as
    ["at least 2 (the system log can drop denials, so this is a lower bound)"]
    or ["0 relay failures (Linux records no other kind of denial)"]. *)

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
    socket path limit), removed when [sw] ends. In the virtual machine the
    session runs [bactrian __vm-bridge] around the runtime's [run] command
    ({!Sandbox_vm.start}), and the container is removed when [sw] ends. *)

val checker :
  t -> temp_dir:string -> string list -> (string list, string) result
(** [checker t ~temp_dir argv] is a checker's command wrapped in the sandbox of
    [t] (design §5.3): no network at all, not even loopback, writes only to
    [temp_dir], and reads denied as for the agent, except that nothing under
    [.bactrian/runs] is readable and [sandbox.allow_write] does not apply. On
    Linux it runs under [bactrian __sandbox-init] with nothing to forward.
    Unsandboxed, [argv] unchanged. What the sandbox denies a checker is not
    collected. *)

val preload_hidden : t -> string -> bool
(** [preload_hidden t library] is whether the library, a resolved absolute path,
    would not be readable inside the sandbox of [t] although it is bound there:
    on Linux, when it lies under a path the deny-list masks after the bind
    ([sandbox.deny_read] among them). Never when unsandboxed or on macOS. *)

val collect : env:Eio_unix.Stdenv.base -> started -> violations
(** After the agent exited: on macOS, the Seatbelt denials logged with the run's
    tag since it started, read with [log show] until the end marker is there and
    two reads a second apart agree (at most about 30 seconds, see
    {!Bactrian_core.Sandbox_log.settle}), always with the gap that the log can
    drop denials, and a second one when the reads had not settled by then; on
    Linux and in the virtual machine, the relay failures from [sandbox.log]. An
    unsandboxed run gives {!unsandboxed_violations}. *)
