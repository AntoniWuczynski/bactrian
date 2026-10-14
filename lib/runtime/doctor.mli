(** [bactrian doctor] (design §9): the checks that decide whether a run can
    start on this host, reported one by one without running an agent. *)

open Bactrian_core

type status = Pass | Warn | Fail | Skip

val status_name : status -> string
(** [ok], [warn], [fail] or [skip]. *)

type check = { name : string; status : status; message : string }
(** [message] may span several lines (a sandbox fix has commands). *)

type report = { checks : check list; ok : bool }
(** [ok] holds when no check is [Fail]. *)

val report : check list -> report

val to_json : report -> Yojson.Safe.t
(** [{"checks": [{"name", "status", "message"}], "ok": bool}]. *)

val render_text : report -> string
(** One line per check, [STATUS NAME: MESSAGE], with the lines after the first
    of a long message indented. *)

type target = {
  agent : Experiment.agent;
  dir : string;  (** The experiment file's directory, where the agent runs. *)
  sandbox : Sandbox.mode;
  backend : Sandbox.backend;
  image : string option;
  runtime : Experiment.sandbox_runtime option;
  faketime : bool;
  funded : (Model_proxy.provider * string) list;
      (** The providers the experiment calls live ({!Executor.funded}). *)
}
(** What an experiment says that the checks depend on. *)

val target : dir:string -> Experiment.t -> target

(** {2 The checks}

    Each takes what it depends on as arguments, so a test can state the host. *)

val key_checks :
  keys:Model_proxy.provider list ->
  funded:(Model_proxy.provider * string) list option ->
  check list
(** One check per provider, named [key-<provider>], naming the variable and
    never its value. [keys] are the providers whose variable is set and
    non-empty. Without an experiment ([funded = None]) a missing key is [Warn].
    With one, it is [Fail] for a funded provider and [Skip] for the others. *)

val upstream_checks : upstream:(Model_proxy.provider -> Uri.t) -> check list
(** One check per provider whose upstream is not the real one
    ({!Run.overridden_upstreams}), named [upstream-<provider>] and showing the
    URL. [Pass], or [Warn] for plain [http] to a host other than this machine,
    because the real API key goes there unencrypted. *)

val agent_check : path:string option -> cwd:string -> Experiment.agent -> check
(** A command agent's executable must be found ({!Proc_util.find_executable}).
    An HTTP agent's URL must be an [http] or [https] URL with a host, and is
    shown without its user information, query and fragment, which can hold
    credentials. Nothing connects. *)

val loopback_check : unit -> check
(** Binds 127.0.0.1 on a port the system picks, and closes it. *)

val sandbox_check :
  mode:Sandbox.mode ->
  http_agent:bool ->
  probe:(unit -> (unit, string) result) ->
  check
(** [probe] runs only when the sandbox would apply. An unavailable sandbox is
    [Fail] under [`Strict] and [Warn] under [`Auto]. [`Off] and an HTTP agent
    are [Skip]. *)

type vm_host = {
  macos : bool;
  detect : unit -> (string, string) result;
      (** The runtime's name and architecture, or why it does not work. *)
  image_present : string -> (unit, string) result;
  relay_cached : unit -> bool;
}
(** What the [vm] check asks of this host. *)

val vm_host : env:Eio_unix.Stdenv.base -> vm_host
(** This host, through {!Vm_runtime}. Nothing is pulled or built. *)

val vm_check : host:vm_host -> target option -> check
(** The virtual machine sandbox: the runtime works ([info]), the experiment's
    image is present locally (never pulled), and whether the relay is built yet.
    [Fail] under [strict] (and [Warn] under [auto]) when the experiment's agent
    would run in the virtual machine and something is missing. On macOS without
    one it is [Warn], because the agent then runs under Seatbelt, which leaves
    the launch environments of the user's other processes readable; the message
    names OrbStack and Docker Desktop. [Skip] when the experiment's agent is not
    sandboxed, or the experiment names [seatbelt] or [bubblewrap]. *)

val clock_check :
  linux:bool ->
  library_override:string option ->
  path:string option ->
  target option ->
  check
(** With a target, the label its command agent would get
    ({!Agent_clock.decide}). Without, whether this host can enforce the clock at
    all. Contract-only on macOS is expected, so it is [Pass]. *)

val tls_check :
  available:bool ->
  upstream:(Model_proxy.provider -> Uri.t) ->
  target option ->
  check
(** [Pass] when the build has the TLS stack. Otherwise [Skip], or [Fail] when
    the experiment calls a provider whose [upstream] is an [https] URL, or an
    agent at one. An [http] upstream does not count. *)

val egress_checks : check list
(** The CA and the Node version, which only matter once egress is intercepted:
    both [Skip]. *)

val run :
  env:Eio_unix.Stdenv.base ->
  bactrian_exe:string ->
  tls_available:bool ->
  upstream:(Model_proxy.provider -> Uri.t) ->
  keys:(Model_proxy.provider * string) list ->
  path:string option ->
  target option ->
  check list
(** Every check, in the order they are reported. An agent that would run in the
    virtual machine is not looked up on this host ([agent] is [Skip]). [keys] is
    {!Run.keys_from_environment}: only which providers appear in it is used. *)
