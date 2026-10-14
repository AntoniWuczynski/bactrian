(** The virtual machine backend of {!Sandbox} (design §5.3): the agent runs in a
    Linux container with no network, started through {!Vm_runtime}, and reaches
    bactrian only through the relay's frames ({!Vm_bridge}). *)

type t
(** A runtime that works, the image that is present, and the relay built for the
    runtime's architecture. *)

val runtime_name : t -> string
val image : t -> string

val prepare :
  env:Eio_unix.Stdenv.base ->
  Bactrian_core.Experiment.sandbox_runtime option ->
  image:string ->
  (t, string) result
(** Finds the runtime, checks that the image is present (it is never pulled) and
    builds the relay if it is not cached. *)

type layout = {
  project_root : string;
  bactrian_dir : string;
  allow_write : string list;
      (** Resolved; those that do not exist are skipped. *)
  deny : (string * Bactrian_core.Sandbox_policy.kind) list;
      (** The deny-list with what is at each path. *)
}
(** What every container of a command or experiment shares. *)

val probe :
  env:Eio_unix.Stdenv.base ->
  t ->
  layout ->
  dir:string ->
  denied:string ->
  (unit, string) result
(** Runs the relay's probe in a container built like a run's, in [dir] (its
    [state] and [tmp] made here, under the project), with a probe listener of
    its own, and judges the report ({!Bactrian_core.Vm_probe.judge}). [denied]
    is a file under a directory in [layout.deny]. The container is removed
    after. *)

type session = {
  argv : string list;
      (** [bactrian __vm-bridge forward=PORT container=NAME -- RUNTIME run ...]
      *)
  env : (string * string) list;  (** The bridge's whole environment. *)
}

val start :
  sw:Eio.Switch.t ->
  t ->
  layout ->
  bactrian_exe:string ->
  run_id:string ->
  run_dir:string ->
  state_dir:string ->
  temp_dir:string ->
  cwd:string ->
  port:int ->
  agent_env:(string * string) list ->
  string list ->
  (session, string) result
(** Writes the agent's environment, minus [PATH] (the image's own applies), to
    [agent.env] in [run_dir] (mode 0600), and gives the command that runs the
    agent in a container named [bactrian-RUN_ID] with [port] forwarded to the
    host's loopback. When [sw] ends the container is removed ([rm -f]), even if
    the bridge was killed, and so is [agent.env]. *)
