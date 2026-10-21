(** The virtual machine backend of {!Sandbox} (design §5.3): the agent runs in a
    Linux container with no network, started through {!Vm_runtime}, and reaches
    bactrian only through the relay's frames ({!Vm_bridge}). *)

type t
(** A runtime that works, the image that is present, and the relay built for the
    runtime's architecture. *)

val runtime_name : t -> string

val image : t -> string
(** The image as configured. *)

val image_id : t -> string
(** The ID the image had at {!prepare} (container's digest of it), which every
    container runs, so a tag moved since cannot change what the agent runs in.
    Docker runs the ID itself. container cannot, so its containers are created
    from the reference and refused unless they hold this digest. *)

val prepare :
  env:Eio_unix.Stdenv.base ->
  Bactrian_core.Experiment.sandbox_runtime option ->
  image:string ->
  (t, string) result
(** Finds the runtime, removes the containers that a bactrian killed with its
    bridge left behind ({!Vm_runtime.remove_stale}), checks that the image is
    present (it is never pulled) and builds the relay if it is not cached. *)

type layout = {
  project_root : string;
  bactrian_dir : string;
  allow_write : string list;
      (** Resolved; those that do not exist are skipped. *)
  read_only : string list;
      (** Directories in the run's own that are shown read-only (the probe's
          directory of a masked file, which container requires). *)
  deny : (string * Bactrian_core.Sandbox_policy.kind) list;
      (** The deny-list with what is at each path. *)
}
(** What every container of a command or experiment shares. *)

val probe :
  env:Eio_unix.Stdenv.base ->
  t ->
  layout ->
  dir:string ->
  Bactrian_core.Vm_args.probe ->
  (unit, string) result
(** Runs the relay's probe in a container built like a run's, for a run whose
    directory is [dir] under [.bactrian/runs] (its [state] and [tmp] already
    made), with a probe listener of its own, and judges the report
    ({!Bactrian_core.Vm_probe.judge}) and that neither file it tried to write
    outside appeared on the host. The paths' [denied] is a file under a
    directory, and [masked] a file, of [layout.deny]. The container is removed
    after. *)

type session = {
  argv : string list;
      (** [bactrian __vm-bridge forward=PORT container=NAME -- RUNTIME run ...],
          or for container
          [... container=NAME signals=kill -- container start -a -i NAME]. *)
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
    host's loopback. For container it first makes the staging directories
    ({!Bactrian_core.Vm_args.stage}) under [run_dir], creates the container and
    checks its image ({!image_id}). When [sw] ends the container is removed,
    even if the bridge was killed, and so are [agent.env] and the staging
    directories. *)
