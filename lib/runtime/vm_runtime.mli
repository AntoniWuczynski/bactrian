(** The container runtime behind a virtual machine sandbox (design §5.3): a
    Docker-compatible command line, which on macOS drives a Linux virtual
    machine (OrbStack, Docker Desktop), or Apple's [container], which starts a
    virtual machine for each container. Finds it, checks images, builds the
    relay ([vm/relay.c]) and removes containers.

    Every command it runs gets {!client_env}, never bactrian's own environment,
    which holds the provider keys (decision 0010). *)

type t
(** A runtime that answered. *)

val kind : t -> Bactrian_core.Experiment.sandbox_runtime

val name : t -> string
(** ["docker"] or ["container"]. *)

val exe : t -> string
(** The executable, an absolute path. *)

val client_env : t -> string array
(** [NAME=VALUE] for the variables the runtime's client reads ([PATH], [HOME],
    [USER], [LOGNAME], [TMPDIR], [XDG_RUNTIME_DIR], [XDG_CONFIG_HOME], and for
    docker the [DOCKER_*] ones that select and reach the daemon), as bactrian
    has them. *)

val arch : t -> string
(** The architecture of the runtime's machines, such as ["arm64"] or ["amd64"].
*)

val detect :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  Bactrian_core.Experiment.sandbox_runtime option ->
  (t, string) result
(** Looks the runtime up on [PATH] and asks it for its architecture: docker's
    daemon ([version]), or container's services, which must be running
    ([system status]). Unset, it is docker when that answers, otherwise
    container. The error says what is missing and how to get it, for both when
    neither works. *)

val image_id :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  t ->
  string ->
  (string, string) result
(** The image in the runtime's local store ([image inspect -- IMAGE]): docker's
    image ID, or the digest ([sha256:...]) container names it by. Never pulls:
    the error gives the command that does. *)

val create :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  t ->
  string list ->
  (unit, string) result
(** Runs container's [create] command ({!Bactrian_core.Vm_args.argv}). Gives up
    after 120 seconds. *)

val created_image :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  t ->
  string ->
  (string, string) result
(** The digest of the image a container of container's was created from
    ([inspect -- NAME]). *)

val relay_cache_dir : unit -> string
(** [$XDG_CACHE_HOME/bactrian/relay], or [~/.cache/bactrian/relay]. *)

val relay_name : t -> string
(** The cached relay's file name: a hash of {!Relay_source.text} and the
    architecture, so a changed relay is built again, and the same for both
    runtimes on one host. *)

val build_image : string
(** The Alpine image, pinned by digest, that builds the relay. *)

val build_packages : string list
(** The Alpine packages the build installs. *)

val relay :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  t ->
  (string, string) result
(** The relay binary for the runtime's architecture: the cached one, or one
    built now in a container of {!build_image} (which needs the network, for
    Alpine's packages) and cached under {!relay_cache_dir}, which is made with
    mode 0700. The directory and the binary must not be symbolic links, must
    belong to this user and must not be writable by its group or others
    ([lstat]): otherwise the relay is refused. *)

val relay_cached : t -> (bool, string) result
(** Whether the relay for the runtime's architecture is cached, and passes the
    checks of {!relay}, without building it. *)

val labels : run_id:string -> (string * string) list
(** The labels of a container bactrian starts: [bactrian.run] (the run's ID),
    [bactrian.pid] (this process) and [bactrian.host] (this host's name). *)

val remove_stale :
  process_mgr:_ Eio.Process.mgr -> clock:_ Eio.Time.clock -> t -> string list
(** Removes every container named [bactrian-...] that carries {!labels} of this
    host and a process ID that is no longer alive: one left behind when bactrian
    and its bridge were both killed. Gives the names removed. A listing that
    fails removes nothing. *)

val remove :
  process_mgr:_ Eio.Process.mgr -> clock:_ Eio.Time.clock -> t -> string -> unit
(** [remove t name] removes the container ([rm -f NAME], or container's
    [delete --force NAME]), whether or not it is there. Gives up after 30
    seconds. *)
