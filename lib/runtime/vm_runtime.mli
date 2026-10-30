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
(** Runs container's [create] command ({!Bactrian_core.Vm_args.val-argv}). Gives
    up after 120 seconds. *)

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

val faketime_name : t -> string
(** The directory, beside the relay, that holds the libfaketime libraries built
    for the runtime's architecture: a hash of the pinned source, the builder
    images and the build scripts, so a change is built again. It has a directory
    for each C library ([musl], [glibc]) with [libfaketimeMT.so.1] in it. *)

val faketime_archive : string
(** libfaketime 0.9.13 (published 2026-08-31) as GitHub serves it, kept in the
    cache once downloaded and checked against its SHA-256 digest each time it is
    used. *)

val glibc_build_image : string
(** The Debian image, pinned by digest, that builds the glibc library. The musl
    one builds in {!build_image}. *)

val faketime :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  t ->
  Bactrian_core.Vm_exec_probe.libc ->
  (string, string) result
(** The directory ({!faketime_name} under {!relay_cache_dir}) that holds the
    library for that C library, built now if it is not cached: the source is
    downloaded and checked in an Alpine container, then compiled in
    {!build_image} (musl) or {!glibc_build_image} (glibc). It needs the network
    once. The directories and the library must not be symbolic links, must
    belong to this user and must not be writable by its group or others, as for
    {!relay}. *)

val faketime_cached :
  t -> Bactrian_core.Vm_exec_probe.libc -> (bool, string) result
(** Whether that library is cached and passes the checks of {!faketime}. *)

val labels : run_id:string -> (string * string) list
(** The labels of a container bactrian starts: [bactrian.run] (the run's ID),
    [bactrian.pid] (this process) and [bactrian.host] (this host's name). *)

val owner_gone : pid:string -> host:string -> bool
(** Whether [pid] (a process ID, as {!labels} write it) is of this host ([host])
    and no longer alive, checked by the process ID alone ([kill -0]). A process
    ID reused since is taken for alive. *)

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
