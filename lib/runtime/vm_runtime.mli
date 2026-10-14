(** The container runtime behind a virtual machine sandbox (design §5.3): a
    Docker-compatible command line, which on macOS drives a Linux virtual
    machine (OrbStack, Docker Desktop). Finds it, checks images, builds the
    relay ([vm/relay.c]) and removes containers.

    Every command it runs gets {!client_env}, never bactrian's own environment,
    which holds the provider keys (decision 0010). *)

type t
(** A runtime whose daemon answered. *)

val name : t -> string
(** ["docker"]. *)

val exe : t -> string
(** The executable, an absolute path. *)

val client_env : t -> string array
(** [NAME=VALUE] for the variables the runtime's client reads ([PATH], [HOME],
    [USER], [LOGNAME], [TMPDIR], [XDG_RUNTIME_DIR], [XDG_CONFIG_HOME] and the
    [DOCKER_*] ones that select and reach the daemon), as bactrian has them. *)

val arch : t -> string
(** The daemon's architecture, such as ["arm64"] or ["amd64"]. *)

val detect :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  Bactrian_core.Experiment.sandbox_runtime option ->
  (t, string) result
(** Looks the runtime up on [PATH] ([docker]: the only one verified so far) and
    asks the daemon for its architecture ([version]). The error says what is
    missing and how to get it. *)

val image_id :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  t ->
  string ->
  (string, string) result
(** The ID ([sha256:...]) of the image in the runtime's local store
    ([image inspect -- IMAGE]). Never pulls: the error gives the command that
    does. *)

val relay_cache_dir : unit -> string
(** [$XDG_CACHE_HOME/bactrian/relay], or [~/.cache/bactrian/relay]. *)

val relay_name : t -> string
(** The cached relay's file name: a hash of {!Relay_source.text} and the
    architecture, so a changed relay is built again. *)

val build_image : string
(** The Alpine image, pinned by digest, that builds the relay. *)

val relay :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  t ->
  (string, string) result
(** The relay binary for the runtime's architecture: the cached one, or one
    built now in a container of {!build_image} (which needs the network, for
    Alpine's packages) and cached under {!relay_cache_dir}. *)

val remove :
  process_mgr:_ Eio.Process.mgr -> clock:_ Eio.Time.clock -> t -> string -> unit
(** [remove t name] is [rm -f NAME], whether or not the container is there.
    Gives up after 30 seconds. *)
