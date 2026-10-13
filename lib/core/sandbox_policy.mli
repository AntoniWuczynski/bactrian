(** What an agent's or a checker's operating-system sandbox allows, as a macOS
    Seatbelt profile or a Linux bubblewrap command line (design §5.3). Pure:
    nothing here touches the file system, the network or a process. The caller
    starts the command this module returns.

    Every path in a Seatbelt profile reaches [sandbox-exec] as a [-D NAME=VALUE]
    parameter and is named in the profile only as [(param "NAME")], so a path
    with quotes, parentheses or newlines cannot change the profile. Paths must
    be absolute and, on macOS, free of symbolic links, because Seatbelt matches
    the resolved path. *)

type platform = Macos | Linux

type listener = {
  port : int;  (** Loopback port the agent connects to, from 1 to 65535. *)
  socket : string option;
      (** Absolute path of the host's Unix socket behind the port. Linux needs
          one for every listener, because the sandbox has no network and
          [bactrian __sandbox-init] relays the port to it. On macOS it is
          optional and, when given, the agent may also connect to that socket.
      *)
}

type input = {
  platform : platform;
  home : string;
  project_root : string;
  bactrian_dir : string;  (** The [.bactrian] directory. *)
  state_dir : string option;
      (** The run's state directory. Writable. [None] for a checker, which has
          no run state: then nothing under [.bactrian/runs] is readable, not
          even [temp_dir]. *)
  temp_dir : string;
      (** The run's or the checker's temporary directory. Writable. *)
  allow_write : string list;
      (** More writable paths ([sandbox.allow_write]). *)
  deny_read : string list;  (** More unreadable paths ([sandbox.deny_read]). *)
  listeners : listener list;  (** Distinct ports. *)
  agent : string list;
      (** The agent's or the checker's command and arguments. Non-empty. *)
  bactrian_exe : string;
      (** Absolute path of the bactrian executable, which Linux re-executes as
          [__sandbox-init] inside the sandbox. Unused on macOS. *)
  preload : string option;
      (** Absolute path, symbolic links resolved, of a library the agent's
          [LD_PRELOAD] names. Linux binds it read-only like the executable, so
          it is there even under a path the fresh tmpfs mounts hide. Unused on
          macOS. *)
  tag : string;
      (** Text that Seatbelt appends to the system log line of every denial, so
          the caller can find the run's violations. Non-empty. Unused on Linux.
      *)
}

type error =
  | Empty_agent
  | Nul_byte of { what : string; value : string }
      (** [what] names the field. A NUL byte cannot be passed in a command line.
      *)
  | Not_absolute of { what : string; path : string }
  | Invalid_port of int
  | Duplicate_port of int
  | Missing_socket of int  (** A Linux listener without a Unix socket. *)
  | Empty_tag  (** An empty [tag] on macOS. *)

val describe_error : error -> string

val deny_list : ?resolve:(string -> string) -> input -> string list
(** The paths the agent may not read, in the order they are applied: the default
    list of design §5.3 expanded against [home], [project_root] and
    [bactrian_dir] (credential directories and files under [home], the project's
    [.env] files, [.bactrian/ca] and [.bactrian/runs]), then [deny_read]. A
    run's own state and temporary directories stay readable even when they sit
    under [.bactrian/runs], except for paths in [deny_read].

    [resolve] (default: the identity) gives a default path with its symbolic
    links resolved. Where that differs, the resolved path is denied, because
    both sandboxes act on the file a path leads to. On macOS the link's own path
    stays in the list as well. On Linux it is left out, since a mount on a link
    to an absolute path would land outside the sandbox's root. [deny_read] is
    taken as given, already resolved.

    Only the [.env] files at the project root are listed, not ones in
    subdirectories, because neither Seatbelt parameters nor bubblewrap mounts
    take patterns. *)

type kind = [ `Dir | `File | `Absent ]
(** What is at a path on the host, as the caller found it. *)

type seatbelt = {
  profile : string;  (** Mentions no path except [/dev] device files. *)
  params : (string * string) list;  (** [(NAME, VALUE)] for [-D NAME=VALUE]. *)
  argv : string list;
      (** [/usr/bin/sandbox-exec -p PROFILE -D NAME=VALUE... -- AGENT...] *)
}

type output =
  | Seatbelt of seatbelt
  | Bwrap of string list
      (** The whole command line, starting with [bwrap] (found on [PATH] by the
          caller). *)

val generate :
  ?resolve:(string -> string) ->
  exists:(string -> kind) ->
  input ->
  (output, error) result
(** The sandbox command for [input.platform], with the {!deny_list} that
    [resolve] gives.

    macOS: the profile starts from [(deny default)] and allows only what Node
    and Python agents were found to need, with reads of everything except
    {!deny_list}, writes only to [state_dir], [temp_dir], [allow_write] and a
    few [/dev] files, and outbound connections only to [localhost] on the
    listeners' ports (and their Unix sockets). DNS and every other address fail.
    Every denial is logged with [tag] ([(with message (param "TAG"))]). [exists]
    is not used.

    Linux: [bwrap] with a new network, process and IPC namespace over a
    read-only copy of [/], fresh [/dev], [/proc], [/tmp], [/dev/shm] and [/run],
    the bactrian executable and the [preload] library read-only (they may live
    under one of those), then the writable binds, the sockets read-only, and the
    {!deny_list} masks ([--tmpfs] over a directory, [/dev/null] over a file),
    then each directory mask made read-only with [--remount-ro] (a tmpfs is
    writable), then [bactrian __sandbox-init PORT=SOCKET... -- AGENT...]. Mounts
    come in that order because a later mount hides what an earlier one put at
    the same path. The run's own directories (and sockets under them) are bound
    again after the [.bactrian/runs] mask that hides the other runs, and no
    [allow_write] path is, so one inside another run stays hidden. A deny-list
    path that [exists] reports [`Absent] is left out, since there is nothing to
    hide, and so is an [allow_write] path that is [`Absent], which leaves it
    read-only. [state_dir], [temp_dir] and the sockets must exist when the
    command runs.

    A checker's input has no [state_dir], no [listeners] and no [allow_write],
    so on both platforms it has no network at all, not even loopback, and can
    write only to [temp_dir]. *)
