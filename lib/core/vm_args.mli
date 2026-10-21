(** The command line that starts an agent in a virtual machine sandbox (design
    §5.3): a Docker-compatible [run], or Apple's [container create], of the
    image with no network, a read-only root and the relay ([vm/relay.c]) as its
    entrypoint, and the file of the agent's environment. Pure.

    Every host path is mounted at the same absolute path inside, so the agent's
    command and the paths in its arguments and environment work unchanged: the
    project root read-only, the run's state and temporary directories and the
    [allow_write] paths read-write. The deny-list is masked where it is visible:
    an empty tmpfs over a directory, and for a file a replacement of its
    directory with every other entry of that directory there again, so the file
    is not there under any spelling of its name (a bind over the file alone
    covers one name, and a case-insensitive host has others). Docker's
    replacement is an empty read-only tmpfs with each entry bound back.
    container mounts directories only, so its replacement is a staging directory
    ({!stage}) bound read-only, holding a hard link of each other file and an
    empty directory for each mount beneath. Nothing outside those mounts is
    there at all, so the deny-list entries under the user's home need no mask
    unless a mount shows them. *)

type probe = {
  outside : string;  (** To create, outside every writable mount. *)
  inside : string;  (** To create, in the state directory. *)
  denied : string;  (** To read, under a masked directory. *)
  masked : string;  (** To read, a masked file. *)
  masked_case : string;  (** To read, [masked] with its name's case changed. *)
  root : string;  (** To create, directly under [/]. *)
  other_run : string;  (** To create, in another run's directory. *)
}
(** The paths the relay's probe tries (see {!Vm_probe}). *)

type command =
  | Agent of string list  (** The agent's command and arguments. Non-empty. *)
  | Probe of probe  (** The relay's own probe instead of an agent. *)

type entry_kind =
  | File  (** A regular file. *)
  | Dir  (** A directory. *)
  | Special  (** A socket, a pipe or a device. *)
  | Link_to_file of string
      (** A symbolic link to a regular file, at this resolved path. *)
  | Other_link  (** A symbolic link to anything else, or to nothing. *)

type entry = {
  name : string;
  kind : entry_kind;
  identity : (int * int) option;
      (** The device and inode the entry led to when it was listed (through a
          symbolic link), if it could be read. *)
  masked_file : bool;
      (** It leads to a file of the deny-list under another name: a hard link,
          or a symbolic link to it. *)
}
(** An entry of a directory that holds a masked file. *)

type input = {
  kind : Experiment.sandbox_runtime;
  runtime : string;  (** The runtime's executable, such as [docker]. *)
  name : string;  (** The container's name. *)
  labels : (string * string) list;
      (** [--label KEY=VALUE] each, so a later bactrian can find the container.
      *)
  uid : int;
  gid : int;  (** The agent runs as this user and group. *)
  image : string;
      (** The image's ID for Docker. container cannot run an ID, so there it is
          the reference, and the caller checks the digest after [create]. *)
  workdir : string;
  env_file : string;  (** The file {!val-env_file} wrote. *)
  relay : string;
      (** The relay binary on the host. container mounts its directory. *)
  project_root : string;
  bactrian_dir : string;
      (** [.bactrian]. A mask over it leaves the run's own [state_dir] and
          [temp_dir] and the [allow_write] paths in it visible, except those in
          another mask beneath it. *)
  runs_dir : string;
      (** [.bactrian/runs]. Its mask leaves the run's own [state_dir] and
          [temp_dir] visible, and no other mask does. *)
  state_dir : string;
  temp_dir : string;
  allow_write : (string * Sandbox_policy.kind) list;
      (** Existing paths only, each with what is at it. *)
  read_only : string list;
      (** Directories of the run's own, shown read-only although a writable
          mount holds them (the probe's directory of a masked file). *)
  deny : (string * Sandbox_policy.kind) list;
      (** The deny-list ({!Sandbox_policy.deny_list}, links resolved), each with
          what is at the path. *)
  staging_dir : string;
      (** Where container's staging directories go, one numbered directory each:
          in the run's directory, which no mount shows. *)
  forwards : int list;  (** Ports the relay listens on, from 1 to 65535. *)
  command : command;
  list_dir : string -> (entry list, string) result;
      (** The entries of a directory that holds a masked file. *)
}

type error =
  | Bad_path of { what : string; path : string }
      (** Not absolute, or holding a comma, a colon, a double quote, a control
          character or a NUL byte, which a [--mount] or [--tmpfs] value cannot
          carry. *)
  | Bad_argument of string  (** An argument holding a NUL byte. *)
  | Bad_variable of { name : string; reason : string }
  | Bad_port of int
  | Empty_agent
  | Hidden_workdir of string
      (** The working directory is in none of the mounts. *)
  | Mask_in_root of string  (** A masked file directly under [/]. *)
  | Crowded_dir of { file : string; dir : string; entries : int }
      (** A masked file whose directory has more than 1000 entries, each of
          which would be one more mount. *)
  | Unlistable of { dir : string; reason : string }
  | Root_user
      (** container as user ID 0, which could write to its masks, owned by root.
      *)
  | Writable_mask_dir of { file : string; dir : string }
      (** container, with a masked file in a writable directory, which its
          staging directory would make read-only. *)
  | File_allow_write of string
      (** container, with an [allow_write] path that is a file. *)

val describe_error : error -> string

val relay_path : string
(** [/bactrian/relay], where Docker mounts the relay. *)

val relay_dir : string
(** [/bactrian], where container mounts the relay's directory. *)

type link = {
  name : string;  (** In the staging directory. *)
  source : string;  (** The host file it is a hard link of. *)
  identity : int * int;
      (** The device and inode [source] had when it was listed. The link made
          must be this file, or the staging fails. *)
}

type stage = {
  dir : string;  (** The staging directory, on the host, to make. *)
  over : string;  (** The masked file's directory, which it replaces. *)
  links : link list;
  mount_points : string list;
      (** Names of the empty directories to make in it, where mounts go. *)
}
(** One of container's staging directories. Making it, before [create], and
    removing it, with the container, are the caller's. *)

type invocation = {
  argv : string list;
  stages : stage list;  (** Empty for Docker. *)
}

val argv : input -> (invocation, error) result
(** For Docker:
    [RUNTIME run --rm -i --name NAME --label KEY=VALUE... --network none
     --read-only --cap-drop ALL --security-opt no-new-privileges --user UID:GID
     --tmpfs /tmp:rw,noexec,nosuid,nodev,size=1g --pids-limit 1024 --memory 4g
     --memory-swap 4g --log-driver none --pull never --workdir DIR --env-file
     FILE --entrypoint /bactrian/relay --mount|--tmpfs... -- IMAGE --forward
     PORT... -- AGENT...] (or
    [--probe OUTSIDE INSIDE DENIED MASKED MASKED_CASE ROOT OTHER_RUN] in place
    of [-- AGENT...]).

    For container:
    [RUNTIME create --rm --name NAME --label KEY=VALUE... --network none
     --read-only --cap-drop ALL --user UID:GID --tmpfs
     /tmp:rw,noexec,nosuid,nodev,size=1g,mode=1777 --ulimit nproc=1024:1024
     --memory 4g --workdir DIR --env-file FILE --entrypoint /bactrian/RELAY
     --mount type=bind,source=RELAY_DIR,target=/bactrian,readonly
     --mount|--tmpfs... -- IMAGE --forward PORT... -- AGENT...]. A directory
    mask is [--tmpfs PATH:rw,mode=0755,size=1m], owned by root and writable
    because container cannot make a mount point in a read-only one, so user ID 0
    is [Error Root_user]. A file mask binds its stage read-only over the file's
    directory, which must not be writable ([Writable_mask_dir]). The stage holds
    a hard link of each other regular file, and of the file a symbolic link
    leads to when the container shows that file, and nothing for the masked file
    under any name, other links, sockets, pipes and devices. Each directory
    beside the file is bound again. An [allow_write] file is
    [Error File_allow_write].

    Mounts are given in order of depth, as the runtime applies them, then of
    path. A directory mask is [--tmpfs PATH:ro,mode=0755] for Docker. For a file
    mask the same tmpfs covers the file's directory, and each other entry of it
    ([list_dir]) is bound again as the mounts above showed it, read-only or
    read-write. An entry that is the masked file under another name is left out,
    and so is a symbolic link, unless it leads to a file the container shows (a
    bind follows the link). An entry with a mount of its own keeps that one, so
    masks nest. A writable path under a directory mask is left out, except under
    [bactrian_dir] and, for the run's own directories, under [runs_dir], so a
    path inside another run stays hidden. A mask is left out where no mount
    shows its path, and a mount whose path equals a mask's is replaced by the
    mask. *)

val env_file : (string * string) list -> (string, error) result
(** The agent's environment in the runtime's env-file format: one [NAME=VALUE]
    per line, taken as written (no quoting or escaping, docker/cli [pkg/kvfile],
    which container ports). [Error] for a name that is empty, starts with [#] or
    holds [=] or whitespace, and for a line the format cannot carry: a line
    break of either reader (U+000A to U+000D, U+0085, U+2028 or U+2029), a NUL
    byte, invalid UTF-8, or a line over 64 KiB. *)
