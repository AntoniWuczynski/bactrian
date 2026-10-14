(** The command line that starts an agent in a virtual machine sandbox (design
    §5.3): a Docker-compatible [run] of the image with no network, a read-only
    root and the relay ([vm/relay.c]) as its entrypoint, and the file of the
    agent's environment. Pure.

    Every host path is mounted at the same absolute path inside, so the agent's
    command and the paths in its arguments and environment work unchanged: the
    project root read-only, the run's state and temporary directories and the
    [allow_write] paths read-write. The deny-list is masked where it is visible:
    an empty read-only tmpfs over a directory, and for a file an empty read-only
    tmpfs over its directory with every other entry of that directory mounted
    again, so the file is not there under any spelling of its name (a bind over
    the file alone covers one name, and a case-insensitive host has others).
    Nothing outside those mounts is there at all, so the deny-list entries under
    the user's home need no mask unless a mount shows them. *)

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
  | Plain  (** Not a symbolic link. *)
  | Link_to_file of string
      (** A symbolic link to a regular file, at this resolved path. *)
  | Other_link  (** A symbolic link to anything else, or to nothing. *)

type entry = {
  name : string;
  kind : entry_kind;
  masked_file : bool;
      (** It leads to a file of the deny-list under another name: a hard link,
          or a symbolic link to it. *)
}
(** An entry of a directory that holds a masked file. *)

type input = {
  runtime : string;  (** The runtime's executable, such as [docker]. *)
  name : string;  (** The container's name. *)
  uid : int;
  gid : int;  (** The agent runs as this user and group. *)
  image : string;  (** The image's ID. *)
  workdir : string;
  env_file : string;  (** The file {!val-env_file} wrote. *)
  relay : string;  (** The relay binary on the host. *)
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
  allow_write : string list;  (** Existing paths only. *)
  deny : (string * Sandbox_policy.kind) list;
      (** The deny-list ({!Sandbox_policy.deny_list}, links resolved), each with
          what is at the path. *)
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

val describe_error : error -> string

val relay_path : string
(** [/bactrian/relay], where the relay is mounted inside. *)

val argv : input -> (string list, error) result
(** [RUNTIME run --rm -i --name NAME --network none --read-only --cap-drop ALL
     --security-opt no-new-privileges --user UID:GID --tmpfs
     /tmp:rw,noexec,nosuid,nodev,size=1g --pids-limit 1024 --memory 4g
     --memory-swap 4g --log-driver none --pull never --workdir DIR --env-file
     FILE --entrypoint /bactrian/relay --mount|--tmpfs... -- IMAGE --forward
     PORT... -- AGENT...] (or
    [--probe OUTSIDE INSIDE DENIED MASKED MASKED_CASE ROOT OTHER_RUN] in place
    of [-- AGENT...]).

    Mounts are given in order of depth, as the runtime applies them, then of
    path. A directory mask is [--tmpfs PATH:ro,mode=0755]. For a file mask the
    same tmpfs covers the file's directory, and each other entry of it
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
    per line, taken as written (no quoting or escaping, docker/cli
    [pkg/kvfile]). [Error] for a name that is empty, starts with [#] or holds
    [=] or whitespace, and for a value the format cannot carry: a newline, a
    final carriage return, a NUL byte, invalid UTF-8, or a line over 64 KiB. *)
