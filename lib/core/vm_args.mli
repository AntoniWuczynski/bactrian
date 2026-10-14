(** The command line that starts an agent in a virtual machine sandbox (design
    §5.3): a Docker-compatible [run] of the image with no network, a read-only
    root and the relay ([vm/relay.c]) as its entrypoint, and the file of the
    agent's environment. Pure.

    Every host path is mounted at the same absolute path inside, so the agent's
    command and the paths in its arguments and environment work unchanged: the
    project root read-only, the run's state and temporary directories and the
    [allow_write] paths read-write. The deny-list is masked where it is visible:
    an empty read-only tmpfs over a directory, an empty read-only file over a
    file. Nothing outside those mounts is there at all, so the deny-list entries
    under the user's home need no mask unless a mount shows them. *)

type command =
  | Agent of string list  (** The agent's command and arguments. Non-empty. *)
  | Probe of { outside : string; inside : string; denied : string }
      (** The relay's own probe (see {!Vm_probe}) instead of an agent. *)

type input = {
  runtime : string;  (** The runtime's executable, such as [docker]. *)
  name : string;  (** The container's name. *)
  uid : int;
  gid : int;  (** The agent runs as this user and group. *)
  image : string;
  workdir : string;
  env_file : string;  (** The file {!val-env_file} wrote. *)
  relay : string;  (** The relay binary on the host. *)
  empty_file : string;  (** An empty file on the host, mounted over files. *)
  project_root : string;
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

val describe_error : error -> string

val relay_path : string
(** [/bactrian/relay], where the relay is mounted inside. *)

val argv : input -> (string list, error) result
(** [RUNTIME run --rm -i --name NAME --network none --read-only --cap-drop ALL
     --security-opt no-new-privileges --user UID:GID --tmpfs /tmp --log-driver
     none --pull never --workdir DIR --env-file FILE --entrypoint
     /bactrian/relay --mount|--tmpfs... IMAGE --forward PORT... -- AGENT...] (or
    [--probe OUTSIDE INSIDE DENIED] in place of [-- AGENT...]).

    Mounts are given in order of depth, as the runtime applies them. A directory
    mask is [--tmpfs PATH:ro,mode=0755], a file mask a read-only bind of
    [empty_file]. A writable path under a directory mask is left out, except the
    run's own directories under [runs_dir], so a path inside another run stays
    hidden. A mask is left out where no mount shows its path, and a mount whose
    path equals a mask's is replaced by the mask. *)

val env_file : (string * string) list -> (string, error) result
(** The agent's environment in the runtime's env-file format: one [NAME=VALUE]
    per line, taken as written (no quoting or escaping, docker/cli
    [pkg/kvfile]). [Error] for a name that is empty, starts with [#] or holds
    [=] or whitespace, and for a value the format cannot carry: a newline, a
    final carriage return, a NUL byte, invalid UTF-8, or a line over 64 KiB. *)
