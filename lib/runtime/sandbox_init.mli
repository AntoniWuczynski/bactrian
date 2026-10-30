(** [bactrian __sandbox-init [--dns=UID:GID] PORT=SOCKET... -- AGENT ARGV...]:
    what bactrian runs inside a Linux bubblewrap sandbox that has no network
    (design §5.3).

    For each [PORT=SOCKET] pair it listens on [127.0.0.1:PORT] inside the
    sandbox and relays every accepted connection to the Unix socket [SOCKET],
    which is bind-mounted in from the host. It then starts the agent as its
    child and exits with the agent's status. It connects nowhere but to the
    given sockets and listens nowhere but on loopback. The logic does not depend
    on bubblewrap, so it is tested on every platform. *)

type forward = {
  port : int;  (** Loopback port the agent connects to. *)
  socket : string;  (** Absolute path of the host's Unix socket. *)
}

type identity = { uid : int; gid : int }

type config = {
  forwards : forward list;  (** Distinct ports, possibly none. *)
  dns : identity option;
      (** [--dns=UID:GID]: also answer DNS queries on port 53 ({!Sandbox_dns}),
          which only root of the sandbox's user namespace may bind, and so start
          the agent through [__sandbox-user] as [UID:GID], bactrian's own IDs
          outside ({!enter_user_namespace}). *)
  agent : string list;  (** Non-empty: the command and its arguments. *)
}

type parse_error =
  | Bad_identity of string
      (** Not two decimal numbers [UID:GID], each at most 4294967294. *)
  | Bad_pair of string  (** Not of the form [PORT=SOCKET]. *)
  | Bad_port of string  (** Not a decimal number from 1 to 65535. *)
  | Bad_socket of string  (** Not an absolute path. *)
  | Duplicate_port of int
  | Missing_separator  (** No [--] between the pairs and the agent. *)
  | Empty_agent  (** Nothing follows [--]. *)

val parse : string list -> (config, parse_error) result
(** [parse args] reads the arguments after the subcommand name, including the
    [--]. [--dns=UID:GID] may only come first. Only the first [--] separates;
    later ones belong to the agent. *)

val describe_parse_error : parse_error -> string

val log_variable : string
(** [BACTRIAN_SANDBOX_LOG_FD]: the number of a descriptor the forwarder inherits
    from bactrian, the write end of a pipe that bactrian copies into the run's
    violation log, which the forwarder passes to {!run} as [log] and
    [hidden_fd]. The agent does not inherit the descriptor, and the forwarder
    makes itself not dumpable first ({!set_undumpable}), so the agent cannot
    reopen it through [/proc/PID/fd] either. Through the pipe the forwarder can
    only add lines. {!run} removes the variable from the agent's environment. *)

val signal_variable : string
(** [BACTRIAN_SANDBOX_SIGNAL_FD]: the number of a descriptor the forwarder
    inherits, the read end of a pipe on which each byte bactrian writes asks the
    forwarder to send SIGTERM to the agent's process group, which {!run} reads
    as [signals]. bubblewrap's [--new-session] puts the forwarder in a session
    of its own, so a signal to bubblewrap's process group never reaches it, and
    bubblewrap's death kills the sandbox outright. Like {!log_variable}, the
    agent neither inherits the descriptor nor keeps the variable. *)

val set_undumpable : unit -> bool
(** On Linux, [prctl(PR_SET_DUMPABLE, 0)] for the calling process: the files of
    a process that is not dumpable under [/proc/PID] belong to root, so a
    process of the same user without privileges, such as the agent, cannot open
    its descriptors through [/proc/PID/fd]. Whether that worked. Elsewhere it
    does nothing and gives [true]. The forwarder calls it before it starts the
    agent, which [exec] makes dumpable again. *)

type error =
  | Listen_failed of { port : int; reason : string }
  | Dns_failed of Sandbox_dns.error
  | Spawn_failed of string

val describe_error : error -> string

val run :
  ?on_listening:(requested:int -> bound:Eio.Net.Sockaddr.stream -> unit) ->
  ?stdout:Eio_unix.Fd.t ->
  ?log:Eio_unix.Fd.t ->
  ?signals:Eio_unix.Fd.t ->
  ?hidden_fds:int list ->
  net:_ Eio.Net.t ->
  process_mgr:_ Eio_unix.Process.mgr ->
  config ->
  (int, error) result
(** [run ~net ~process_mgr config] binds every listener, starts the agent, and
    returns the exit code the forwarder should exit with: the agent's, or 128
    plus the signal number that ended it (255 for a signal this build cannot
    number). Listeners are bound before the agent starts, so it can connect at
    once. Binding or starting failures are returned as an [error] and nothing is
    left running.

    Each connection is relayed by two copy fibres in its own switch, with
    half-close in each direction, so one failing connection is torn down alone.
    A connection whose socket cannot be reached is closed after one line
    [{"kind":"relay_failed","port":P,"socket":"S","reason":"R"}] is written to
    [log], a descriptor open for writing, the pipe bactrian drains (nothing is
    written without [log]). An I/O error in the middle of a relayed connection
    just ends that connection. With [dns] it also answers DNS on port 53 of
    {!Sandbox_dns.addresses}, writing each query to [log].

    The agent inherits stdin, stdout and stderr and the forwarder's environment
    minus {!log_variable} and {!signal_variable}, and runs in its own process
    group. Each descriptor of [hidden_fds] is [/dev/null] in the agent: an
    inherited descriptor is not closed on exec, so this is how the ones [log]
    and [signals] came from stay out of the agent's reach. SIGTERM and SIGINT
    received by the forwarder are sent to that group while it runs, and so is
    SIGTERM for each byte read from [signals]. When the agent exits, the
    listeners and open connections are dropped and the group is killed, so no
    stray child outlives the call. Must be called inside [Eio_main.run].

    [on_listening] is called once per listener after it is bound, with the
    requested port and the address actually bound, and [stdout] replaces the
    agent's standard output. Both exist for tests, which ask for port 0 and read
    the real port back. The command line never produces port 0. *)

(** {2 [bactrian __sandbox-user UID:GID -- AGENT ARGV...]} *)

val parse_user : string list -> (identity * string list, string) result
(** The arguments after the subcommand name: the IDs and the agent. *)

val enter_user_namespace : identity -> (unit, string) result
(** Moves the calling process into a new user namespace in which it is
    [uid:gid], mapped to root of the namespace it leaves, with [setgroups]
    denied: the nested namespace bubblewrap itself makes when it runs a command
    as a user other than root. The agent then has no capability, and in
    particular cannot bind port 53. The process must be single-threaded, which
    is why it is a subcommand of its own, run before any Eio loop and then
    replaced by the agent with [execvp]. On Linux only: elsewhere [Error]. *)
