(** [bactrian __sandbox-init PORT=SOCKET... -- AGENT ARGV...]: what bactrian
    runs inside a Linux bubblewrap sandbox that has no network (design §5.3).

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

type config = {
  forwards : forward list;  (** Distinct ports, possibly none. *)
  agent : string list;  (** Non-empty: the command and its arguments. *)
}

type parse_error =
  | Bad_pair of string  (** Not of the form [PORT=SOCKET]. *)
  | Bad_port of string  (** Not a decimal number from 1 to 65535. *)
  | Bad_socket of string  (** Not an absolute path. *)
  | Duplicate_port of int
  | Missing_separator  (** No [--] between the pairs and the agent. *)
  | Empty_agent  (** Nothing follows [--]. *)

val parse : string list -> (config, parse_error) result
(** [parse args] reads the arguments after the subcommand name, including the
    [--]. Only the first [--] separates; later ones belong to the agent. *)

val describe_parse_error : parse_error -> string

val log_variable : string
(** [BACTRIAN_SANDBOX_LOG_FD]: the number of a descriptor the forwarder inherits
    from bactrian, open for append on the run's violation log, which it passes
    to {!run} as [log] and [hidden_fd]. The log lies outside every path the
    sandbox lets the agent write, and the agent does not inherit the descriptor,
    so the agent cannot add lines to it. {!run} removes the variable from the
    agent's environment. *)

type error =
  | Listen_failed of { port : int; reason : string }
  | Spawn_failed of string

val describe_error : error -> string

val run :
  ?on_listening:(requested:int -> bound:Eio.Net.Sockaddr.stream -> unit) ->
  ?stdout:Eio_unix.Fd.t ->
  ?log:Eio_unix.Fd.t ->
  ?hidden_fd:int ->
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
    [log], a descriptor open for append (nothing is written without [log]). An
    I/O error in the middle of a relayed connection just ends that connection.

    The agent inherits stdin, stdout and stderr and the forwarder's environment
    minus {!log_variable}, and runs in its own process group. Descriptor
    [hidden_fd], when given, is [/dev/null] in the agent: an inherited
    descriptor is not closed on exec, so this is how the one [log] came from
    stays out of the agent's reach. SIGTERM and SIGINT received by the forwarder
    are sent to that group while it runs. When the agent exits, the listeners
    and open connections are dropped and the group is killed, so no stray child
    outlives the call. Must be called inside [Eio_main.run].

    [on_listening] is called once per listener after it is bound, with the
    requested port and the address actually bound, and [stdout] replaces the
    agent's standard output. Both exist for tests, which ask for port 0 and read
    the real port back. The command line never produces port 0. *)
