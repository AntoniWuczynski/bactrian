(** Command-mode driver for an agent subprocess (spec §5.1).

    The agent runs in its own process group with exactly the environment given
    (nothing is inherited). bactrian writes {!Agent_protocol.to_agent} messages
    to its stdin and reads {!Agent_protocol.from_agent} messages from its
    stdout, one JSON object per line. Its stderr goes to a file.

    When {!with_agent}'s callback returns (or raises, or the fibre is cancelled)
    the agent is shut down: stdin is closed, the agent gets [exit_grace] seconds
    to exit, then the whole process group receives SIGTERM, then SIGKILL after
    [kill_grace] seconds. The group is signalled even when the leader has
    already exited, so stray children cannot outlive the run. *)

module Agent_protocol = Bactrian_core.Agent_protocol

type config = {
  argv : string list;
      (** Command and arguments. The executable is looked up on bactrian's own
          [PATH]. *)
  env : (string * string) list;  (** The agent's whole environment. *)
  cwd : Eio.Fs.dir_ty Eio.Path.t option;
  stderr_path : Eio.Fs.dir_ty Eio.Path.t;
      (** Created or truncated, mode 0600. *)
  turn_timeout : float;  (** Seconds {!recv} waits for one message. *)
  total_timeout : float;
      (** Seconds from spawn after which every {!recv} fails. *)
  exit_grace : float;
      (** Seconds to wait for a voluntary exit after closing stdin. *)
  kill_grace : float;  (** Seconds between SIGTERM and SIGKILL. *)
}

type error =
  | Spawn_failed of string
  | Write_failed of string
      (** Typically the agent closed its stdin or exited. *)
  | Protocol_violation of { line : string; error : Agent_protocol.error }
      (** The agent wrote a line that is not a valid message. *)
  | Line_too_long  (** A line over 1 MiB. *)
  | Turn_timeout
  | Total_timeout
  | Agent_exited of Eio.Process.exit_status option
      (** Stdout reached end of file. The status is [None] if the process had
          not exited within [exit_grace]. *)

type t

val pid : t -> int
(** The leader's pid, which is also the process group id. *)

val send : t -> Agent_protocol.to_agent -> (unit, error) result

val recv : t -> (Agent_protocol.from_agent, error) result
(** Next message, waiting at most [turn_timeout] and never past [total_timeout].
*)

val with_agent :
  process_mgr:_ Eio_unix.Process.mgr ->
  clock:_ Eio.Time.clock ->
  config ->
  (t -> 'a) ->
  ('a * Eio.Process.exit_status, error) result
(** [with_agent ~process_mgr ~clock config f] spawns the agent, runs [f], shuts
    the agent down and returns [f]'s result with the leader's exit status. Only
    [Spawn_failed] is returned as an error here. *)
