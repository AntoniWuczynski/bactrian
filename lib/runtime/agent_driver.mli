(** Driver for an agent (design §5.1): a subprocess in command mode, a server in
    HTTP mode.

    {2 Command mode}

    The agent runs in its own process group with exactly the environment given
    (nothing is inherited). bactrian writes {!Agent_protocol.to_agent} messages
    to its stdin and reads {!Agent_protocol.from_agent} messages from its
    stdout, one JSON object per line. Its stderr goes to a file.

    When {!with_agent}'s callback returns (or raises, or the fibre is cancelled)
    the agent is shut down: stdin is closed, the agent gets [exit_grace] seconds
    to exit, then the whole process group receives SIGTERM, then SIGKILL after
    [kill_grace] seconds. When the run is stopped instead of finished (a turn or
    total timeout, a protocol violation, an exception or cancellation in the
    callback) there is no exit grace: SIGTERM follows at once. The group is
    signalled even when the leader has already exited, so stray children cannot
    outlive the run.

    {2 HTTP mode}

    The agent is a long-lived server that the user started: {!with_http_agent}
    starts nothing and stops nothing. Each message to the agent is one
    [POST <url>] whose response body is a JSON array of the agent's messages for
    that step, handed out by {!recv} in order. *)

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

type http_config = {
  url : Uri.t;  (** [http://]. *)
  namespace_header : string;
      (** Carries the run ID on every request, beside [X-Bactrian-Run]. *)
  run_id : string;
  turn_timeout : float;  (** Seconds one POST may take. *)
  total_timeout : float;  (** Seconds from the start of the session. *)
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
  | Http_error of string
      (** HTTP mode: the agent could not be reached, or the connection failed or
          was cut off. *)
  | Http_status of { status : int; body : string }
      (** HTTP mode: a status outside 2xx, with the first 2 KiB of the body. *)
  | No_message
      (** HTTP mode: {!recv} found every message of the responses already taken.
      *)

type t

val pid : t -> int option
(** The leader's pid, which is also the process group id. [None] in HTTP mode.
*)

val send : t -> Agent_protocol.to_agent -> (unit, error) result
(** In HTTP mode, the POST and its whole response: a non-2xx status is
    [Http_status], a body that is not a JSON array of valid messages is
    [Protocol_violation] (with the body in [line]), and the call is cut off by
    [turn_timeout] or [total_timeout]. *)

val recv : t -> (Agent_protocol.from_agent, error) result
(** Next message, waiting at most [turn_timeout] and never past [total_timeout].
    In HTTP mode it never waits: {!send} has already taken the whole response,
    and the messages are handed out in order. *)

val with_agent :
  ?fds:(int * Eio_unix.Fd.t) list ->
  process_mgr:_ Eio_unix.Process.mgr ->
  clock:_ Eio.Time.clock ->
  config ->
  (t -> 'a) ->
  ('a * Eio.Process.exit_status, error) result
(** [with_agent ~process_mgr ~clock config f] spawns the agent, runs [f], shuts
    the agent down and returns [f]'s result with the leader's exit status. Only
    [Spawn_failed] is returned as an error here. [fds] are further descriptors
    the agent inherits, by number (above 2). *)

val with_http_agent :
  net:[> [ `Generic | `Unix ] Eio.Net.ty ] Eio.Resource.t ->
  clock:_ Eio.Time.clock ->
  ?connector:Upstream.connector ->
  http_config ->
  (t -> 'a) ->
  'a
(** [with_http_agent ~net ~clock config f] runs [f] over a session with the
    server. [connector] defaults to {!Upstream.plain}, which refuses [https]. *)
