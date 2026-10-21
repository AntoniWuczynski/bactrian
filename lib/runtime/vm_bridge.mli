(** The host side of a virtual machine sandbox (design §5.3): runs the container
    runtime's [run] command, whose standard input and output carry the relay's
    frames ({!Bactrian_core.Vm_frame}), and takes them apart.

    Channel 0 becomes the agent's standard input and output, each connection the
    agent opens inside the container becomes a connection to the same port on
    the host's loopback (only ports in [forwards]), and [log] frames become
    lines of the violation log. The container's standard error passes through
    unframed.

    bactrian runs it as [bactrian __vm-bridge], the command the agent driver
    starts in place of the agent ({!main}), and in its own process for the
    preflight probe ({!run}). *)

type error =
  | Spawn_failed of string
  | Bad_frame of string
      (** The relay sent a frame bactrian cannot read or did not expect. *)
  | No_exit of Eio.Process.exit_status
      (** The runtime ended before the relay reported the agent's status, as
          when the image is missing or the container could not start. *)

val describe_error : error -> string

val run :
  ?stderr:_ Eio.Flow.sink ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  process_mgr:_ Eio.Process.mgr ->
  env:string array ->
  forwards:int list ->
  stdin:_ Eio.Flow.source ->
  stdout:_ Eio.Flow.sink ->
  log:(string -> unit) ->
  string list ->
  (int, error) result
(** [run ~net ~clock ~process_mgr ~env ~forwards ~stdin ~stdout ~log argv] runs
    [argv] (the runtime's [run] command, see {!Bactrian_core.Vm_args.argv}) with
    environment [env] and relays until the runtime exits, sending the relay a
    heartbeat every {!Bactrian_core.Vm_frame.heartbeat_interval} seconds. The
    result is the agent's exit code from the relay's [exit] frame: its status,
    or 128 plus the signal number. [stderr] defaults to bactrian's own.

    Memory is bounded: a connection whose host side does not take data stops the
    reading of every frame until it does. A host connection that cannot be made
    is closed in the container and reported to [log] as a line
    [{"kind":"relay_failed","port":P,"socket":"host loopback","reason":"R"}],
    the shape of the Linux forwarder's. So is an [open] beyond 64 connections at
    once, the relay's own limit, which is closed at once. *)

type config = {
  forwards : int list;
  container : string;  (** The container's name, for [rm -f]. *)
  kill_signals : bool;
      (** Whether the bridge sends SIGTERM and SIGINT to the container itself,
          with [RUNTIME kill --signal SIGNAL NAME], because the runtime's client
          does not pass them on (container 1.5.0). *)
  argv : string list;  (** The runtime's [run] command. Non-empty. *)
}

val parse : string list -> (config, string) result
(** The arguments of [__vm-bridge]: [forward=PORT]..., [container=NAME],
    [signals=kill] for {!field-kill_signals}, then [--] and the runtime's
    command. *)

val main :
  env:Eio_unix.Stdenv.base ->
  log:Eio_unix.Fd.t option ->
  config ->
  (int, string) result
(** [__vm-bridge] itself: {!run} over bactrian's own standard input and output,
    with its environment (the runtime client's, set by {!Sandbox.start}). [Ok]
    carries the agent's exit code, the one to end with. Each log line is written
    to [log].

    SIGTERM and SIGINT do not stop it: they reach the runtime's client, in the
    same process group, which passes them to the relay and so to the agent (or
    with [kill_signals] the bridge sends them to the container), and the bridge
    relays until the agent has gone. When its parent dies (bactrian killed with
    SIGKILL) it stops the container with [rm -f], which the runtime's client
    alone would leave running. *)
