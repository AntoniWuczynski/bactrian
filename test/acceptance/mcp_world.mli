(** What the tests that drive [bactrian serve-mcp] share, over either transport:
    the flags that let jobs run agents unsandboxed, an agent that hangs until it
    is killed, and the experiment that [run_start] runs. *)

open Bactrian_core

val trusting : string list
(** [--allow-unsandboxed] and [--trust-project]: the tests run agents outside
    the sandbox and pass them variables. *)

val check_exit : string -> int -> Unix.process_status -> unit
(** The server exited with this status, and was not killed. *)

(** {2 An agent that hangs} *)

val hang_script : string
(** Python that starts a [sleep] child, writes [agent.pid] and [child.pid] in
    its working directory and reads its input forever. *)

val hung_arguments : string -> (string * Canon.t) list
(** The [replay_start] arguments that replay this run with {!hang_script}
    unsandboxed, within long timeouts. *)

type hung = { agent_pid : int; child_pid : int }

val with_recording : string -> (Acceptance_world.world -> string -> 'a) -> 'a
(** A world in which the first example agent made a recording, with its run ID.
*)

val await_hung : string -> hung
(** Waits until the hanging agent in this directory has started, and checks it
    and its child are alive. *)

val gone : hung -> unit
(** Waits until the agent and its child are gone. *)

(** {2 The experiment of [run_start]} *)

val run_flags : string list
(** [run exp.json --sandbox off --json]. *)

val upstream_variable : string
(** [BACTRIAN_UPSTREAM_OPENAI], which points the experiment at a fake upstream.
*)

val run_args : ?resume:string -> unit -> (string * Canon.t) list
(** [run_start]'s arguments for [exp.json], unsandboxed. *)

val run_statuses : Canon.t -> string list
(** The status of each run of an experiment summary. *)

val summary_normalised : Canon.t -> Canon.t
(** An experiment summary without its experiment ID and run IDs, which differ
    between two experiments of the same file. *)
