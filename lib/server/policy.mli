(** What the user allows the MCP server to do, set on the command line that
    registers it and never by a tool argument (decision record 0011), and the
    checks that hold the server's work to it. *)

type t = {
  max_jobs : int;
      (** The most jobs ([run_start], [record_start], [replay_start]) running at
          once ([--max-jobs]). *)
}

val default : t
(** At most 4 jobs. *)

val experiment :
  t -> Confine.t -> Bactrian_runtime.Executor.contents -> (unit, string) result
(** Refuses an experiment loaded for [run_start] that names a path outside the
    root which it hands to a process: a [sandbox.allow_write] path, an MCP stdio
    server's [cwd] or a scenario's [fixtures]. The error names the key. *)
