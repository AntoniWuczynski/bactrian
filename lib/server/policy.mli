(** What the user allows the MCP server to do, set on the command line that
    registers it and never by a tool argument (decision record 0011). *)

type t = {
  max_jobs : int;
      (** The most jobs ([run_start], [record_start], [replay_start]) running at
          once ([--max-jobs]). *)
}

val default : t
(** At most 4 jobs. *)
