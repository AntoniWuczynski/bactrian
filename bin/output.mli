(** Terminal rendering of finished runs, experiments and golden checks. *)

val colour : bool Lazy.t
(** Whether to colour output: stdout is a terminal and [NO_COLOR] is unset. *)

val paint : colour:bool -> string -> string -> string
(** [paint ~colour code text] wraps [text] in the terminal colour [code] (such
    as ["31"], red) when [colour] holds. *)

val render_summary_text : colour:bool -> Bactrian_app.Run_command.t -> string
(** One line per planned run, then a count per outcome. When the sandbox label
    says the runs were not sandboxed, a warning line comes first. *)

val render_golden_text : colour:bool -> Bactrian_runtime.Golden.report -> string
(** [ok FILE], or [FAIL FILE], then one line per error of the golden set and per
    offending golden trace, then one [warning:] line per warning (also under
    [ok]). *)

val render_text : Bactrian_runtime.Run.result -> string
(** The outcome and directory, an [UNSANDBOXED] warning when the agent ran
    outside the sandbox, one line per sandbox violation, divergences, errors and
    spend. *)
