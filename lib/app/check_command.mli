(** [bactrian check]: check scenarios' assertions and checkers against their
    golden traces. *)

type request = {
  scenarios : string list;  (** Scenario files (YAML or JSON). *)
  sandbox : Bactrian_runtime.Sandbox.mode;  (** What the checkers run in. *)
}

type t = {
  reports : Bactrian_runtime.Golden.report list;
      (** One report per scenario, in order. *)
  sandbox : Bactrian_runtime.Sandbox.label option;
      (** What the checkers ran in. [None] when no scenario has a checker. *)
}

val run :
  ?files:Eio.Fs.dir_ty Eio.Path.t ->
  ?vet:(Bactrian_core.Scenario.t -> (unit, string) result) ->
  on_progress:(Progress.t -> unit) ->
  bactrian_exe:string ->
  env:Eio_unix.Stdenv.base ->
  request ->
  (t, Command_error.t) result
(** Loads every scenario through [files] (default the whole filesystem) and lets
    [vet] refuse it. Then, when one has checkers, checks the sandbox once
    ({!Bactrian_runtime.Sandbox.preflight} in [./.bactrian], re-running
    [bactrian_exe] as the probe), and runs {!Bactrian_runtime.Golden.check} on
    each scenario. [Error (Configuration _)] with the errors of every invalid
    scenario, and [Error (Environment _)] (exit 69) when the sandbox does not
    work under [strict]. Under [auto] that is a warning through [on_progress],
    and the checkers run unsandboxed. *)

val passed : t -> bool
(** Whether every scenario's golden set passed. *)

val exit_code : t -> int
(** {!Exit_code.ok} when {!passed}, else {!Exit_code.failed}. *)

val to_json : t -> Yojson.Safe.t
(** [{"passed", "sandbox", "scenarios": [{"scenario", "passed", "offenders":
     [{"trace", "expected", "reason"}], "errors", "warnings"}]}], where
    [expected] is ["pass"] or ["fail"] and [sandbox] is
    [{"mode", "applied", "reason"?}], or [null] when no scenario has a checker.
*)
