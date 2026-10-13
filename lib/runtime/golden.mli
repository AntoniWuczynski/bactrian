(** Checking a scenario's assertions and checkers against its golden traces
    (design §6, [bactrian check]): every [golden.pass] trace must pass them all
    and every [golden.fail] trace must fail at least one, so a broken assertion
    or checker is caught before it judges real runs. *)

type expected = Pass | Fail

type offender = {
  trace : string;  (** The golden path as written in the scenario. *)
  expected : expected;
  reason : string;
}

type report = {
  scenario : string;
  offenders : offender list;
  errors : string list;
      (** Problems with the scenario's golden set as a whole: it has assertions
          or checkers and no golden trace at all. *)
  warnings : string list;
      (** It has golden [pass] traces and no [fail] trace. *)
}
(** [scenario] is the scenario file as given. *)

val passed : report -> bool
(** No offender and no error. Warnings do not fail a check. *)

val load :
  files:Eio.Fs.dir_ty Eio.Path.t ->
  ?vet:(Bactrian_core.Scenario.t -> (unit, string) result) ->
  scenario_file:string ->
  unit ->
  (Bactrian_core.Scenario.t, Config_loader.located_error list) result
(** The scenario, read through [files]. [vet] may refuse it (an error located at
    its [checkers]). [Error] means the scenario is invalid or refused. *)

val check :
  ?files:Eio.Fs.dir_ty Eio.Path.t ->
  env:Eio_unix.Stdenv.base ->
  sandbox:Sandbox.t ->
  scenario_file:string ->
  Bactrian_core.Scenario.t ->
  report
(** Golden paths are relative to the scenario file's directory, where the
    checkers also run, inside [sandbox] ({!Sandbox.checker}). The traces are
    read through [files] (default the whole filesystem). A trace line that is
    not JSON is reported by number, never quoted. A golden trace that cannot be
    read or decoded is an offender, as is a [fail] trace on which a checker
    errored (that proves nothing about the trace). *)
