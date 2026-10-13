(** [bactrian check]: check scenarios' assertions and checkers against their
    golden traces. *)

type request = {
  scenarios : string list;  (** Scenario files (YAML or JSON). *)
}

type t = Bactrian_runtime.Golden.report list
(** One report per scenario, in order. *)

val run : env:Eio_unix.Stdenv.base -> request -> (t, Command_error.t) result
(** [Error (Configuration _)] with the errors of every invalid scenario. *)

val passed : t -> bool
(** Whether every scenario's golden set passed. *)

val exit_code : t -> int
(** {!Exit_code.ok} when {!passed}, else {!Exit_code.failed}. *)

val to_json : t -> Yojson.Safe.t
(** [{"passed", "scenarios": [{"scenario", "passed", "offenders": [{"trace",
     "expected", "reason"}], "errors", "warnings"}]}], where [expected] is
    ["pass"] or ["fail"]. *)
