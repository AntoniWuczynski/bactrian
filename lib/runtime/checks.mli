(** A scenario's assertions and checkers evaluated over one trace (design §6).
*)

open Bactrian_core

type verdict =
  | Pass
  | Fail of string  (** The assertion or checker judged the trace bad. *)
  | Checker_failed of string
      (** The checker could not give a verdict (it crashed, hung or printed
          garbage). Neither a pass nor a fail. *)

type item = { name : string; verdict : verdict }
(** [name] is e.g. ["assertions[2] max_steps"] or
    ["checkers[0] python3 check.py"]. *)

val cost :
  ?table:Pricing.table -> date:string -> Trace.entry list -> float option
(** Sum of {!Bactrian_core.Pricing.cost} over the model calls, priced at [date]
    in [table] (default {!Bactrian_core.Pricing.default}) (["YYYY-MM-DD"]).
    [None] when a model call has no usage or no price. *)

val trace_date : Trace.entry list -> string option
(** The simulated date (["YYYY-MM-DD"]) of the trace's [Run_start], the date its
    calls are priced at. [None] without a [Run_start]. *)

val evaluate :
  ?table:Pricing.table ->
  ?stderr_dir:Eio.Fs.dir_ty Eio.Path.t ->
  process_mgr:_ Eio_unix.Process.mgr ->
  clock:_ Eio.Time.clock ->
  fs:_ Eio.Path.t ->
  sandbox:Sandbox.t ->
  timeout:float ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  date:string ->
  assertions:Assertion.t list ->
  checkers:string list list ->
  Trace.entry list ->
  item list
(** Every assertion in order, then every checker. [timeout] bounds each checker,
    [cwd] is where it runs and [sandbox] what it runs in ({!Checker_runner.run},
    with [fs]). A checker's stderr never reaches bactrian's terminal: with
    [stderr_dir], checker [i]'s non-empty stderr (its last 64 KiB) is written to
    [checker-i.stderr] there, and a checker error names that file. *)

val checker_errors : item list -> string list
(** One line per {!Checker_failed} item, naming it and giving the reason. *)

val problems : item list -> string list
(** One line per item that is not {!Pass}, naming it and giving the reason. *)

val to_json : ?checker_sandbox:Sandbox.label -> item list -> Yojson.Safe.t
(** The content of [verdicts.json]:
    [{"passed": bool, "items": [{"name", "verdict":
     "pass"|"fail"|"checker_failed", "reason"?}], "checker_sandbox"?: {"mode",
     "applied", "reason"?}}], with [checker_sandbox] when given: the label of
    the sandbox the checkers ran in. *)
