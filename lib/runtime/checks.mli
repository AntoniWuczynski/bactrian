(** A scenario's assertions and checkers evaluated over one trace (spec §6). *)

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

val evaluate :
  ?table:Pricing.table ->
  process_mgr:_ Eio_unix.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  date:string ->
  assertions:Assertion.t list ->
  checkers:string list list ->
  Trace.entry list ->
  item list
(** Every assertion in order, then every checker. [timeout] bounds each checker
    and [cwd] is where it runs. *)

val problems : item list -> string list
(** One line per item that is not {!Pass}, naming it and giving the reason. *)

val to_json : item list -> Yojson.Safe.t
(** The content of [verdicts.json]:
    [{"passed": bool, "items": [{"name", "verdict":
     "pass"|"fail"|"checker_failed", "reason"?}]}]. *)
