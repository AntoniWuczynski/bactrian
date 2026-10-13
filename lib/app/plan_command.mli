(** [bactrian plan]: say how many repeats an experiment needs. *)

open Bactrian_core

type request = {
  rates : float list;
      (** Baseline pass rates, one per scenario or a single one for all of them.
      *)
  scenarios : int option;
      (** The number of scenarios; the number of rates when [None]. *)
  from : string option;
      (** An earlier experiment under [./.bactrian/experiments] to take the
          scenarios and rates from instead of [rates] and [scenarios]. *)
  variant : string option;
      (** With [from], the variant whose rates to take; the experiment's
          [compare.baseline] when [None]. *)
  effect_ : float option;
      (** The assumed true drop in pass rate; twice [threshold] when [None]. *)
  threshold : float;  (** The regression threshold. *)
  population : [ `Suite | `Scenarios ];
  between_sd : float option;
      (** The spread of the effect between scenarios, needed for [`Scenarios].
      *)
  gate_metrics : int;  (** From 1 to 4. *)
  power : float;
  confidence : float;
  max_repeats : int;
  simulations : int;
  seed : int;
}

type t = { assumption : Plan_sim.assumption; answer : Plan_sim.answer }

val run : env:Eio_unix.Stdenv.base -> request -> (t, Command_error.t) result
(** Validates the request, then simulates {!Bactrian_core.Plan_sim.plan}. Every
    error is [Usage]. *)

val target_name : Plan_sim.assumption -> string
(** The verdict the plan aims for, as {!Bactrian_core.Report_data.verdict_name}
    names it. *)

val exit_code : t -> int
(** {!Exit_code.ok}. *)

val to_json : t -> Yojson.Safe.t
(** The schema version, the assumptions, the target and the answer. *)
