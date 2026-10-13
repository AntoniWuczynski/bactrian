(** Power simulation for [bactrian plan].

    The simulator runs the exact decision rule of {!Compare}: it draws pass/fail
    outcomes for both arms, hands them to {!Compare.val-run} and records whether
    the pass-rate verdict is the correct one. It is pure and deterministic:
    every random draw comes from a [Random.State.t] derived from [seed], the
    repeat count and the simulation index, never from the global generator.

    Inside the simulation the bootstrap uses 400 resamples rather than the
    10,000 of a real comparison, to keep a planning call fast. The Monte Carlo
    error of the power estimate is reported through [mc_se]. *)

type assumption = {
  baseline_rates : (string * float) list;
      (** True baseline pass rate per scenario. *)
  true_effect : float;
      (** True drop in pass rate (positive means the candidate is worse). The
          candidate rate of a scenario is [baseline - true_effect], clamped to
          [0, 1]. *)
  threshold : float;  (** The regression threshold δ, as given to {!Compare}. *)
  between_sd : float option;
      (** Under [`Scenarios], the standard deviation of the per-scenario true
          effect around [true_effect], drawn afresh in every simulation. Ignored
          under [`Suite]. *)
  population : [ `Suite | `Scenarios ];
  confidence : float;
  power : float;  (** Required probability of the correct verdict. *)
  max_repeats : int;
      (** Largest repeat count per scenario and variant tried. *)
  seed : int;
  simulations : int;  (** Simulated experiments per candidate repeat count. *)
}

type answer =
  | Repeats of { n : int; achieved_power : float; mc_se : float }
      (** Smallest repeat count found reaching the power, with the simulated
          power and its Monte Carlo standard error
          [sqrt (p (1 - p) / simulations)]. *)
  | Unreachable of { best_power : float }
      (** No repeat count up to [max_repeats] reaches the power. *)

val plan : assumption -> answer
(** The correct verdict is [Regression] when [true_effect >= threshold],
    [Improvement] when [true_effect <= -threshold] and [No_change] otherwise.
    The repeat count is searched by doubling, then bisection.

    Raises [Invalid_argument] if [baseline_rates] is empty, [max_repeats] or
    [simulations] is below 1, or {!Compare.val-run} rejects the simulated input
    (for example [`Scenarios] with fewer than 10 scenarios). *)

val power_at : assumption -> repeats:int -> float
(** Simulated probability of the correct verdict at one repeat count ([power]
    and [max_repeats] are ignored). Same exceptions as {!plan}. *)

val extra_runs_needed :
  Compare.input -> metric:Compare.metric -> [ `Repeats of int | `Cannot_resolve ]
(** Extra repeats per scenario and variant that would resolve the metric's
    verdict, simulating with the observed per-scenario pass rates of both arms
    as the truth (power 0.8, 200 simulations, at most 200 repeats in total).
    [`Repeats 0] when the verdict is already conclusive. [`Cannot_resolve] when
    no repeat count reaches the power (for example an estimate sitting on the
    threshold), when the comparison itself fails, or for metrics other than
    [Pass_rate], which the simulator does not model. *)
