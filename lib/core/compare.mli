(** Comparison of a candidate variant with a baseline (spec §8).

    Pass rate is compared on the difference of equal-weighted per-scenario
    means. Cost, median latency and tokens are compared on
    [log (median_candidate / median_baseline)], with thresholds given as
    relative changes [δ] and applied as [log (1 + δ)]. *)

type metric = Pass_rate | Cost | Latency_p50 | Tokens

type verdict =
  | Regression
  | Improvement
  | No_change
  | Inconclusive of { lean : [ `Worse | `Better | `None ] }

type metric_result = {
  metric : metric;
  estimate : float;
      (** Pass rate difference, or log ratio for cost and latency. [nan] when
          the metric is undefined (a median is zero, or an arm has no values).
      *)
  interval : Stats.interval;
  threshold : float;
      (** On the scale of [estimate]: [δ] for pass rate, [log (1 + δ)] for the
          other metrics. *)
  level : float;
      (** Confidence level actually used (Bonferroni-adjusted for gate metrics).
      *)
  verdict : verdict;
  in_gate : bool;
      (** [false] for metrics outside the gate and for an undefined gate metric,
          which cannot gate. *)
}

type missing_reason = Errored | Not_run

type run = {
  scenario : string;
  variant : string;
  passed : bool option;  (** [None] for Errored and Not_run. *)
  missing : missing_reason option;
      (** Why [passed] is [None]. [None] here with [passed = None] counts as
          [Not_run]. Ignored when [passed] is [Some _]. *)
  cost : float option;
  latency_ms : float option;
  tokens : int option;
}

type input = {
  runs : run list;
  baseline : string;
  candidate : string;
  population : [ `Suite | `Scenarios ];
  confidence : float;
  gate : (metric * float) list;  (** Threshold per gate metric. *)
  report_metrics : (metric * float) list;
      (** Non-gate metrics with thresholds. *)
  seed : int;
  resamples : int;  (** Default 10_000. *)
  fail_on_inconclusive : bool;  (** Makes an inconclusive gate metric exit 2. *)
}

type result = {
  metrics : metric_result list;  (** Gate metrics first, then the others. *)
  per_scenario : (string * Stats.interval * verdict) list;
      (** Newcombe, Bonferroni across scenarios. Verdict threshold is the
          declared pass rate threshold, or 0 when none is declared. Sorted by
          scenario name. *)
  missing : (string * int * int) list;
      (** Variant, errored, not_run: baseline then candidate. *)
  exit : int;
      (** Spec §8 gate exit codes. 1 when a gate metric is a regression, else 3
          when missing data may bias the comparison, else 2 when a gate metric
          is inconclusive and [fail_on_inconclusive] is set, else 0. *)
}

val decide :
  threshold:float -> worse_is:[ `Lower | `Higher ] -> Stats.interval -> verdict
(** Both ends of the closed interval are compared strictly: an interval whose
    end lies exactly on [-threshold] is not wholly worse. The lean of an
    inconclusive interval follows the sign of its midpoint. *)

val run : input -> (result, string) Stdlib.result
(** [Error] for a [`Scenarios] population with fewer than 10 scenarios, a
    variant with no runs, or no scenario with data in both arms. *)
