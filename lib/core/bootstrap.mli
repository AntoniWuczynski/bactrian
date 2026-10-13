(** Seeded percentile bootstrap intervals. Pure: every random draw comes from
    the [rng] passed in, so the same inputs and state give identical intervals.

    Intervals are percentile intervals (Efron and Tibshirani 1993, ch. 13): the
    [(1 - confidence) / 2] and [(1 + confidence) / 2] type 7 quantiles of the
    [resamples] replicate statistics. Percentile rather than BCa because the
    statistics here are bounded differences of means and log ratios whose
    replicate distributions are close to symmetric, and because percentile
    intervals transform exactly under negation, which the swap property needs.

    Swap symmetry: the draws are assigned to the two arms in a canonical order
    that depends on the data, not on which arm is the baseline, and the interval
    for the other orientation is the exact negation. So swapping [baseline] and
    [candidate] with the same seed gives exactly
    [{ lower = -. upper; upper = -. lower }], unless the two arms hold identical
    data, where the swap changes nothing.

    Shared preconditions, each a programming error raising [Invalid_argument]:
    [resamples >= 1], [confidence] in [(0, 1)], no NaN values. For the
    [arm list] functions, both lists name the same scenarios, each exactly once
    per list, and every arm has at least one value. *)

type arm = { scenario : string; values : float array }
(** One arm's runs of one scenario, e.g. 1.0/0.0 for pass/fail. *)

val stratified_diff :
  rng:Random.State.t ->
  resamples:int ->
  confidence:float ->
  baseline:arm list ->
  candidate:arm list ->
  Stats.interval
(** [suite] estimand: scenarios fixed, runs resampled within each scenario and
    arm; statistic = mean over scenarios of per-scenario mean (candidate minus
    baseline). Percentile interval. *)

val hierarchical_diff :
  rng:Random.State.t ->
  resamples:int ->
  confidence:float ->
  baseline:arm list ->
  candidate:arm list ->
  Stats.interval
(** [scenarios] estimand: scenarios resampled with replacement (the same draw
    for both arms), then runs within each. *)

val log_ratio_of_medians :
  rng:Random.State.t ->
  resamples:int ->
  confidence:float ->
  baseline:float array ->
  candidate:float array ->
  (float * Stats.interval) option
(** Point estimate and interval of log(median_B / median_A). [None] if either
    median is zero. Computed as [log median_B -. log median_A] so that a swap
    negates it exactly. A resample whose median is zero gives an infinite
    replicate, so an interval bound can be infinite.

    @raise Invalid_argument
      if either array is empty or holds a negative value. [None] also when every
      replicate is undefined (both resampled medians zero; such replicates are
      dropped). *)
