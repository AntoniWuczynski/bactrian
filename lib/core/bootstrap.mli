(** Seeded percentile bootstrap intervals. Pure: every random draw comes from
    the [rng] passed in, so the same inputs and state give identical intervals.

    Intervals are percentile intervals (Efron and Tibshirani 1993, ch. 13): the
    [(1 - confidence) / 2] and [(1 + confidence) / 2] type 7 quantiles of the
    [resamples] replicate statistics. Percentile rather than BCa because the
    statistics here are bounded differences of means and log ratios whose
    replicate distributions are close to symmetric, and because percentile
    intervals transform exactly under negation, which the swap property needs.

    A percentile interval has zero width when every resample is identical, for
    example when every run of both arms passes, so pass-rate comparisons do not
    use it alone: see {!Stats.stratified_newcombe_diff} and {!Compare}.

    Swap symmetry: the draws are assigned to the two arms in a canonical order
    that depends on the data, not on which arm is the baseline, and the interval
    for the other orientation is the exact negation. So swapping [baseline] and
    [candidate] with the same seed gives exactly
    [{ lower = -. upper; upper = -. lower }], unless the two arms hold identical
    data, where the swap changes nothing.

    Shared preconditions, each a programming error raising [Invalid_argument]:
    [resamples >= 1], [confidence] in [(0, 1)], no NaN values, both lists name
    the same scenarios, each exactly once per list, and every arm has at least
    one value. *)

type arm = { scenario : string; values : float array }
(** One arm's runs of one scenario, e.g. 1.0/0.0 for pass/fail. *)

val hierarchical_diff :
  rng:Random.State.t ->
  resamples:int ->
  confidence:float ->
  baseline:arm list ->
  candidate:arm list ->
  Stats.interval
(** [scenarios] estimand: scenarios resampled with replacement (the same draw
    for both arms), then runs within each. *)

(** {1 Value metrics}

    Both statistics below are log ratios of the candidate to the baseline over K
    scenarios, with each scenario weighted equally. Each resample draws runs
    with replacement within every scenario and arm, so every scenario keeps its
    run count (stratified resampling: under a fixed suite, varying how many runs
    come from cheap and dear scenarios would add variation that does not exist).
    A resample whose mean or median is zero gives an infinite replicate, so an
    interval bound can be infinite. A replicate that is undefined (zero in both
    arms) is dropped, and the result is [None] when every replicate is.

    Raises [Invalid_argument] on a negative value. *)

val log_ratio_of_means :
  rng:Random.State.t ->
  resamples:int ->
  confidence:float ->
  baseline:arm list ->
  candidate:arm list ->
  (float * Stats.interval) option
(** [log (mean_B / mean_A)], where an arm's mean is the equal-weighted average
    over scenarios of its per-scenario mean. For spend and tokens, which add up:
    a change confined to a few scenarios moves this mean in proportion. [None]
    if either mean is zero.

    The interval is the expanded percentile interval: the percentile interval at
    {!expanded_level} for the fewest runs any scenario has in either arm. A
    plain percentile interval of a mean of skewed values is too narrow with few
    runs (simulated coverage 0.83 to 0.88 at a nominal 0.90 for lognormal costs
    with 5 to 40 runs per scenario). *)

val expanded_level : confidence:float -> n:int -> float
(** The level of Hesterberg's expanded percentile interval (Hesterberg 2015,
    "What Teachers Should Know About the Bootstrap", The American Statistician
    69(4):371-386): the level whose normal critical value is
    [sqrt (n / (n - 1)) * t] with [t] the [(1 + confidence) / 2] quantile of
    Student's t on [n - 1] degrees of freedom. It widens a bootstrap interval of
    a mean from [n] values to make up for the bootstrap's own narrowness at
    small [n]. [confidence] itself when [n < 2]. *)

val median_p90_intervals :
  rng:Random.State.t ->
  resamples:int ->
  confidence:float ->
  float array list ->
  Stats.interval * Stats.interval
(** Percentile intervals for the median and the 90th percentile (type 7) of
    values that come in clusters, such as the model calls of one run. Each
    resample draws whole clusters with replacement and pools their values, so
    values of one cluster stay together. Raises [Invalid_argument] on no
    clusters, an empty cluster, a NaN value, or the shared preconditions on
    [resamples] and [confidence]. *)

val mean_log_ratio_of_medians :
  rng:Random.State.t ->
  resamples:int ->
  confidence:float ->
  baseline:arm list ->
  candidate:arm list ->
  (float * Stats.interval) option
(** [(1/K) Σ_k log (median_B,k / median_A,k)], the equal-weighted average over
    scenarios of per-scenario log ratios of medians: the typical scenario's
    relative change, robust to a few slow runs. [None] if any per-scenario
    median is zero. *)
