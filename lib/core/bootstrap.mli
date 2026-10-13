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

val log_ratio_of_medians :
  rng:Random.State.t ->
  resamples:int ->
  confidence:float ->
  baseline:arm list ->
  candidate:arm list ->
  (float * Stats.interval) option
(** Point estimate and interval of log(median_B / median_A), each median taken
    over the arm's values pooled across its scenarios. Each resample draws runs
    with replacement within every scenario and arm, so every scenario keeps its
    run count, and recomputes the pooled medians (stratified resampling: under a
    fixed suite, varying how many runs come from cheap and dear scenarios would
    add variation that does not exist). [None] if either median is zero.
    Computed as [log median_B -. log median_A] so that a swap negates it
    exactly. A resample whose median is zero gives an infinite replicate, so an
    interval bound can be infinite.

    Raises [Invalid_argument] on a negative value. [None] also when every
    replicate is undefined (both resampled medians zero; such replicates are
    dropped). *)
