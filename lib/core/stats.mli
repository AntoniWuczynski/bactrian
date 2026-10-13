(** Closed-form estimators and intervals. Pure and deterministic. *)

type interval = { lower : float; upper : float }

val wilson : successes:int -> trials:int -> confidence:float -> interval
(** Wilson score interval for a binomial proportion. [trials > 0]. The critical
    value is [-. normal_quantile ((1 -. confidence) /. 2.)], as in statsmodels
    [proportion_confint(method="wilson")]. Bounds are clamped to [[0, 1]], and
    are exactly [0.] at [successes = 0] and [1.] at [successes = trials].

    Raises [Invalid_argument] if [trials <= 0], [successes] is outside
    [[0, trials]] or [confidence] is outside [(0, 1)]. *)

val newcombe_diff :
  s1:int -> n1:int -> s2:int -> n2:int -> confidence:float -> interval
(** Newcombe's hybrid score interval (method 10) for p2 - p1. With Wilson
    intervals [(l1, u1)] and [(l2, u2)] at [confidence] and [d = p2 - p1]: lower
    [= d - sqrt ((p2 - l2)^2 + (u1 - p1)^2)], upper
    [= d + sqrt ((u2 - p2)^2 + (p1 - l1)^2)] (Newcombe 1998, Statistics in
    Medicine 17:873-890, method 10).

    Raises [Invalid_argument] on the same conditions as {!wilson}, per arm. *)

val stratified_newcombe_diff :
  baseline:(int * int) list ->
  candidate:(int * int) list ->
  confidence:float ->
  interval
(** Stratified Newcombe interval for the difference, candidate minus baseline,
    of the equal-weighted mean pass rate over K scenarios. Each list holds
    [(successes, trials)] per scenario, the same scenarios in the same order.

    Per arm, the stratified Wilson interval of Yan and Su (Yan X, Su XG, 2010,
    "Stratified Wilson and Newcombe confidence intervals for multiple binomial
    proportions", Statistics in Biopharmaceutical Research 2(3):329-335) with
    equal weights [w_k = 1/K]:
    - [p = (1/K) Σ_k s_k / n_k]
    - [n_eff = 1 / Σ_k (w_k² / n_k) = K² / Σ_k (1 / n_k)] ([K n] when every
      [n_k = n])
    - the Wilson score interval at [(p, n_eff)]: with
      [z = -. normal_quantile ((1 -. confidence) /. 2.)],
      [(p + z²/(2 n_eff) ± z sqrt (p (1 - p) / n_eff + z²/(4 n_eff²))) / (1 +
       z²/n_eff)], exactly [0.] when every [s_k = 0] and [1.] when every
      [s_k = n_k].

    The two arms are then combined with Newcombe's hybrid rule as in
    {!newcombe_diff}: with [(l1, u1)], [(l2, u2)] the arm intervals and
    [d = p2 - p1], lower [= d - sqrt ((p2 - l2)² + (u1 - p1)²)], upper
    [= d + sqrt ((u2 - p2)² + (p1 - l1)²)].

    With one scenario this is exactly {!newcombe_diff}. Swapping the arms
    negates the interval exactly. It never has zero width, so unanimous arms
    (every run passing) do not look certain: 3/3 against 3/3 gives ±0.56 at
    0.95.

    Raises [Invalid_argument] if the lists are empty or differ in length, or on
    the conditions of {!wilson} for any scenario. *)

val pass_hat_k : successes:int -> trials:int -> k:int -> float
(** Unbiased estimator C(c,k)/C(n,k) of the probability that k independent
    attempts all succeed. Computed as the product of [(c - i) / (n - i)] for
    [i < k], so it never overflows. [0.0] when [successes < k].

    Raises [Invalid_argument] unless [0 <= successes <= trials] and
    [1 <= k <= trials]. *)

val median : float array -> float
(** [quantile xs 0.5].

    Raises [Invalid_argument] if [xs] is empty. *)

val quantile : float array -> float -> float
(** Linear interpolation (type 7). The Hyndman-Fan type 7 quantile, as
    [numpy.quantile(method="linear")], of a sorted copy of the array. When an
    interpolation endpoint is infinite the infinite endpoint is returned.

    Raises [Invalid_argument] if the array is empty, contains NaN, or [q] is
    outside [[0, 1]]. *)

val normal_quantile : float -> float
(** Inverse standard normal CDF (Wichura's AS241, PPND16, relative accuracy
    about 1e-16). [normal_quantile 0. = neg_infinity],
    [normal_quantile 1. = infinity].

    Raises [Invalid_argument] if [p] is NaN or outside [[0, 1]]. *)

val normal_cdf : float -> float
(** Standard normal CDF, [0.5 *. Float.erfc (-. x /. sqrt 2.)], as
    [scipy.stats.norm.cdf]. *)

val student_t_quantile : df:int -> float -> float
(** Inverse CDF of Student's t with [df] degrees of freedom, as
    [scipy.stats.t.ppf]. The CDF is the closed form for integer [df] (Abramowitz
    and Stegun 26.7.3 and 26.7.4), inverted by bisection to the nearest float.

    Raises [Invalid_argument] if [df < 1] or [p] is outside [(0, 1)]. *)

val bonferroni_level : confidence:float -> family:int -> float
(** [1 - (1 - confidence) / family], the per-interval level that keeps the
    family-wise confidence at least [confidence].

    Raises [Invalid_argument] if [family < 1]. *)
