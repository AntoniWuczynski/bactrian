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

val bonferroni_level : confidence:float -> family:int -> float
(** [1 - (1 - confidence) / family], the per-interval level that keeps the
    family-wise confidence at least [confidence].

    Raises [Invalid_argument] if [family < 1]. *)
