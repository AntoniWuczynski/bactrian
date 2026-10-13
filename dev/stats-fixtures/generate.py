# /// script
# requires-python = ">=3.12"
# dependencies = [
#   "scipy==1.18.1",
#   "statsmodels==0.15.0",
#   "numpy==2.5.3",
# ]
# ///
# pyright: strict
"""Generate reference fixtures for bactrian's statistics code."""

import json
import math
from decimal import Decimal, localcontext
from fractions import Fraction
from pathlib import Path
from typing import Final, Protocol, cast

import numpy as np
from numpy.typing import NDArray
import statsmodels.stats.proportion as _sm_proportion  # type: ignore[import-untyped]  # statsmodels ships no type information and no stubs exist
from scipy.stats import norm
from scipy.stats import t as student_t

OUT_DIR: Final = Path(__file__).resolve().parent.parent.parent / "test" / "fixtures" / "stats"

CONFIDENCES: Final = [0.8, 0.9, 0.95, 0.99, 1 - 0.05 / 3, 1 - 0.05 / 12]
WILSON_COUNTS: Final = [
    (0, 1), (1, 1), (0, 10), (10, 10), (1, 10), (5, 10), (3, 7),
    (0, 100), (100, 100), (37, 100), (499, 1000),
]  # fmt: skip
# (s1, n1, s2, n2)
NEWCOMBE_CASES: Final = [
    (0, 10, 10, 10), (10, 10, 0, 10), (5, 10, 5, 10), (0, 1, 1, 1), (1, 1, 0, 1),
    (0, 1, 0, 1), (1, 1, 1, 1), (1, 2, 2, 3), (3, 7, 5, 9), (0, 5, 3, 5),
    (2, 20, 9, 20), (37, 100, 52, 100), (499, 1000, 540, 1000), (0, 100, 100, 100),
    (10, 10, 10, 10), (0, 10, 0, 10), (4, 12, 4, 30),
]  # fmt: skip
NEWCOMBE_CONFIDENCES: Final = [0.9, 0.95, 0.99, 1 - 0.05 / 3]
NORMAL_PS: Final = [1e-12, 1e-6, 0.001, 0.01, 0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975, 0.99, 0.999, 1 - 1e-6]
QUANTILE_ARRAYS: Final = [
    [3.0], [1.0, 2.0, 3.0, 4.0, 5.0], [4.0, 1.0, 3.0, 2.0], [2.0, 2.0, 2.0, 5.0, 9.0, 9.0],
    [0.5, 10.0, 1.5, 7.25, 3.0, 3.0], [-3.0, 0.0, 100.0],
]  # fmt: skip
QUANTILE_QS: Final = [0.0, 0.1, 0.25, 0.5, 0.9, 1.0]
PASS_K_CASES: Final = [
    (1, 0, 1), (1, 1, 1), (5, 0, 1), (5, 3, 1), (5, 3, 3), (5, 3, 4), (5, 5, 5), (5, 2, 5),
    (10, 7, 2), (10, 7, 7), (10, 7, 8), (10, 10, 10), (20, 13, 5), (50, 31, 10), (100, 60, 20),
]  # fmt: skip
BONFERRONI_CASES: Final = [(0.95, 1), (0.95, 3), (0.95, 12), (0.9, 2), (0.99, 5), (0.8, 4)]
# (baseline, candidate), each a list of (successes, trials) per scenario, in the same scenario order.
STRATIFIED_CASES: Final = [
    ([(3, 3)], [(3, 3)]), ([(5, 10)], [(3, 10)]), ([(0, 4)], [(4, 4)]), ([(3, 7)], [(5, 9)]),
    ([(3, 3), (3, 3)], [(3, 3), (2, 3)]),
    ([(5, 5), (5, 5), (5, 5)], [(4, 5), (4, 5), (4, 5)]),
    ([(2, 4), (7, 9), (0, 6)], [(3, 5), (9, 9), (1, 7)]),
    ([(0, 3), (0, 5)], [(0, 3), (0, 5)]),
    ([(3, 3)] * 10, [(3, 3)] * 10),
    ([(10, 10)] * 10, [(10, 10)] * 10),
    ([(20, 20), (19, 20), (16, 20), (10, 20), (4, 20)], [(19, 20), (18, 20), (15, 20), (9, 20), (3, 20)]),
]  # fmt: skip
STRATIFIED_CONFIDENCES: Final = [0.9, 0.95, 1 - 0.05 / 3]
T_DFS: Final = [1, 2, 3, 4, 5, 9, 29, 199]
T_PS: Final = [0.6, 0.9, 0.95, 0.975, 0.995, 1 - 0.05 / 12, 0.9999]
NORMAL_CDF_XS: Final = [-8.0, -3.0, -1.0, 0.0, 0.5, 1.959963984540054, 4.0]
# (runs per scenario and arm, confidence)
EXPANDED_CASES: Final = [(n, c) for n in [2, 5, 10, 40, 400] for c in [0.9, 0.95, 0.975, 1 - 0.05 / 3]]


class _ProportionConfint(Protocol):
    def __call__(self, count: int, nobs: int, alpha: float, method: str) -> tuple[float, float]: ...


class _Confint2Indep(Protocol):
    def __call__(
        self, count1: int, nobs1: int, count2: int, nobs2: int, method: str, compare: str, alpha: float
    ) -> tuple[float, float]: ...


# The only place the untyped statsmodels API is touched: each function is given its documented signature here.
proportion_confint = cast(_ProportionConfint, _sm_proportion.proportion_confint)  # type: ignore[misc,unused-ignore]  # statsmodels is untyped, cast to its documented signature
confint_proportions_2indep = cast(_Confint2Indep, _sm_proportion.confint_proportions_2indep)  # type: ignore[misc,unused-ignore]  # statsmodels is untyped, cast to its documented signature


def _wilson(successes: int, trials: int, confidence: float) -> tuple[float, float]:
    return proportion_confint(successes, trials, alpha=1 - confidence, method="wilson")


def _newcombe_p2_minus_p1(s1: int, n1: int, s2: int, n2: int, confidence: float) -> tuple[float, float]:
    # statsmodels computes p1 - p2 for (count1, nobs1, count2, nobs2), so the arms are swapped to get p2 - p1.
    return confint_proportions_2indep(s2, n2, s1, n1, method="newcomb", compare="diff", alpha=1 - confidence)


def _norm_ppf(p: float) -> float:
    return float(norm.ppf(p))


def _norm_cdf(x: float) -> float:
    return float(norm.cdf(x))


def _t_ppf(p: float, df: int) -> float:
    return float(student_t.ppf(p, df))


def _expanded_level(n: int, confidence: float) -> float:
    # Hesterberg (2015), "What Teachers Should Know About the Bootstrap", The American Statistician 69(4):371-386:
    # the expanded percentile interval uses the bootstrap quantiles at the level whose normal critical value is
    # sqrt(n / (n - 1)) t_{(1 + confidence) / 2, n - 1}, which corrects the narrowness of a small-sample bootstrap.
    z = math.sqrt(n / (n - 1)) * _t_ppf((1 + confidence) / 2, n - 1)
    return 1 - 2 * float(norm.sf(z))


def _quantile(values: list[float], q: float) -> float:
    arr: NDArray[np.float64] = np.array(values, dtype=np.float64)
    return float(np.quantile(arr, q, method="linear"))


def _to_decimal(x: Fraction) -> Decimal:
    return Decimal(x.numerator) / Decimal(x.denominator)


def _stratified_wilson(arm: list[tuple[int, int]], z: Decimal) -> tuple[Decimal, Decimal, Decimal]:
    """Yan and Su stratified Wilson interval for the equal-weighted mean of the per-scenario rates.

    Written independently of the OCaml code: the point estimate and effective sample size are exact
    fractions, and the bounds are the two roots of (p_hat - p)^2 n_eff = z^2 p (1 - p), solved with the
    quadratic formula in 60-digit decimal arithmetic.
    """
    k = len(arm)
    p_hat = sum((Fraction(s, n) for s, n in arm), Fraction(0)) / k
    n_eff = 1 / sum((Fraction(1, k * k * n) for _, n in arm), Fraction(0))
    p, n = _to_decimal(p_hat), _to_decimal(n_eff)
    z2 = z * z
    a = n + z2
    b = -(2 * n * p + z2)
    c = n * p * p
    root = (b * b - 4 * a * c).sqrt()
    return p, (-b - root) / (2 * a), (-b + root) / (2 * a)


def _stratified_newcombe(
    baseline: list[tuple[int, int]], candidate: list[tuple[int, int]], confidence: float
) -> tuple[float, float]:
    with localcontext() as ctx:
        ctx.prec = 60
        z = Decimal(-_norm_ppf((1 - confidence) / 2))
        pa, la, ua = _stratified_wilson(baseline, z)
        pb, lb, ub = _stratified_wilson(candidate, z)
        d = pb - pa
        lower = d - ((pb - lb) ** 2 + (ua - pa) ** 2).sqrt()
        upper = d + ((ub - pb) ** 2 + (pa - la) ** 2).sqrt()
        return float(lower), float(upper)


def _write(name: str, convention: str, entries: list[dict[str, object]]) -> None:
    doc = {"convention": convention, "entries": entries}
    (OUT_DIR / name).write_text(json.dumps(doc, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    wilson: list[dict[str, object]] = []
    for s, n in WILSON_COUNTS:
        for c in sorted(CONFIDENCES):
            lo, hi = _wilson(s, n, c)
            wilson.append({"successes": s, "trials": n, "confidence": c, "lower": lo, "upper": hi})
    _write("wilson.json", "Wilson score interval for a binomial proportion, statsmodels proportion_confint(method='wilson')", wilson)

    newcombe: list[dict[str, object]] = []
    for s1, n1, s2, n2 in NEWCOMBE_CASES:
        for c in sorted(NEWCOMBE_CONFIDENCES):
            lo, hi = _newcombe_p2_minus_p1(s1, n1, s2, n2, c)
            newcombe.append({"s1": s1, "n1": n1, "s2": s2, "n2": n2, "confidence": c, "lower": lo, "upper": hi})
    _write("newcombe.json", "Newcombe hybrid score (method 10) interval for p2 - p1, where p1 = s1/n1 is the baseline and p2 = s2/n2 the candidate", newcombe)

    _write("normal_quantile.json", "scipy.stats.norm.ppf(p)", [{"p": p, "z": _norm_ppf(p)} for p in NORMAL_PS])

    _write("normal_cdf.json", "scipy.stats.norm.cdf(x)", [{"x": x, "p": _norm_cdf(x)} for x in NORMAL_CDF_XS])

    _write(
        "student_t_quantile.json",
        "scipy.stats.t.ppf(p, df)",
        [{"df": df, "p": p, "t": _t_ppf(p, df)} for df in T_DFS for p in T_PS],
    )

    _write(
        "expanded_level.json",
        "Expanded percentile level (Hesterberg 2015): 1 - 2 * scipy.stats.norm.sf(z) with "
        "z = sqrt(n / (n - 1)) * scipy.stats.t.ppf((1 + confidence) / 2, n - 1)",
        [{"n": n, "confidence": c, "level": _expanded_level(n, c)} for n, c in EXPANDED_CASES],
    )

    _write(
        "quantile.json",
        "numpy.quantile(values, q, method='linear'), Hyndman-Fan type 7",
        [{"values": v, "q": q, "quantile": _quantile(v, q)} for v in QUANTILE_ARRAYS for q in QUANTILE_QS],
    )

    _write(
        "pass_k.json",
        "C(c,k)/C(n,k) with exact integers (math.comb), then converted to float; 0.0 when c < k",
        [{"n": n, "c": c, "k": k, "value": math.comb(c, k) / math.comb(n, k)} for n, c, k in PASS_K_CASES],
    )

    _write(
        "bonferroni.json",
        "level = 1 - (1 - confidence) / family",
        [{"confidence": c, "family": m, "level": 1 - (1 - c) / m} for c, m in BONFERRONI_CASES],
    )

    stratified: list[dict[str, object]] = []
    for base, cand in STRATIFIED_CASES:
        for c in sorted(STRATIFIED_CONFIDENCES):
            lo, hi = _stratified_newcombe(base, cand, c)
            if len(base) == 1:
                # With one scenario the construction is Newcombe's method 10, so statsmodels checks the reference.
                (s1, n1), (s2, n2) = base[0], cand[0]
                sm_lo, sm_hi = _newcombe_p2_minus_p1(s1, n1, s2, n2, c)
                assert abs(lo - sm_lo) < 1e-12 and abs(hi - sm_hi) < 1e-12, (base, cand, c)
            stratified.append(
                {
                    "baseline": [list(x) for x in base],
                    "candidate": [list(x) for x in cand],
                    "confidence": c,
                    "lower": lo,
                    "upper": hi,
                }
            )
    _write(
        "stratified_newcombe.json",
        "Stratified Newcombe interval for the candidate minus baseline difference of equal-weighted mean pass rates. "
        "Per arm, the Yan and Su (2010) stratified Wilson interval: p = (1/K) sum s_k/n_k, n_eff = 1 / sum 1/(K^2 n_k), "
        "Wilson score interval at (p, n_eff); the arms are combined with Newcombe's hybrid rule (method 10). "
        "statsmodels has no stratified Wilson interval, so these values come from an independent Python implementation "
        "in generate.py: p and n_eff as exact fractions, the bounds as the roots of (p - x)^2 n_eff = z^2 x (1 - x) in "
        "60-digit decimal arithmetic, with z = scipy.stats.norm.ppf. Entries with one scenario are also checked against "
        "statsmodels confint_proportions_2indep(method='newcomb') to 1e-12. Each arm lists (successes, trials) per scenario, "
        "in the same scenario order.",
        stratified,
    )


if __name__ == "__main__":
    main()
