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
from pathlib import Path
from typing import Final, Protocol, cast

import numpy as np
from numpy.typing import NDArray
import statsmodels.stats.proportion as _sm_proportion  # type: ignore[import-untyped]  # statsmodels ships no type information and no stubs exist
from scipy.stats import norm

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


def _quantile(values: list[float], q: float) -> float:
    arr: NDArray[np.float64] = np.array(values, dtype=np.float64)
    return float(np.quantile(arr, q, method="linear"))


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


if __name__ == "__main__":
    main()
