# Statistics reference fixtures

`generate.py` writes the reference values that the OCaml statistics tests compare against: Wilson and Newcombe intervals (statsmodels), normal quantiles (SciPy), type 7 quantiles (NumPy), the unbiased pass^k estimator and Bonferroni levels. The output goes to `test/fixtures/stats/*.json` and is committed. Each file records its inputs and expected outputs with full float precision, and the Newcombe file states that its interval is for p2 - p1 (baseline 1, candidate 2).

Regenerate with `uv run dev/stats-fixtures/generate.py`. The library versions are pinned in the script's inline metadata, and the output is deterministic.
