The fixture experiment (two variants, three scenarios, ten repeats each) as the
executor would have left it. Its experiment file is gone, so the variants have
to be named.

  $ ../snapshot.exe store .
  $ ID=20261013T090000Z-0badc0de

  $ bactrian compare $ID
  bactrian: --baseline is required: the experiment file demo.yaml that the manifest records is gone
  [64]
  $ bactrian compare $ID --baseline a
  bactrian: --candidate is required: the experiment file demo.yaml that the manifest records is gone
  [64]
  $ bactrian compare $ID --baseline a --candidate a
  bactrian: the baseline and the candidate are the same variant
  [64]
  $ bactrian compare $ID --baseline a --candidate c
  bactrian: no runs of variant "c" in this experiment
  [64]
  $ bactrian compare $ID --baseline a --candidate b
  bactrian: gate metric pass_rate has no threshold, because the experiment file demo.yaml that the manifest records is gone, and a threshold is never assumed
  [64]
  $ bactrian compare $ID --baseline a --candidate b --json
  {"error":"gate metric pass_rate has no threshold, because the experiment file demo.yaml that the manifest records is gone, and a threshold is never assumed","exit":64}
  [64]
  $ bactrian report $ID --baseline a --candidate b -o out/report.html
  bactrian: gate metric pass_rate has no threshold, because the experiment file demo.yaml that the manifest records is gone, and a threshold is never assumed
  [64]
  $ bactrian compare nope --baseline a --candidate b
  bactrian: no experiment "nope"
  [64]
  $ bactrian compare ../x --baseline a --candidate b
  bactrian: no experiment "../x"
  [64]

A gate metric without a threshold in the experiment file is refused too, and
the message names the metric.

  $ ../snapshot.exe experiment .
  $ grep -v thresholds demo.yaml > nothr.yaml && mv nothr.yaml demo.yaml
  $ bactrian compare $ID
  bactrian: gate metric pass_rate has no entry in compare.thresholds
  [64]

A mistake in the experiment file is reported where it is, not as a missing
setting.

  $ ../snapshot.exe experiment .
  $ printf '  confidence: 1.5\n' >> demo.yaml
  $ bactrian compare $ID
  demo.yaml:10:3: compare.confidence: must be strictly between 0 and 1, found 1.5
  [64]
  $ ../snapshot.exe experiment .

The terminal table. Without --gate the exit status is 0 whatever the verdict.

  $ bactrian compare $ID --baseline a --candidate b
  experiment demo (20261013T090000Z-0badc0de): candidate b against baseline a
  estimand suite, confidence 95%, seed 7, 10000 resamples
  cost and tokens compare scenario-weighted means, latency the geometric mean of per-scenario ratios of medians
  gate: pass_rate (level 0.9500 per interval)
  
  metric     gate  estimate  interval              level   threshold  verdict
  Pass rate  yes   -3.3 pp   -20.4 pp to +12.8 pp  0.9500  ±0.0 pp   Inconclusive, leans worse
  
  scenario  baseline  candidate  difference  interval              verdict
  book      8/10      8/8        +20.0 pp    -24.5 pp to +57.4 pp  Inconclusive, leans better
  refund    10/10     7/10       -30.0 pp    -65.9 pp to +12.2 pp  Inconclusive, leans worse
  search    10/10     10/10      0.0 pp      -36.4 pp to +36.4 pp  Inconclusive
  
  a: 30 runs, 0 errored, 0 not run
  b: 30 runs, 1 errored, 1 not run
  new behaviour in refund: lookup_order > refund > notify
  gate status: 3 (missing data may bias the comparison)

With --gate the status is the gate's. One errored and one unrun run in b but none
in a differ by more than 5 percentage points, so the comparison may be biased.

  $ bactrian compare $ID --baseline a --candidate b --gate > /dev/null
  [3]

  $ bactrian compare $ID --baseline a --candidate b --json | grep -o '"exit":[0-9]*'
  "exit":3
  $ bactrian compare $ID --baseline a --candidate b --json | grep -o '"schema_version":[0-9]*'
  "schema_version":1

A regression exits 1 under --gate, and an inconclusive verdict exits 2 only with
--fail-on-inconclusive.

  $ rm -rf .bactrian && ../snapshot.exe store . b-fails
  $ bactrian compare $ID --baseline a --candidate b --gate > /dev/null
  [1]
  $ rm -rf .bactrian && ../snapshot.exe store . clean
  $ bactrian compare $ID --baseline a --candidate b --gate | grep '^gate status'
  gate status: 0 (no gate metric regressed)
  $ bactrian compare $ID --baseline a --candidate b --gate --fail-on-inconclusive > /dev/null
  [2]

The report is one self-contained file.

  $ bactrian report $ID --baseline a --candidate b -o out/report.html
  wrote out/report.html
  $ grep -c '<h1>demo</h1>' out/report.html
  1
  $ grep -cE '(src|href) *= *"?(https?:)?//' out/report.html
  0
  [1]
  $ grep -c '<script' out/report.html
  0
  [1]
  $ rm demo.yaml
  $ bactrian report $ID -o out/report.html
  bactrian: --baseline is required: the experiment file demo.yaml that the manifest records is gone
  [64]
