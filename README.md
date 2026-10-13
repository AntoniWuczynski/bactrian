# bactrian

bactrian is a tool for testing AI agents the way you would test any other program. It will run your agent as a subprocess, whatever language it is written in and whichever model provider it calls. It records every model call, tool call and network request the agent makes, and can replay a recording exactly, so you can see the precise step where a new version of the agent started behaving differently. It also runs agents live against several models or configurations, repeats each scenario enough times to separate a real change from noise, and tells you whether a change is a regression, an improvement, no change, or still too close to call.

The name comes from the two-humped camel. Most of what bactrian does is compare two things.

## Status

Early development. Strict replay of existing recordings works. Live recording against real providers does not work yet, because it needs HTTPS support that is not built. MCP interception, the sandbox and the MCP server are also still to come. Until then the settings that ask for them (`sandbox`, `egress`, `intercept`, `mcp` and `clock`) are read but not enforced. `sandbox: strict` stops with exit status 69 because there is no sandbox to be strict about, and every other unenforced setting is named in a warning when a run starts and recorded in each run's `meta.json` under `unenforced`. A `no_egress` assertion fails as unverifiable, because no HTTP call is recorded yet.

## Recording and replaying an agent

An agent talks to bactrian over a small JSON Lines protocol on stdin and stdout: it receives `start`, one `turn` per user message and `end`, and answers each turn with a `reply`. bactrian points the agent's model SDK at its own proxy through the usual base-URL variables (`OPENAI_BASE_URL`, `ANTHROPIC_BASE_URL`, `GOOGLE_GEMINI_BASE_URL`, `OPENROUTER_BASE_URL`) and gives it placeholder API keys. The real keys are never placed in the agent's environment and are added to requests on their way upstream. Until the sandbox arrives the agent runs as the same user as bactrian, so this keeps keys out of the agent's environment but does not stop a hostile agent from finding them by other means. [`examples/agents/`](examples/agents/) has a TypeScript agent (build it with `pnpm install --dir examples/agents/typescript` and `pnpm --dir examples/agents/typescript build`) and a Python agent managed by uv.

Record a run (live spending needs an explicit cap per provider, in US dollars). This needs a build with HTTPS support, which does not exist yet, so for now it cannot reach a real provider:

```sh
BACTRIAN_CAP_OPENAI=0.50 bactrian record --turn "What is the weather in Paris?" -- node examples/agents/typescript/dist/agent.js
```

Replay it without touching the network, and see where a changed agent first behaves differently:

```sh
bactrian replay <run-id> -- node examples/agents/typescript/dist/agent.js
bactrian replay <run-id> --mode until-divergence -- uv run --project examples/agents/python examples/agents/python/agent.py
```

Recordings live in `./.bactrian/runs/`. Each run directory holds the recording (`cassette.jsonl`), a normalised trace of the conversation (`trace.jsonl`), `meta.json`, `divergences.json` and the agent's stderr. `meta.json` also carries the run's cost in US dollars as the ledger attributed it, the total latency and token count of its model calls, and the `normalise` and `redact` patterns it was recorded under. A replay applies those patterns, whatever the command line says, so requests that a pattern changed still match. `divergences.json` lists each replay divergence with its kind, the nearest recorded exchange, the similarity and the paths that differ. Add `--json` for machine-readable output. Exit status 0 means the run passed, 1 that the agent failed or the replay diverged. `bactrian --help` lists the rest.

## Experiments

A scenario says what to ask the agent and how to judge the answer. An experiment runs scenarios against variants of the agent, several times each. Both are YAML (JSON also works) and unknown fields are rejected.

```yaml
# scenarios/schedule.yaml
version: 1
id: schedule-meeting
turns:
  - user: "You promised Amy a meeting, can you set it up?"
assertions:
  - tool_sequence: { mode: contains, tools: [calendar_read, calendar_create] }
  - output: { turn: last, regex: "(?i)wednesday" }
  - max_steps: 12
checkers:
  - command: ["python3", "checks/no_double_booking.py"]
golden:
  pass: [golden/ok.trace.jsonl]
  fail: [golden/double-booked.trace.jsonl]
```

```yaml
# experiment.yaml
version: 1
name: luna-vs-flash
agent:
  protocol: command
  command: ["node", "dist/adapter.js"]
scenarios: ["scenarios/*.yaml"]
variants:
  luna: { env: { AGENT_MODEL: gpt-6-luna } }
  flash: { env: { AGENT_MODEL: gemini-3.8-flash } }
repeats: 20
concurrency: 4
caps_usd: { openai: 10, gemini: 20 }
seed: 1
```

Check the assertions and checkers against the scenario's golden traces before trusting them (every `pass` trace must pass and every `fail` trace must fail), then run the experiment. A scenario that has assertions or checkers must have golden traces, and a scenario with `pass` traces and no `fail` trace gets a warning, because an assertion that always passes would go unnoticed:

```sh
bactrian check scenarios/*.yaml
bactrian run experiment.yaml
```

`check` exits 1 and lists the offending traces (or the missing golden traces) otherwise, and both commands exit 64 on a configuration error, printing every mistake of every file as `file:line:column` in the order it appears. `run` allocates the runs in a seeded, blocked and interleaved order, keeps its progress in `./.bactrian/experiments/<id>/manifest.json`, stops starting runs once a spend cap is reached, and prints a line per run (`--json` for one JSON object). Interrupt it with Ctrl-C and carry on later with `bactrian run experiment.yaml --resume <id>`. A run that passed, failed or diverged is never repeated. A run that never started, was interrupted, was not run because a spend cap was reached or errored (a fault of bactrian's own, or a checker that could not give a verdict) is run again under a new run ID, and the manifest keeps the old one under `previous_run_ids`. So raising `caps_usd` and resuming fills in the runs that the cap held back. The resume is refused, naming what changed, if the experiment file (apart from `caps_usd` and `concurrency`), the agent, a scenario file or the price file is different from when the experiment started. Only one process can run an experiment at a time. Each finished run's directory also gets a `verdicts.json` with the result of every assertion and checker. A failing assertion or checker makes a passing run `failed`, while a checker that crashes, hangs or prints something that is not a verdict makes it `errored`, which is left out of the pass rates and listed separately. `run` exits 70 if any run errored, so a continuous integration job sees it. The checker commands run in the scenario file's directory and the agent in the experiment file's. Like recording, `run` calls real providers, so for now it needs HTTPS support that is not built yet.

Four more fields shape a run. A scenario's `fixtures` names a directory, relative to the scenario file, whose files are copied into the run's `BACTRIAN_STATE_DIR` before the agent starts, and a directory that does not exist is a configuration error. In the experiment, `normalise` lists regular expressions that are masked out of each model request before it is keyed, so two runs that differ only in, say, a date still match on replay, and each run's `meta.json` counts the masks that fired in `findings.normalise_masks`. `redact` lists further regular expressions whose matches are replaced by numbered placeholders in the cassette, on top of the provider key formats. They are matched against the strings inside the JSON of each request and response, one string at a time, so a wide pattern such as `Bearer .+` stops at the end of its string and cannot damage the JSON around it. `normalise` leaves those placeholders alone. `pricing` names a YAML file, relative to the experiment, that overrides or extends the built-in price table:

```yaml
# prices.yaml (USD per million tokens)
version: 1
prices:
  - model: gpt-6-luna
    provider: openai          # openai, anthropic, gemini or openrouter
    effective_from: 2026-11-01  # optional
    rates: { input: 0.2, cached_input: 0.02, cache_write: 0.25, output: 1.0 }
    max_output: 65536
    max_input: 272000
```

An entry replaces the built-in one with the same model and date, and any other entry adds a model or a new price period. The table is used for spend reservations, settlement and `max_cost_usd`. The names under `caps_usd` must be `openai`, `anthropic`, `gemini` or `openrouter`.

## Comparing variants

Once an experiment has run, `compare` turns its runs into a verdict per metric, with confidence intervals rather than bare averages:

```sh
bactrian compare <experiment-id> --baseline luna --candidate flash --gate
```

The estimand (`suite` or `scenarios`), the confidence level, the gate and the thresholds come from the `compare` section of the experiment file, which the manifest points back to. `--baseline` and `--candidate` override it, the candidate defaults to the other variant when there are two, and if the experiment file is gone both flags are required. A mistake in the file is reported at its line and column, exit 64. Every gate metric needs a threshold under `thresholds`. None is ever assumed, so a missing one (including a missing experiment file or `compare` section) is a usage error, exit 64, naming the metric.

```yaml
compare:
  baseline: luna
  confidence: 0.95
  population: suite          # or scenarios, which needs at least 10 scenarios
  gate: [pass_rate, cost]    # default: [pass_rate]
  thresholds: { pass_rate: 0.05, cost: 0.20, latency_p50: 0.25 }
```

Each metric is a `regression`, an `improvement`, `no-change` or `inconclusive`, depending on whether its whole interval lies beyond, within or across the threshold. Cost, latency and token thresholds are relative and apply to the log of the ratio, so `cost: 0.20` calls a regression above 20% dearer and an improvement below 16.7% cheaper, and the report states both. Cost and tokens compare the mean per run, weighting every scenario equally, so a change confined to a few scenarios still shows. Latency compares each scenario's median and averages the ratios over scenarios (a geometric mean), so a few slow runs do not dominate. Cost, latency and tokens need at least 5 values per scenario in each arm, otherwise the metric is undefined. Intervals for gate metrics are widened (Bonferroni) so the chance of any false regression stays within the confidence level. The report also lists new tool-call sequences, meaning ones in at least 10% of the candidate's runs and none of the baseline's.

Without `--gate` the exit status is 0. With it, 1 means a gate metric regressed, 2 that one is inconclusive (only with `--fail-on-inconclusive`), and 3 that missing data could bias the result, because more than 5% of an arm errored, the share of errored and unrun runs differs between the arms by more than 5 percentage points, or a scenario has no finished run in one arm (it is left out of the comparison and listed). They are checked in the order 1, 3, 2: a regression wins over missing data, and missing data over an inconclusive metric. An undefined gate metric counts as inconclusive, never as a pass. For an inconclusive pass rate in the gate, `compare` and `report` also say how many more repeats per scenario and variant would resolve it, by simulating the same decision rule with the observed rates as the truth, or that more repeats would not. Under `population: scenarios` that simulation runs a bootstrap per simulated experiment and can take minutes. `--json` prints the verdicts as one JSON object with a schema version, with `extra_repeats` on each metric.

`plan` says how many repeats are needed before you run anything, by simulating the same decision rule:

```sh
bactrian plan --baseline-rate 0.9 --threshold 0.05 --scenarios 12
```

The assumed effect defaults to twice the threshold and the power to 0.8, and the first line names the verdict it plans for: a regression when the assumed drop is at least the threshold, an improvement when it is at most minus the threshold, otherwise no change. The power it prints comes from a fresh simulation at the chosen repeat count, so it can sit just below the target. It answers `unreachable` if no repeat count up to `--max-repeats` (200) gets there. `--population scenarios` also needs `--between-sd`. When the gate has more than one metric, pass `--gate-metrics N`: the pass rate is then judged at the Bonferroni level, as `compare` judges it, which needs more repeats. `--from <experiment-id>` takes the scenarios and their baseline rates from an earlier experiment's finished runs, of `compare.baseline` unless `--variant` names another.

`report` writes everything as one HTML file with inline styles and charts and no scripts or external assets, so it works offline and follows the system's light or dark theme:

```sh
bactrian report <experiment-id> -o report.html
```

It holds the estimand, gate and missing data per arm, the verdicts, a forest plot per scenario, each scenario and variant's pass rate (with a Wilson interval), pass^k for every k and the median and 90th percentile of cost, tokens, latency and time to first token (with bootstrap intervals), cost, latency and token charts over the compared scenarios, the signature comparison, and each failure with its assertion messages and a collapsible trace.

## Building from source

You need [opam](https://opam.ocaml.org). From the repository root:

```sh
opam switch create . ocaml-base-compiler.5.5.1 --deps-only --with-test --locked
opam exec -- dune build
opam exec -- dune test
```

`dune test` needs only OCaml. The acceptance test runs the real example agents, so it has its own alias, `opam exec -- dune build @acceptance`. It needs Node 24 with pnpm, and uv. Prepare the agents first:

```sh
pnpm install --frozen-lockfile --dir examples/agents/typescript
pnpm --dir examples/agents/typescript build
uv sync --locked --project examples/agents/python
```

The design decisions behind the code are recorded in [`docs/decisions/`](docs/decisions/).

## Licence

Apache-2.0. See [LICENSE](LICENSE).
