# bactrian

bactrian is a tool for testing AI agents the way you would test any other program. It will run your agent as a subprocess, whatever language it is written in and whichever model provider it calls. It records every model call, tool call and network request the agent makes, and can replay a recording exactly, so you can see the precise step where a new version of the agent started behaving differently. It also runs agents live against several models or configurations, repeats each scenario enough times to separate a real change from noise, and tells you whether a change is a regression, an improvement, no change, or still too close to call.

The name comes from the two-humped camel. Most of what bactrian does is compare two things.

## Status

Early development. Strict replay of existing recordings works. Live recording against real providers does not work yet, because it needs HTTPS support that is not built. Comparing variants with statistics, MCP interception, the sandbox and the MCP server are also still to come.

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

Recordings live in `./.bactrian/runs/`. Each run directory holds the recording (`cassette.jsonl`), a normalised trace of the conversation (`trace.jsonl`), `meta.json` and the agent's stderr. Add `--json` for machine-readable output. Exit status 0 means the run passed, 1 that the agent failed or the replay diverged. `bactrian --help` lists the rest.

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

Check the assertions and checkers against the scenario's golden traces before trusting them (every `pass` trace must pass and every `fail` trace must fail), then run the experiment:

```sh
bactrian check scenarios/*.yaml
bactrian run experiment.yaml
```

`check` exits 1 and lists the offending traces otherwise, and both commands exit 64 on a configuration error, printing every mistake as `file:line:column`. `run` allocates the runs in a seeded, blocked and interleaved order, keeps its progress in `./.bactrian/experiments/<id>/manifest.json`, stops starting runs once a spend cap is reached, and prints a line per run (`--json` for one JSON object). Interrupt it with Ctrl-C and carry on later with `bactrian run experiment.yaml --resume <id>`: runs that finished are not repeated. Each run directory also gets a `verdicts.json` with the result of every assertion and checker. The checker commands run in the scenario file's directory and the agent in the experiment file's. Like recording, `run` calls real providers, so for now it needs HTTPS support that is not built yet.

Four more fields shape a run. A scenario's `fixtures` names a directory, relative to the scenario file, whose files are copied into the run's `BACTRIAN_STATE_DIR` before the agent starts, and a directory that does not exist is a configuration error. In the experiment, `normalise` lists regular expressions that are masked out of each model request before it is keyed, so two runs that differ only in, say, a date still match on replay, and each run's `meta.json` counts the masks that fired in `findings.normalise_masks`. `redact` lists further regular expressions whose matches are replaced by numbered placeholders in the cassette, on top of the provider key formats. `pricing` names a YAML file, relative to the experiment, that overrides or extends the built-in price table:

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
