# bactrian

bactrian is a tool for testing AI agents the way you would test any other program. It will run your agent as a subprocess, whatever language it is written in and whichever model provider it calls. It records every model call, tool call and network request the agent makes, and can replay a recording exactly, so you can see the precise step where a new version of the agent started behaving differently. It also runs agents live against several models or configurations, repeats each scenario enough times to separate a real change from noise, and tells you whether a change is a regression, an improvement, no change, or still too close to call.

The name comes from the two-humped camel. Most of what bactrian does is compare two things.

## Status

Early development. Strict replay of existing recordings works. Live recording against real providers does not work yet, because it needs HTTPS support that is not built. Live comparison, statistics, MCP interception, the sandbox and the MCP server are also still to come.

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
