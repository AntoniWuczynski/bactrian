# bactrian

bactrian is a tool for testing AI agents the way you would test any other program. It will run your agent as a subprocess, whatever language it is written in and whichever model provider it calls. It records every model call, tool call and network request the agent makes, and can replay a recording exactly, so you can see the precise step where a new version of the agent started behaving differently. It also runs agents live against several models or configurations, repeats each scenario enough times to separate a real change from noise, and tells you whether a change is a regression, an improvement, no change, or still too close to call.

The name comes from the two-humped camel. Most of what bactrian does is compare two things.

The design, including the parts that are still planned, is described in [`docs/design.md`](docs/design.md).

## Status

Early development. Strict replay of existing recordings works, with every agent inside an operating-system sandbox where the host has one. Live recording against real providers does not work yet, because it needs HTTPS support that is not built. Egress recording is also still to come. Until then the `egress` setting is read but not enforced. It is named in a warning when a run starts and recorded in each run's `meta.json` under `unenforced`. A `no_egress` assertion fails as unverifiable, because no HTTP call is recorded yet.

## Recording and replaying an agent

An agent talks to bactrian over a small JSON Lines protocol on stdin and stdout: it receives `start`, one `turn` per user message and `end`, and answers each turn with a `reply`. bactrian points the agent's model SDK at its own proxy through the usual base-URL variables (`OPENAI_BASE_URL`, `ANTHROPIC_BASE_URL`, `GOOGLE_GEMINI_BASE_URL`, `OPENROUTER_BASE_URL`) and gives it placeholder API keys. The real keys are never placed in the agent's environment and are added to requests on their way upstream. The agent runs inside a sandbox (see [Sandbox](#sandbox)), which stops it from reaching anything but bactrian's proxy and from writing outside its run. [`examples/agents/`](examples/agents/) has a TypeScript agent (build it with `pnpm install --dir examples/agents/typescript` and `pnpm --dir examples/agents/typescript build`) and a Python agent managed by uv.

Record a run (live spending needs an explicit cap per provider, in US dollars). This needs a build with HTTPS support, which does not exist yet, so for now it cannot reach a real provider:

```sh
BACTRIAN_CAP_OPENAI=0.50 bactrian record --turn "What is the weather in Paris?" -- node examples/agents/typescript/dist/agent.js
```

Replay it without touching the network, and see where a changed agent first behaves differently:

```sh
bactrian replay <run-id> -- node examples/agents/typescript/dist/agent.js
bactrian replay <run-id> --mode until-divergence -- uv run --no-sync --project examples/agents/python examples/agents/python/agent.py
```

Recordings live in `./.bactrian/runs/`. Each run directory holds the recording (`cassette.jsonl`), a normalised trace of the conversation (`trace.jsonl`), `meta.json`, `divergences.json`, the agent's stderr and, for each checker that wrote any, its stderr (`checker-<i>.stderr`, the last 64 KiB). A checker's stderr never reaches the terminal. `meta.json` also carries the run's cost in US dollars as the ledger attributed it, the total latency and token count of its model calls, and the `normalise` and `redact` patterns it was recorded under. A replay applies those patterns, whatever the command line says, so requests that a pattern changed still match. `divergences.json` lists each replay divergence with its kind, the nearest recorded exchange, the similarity and the paths that differ. Add `--json` for machine-readable output. Exit status 0 means the run passed, 1 that the agent failed or the replay diverged. `bactrian --help` lists the rest.

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

## HTTP-mode agents

A command agent is a process that bactrian starts for each run. An agent that is already a long-lived server can be driven over HTTP instead, in an experiment (`bactrian record` and `bactrian replay` start command agents only). Give the server's address and the name of the header that carries the run ID:

```yaml
agent:
  protocol: http
  url: http://127.0.0.1:3000/agent
  namespace_header: X-Agent-Run
```

You start the server yourself, before `bactrian run`. For each message of the conversation (`start`, one `turn` per user message and `end`, the same JSON as in command mode) bactrian sends `POST <url>` with `Content-Type: application/json`, the message as the body, and the run ID in `X-Bactrian-Run` and in the namespace header. The response body is a JSON array of the agent's messages for that step: `[]` for `start` and `end`, and for a turn any `event` messages followed by the `reply`. A status outside 2xx ends the run as `failed` with `agent_http_error`, the status and the first 2 KiB of the body. The turn and total timeouts apply as in command mode, the turn timeout to each request. Only `http://` URLs work for now. This build has no HTTPS support, so an `https://` URL stops with exit status 69.

A server that is already running cannot be given environment variables, so the `start` message of an HTTP-mode agent carries them in an `env` object. It holds exactly the per-run values a command agent gets as its environment: the provider base URLs with the run token, the placeholder API keys, `SIM_NOW`, `TZ`, `BACTRIAN_RUN_ID`, `BACTRIAN_STATE_DIR`, the MCP variables when the run has MCP servers, and the variant's own variables. Nothing else is included. The server owns its own process, so there is no `PATH`, no passthrough variable and none of the variables bactrian redirects for a command agent (`HOME`, the `XDG_*` and npm cache directories, `TMPDIR` and `PYTHONDONTWRITEBYTECODE`), and the real provider keys never appear. The agent has to use these values for the calls of that run, and not its own process environment, and it has to keep its state per run ID, because several runs can be in flight at once when `concurrency` is above 1. The `env` field is absent from command-mode `start` messages, so existing agents are unaffected. The example agents show the pattern: `--http PORT` serves the same logic as a server (port 0 picks a free one), prints the port it bound as the first line of its output, and reads each run's configuration from its `env`.

Because bactrian does not start the server, it cannot put it in a sandbox, give it a fresh process for each run or reset what it remembers between runs. Every HTTP-mode run is therefore unsandboxed whatever `sandbox` says, and carries the usual label in its `meta.json` with the reason `http mode` (`{"mode": "auto", "applied": false, "reason": "http mode"}`), so reports and comparisons flag it like any other unsandboxed run. No sandbox probe is run for such an experiment, and it prints a warning when it starts, also listed under `unenforced`. `sandbox: strict`, in any of its forms, or `--sandbox strict` with `protocol: http` is a configuration error, because the guarantee cannot be met. The server can also reach the network directly, so only the calls that go through the base URLs in `env` are recorded.

## MCP tools

Agents often reach their tools through the Model Context Protocol (MCP). bactrian records those calls next to the model calls and replays them, so a replay needs neither the real server nor the network. It handles servers that speak the 2026-07-28 revision (stateless) and the 2025-11-25 one and earlier (which opens with `initialize`), over stdio and over Streamable HTTP.

Declare the servers in the experiment:

```yaml
mcp:
  calendar:
    command: ["python3", "tools/calendar_server.py"]  # a stdio server
    env: { CALENDAR_DB: fixtures/calendar.db }
    cwd: tools
    policy: record
  search:
    url: https://search.example.com/mcp               # a Streamable HTTP server
    policy: passthrough
```

A server has either a `command` or a `url`, and a name made of letters, digits, `_` and `-`. `env` and `cwd` belong to stdio servers. bactrian starts a stdio server itself the first time the agent uses it, outside the agent's process group, and stops it when the run ends. The server gets only the `env` you declare plus `PATH`, `HOME` and `LANG`, so it never sees the placeholder provider keys or bactrian's own variables. `cwd` is relative to the experiment file, which is also where the server runs if you leave it out.

The `policy` says what happens to the server's traffic. `record` is the default and follows the run: the server is live and recorded when the run records, and answered from the recording when the run replays. `passthrough` is always live and never recorded. `block` refuses every request, with a JSON-RPC error `-32601` on stdio and HTTP status 403 on HTTP, and drops notifications. `replay` is accepted by the format but refused in an experiment, with the position of the line, because an experiment's runs record and there is nothing yet to replay from.

The agent has to talk to bactrian instead of the real server. For a stdio server, put `bactrian mcp-shim <name>` where the agent's MCP configuration has the server's own command:

```json
{ "mcpServers": { "calendar": { "command": "bactrian", "args": ["mcp-shim", "calendar"] } } }
```

The shim reads JSON-RPC lines on its stdin, hands each one to bactrian and writes the replies to its stdout. Requests go out in parallel. When the server is live, a notification or a response is on the server's stdin before the shim reads the next line, so a legacy client's `notifications/initialized` always reaches the server before its next request. It finds bactrian through `BACTRIAN_MCP_URL`, which bactrian sets for the agent, and exits when its stdin closes. For an HTTP server, give the agent the URL in `BACTRIAN_MCP_HTTP_<NAME>` as the server's address. The name is upper-cased and every character that is not a letter or a digit becomes `_`, so `search` is `BACTRIAN_MCP_HTTP_SEARCH`. Both variables are set only when `intercept` includes `mcp` (it does by default) and the experiment has servers, and a variant cannot set them. An experiment that declares servers while `intercept` leaves out `mcp` gets a warning, also listed under `unenforced` in each run's `meta.json`, because those servers are never started. A request for a server the run does not have gets a JSON-RPC error `-32601` with the request's id. The example agents read `AGENT_MCP_COMMAND` (a JSON array such as `["bactrian", "mcp-shim", "weather"]`) and `AGENT_MCP_PROTOCOL` (`2026-07-28` by default, or `2025-11-25`), and then run their weather tool on that server.

`bactrian record` declares its servers with flags instead of an experiment, all repeatable:

```sh
bactrian record --turn "What is the weather in Paris?" \
  --mcp-stdio 'weather=["python3","tools/weather_server.py"]' \
  --mcp-http search=https://search.example.com/mcp \
  --mcp-policy search=passthrough \
  -- node agent.js
```

`--mcp-stdio NAME=JSON` takes the server's command as a JSON array of strings after the `=`. `--mcp-http NAME=URL` takes an `http://` or `https://` URL. `--mcp-policy NAME=POLICY` is `record` (the default), `passthrough` or `block` for a server declared by one of the other two flags, and `replay` is refused for the same reason as in an experiment. Names and URLs are checked as in an experiment, and a usage error (exit status 64) names the flag at fault. A stdio server runs in the current directory with only `PATH`, `HOME` and `LANG`, so a server that needs more environment or another directory has to be declared in an experiment. The agent gets `BACTRIAN_MCP_URL` and `BACTRIAN_MCP_HTTP_<NAME>` as under `bactrian run`.

Every run's `meta.json` has an `mcp` list with each server's name, transport (`stdio` or `http`) and policy, and never its command, environment or URL, which may hold secrets. `bactrian replay` reads that list from the recording and serves every server from it, so it takes no MCP flags and never starts a real server. A server that was blocked stays blocked, and one that was `passthrough` left nothing to replay, so its requests are divergences. A recording made before the list existed replays with no MCP servers.

Each request and its response become one exchange in the cassette. The key is built from the server, the method and the parameters, without the JSON-RPC id and without the `_meta` entries that change from one client or run to the next (client information, trace context, progress tokens). Matching ignores order, so parallel calls answered in a different order still replay, and a replayed answer carries the id of the request it answers. A request that was never recorded is a divergence, and when it is a handshake (a legacy client against a modern recording, or the reverse) the error the agent gets, and the run's errors in `meta.json`, say so. Notifications have no response and are not recorded. In a recorded event stream only the answer to the request takes the live id, and the server's own messages on that stream keep theirs.

In the trace, a `tools/call` is joined to the model's tool call of the same turn by tool name and equal arguments. The tool call's `mcp` field says `joined`, `joined_by_order` (several identical calls, matched in order) or `unjoined` (no MCP record, as for any tool that is not on an MCP server). An MCP call with no model call to join shows up as an agent event named `mcp_call`, and assertions count only the model's tool calls.

bactrian gives each request on a stdio server its own id, so that requests from several clients never clash. When the agent cancels a request with `notifications/cancelled`, the server gets the cancellation with that id, and bactrian answers the cancelled request itself (the server will not) and records nothing for it. A cancellation that names no request in flight, or an id that two clients both have in flight, is dropped and listed in the run's errors. That includes a cancellation sent right after its request that overtakes it, since requests travel in parallel. A request the end of the run cuts off is not recorded either, and neither counts as a transport error. A stdio server that does not answer, or stops reading its stdin, within ten minutes fails that request, and in the second case every later one, as a crashed server does. An error without an id goes to the request in flight if there is only one, and is listed in the run's errors otherwise.

For an HTTP server, the agent's path goes below the declared URL's path and its query after the declared query, and a path with a `.` or `..` segment is refused with status 400.

Known limits. HTTP replies are buffered, so an event stream answering a POST reaches the agent whole. GET and DELETE answer 405, which means no server-to-client stream and no session termination. A legacy stdio server's own requests (sampling, roots, elicitation) cannot reach the agent through the shim, so bactrian answers each with a JSON-RPC error `-32601` and the server does not wait for an answer that never comes. The servers are part of the world the agent acts on, not of the agent, so they run outside its sandbox, as the same user as bactrian.

## Sandbox

Every agent runs inside an operating-system sandbox when the host supports one. Inside it the agent:

- can connect only to bactrian's own listeners, so a direct HTTPS request, a raw TCP connection, a WebSocket and a DNS lookup all fail, whatever proxy variables the agent ignores
- can write only to its run's state directory, its temporary directory and the paths you allow
- cannot read credential paths: `~/.ssh`, `~/.gnupg`, `~/.aws`, `~/.azure`, `~/.config/gcloud`, `~/.kube`, `~/.docker`, `~/.netrc`, `~/.npmrc`, `~/.pypirc`, `~/.git-credentials`, `~/Library/Keychains`, the `.env` files at the project root, `.bactrian/ca` and the other runs in `.bactrian/runs`
- holds a placeholder API key (`bactrian-<token>`) and never the real one, which only bactrian's proxy adds on the way upstream

On macOS this is `sandbox-exec` with a profile written for each run. On Linux it is bubblewrap with no network at all. bactrian's proxy also listens on a Unix socket that is mounted into the sandbox, and a small forwarder inside it (`bactrian __sandbox-init`) passes the agent's loopback connections on to that socket. MCP servers are part of the world, not of the agent, so they run outside the sandbox.

The mode is `auto` (the default), `strict` or `off`, set in the experiment or with `--sandbox` on `record`, `replay` and `run` (which overrides the experiment). `allow_write` adds writable paths and `deny_read` adds unreadable ones, both relative to the experiment file:

```yaml
sandbox: auto
# or, with more paths:
sandbox: { mode: strict, allow_write: [out], deny_read: [secrets] }
```

Before the first run, bactrian checks the sandbox once by running a probe inside it: a connection to `1.1.1.1:443` has to fail without sending anything, a connection to a listener of bactrian's has to succeed, and a write outside the state directory has to fail. When the sandbox does not work, `auto` warns with the cause and the fix and runs the agents unsandboxed, and `strict` stops with exit status 69. `off` skips the sandbox and the probe. Each run's `meta.json` records `sandbox: {mode, applied, reason}`, and the terminal output, `compare` and `report` flag every run that was not sandboxed.

On Linux, bubblewrap 0.5 or later must be installed (`sudo apt install bubblewrap`), and unprivileged user namespaces must be allowed. Ubuntu 23.10 and later block them for bubblewrap through AppArmor. Fix it once, either with `sudo sysctl -w kernel.apparmor_restrict_unprivileged_userns=0` or with an AppArmor profile that grants bubblewrap `userns`. The warning prints both. bactrian never runs `sudo` itself.

The agent's environment is the same with or without the sandbox. `HOME` is its state directory, `XDG_CACHE_HOME`, `XDG_CONFIG_HOME`, `XDG_DATA_HOME` and `npm_config_cache` point inside it, `TMPDIR` is the run's temporary directory (removed when the run ends) and `PYTHONDONTWRITEBYTECODE=1` keeps Python from writing `__pycache__` beside its modules. The agent must be started with pipes, which bactrian does, because the sandbox refuses the terminal. Start a uv project with `uv run --no-sync` after `uv sync`, because a plain `uv run` locks the project's virtual environment, which is a write outside the run.

What the sandbox refused goes into the run's trace as `sandbox_violation` events and into `meta.json` under `sandbox_violations`. The two platforms differ:

- On macOS these are the sandbox's own entries in the system log, such as `network-outbound remote:*:443`, `file-write-create` or `file-read-data`. A DNS lookup shows as a `network-outbound` entry on `mDNSResponder`. bactrian reads them after the agent exits, which takes a second or two, and says so if it cannot (`collected: false` with the reason). Repeats of one denial by one process can be folded into a single entry.
- On Linux the operating system logs nothing, so the only violations reported are connections the forwarder could not pass on. A denied connection, write or read shows up as the agent's own error (`ENETUNREACH`, `EROFS`, or a missing file for a denied directory).

Known limit on macOS: Seatbelt does not govern the `sysctl` that returns another process's arguments and environment, so an agent can read the launch-time environment of any other process of yours that is not an Apple binary, bactrian included. If bactrian was started with a provider key exported, a hostile agent can read that key, though it can only pass it on through bactrian. Start bactrian without provider keys in its environment when you do not trust the agent. Linux is not affected, because the agent cannot see other processes.

## Clock

Every agent gets `SIM_NOW` (the scenario's `now`, an RFC 3339 time, or the current time) and `TZ` (the scenario's `timezone`, or `UTC`) in its environment. That is the contract, and an agent that follows it behaves the same on every run. On Linux, bactrian also makes the agent's own system clock agree. It preloads `libfaketime` (`sudo apt install libfaketime`, or point `BACTRIAN_LIBFAKETIME` at `libfaketimeMT.so.1`) so that the wall clock starts at `SIM_NOW` and then runs at normal speed. The monotonic clock moves with it, so intervals stay true. Set `clock: { faketime: off }` in the experiment to turn this off.

macOS runs are always contract-only. System Integrity Protection removes the variables that preloading depends on from the environment of the system's programs, so bactrian does not try.

Each run says which it was. `meta.json` has `clock: {mode, reason}`, where `mode` is `enforced` or `contract_only`, and the trace header repeats the mode. A report, `compare` and the `--json` output of `record` and `replay` flag the runs that were contract-only with the reason: `macos`, `faketime-off`, `library-not-found`, `invalid-sim-now`, `static-binary`, `go-binary` or `unreadable-executable`. `enforced` means the library was injected and the agent's program (the command's first word, or the interpreter of a `#!` script) is a dynamically linked, non-Go ELF file. It does not prove the agent read the faked clock. Go programs and statically linked ones ignore the preload. So can runtimes that read the clock without the C library or cache it at start. The reasoning is in [`docs/decisions/0009-agent-clock.md`](docs/decisions/0009-agent-clock.md).

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
