# bactrian

bactrian is a tool for testing AI agents the way you would test any other program. It will run your agent as a subprocess, whatever language it is written in and whichever model provider it calls. It records every model call, tool call and network request the agent makes, and can replay a recording exactly, so you can see the precise step where a new version of the agent started behaving differently. It also runs agents live against several models or configurations, repeats each scenario enough times to separate a real change from noise, and tells you whether a change is a regression, an improvement, no change, or still too close to call.

The name comes from the two-humped camel. Most of what bactrian does is compare two things.

The design, including the parts that are still planned, is described in [`docs/design.md`](docs/design.md).

## Status

Early development. Strict replay of existing recordings works, with every agent inside an operating-system sandbox where the host has one, HTTP-mode agents and the simulated clock included. A coding agent can drive bactrian through its MCP server (see [Driving bactrian from a coding agent](#driving-bactrian-from-a-coding-agent)). JSON Schemas for the file formats and prebuilt binaries are not built yet. Live calls go to the providers over HTTPS, and to a server on plain HTTP (see [Local and self-hosted models](#local-and-self-hosted-models)). HTTPS is tested against a local server with a certificate authority made for the test, not yet against the real providers. An agent's own HTTPS calls to other services are intercepted, recorded and replayed per host (see [HTTPS egress](#https-egress)), tested with Node `fetch`, httpx and requests against a local server. Plain HTTP egress, WebSockets and HTTP/2-only clients are not intercepted.

Run `bactrian doctor` to see whether this host is ready (see [Checking the host](#checking-the-host)).

## Recording and replaying an agent

An agent talks to bactrian over a small JSON Lines protocol on stdin and stdout: it receives `start`, one `turn` per user message and `end`, and answers each turn with a `reply`. bactrian points the agent's model SDK at its own proxy through the usual base-URL variables (`OPENAI_BASE_URL`, `ANTHROPIC_BASE_URL`, `GOOGLE_GEMINI_BASE_URL`, `OPENROUTER_BASE_URL`) and gives it placeholder API keys. The real keys are never placed in the agent's environment and are added to requests on their way upstream. The agent runs inside a sandbox (see [Sandbox](#sandbox)), which stops it from reaching anything but bactrian's proxy and from writing outside its run. [`examples/agents/`](examples/agents/) has a TypeScript agent (build it with `pnpm install --dir examples/agents/typescript` and `pnpm --dir examples/agents/typescript build`) and a Python agent managed by uv.

Record a run (live spending needs an explicit cap per provider, in US dollars):

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

`check` exits 1 and lists the offending traces (or the missing golden traces) otherwise, and both commands exit 64 on a configuration error, printing every mistake of every file as `file:line:column` in the order it appears. `check --json` prints one JSON object instead, with each scenario's offenders, errors and warnings, and under `sandbox` what the checkers ran in (`null` when no scenario has a checker). `run` allocates the runs in a seeded, blocked and interleaved order, keeps its progress in `./.bactrian/experiments/<id>/manifest.json`, stops starting runs once a spend cap is reached, and prints a line per run (`--json` for one JSON object). Interrupt it with Ctrl-C and carry on later with `bactrian run experiment.yaml --resume <id>`. A run that passed, failed or diverged is never repeated. A run that never started, was interrupted, was not run because a spend cap was reached or errored (a fault of bactrian's own, or a checker that could not give a verdict) is run again under a new run ID, and the manifest keeps the old one under `previous_run_ids`. So raising `caps_usd` and resuming fills in the runs that the cap held back. The resume is refused, naming what changed, if the experiment file (apart from `caps_usd` and `concurrency`), the agent, a scenario file or the price file is different from when the experiment started. Only one process can run an experiment at a time. Each finished run's directory also gets a `verdicts.json` with the result of every assertion and checker. A failing assertion or checker makes a passing run `failed`, while a checker that crashes, hangs or prints something that is not a verdict makes it `errored`, which is left out of the pass rates and listed separately. `run` exits 70 if any run errored, so a continuous integration job sees it. The checker commands run in the scenario file's directory, inside the sandbox (see [Sandbox](#sandbox)), and the agent in the experiment file's. Like recording, `run` calls providers live, so for now it reaches only a server on plain HTTP (see [Local and self-hosted models](#local-and-self-hosted-models)).

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

## HTTPS

bactrian's own calls over HTTPS, to the providers, to an `https` upstream, to an HTTP-mode agent at an `https` URL and to a Streamable HTTP MCP server at one, check the server's certificate against the system's root certificates, which are the system keychains on macOS and the distribution's bundle on Linux (`/etc/ssl/certs/ca-certificates.crt` and the other usual paths). The host name in the URL is sent as Server Name Indication and must match the certificate, and an address host must be in the certificate as an address. Only HTTP/1.1 is offered.

To trust another certificate authority as well, for a proxy that inspects TLS or a test server, set `BACTRIAN_EXTRA_CA_CERTS` to a file of PEM certificates. It adds to the system's roots and never replaces them. Its certificates are trusted for every host, the real providers included, so `doctor` shows a `warn` naming the variable, the file and the number of certificates in it, and `record`, `replay` and `run` print the same warning before calling anything over HTTPS. The file must belong to you or to root, and neither its group nor others may write to it. bactrian does not read `SSL_CERT_FILE`, `NIX_SSL_CERT_FILE` or `OCAML_EXTRA_CA_CERTS`, because those are for the agent's own clients (see [decision 0017](docs/decisions/0017-upstream-tls-trust.md)).

When the roots cannot be loaded (no bundle, or a `BACTRIAN_EXTRA_CA_CERTS` that cannot be read, holds no certificate or could be changed by another user), a command that would call a provider, an agent or an MCP server over HTTPS stops with exit status 69 and says why. An MCP server counts when the run calls it live: `passthrough`, or `record` outside a replay. A certificate that does not verify fails the call. The agent gets an error from the proxy, and the run's reason names the TLS failure. `bactrian doctor` shows whether the roots load.

## HTTPS egress

An agent's own HTTPS calls, to a weather service or a search API its tools use, go through bactrian too. Each run gives the agent `HTTPS_PROXY` (and `https_proxy`) naming bactrian's proxy port, with the run ID and run token as the proxy's user name and password, and `NODE_USE_ENV_PROXY=1`, which makes Node's `fetch` use it. bactrian answers each `CONNECT` with a certificate for the requested host, signed by a certificate authority of the project, and the agent trusts that authority through a bundle of the system's root certificates and the project's, named by `SSL_CERT_FILE`, `NODE_EXTRA_CA_CERTS` and `REQUESTS_CA_BUNDLE`. The bundle is written into the run's temporary directory and deleted with it. The proxy shares the model proxy's port, so the sandbox still lets the agent reach one port and nothing else (see [decision 0026](docs/decisions/0026-egress-through-the-model-proxy.md)).

Every host is blocked unless it is listed. In an experiment:

```yaml
intercept: [model, mcp, fixtures, egress]
egress: { default: block, hosts: { "api.weather.example": record, "search.example:8443": passthrough } }
```

and for `bactrian record`, `--egress HOST=POLICY` (repeatable) and `--egress-default POLICY`. A host is matched as `host:port` first, then as `host`, without regard to case or a trailing dot. A key is a DNS name or an IP address, with or without a port: a scheme, a wildcard, a path or two keys for the same host is a configuration error. An experiment whose `intercept` leaves out `egress` but lists hosts, or a default other than `block`, gets a warning, also listed under `unenforced`, because its agent's HTTPS then goes direct wherever the sandbox lets it. The policies:

- `record`: called and recorded when recording, answered from the recording in a replay. `bactrian replay` uses the policy the recording was made with.
- `replay`: always answered from the recording, and refused as a replay miss when there is none.
- `passthrough`: always called, never recorded.
- `block` (the default): the `CONNECT` is refused with 403 and `bactrian_egress_blocked`, before any TLS. The attempt still appears in the trace.

A request inside a tunnel goes only to the host and port the `CONNECT` named. Its target must be a path, or an `https` URL of that same host and port, and anything else (another host, user information, another scheme) gets 400 with `bactrian_bad_request`. The `Host` header is always the tunnel's.

Each call is an exchange on the cassette's `http` channel, keyed on its method, host and port, path, sorted query, `content-type` and body (canonical JSON, or a hash of other bytes), with the run's `normalise` and `redact` patterns applied as for model calls. A strict replay serves it without calling the host, in any order, because tools that fetch URLs are often called in parallel, and a call the recording lacks gets 409 with `bactrian_replay_miss` and is a divergence whose diff names what changed. The trace has an `http_call` for every call, blocked ones included (method `CONNECT`, status 403), so a `no_egress` assertion can be checked. Without the sandbox a client that ignores the proxy goes direct and leaves no call, so in an unsandboxed run `no_egress` fails as unverifiable unless a listed host was called, which fails it anyway. `meta.json` records the policy under `egress`.

The certificate authority lives in `.bactrian/ca/`: the directory and the key `ca.key` are readable by you alone, and a key that others can read is refused. It is made on first use and is valid for a year. `bactrian ca path` prints its certificate's absolute path (to trust it elsewhere) and `bactrian ca rotate` replaces it. Leaf certificates are valid from 2 days before the earlier of now and the run's `SIM_NOW` to 7 days after the later, and the authority from 1970 on, so an agent whose clock `libfaketime` sets to a past `SIM_NOW` (a scenario's fixed `now`, or an old recording's in a replay) still accepts them. A `SIM_NOW` after the authority expires fails under `libfaketime`. Leaves are kept in memory only, one per host. bactrian's own calls to these hosts check their certificates as in [HTTPS](#https), and never trust the bundle it gives the agent.

What is covered:

| Client | Proxy | Trust |
|---|---|---|
| Node `fetch` (the OpenAI and Anthropic SDKs use it) | `NODE_USE_ENV_PROXY`, Node 24.0 or later, or 22.21 or later | `NODE_EXTRA_CA_CERTS` |
| Python httpx, httpx2 (the OpenAI and Anthropic SDKs) | `HTTPS_PROXY` | `SSL_CERT_FILE` |
| Python requests | `HTTPS_PROXY` | `REQUESTS_CA_BUNDLE` |

Not covered: plain `http://` egress (no `HTTP_PROXY` is set, so it goes direct and fails in the sandbox), WebSockets (an `Upgrade` inside the tunnel gets 501), HTTP/2-only and gRPC clients, clients that ignore the proxy variables or bring their own roots (Node's `ws`, Go before 1.27 and Rust's platform verifier on macOS, `trust_env=False`), and HTTP-mode agents, which bactrian does not start, so their egress is not intercepted and their runs say so under `unenforced`. Package managers honour the proxy as well, so a `uv run` or `npx` that downloads packages when it starts is refused unless its index is listed: install the agent's packages before the run (the examples use `uv run --no-sync`). Bodies are passed on whole, not streamed, and a body over 64 MiB is refused: a request with 413, a response with 502, recorded as a transport error. Only the response headers bactrian stores for model calls (`content-type`, `retry-after`, the rate-limit headers and `x-request-id`) reach the agent.

## Local and self-hosted models

Each provider's address can be replaced with an environment variable, to use a server you run yourself (Ollama, vLLM, LM Studio, LiteLLM or any OpenAI-compatible one) or a gateway: `BACTRIAN_UPSTREAM_OPENAI`, `BACTRIAN_UPSTREAM_ANTHROPIC`, `BACTRIAN_UPSTREAM_GEMINI` and `BACTRIAN_UPSTREAM_OPENROUTER`. The URL may be `http` or `https`. The agent is not changed: it still calls bactrian's proxy through the usual base-URL variables, and the proxy forwards to your server.

Ollama serves its OpenAI-compatible API under `/v1` at `http://localhost:11434` ([Ollama's documentation](https://docs.ollama.com/api/openai-compatibility)). The `/v1` is part of the path the OpenAI SDKs already send, so leave it out of the variable:

```sh
export BACTRIAN_UPSTREAM_OPENAI=http://127.0.0.1:11434
export OPENAI_API_KEY=ollama           # any value, see below
export BACTRIAN_CAP_OPENAI=1           # for record; an experiment uses caps_usd
bactrian run experiment.yaml
```

- The value must be an absolute `http` or `https` URL with a host, an optional port and an optional path prefix, for a gateway that mounts the provider under a path. User information, a query and a fragment are refused with exit 64, and the message names the variable. The path prefix is shown by `doctor` and recorded in `meta.json` as written, so do not put a key in it.
- bactrian still requires the provider's key variable when the provider has a spend cap above 0, and the cap itself, whatever the upstream. A server that needs no key is given any placeholder value, as Ollama's documentation does. The key is sent to the upstream, so use a placeholder, or a real key only over HTTPS or to your own machine: `record`, `replay`, `run` and `bactrian doctor` warn about plain `http` to another machine (anything but `localhost`, 127.0.0.0/8 and `::1`).
- The refusal when the trusted root certificates cannot be loaded (exit 69, see [HTTPS](#https)) applies only when a provider the run calls has an `https` upstream.
- A model with no price is refused, not treated as free: the agent gets a 402, and the run is `errored` with "no price for model llama3.2". Give the model a price in the experiment's price file, with zero rates for a local model. `record` and `replay` use the built-in prices only, so use an experiment for a local model.

```yaml
# prices.yaml
version: 1
prices:
  - model: llama3.2
    provider: openai
    rates: { input: 0, cached_input: 0, cache_write: 0, output: 0 }
    max_output: 4096
    max_input: 131072
```

- `meta.json` records each overridden upstream under `upstreams` (provider name to URL), so a run against a local server can be told from one against the real provider. A strict replay, which calls nobody, records none. `bactrian doctor` prints an `upstream-<provider>` line for each. See [decision 0012](docs/decisions/0012-upstream-override.md).

## HTTP-mode agents

A command agent is a process that bactrian starts for each run. An agent that is already a long-lived server can be driven over HTTP instead, in an experiment (`bactrian record` and `bactrian replay` start command agents only). Give the server's address and the name of the header that carries the run ID:

```yaml
agent:
  protocol: http
  url: http://127.0.0.1:3000/agent
  namespace_header: X-Agent-Run
```

You start the server yourself, before `bactrian run`. For each message of the conversation (`start`, one `turn` per user message and `end`, the same JSON as in command mode) bactrian sends `POST <url>` with `Content-Type: application/json`, the message as the body, and the run ID in `X-Bactrian-Run` and in the namespace header. The response body is a JSON array of the agent's messages for that step: `[]` for `start` and `end`, and for a turn any `event` messages followed by the `reply`. A status outside 2xx ends the run as `failed` with `agent_http_error`, the status and the first 2 KiB of the body. The turn and total timeouts apply as in command mode, the turn timeout to each request. An `https://` URL is checked like a provider's (see [HTTPS](#https)), and a certificate that does not verify fails the run with `agent_http_error`.

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

- can connect only to bactrian's own listeners, so a direct HTTPS request, a raw Transmission Control Protocol (TCP) connection, a WebSocket and a Domain Name System (DNS) lookup all fail, whatever proxy variables the agent ignores
- can write only to its run's state directory, its temporary directory and the paths you allow
- cannot read credential paths: `~/.ssh`, `~/.gnupg`, `~/.aws`, `~/.azure`, `~/.config/gcloud`, `~/.kube`, `~/.docker`, `~/.netrc`, `~/.npmrc`, `~/.pypirc`, `~/.git-credentials`, `~/Library/Keychains`, the `.env` files at the project root, `.bactrian/ca`, the other runs in `.bactrian/runs`, every run's recorded payloads, the experiments and the spend ledger (`.bactrian/blobs`, `.bactrian/experiments`, `.bactrian/ledger.jsonl`, and in the virtual machine all of `.bactrian` but the run's own directories), and, where one of these is a symbolic link, the file or directory it leads to
- holds a placeholder API key (`bactrian-<token>`) and never the real one, which only bactrian's proxy adds on the way upstream

There are three backends, chosen with `backend`:

- `vm` runs the agent in a Linux container with no network, through Docker or Apple's `container`. On macOS the container is inside a virtual machine (OrbStack's or Docker Desktop's, or one for each container with `container`), so the agent sees none of your processes. Its standard input and output carry the agent's own and every connection it makes to bactrian's proxy, through a small relay that is the container's first process. The container shows only the project (the directory holding `.bactrian`), read-only, with the run's directories and the `allow_write` paths writable, so the agent's command and files must be in the project. A hidden file, such as `.env`, is not there at all under any spelling of its name, because its directory is replaced by a read-only one that holds the other entries alone. Symbolic links in that directory are left out unless they lead to a file the container shows, and nothing new can be created in it. The container may start at most 1024 processes and use 4 GiB of memory, with 1 GiB in `/tmp` (and no swap with Docker), and no program in it can gain privileges, set-user-ID ones included. It runs in the image you name, which must hold the agent's runtime and its packages: pin it by digest and pull it first (`docker pull` or `container image pull`), because bactrian never pulls it. The relay is built once, in an Alpine container, which needs the network that one time, and cached under `~/.cache/bactrian/relay` for both runtimes. Podman is not supported yet.
- `seatbelt` (macOS) is `sandbox-exec` with a profile written for each run, which lets the agent reach one loopback port: a listener of bactrian's proxy that serves that run and no other.
- `bubblewrap` (Linux) has no network at all. bactrian's proxy also listens on a Unix socket that is mounted into the sandbox, and a small forwarder inside it (`bactrian __sandbox-init`) passes the agent's loopback connections on to that socket.

`backend: auto`, the default, is bubblewrap on Linux. On macOS it is the virtual machine when `image` is set and Docker or `container` answers. Otherwise `sandbox: auto` falls back to Seatbelt with a warning, and `sandbox: strict` stops with exit status 69, because Seatbelt leaves the launch environments of your other processes readable (see below). Set `backend: seatbelt` to accept that and silence the warning. MCP servers are part of the world, not of the agent, so they run outside the sandbox.

The mode is `auto` (the default), `strict` or `off`, set in the experiment or with `--sandbox` on `record`, `replay` and `run` (which overrides the experiment). `record` and `replay` also take `--sandbox-backend`, `--sandbox-image` and `--sandbox-runtime`. `allow_write` adds writable paths and `deny_read` adds unreadable ones, both relative to the experiment file:

```yaml
sandbox: auto
# or, with more paths:
sandbox: { mode: strict, allow_write: [out], deny_read: [secrets] }
# or, in a virtual machine:
sandbox:
  mode: strict
  backend: vm
  image: node:24-alpine@sha256:ebfe2f90462722a7a4de65e91990e97fe0d401c70e0e762c5b53302f905ec1c1
```

`runtime` picks the virtual machine's runtime: `docker` (Docker Engine, Docker Desktop or OrbStack) or `container` (Apple's, on macOS 26 and Apple silicon, started once with `container system start`). Left out, it is docker when docker answers, otherwise `container`. `container` differs in a few ways, which [decision 0015](docs/decisions/0015-apple-container-runtime.md) lists. It mounts directories only, so the directory of a hidden file becomes a read-only copy made of hard links in the run's directory. That copy cannot hold a file on another volume, so such a file stops the run, and so does a writable directory (the run's own or an `allow_write` path) holding a hidden file, an `allow_write` path that is a file, or running bactrian as root. While a container exists, `container` keeps its configuration, with the agent's placeholder keys, and its output log in `~/Library/Application Support/com.apple.container`.

A Python agent's virtual environment made on macOS cannot run in a Linux container. Build one for the image inside the project, for example with `uv sync --locked` run in the same image with `UV_PROJECT_ENVIRONMENT` pointing into the project, and start the agent with that environment's `python`.

Before the first run, bactrian checks the sandbox once by running a probe inside it: a connection to `1.1.1.1:443` has to fail without sending anything, a connection to a listener of bactrian's has to succeed, a write outside the state directory has to fail, and a file under a denied directory must not be readable. In the virtual machine it also checks that a hidden file is missing under its name in another case, and that `/` and another run's directory cannot be written. When the sandbox does not work, `auto` warns with the cause and the fix and runs the agents and checkers unsandboxed, and `strict` stops with exit status 69. `off` skips the sandbox and the probe. Each run's `meta.json` records `sandbox: {mode, applied, reason}`, and the terminal output, `compare` and `report` flag every run that was not sandboxed.

On Linux, bubblewrap 0.5 or later must be installed (`sudo apt install bubblewrap`), and unprivileged user namespaces must be allowed. Ubuntu 23.10 and later block them for bubblewrap through AppArmor. Fix it once, either with `sudo sysctl -w kernel.apparmor_restrict_unprivileged_userns=0` or with an AppArmor profile that grants bubblewrap `userns`. The warning prints both. bactrian never runs `sudo` itself.

The agent's environment is the same with or without the sandbox, except that in the virtual machine `PATH` is the image's own. `HOME` is its state directory, `XDG_CACHE_HOME`, `XDG_CONFIG_HOME`, `XDG_DATA_HOME` and `npm_config_cache` point inside it, `TMPDIR` is the run's temporary directory (removed when the run ends) and `PYTHONDONTWRITEBYTECODE=1` keeps Python from writing `__pycache__` beside its modules. The agent must be started with pipes, which bactrian does, because the sandbox refuses the terminal. Start a uv project with `uv run --no-sync` after `uv sync`, because a plain `uv run` locks the project's virtual environment, which is a write outside the run.

Checkers run in the sandbox too, in the experiment's mode, or with `check --sandbox` (`auto` by default). A checker only judges the trace it reads on standard input, so its sandbox is tighter than the agent's. It has no network at all, not even bactrian's listeners. It can write only to a fresh temporary directory, which is its `HOME` and `TMPDIR` with or without the sandbox and is removed when it ends, so `allow_write` does not apply to it. It cannot read what the agent cannot, `deny_read` included, nor anything under `.bactrian/runs`. It still runs in the scenario file's directory. `check` runs the probe only when a scenario has checkers, and so does an experiment with an HTTP-mode agent, which has no probe of its own. What the sandbox refuses a checker is not recorded as a violation. It shows up as the checker's own error, so the run is `errored` with the last line the checker wrote to standard error. A checker that needs the network, such as one that asks a model, has to run with `sandbox: off`. Each run's `verdicts.json` records what its checkers ran in under `checker_sandbox`. `check` warns when they ran unsandboxed, and `compare` and `report` name the runs whose checkers did. The reasoning is in [decision 0013](docs/decisions/0013-checkers-in-the-sandbox.md).

What the sandbox refused goes into the run's trace as `sandbox_violation` events and into `meta.json` under `sandbox_violations`. The two platforms differ:

- On macOS these are the sandbox's own entries in the system log, such as `network-outbound remote:*:443`, `file-write-create` or `file-read-data`. A DNS lookup shows as a `network-outbound` entry on `mDNSResponder`. bactrian reads them after the agent exits, which takes a second or two, and says so if it cannot (`collected: false` with the reason). Repeats of one denial by one process can be folded into a single entry. The system log can drop entries when it is busy, so the list is a lower bound: `meta.json` has `coverage: "seatbelt-log"` and `complete: false` with that reason, and the terminal output and reports say "at least N".
- On Linux and in the virtual machine the operating system logs nothing, so the only violations reported are connections the forwarder or the relay could not pass on (`coverage: "relay-failures-only"`). A denied connection, write or read shows up as the agent's own error (`ENETUNREACH`, `EROFS`, or a missing file for a denied directory), and the terminal output and reports say that no other kind is recorded.

On macOS, Seatbelt does not govern the `sysctl` that returns another process's launch-time arguments and environment, so an agent can read those of any process of yours that is not an Apple binary. `record`, `replay`, `run`, `check` and `serve-mcp` therefore restart themselves at once with only `PATH`, `HOME`, `USER`, `LOGNAME`, `SHELL`, `LANG`, `LC_*`, `TERM`, `TZ` and `TMPDIR` in their launch environment and take the rest, provider keys included, back from an unlinked temporary file. No process bactrian starts inherits its environment either: an MCP server gets a key only if the experiment sets it in that server's `env`, and a checker never gets one. That `env`, and an MCP stdio server's command line, are in the server's own launch environment, which the agent can read on macOS like any other: give such a server no credential you would not give the agent. bactrian warns once when it will start a stdio server with arguments or `env` under the macOS sandbox. What remains readable is the launch environment of your other processes, such as a Homebrew shell, `tmux`, `direnv` or a `uv run --env-file` that started bactrian. When you do not trust the agent, use the virtual machine (`backend: vm`, or an `image` with `backend: auto`), where it sees no process of yours. On Linux the agent cannot see other processes. Checkers stay in Seatbelt on macOS, so a checker can still read those launch environments, but it has no network and no keys, and its output only goes into the run's results. The reasoning is in [decision 0014](docs/decisions/0014-virtual-machine-sandbox.md).

A process the agent starts in a new session or process group (`setsid`, a daemon) escapes the group signal that ends a run, so on macOS and in unsandboxed runs it can outlive the run, and a bactrian killed with SIGKILL leaves its agents and MCP servers running there. On macOS such a process stays inside its run's sandbox and reaches only that run's port, which closes when the run ends. On Linux the sandbox's process namespace ends it with the run, and in the virtual machine the container's removal does. If bactrian is killed with SIGKILL, the bridge it started removes the container. If the bridge and the `docker` client are killed too, the relay in the container stops the agent after 10 seconds without a heartbeat from bactrian, and the next bactrian to check the virtual machine removes what is left.

## Clock

Every agent gets `SIM_NOW` (the scenario's `now`, an RFC 3339 time, or the current time) and `TZ` (the scenario's `timezone`, or `UTC`) in its environment. That is the contract, and an agent that follows it behaves the same on every run. On Linux, bactrian also makes the agent's own system clock agree. It preloads `libfaketime` (`sudo apt install libfaketime`, or point `BACTRIAN_LIBFAKETIME` at `libfaketimeMT.so.1`) so that the wall clock starts at `SIM_NOW` and then runs at normal speed. The monotonic clock moves with it, so intervals stay true. Set `clock: { faketime: off }` in the experiment to turn this off.

macOS runs are always contract-only. System Integrity Protection removes the variables that preloading depends on from the environment of the system's programs, so bactrian does not try.

An [HTTP-mode agent](#http-mode-agents) is a server that is already running, so bactrian does not start its process and cannot preload anything into it. Its runs are `contract-only` with the reason `http-mode`, whatever `faketime` says. The `env` object of its `start` message carries `SIM_NOW` and `TZ` as before, and never `LD_PRELOAD` or any `FAKETIME` variable. Start the server with `libfaketime` yourself if it needs a faked clock.

Each run says which it was. `meta.json` has `clock: {mode, reason}`, where `mode` is `enforced` or `contract_only`, and the trace header repeats the mode. A report, `compare` and the `--json` output of `record` and `replay` flag the runs that were contract-only with the reason: `macos`, `faketime-off`, `library-not-found`, `library-hidden`, `setuid-sandbox` (a setuid `bwrap` drops the preload), `invalid-sim-now`, `static-binary`, `go-binary`, `unreadable-executable`, `http-mode` or `vm` (the agent ran in the virtual machine, where bactrian preloads nothing). `enforced` means the library was injected and the agent's program (the command's first word, or the interpreter of a `#!` script) is a dynamically linked, non-Go Executable and Linkable Format (ELF) file. It does not prove the agent read the faked clock. Go programs and statically linked ones ignore the preload. So can runtimes that read the clock without the C library or cache it at start. The reasoning is in [`docs/decisions/0009-agent-clock.md`](docs/decisions/0009-agent-clock.md).

## Checking the host

`bactrian doctor` checks that a run can start, without running an agent or contacting anything. It prints one line per check: `ok`, `warn`, `fail` or `skip`, the check's name and a short message. It exits with status 69 when any check fails, and 0 otherwise.

```sh
bactrian doctor
bactrian doctor --experiment experiment.yaml
bactrian doctor --json
```

```text
warn  key-openai: OPENAI_API_KEY is not set
ok    loopback: a port on 127.0.0.1 can be bound
ok    sandbox: the sandbox works on this host
ok    tls: the trusted root certificates load (161)
```

- `key-<provider>`: whether the provider's key variable is set. Only the variable's name is printed, never any part of its value. A missing key is a `warn`. With `--experiment` it is a `fail` for a provider whose `caps_usd` is above 0, because the experiment calls that provider live, and a `skip` for the others.
- `caps` (with `--experiment`): some provider has a spend cap above 0 in `caps_usd`. `run` refuses an experiment without one, so this is a `fail`.
- `loopback`: a port on 127.0.0.1 can be bound.
- `agent` (with `--experiment`): the agent command is found on `PATH`, or relative to the experiment file's directory. For an HTTP-mode agent the URL is well-formed. It is not contacted. An agent that would run in the virtual machine is `skip`, since its command is the image's.
- `sandbox`: the sandbox works here. It runs the same probe as a run's preflight and prints the same cause and fix, including the two AppArmor commands on Linux. For an agent in the virtual machine whose relay is not built yet it is `skip`, because the probe would build the relay, which `doctor` leaves to the first run. An unavailable sandbox is a `fail` under `sandbox: strict` in the experiment and a `warn` otherwise. `off` and HTTP-mode agents are `skip`.
- `vm`: the runtime (the experiment's, or docker else `container`) answers, the experiment's image is present (it is never pulled) and the relay is built or will be on first use. A `fail` under `strict` when the experiment's agent would run in the virtual machine and something is missing. On macOS a `warn` when there is no virtual machine, because the agent then runs under Seatbelt. `skip` when the experiment names `seatbelt` or `bubblewrap`, or its agent is not sandboxed.
- `clock`: with an experiment, the clock label its command agent would get (`enforced`, or `contract-only` with the reason). Without one, whether `libfaketime` is found on Linux. Contract-only on macOS is expected and is `ok`.
- `upstream-<provider>` (only for a provider whose `BACTRIAN_UPSTREAM_<PROVIDER>` is set): the URL. A `warn` for plain `http` to a host other than this machine, because the API key is sent to it unencrypted.
- `tls`: the trusted root certificates load (see [HTTPS](#https)), and how many there are. A `warn` when `BACTRIAN_EXTRA_CA_CERTS` adds some. When they do not load, a `fail` if the experiment calls a provider whose upstream is an `https` URL, an agent or an MCP server over HTTPS, and a `warn` otherwise.
- `ca`: the project certificate authority loads, or is made when missing, with its path, expiry and fingerprint. A key that others can read, or an expired authority, is a `fail` naming the fix. `skip` when the experiment does not intercept egress.
- `node`: the `node` on `PATH` honours `NODE_USE_ENV_PROXY` (24.0 or later, or 22.21 or later). An older one is a `warn`, because a Node agent's `fetch` would bypass the egress proxy. `skip` without `node`, or when the experiment does not intercept egress.

`--json` prints `{"checks": [{"name", "status", "message"}], "ok": true}`. A configuration error in the experiment file is reported with its location and exits 64, as for `bactrian run`.

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

The assumed effect defaults to twice the threshold and the power to 0.8, and the first line names the verdict it plans for: a regression when the assumed drop is at least the threshold, an improvement when it is at most minus the threshold, otherwise no change. The power it prints comes from a fresh simulation at the chosen repeat count, so it can sit just below the target. It answers `unreachable` if no repeat count up to `--max-repeats` (200) gets there. To keep a plan from running for hours, it takes at most 10,000 scenarios, and the number of scenarios times `--simulations` times `--max-repeats` may be at most 100,000,000. `--population scenarios` also needs `--between-sd`. When the gate has more than one metric, pass `--gate-metrics N`: the pass rate is then judged at the Bonferroni level, as `compare` judges it, which needs more repeats. `--from <experiment-id>` takes the scenarios and their baseline rates from an earlier experiment's finished runs, of `compare.baseline` unless `--variant` names another.

`report` writes everything as one HTML file with inline styles and charts and no scripts or external assets, so it works offline and follows the system's light or dark theme:

```sh
bactrian report <experiment-id> -o report.html
```

With `--json` it prints the file it wrote and the same comparison as `compare --json` as one JSON object. The report holds the estimand, gate and missing data per arm, the verdicts, a forest plot per scenario, each scenario and variant's pass rate (with a Wilson interval), pass^k for every k and the median and 90th percentile of cost, tokens, latency and time to first token (with bootstrap intervals), cost, latency and token charts over the compared scenarios, the signature comparison, and each failure with its assertion messages and a collapsible trace.

## Driving bactrian from a coding agent

`bactrian serve-mcp` offers bactrian's commands to a coding agent as Model Context Protocol (MCP) tools over standard input and output. Start it in the project directory, which becomes its root. For Claude Code:

```sh
claude mcp add bactrian -- bactrian serve-mcp
```

`compare`, `plan`, `report`, `check` and `doctor` return the JSON their commands print with `--json`, and `divergences` returns one run's stored divergences as `replay --json` reports them. `run`, `record` and `replay` take longer than a tool call should, so `run_start`, `record_start` and `replay_start` return a job ID at once. `job_status` then gives the job's state, its progress in runs and, once it has finished, the command's JSON. `job_cancel` stops a job as Ctrl-C would, and a cancelled experiment's error names the ID to pass back to `run_start` as `resume`. At most four jobs run at once (`serve-mcp --max-jobs N` changes that), and a further one is refused until one ends. A finished job can be queried for an hour, and only the newest 100 finished jobs are kept. What a job wrote stays under `.bactrian`.

Every path argument must lie inside the server's working directory, symbolic links included, and run and experiment IDs cannot climb out of `.bactrian`. So must the files that an experiment or a scenario names (scenarios, the price file, golden traces, fixtures, writable paths, an MCP server's directory), and a report is not written under `.bactrian` or `.git`. A symbolic link with an absolute target cannot be followed through the server. Spend is capped by the `BACTRIAN_CAP_<PROVIDER>` variables of the server's own environment, and no tool can change a cap. Warnings go to standard error. When the agent closes the server's input, running jobs are cancelled (an experiment stays resumable) and the server exits once their agents are gone.

What the server may run is set by two flags on the command line that registers it, and never by a tool or an experiment. The tools' instructions state which are in force, and each refusal names the flag that would allow it.

- Without flags, every job runs its agent in the sandbox in `strict` mode, so the agent the coding agent names can reach only bactrian's proxy and write only to its run. On macOS that means the virtual machine, so a job needs an image (the experiment's `sandbox.image`, or the `sandbox_image` argument of `record_start` and `replay_start`). `sandbox: auto` or `off`, `backend: seatbelt` (in the experiment or as `sandbox_backend`, because under Seatbelt the agent can read the launch environment of your other processes) and HTTP-mode agents are refused, and a refusal never suggests `seatbelt` to the coding agent, and on a host where the sandbox does not work every job fails. Checkers run in the same `strict` sandbox, in `check` and in experiments, so they need no flag. MCP servers, passing environment variables to the agent and writable paths outside the project are refused, because they run or reach outside the sandbox.
- `--allow-unsandboxed` lets jobs run their agent outside the sandbox, or under Seatbelt, with the mode chosen as on the command line (`auto` unless the experiment or the `sandbox` argument says otherwise), and lets `check` take its `sandbox` argument the same way. Checkers in `auto` or `off` still need `--trust-project`, because those modes can run them unsandboxed.
- `--trust-project` allows MCP servers (`mcp_stdio`, `mcp_http`, an experiment's `mcp`), `env` and `agent.env_passthrough`, `allow_write` paths outside the project, and checkers in `auto` or `off`. MCP stdio servers, and checkers outside the sandbox, run as you with nothing to hold them, so this flag makes the server as powerful as a shell in this project: whatever can write a file here and call a tool can run any program as you. Give it only for a project and an agent you would trust with a shell.

```sh
claude mcp add bactrian -- bactrian serve-mcp --trust-project
```

The server speaks the 2026-07-28 revision, which Claude Code negotiates, and the 2025-11-25 revision and earlier for clients that open with `initialize`. Cursor is one of those, as it does not speak 2026-07-28 yet. The reasoning is in [`docs/decisions/0011-mcp-server.md`](docs/decisions/0011-mcp-server.md).

## Building from source

You need [opam](https://opam.ocaml.org), and GMP and `pkg-config` for the TLS libraries (`brew install gmp pkgconf` on macOS, `libgmp-dev` and `pkg-config` on Debian and Ubuntu). From the repository root:

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
