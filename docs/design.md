# bactrian: design

This document describes how bactrian is designed. Sections are numbered so that code comments can point at them. Parts of the design that the current build does not implement are marked as planned. The README's Status section says what works today, and [`decisions/`](decisions/) records individual choices with their reasons.

Planned, not yet implemented: interception of HTTPS egress (and with it live recording against real providers), HTTPS for HTTP-mode agents, the `ca` command, published JSON Schemas, and the MCP server mode (`serve-mcp`).

## 1. Purpose

bactrian is a command-line tool, written in OCaml, for simulating and evaluating AI agents. It treats the agent as a black box. The agent can be written in any language and use any model provider. bactrian runs it as a subprocess, intercepts everything it talks to (model application programming interfaces, Model Context Protocol (MCP) tool servers, other HTTPS services and the clock), records it, and can replay it deterministically. On top of that it runs live experiments across models and agent versions, repeats them, and says with honest statistics whether a change made the agent better, worse, no different, or whether there is not enough data yet.

It is a general tool. It has no special support for any particular agent. An agent is adapted to it through the wire contract in section 5.1.

### Guarantee scope

Three guardrails apply to every run, sandboxed or not. Agents receive placeholder API keys and never the real ones, egress is blocked unless a host is listed, and the agent's environment is built from an allowlist (section 5.3).

On top of that, every agent runs inside an operating-system sandbox when the host supports one (`sandbox: auto`, the default). Inside it the agent can reach the network only through bactrian's proxies and can write only inside its run's state and temporary directories, so traffic that bypasses the proxies fails at the operating-system level. When the sandbox cannot be set up (for example Ubuntu's AppArmor restriction without the one-time administrator fix), `auto` runs the agent unsandboxed, prints a warning with the fix, and labels the run `unsandboxed` in `meta.json` and in every report. `sandbox: strict` fails closed instead, for continuous integration and anyone who needs the hard guarantee. `sandbox: off` skips the sandbox deliberately.

Replay is deterministic for every exchange that crosses the proxies, and in a sandboxed run nothing can avoid them. The clock is `enforced` or `contract-only`, never silently assumed.

### Success criteria for v1

1. A toy agent in TypeScript and one in Python can be recorded, replayed strictly with zero divergence, and a prompt change produces exactly the expected divergence report.
2. An experiment comparing two live model variants over several scenarios with repeats produces a verdict per metric (`regression`, `improvement`, `no-change`, `inconclusive`) whose statistics match reference implementations.
3. A coding agent can drive all of this through the MCP server (planned).
4. Prebuilt binaries run on macOS arm64 and Linux x86_64 and arm64 (planned).
5. Every run is sandboxed by default where the host allows it, and otherwise visibly labelled `unsandboxed`. In sandboxed runs, escape attempts (direct network, writes outside the run, reads of credential paths) fail on macOS and Linux, and real API keys never enter the sandbox.

## 2. Decisions

| Topic | Decision |
|---|---|
| Name | `bactrian`, after the two-humped camel, because most of what it does is compare two things. |
| Licence | Apache-2.0 |
| Interfaces | Command line first, with `--json` on every command. An MCP server mapping onto the command line is planned. |
| Agent contract | Two driving protocols, both interactive JSON Lines from the start: command mode (subprocess, standard input and output) and HTTP mode (a long-lived server, one POST per message, run ID and state namespace on every request, the per-run environment in the `start` message; decision 0008). |
| Model comparison | v1: the agent picks the model from variant environment variables. Later: bactrian translates between protocols to swap models underneath an agent. |
| Provider protocols | Five adapters: OpenAI Chat Completions (also used for OpenRouter), OpenAI Responses, Anthropic Messages, Gemini `generateContent` and Gemini Interactions. |
| Tool determinism | Three mechanisms, picked per experiment: MCP interception, fixtures seeded into the state directory, and HTTPS egress interception with a generated certificate authority (planned). Per-host or per-server policy: `record`, `replay`, `passthrough`, `block`. |
| Clock | A `SIM_NOW` and `TZ` contract for every agent, plus `libfaketime` on Linux. macOS is contract-only in v1, because SIP strips `DYLD_*` variables from platform binaries (decision record 0009). Each run records whether the clock was enforced or contract-only, and why. |
| Scoring | v1: declarative YAML assertions plus external checker commands. Golden pass and fail traces test the assertions themselves. Language-model judges and human evaluation come later. |
| Regression rule | A regression only when the whole confidence interval of the difference lies beyond the threshold. Otherwise `improvement`, `no-change` or `inconclusive` (with the extra runs needed). |
| Process architecture | One binary, one process, Eio fibres. One listener per proxy type, routed by run. |
| Sandbox | On by default when available (`auto`), an unsandboxed fallback with a warning and a label, `strict` to fail closed, `off` to skip. Placeholder keys, egress blocked by default and an allowlisted environment apply in every mode. macOS: `sandbox-exec` with a generated Seatbelt profile. Linux: bubblewrap with the network namespace removed and the proxies reached through bind-mounted Unix sockets. |
| Platforms | macOS and Linux. No Windows. |
| Distribution | Prebuilt binaries (static musl on Linux, native on macOS) are planned. |

## 3. Reserved

This number is kept free so that the sections that follow keep their numbers.

## 4. Architecture

One binary, one process. Pure logic and input and output live in separate libraries. Everything that can be pure is pure, so it can be property-tested.

### `bactrian.core` (pure, no Eio, no Unix)

| Module | Responsibility |
|---|---|
| `Trace` | The event type for a run: run start and end, model request and response with chunk timing, MCP call, HTTP exchange, agent output, clock reads. Versioned, with JSON codecs. |
| `Sse` | Incremental Server-Sent Events (SSE) parser and writer. Must give the same result however the input bytes are split. |
| `Canon` | Canonical JSON (sorted keys, one number form) and BLAKE2b-256 keys from the standard library (decision 0002). |
| `Protocol` | The `PROTOCOL` signature and five implementations, one file each: `Openai_chat`, `Openai_responses`, `Anthropic_messages`, `Gemini_generate`, `Gemini_interactions`. Each detects its routes, canonicalises requests, parses responses and streams, and extracts usage. Replay sends the recorded bytes unchanged (decision 0004), so adapters never re-serialise a response. |
| `Matcher` | Layered matching, the three replay modes, the `divergence` type and structural diffs. |
| `Cassette`, `Scenario`, `Experiment` | File formats with strict decoding that collects every error with its location. |
| `Assertion` | Declarative assertions evaluated over a trace. |
| `Stats`, `Bootstrap`, `Plan_sim` | Wilson and Newcombe intervals, pass^k, the hierarchical bootstrap with a seeded pseudo-random number generator, Bonferroni adjustment, and the `plan` simulator. |
| `Compare` | The four-way verdict and gate exit codes. |
| `Clock_label`, `Elf_probe`, `Shebang`, `Rfc3339` | The clock label of a run and the pure parts of deciding it: an ELF reader (dynamic or static, Go build ID note), the `#!` line, and the Unix time of `SIM_NOW`. |
| `Pricing`, `Reservation` | Price table, cost from usage (input, cached input, cache writes, output, thinking tokens, long-context tiers) and the worst-case cost of a request. |
| `Redact`, `Mask` | Deterministic scrubbing of secrets and configured patterns, and the `normalise` masks applied before keying. |
| `Mcp_msg` | MCP messages for both protocol eras (2026-07-28 and 2025-11-25). |
| `Report_data`, `Report_html` | Report data, and its pure rendering to a single self-contained HTML file. |
| `Sandbox_policy` | Pure generation of the Seatbelt profile text and the bubblewrap argument list, so that they can be property-tested. |
| `Sandbox_log`, `Printable` | Sandbox violations from the macOS system log and the Linux forwarder's log, and agent-chosen text made safe to print on a terminal. |

### `bactrian.runtime` (Eio)

| Module | Responsibility |
|---|---|
| `Model_proxy` | One HTTP listener. Routes by run token in the path (`/r/<token>/...`). Record, replay and passthrough, including streams. MCP traffic shares the listener and the run registry through `Mcp_proxy` (decision 0006), so one port and one token cover models and tools. |
| `Mcp_proxy`, `Mcp_shim` | MCP endpoints on the listener for Streamable HTTP servers, and `bactrian mcp-shim <name>` for stdio servers that the agent launches itself. |
| `Egress_proxy`, `Ca` (planned) | HTTP `CONNECT` proxy. The run ID arrives as the proxy username. Mints a leaf certificate per host from the project certificate authority (CA) and terminates Transport Layer Security (TLS) with a fresh single-certificate configuration per connection. Applies per-host policy. Its own process environment is scrubbed of `SSL_CERT_FILE` so its upstream trust is never replaced by its own CA. |
| `Agent_driver` | The command protocol and the HTTP protocol, environment assembly, a fresh state directory, fixture seeding, turn and total timeouts, and killing the whole process group. |
| `Agent_clock` | Finds `libfaketimeMT.so.1`, decides whether the agent's clock is enforced, and adds the `LD_PRELOAD` and `FAKETIME` variables to its environment (section 5 step 2, decision record 0009). |
| `Sandbox` | Builds the per-run sandbox: Seatbelt profile on macOS, bubblewrap invocation on Linux, deny-lists, the preflight probe, and the `__sandbox-init` forwarder. |
| `Launch_env` | Re-executes `record`, `replay` and `run` with an allowlisted launch environment and restores the rest from an unlinked temporary file (section 5.2, decision record 0010). |
| `Proc_util`, `Process_group`, `Interrupt` | Helpers shared by the modules that start processes: paths, finding an executable on `PATH`, exit-status text, ending a process group (SIGTERM, then SIGKILL), and signal handlers. |
| `Checker_runner` | Runs external checker commands with the trace on standard input. Running checkers sandboxed is planned. |
| `Store` | Run directories under `.bactrian/`, content-addressed blobs, atomic writes. |
| `Ledger` | Spend per provider against caps. Shared, guarded by an Eio mutex. |
| `Executor` | Expands experiments, schedules runs with a concurrency limit, one Eio switch per run so a failing run is torn down alone. |

### `bin/`

`Cli` (cmdliner) with `record`, `replay`, `run`, `compare`, `plan`, `report`, `check`, `doctor` and `mcp-shim`. `ca` and `serve-mcp` are planned. The MCP server maps one-to-one onto the command line.

Every library module has an interface file (`.mli`). No source file is over 1000 lines.

## 5. Run lifecycle

Experiment level: load the experiment, expand scenarios x variants x repeats, estimate cost, execute under the concurrency limit, aggregate, report. Allocation is blocked and interleaved: for each scenario, the runs of all variants are put in one seeded random order, and blocks are interleaved across scenarios, so provider drift, cache warming and rate limits spread evenly over the arms. Each run records its execution index, start time, the model version the provider reports, and any rate-limit responses.

One run:
1. Set up. Create the run ID, the run directory, a fresh state directory and a temporary directory. Copy the scenario's fixtures into the state directory. Register the run with the proxies (token to mode, cassette, matcher state).
2. Build the environment:
   - provider base URLs pointing at `http://127.0.0.1:<port>/r/<run-token>/...`
   - `HTTPS_PROXY=http://<run-id>:<run-token>@127.0.0.1:<port>` and `NODE_USE_ENV_PROXY=1` (planned, with egress interception)
   - a trust bundle of the system roots plus the bactrian CA, exported as `SSL_CERT_FILE`, `NODE_EXTRA_CA_CERTS` and `REQUESTS_CA_BUNDLE` (planned)
   - `SIM_NOW`, `TZ`, and on Linux with `libfaketime` installed the variables that start the agent's clock at `SIM_NOW` and let it run: `LD_PRELOAD` (the library first, then the agent's own value), `FAKETIME=@<Unix time of SIM_NOW>` with `FAKETIME_FMT=%s` and `FAKETIME_DONT_RESET=1`. The label `enforced` needs the agent's executable to be a dynamically linked ELF file that is not a Go program; every other run is `contract-only` with a reason (`macos`, `faketime-off`, `library-not-found`, `library-hidden`, `setuid-sandbox`, `invalid-sim-now`, `static-binary`, `go-binary`, `unreadable-executable` or `http-mode`), in `meta.json` under `clock` and in the trace and cassette headers. These variables belong to a command agent's process environment. bactrian does not start an HTTP-mode agent, so it injects nothing there: the run is `contract-only` with `http-mode`, and the `env` of its `start` message has `SIM_NOW` and `TZ` but never `LD_PRELOAD` or a `FAKETIME` variable
   - `BACTRIAN_RUN_ID`, `BACTRIAN_STATE_DIR` and the variant's own variables
   - provider key variables (`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GEMINI_API_KEY`, `OPENROUTER_API_KEY`) set to a placeholder `bactrian-<run-token>`, never the real key (section 5.3)
   - `HOME` set to the state directory, with `XDG_CACHE_HOME`, `XDG_CONFIG_HOME`, `XDG_DATA_HOME` and `npm_config_cache` inside it, `TMPDIR` set to the run's temporary directory, and `PYTHONDONTWRITEBYTECODE=1`, so runtimes keep their caches inside the run. This holds with or without the sandbox, so a run's environment does not depend on the host
   - nothing else from bactrian's own environment except `PATH`, `LANG`, `TERM` and the variables the experiment lists under `agent.env_passthrough`
3. Start the agent inside the sandbox (section 5.3) and drive it through the wire contract in section 5.1. HTTP-mode runs are unsandboxed with the reason `http mode` (section 5.1), because bactrian does not start the server and cannot reset it.
4. Capture. Every exchange (model, MCP, egress, including 429 responses, retries and stream timing) is appended to the cassette as it happens. Live calls are charged to the ledger at once.
5. Tear down on `done`, turn timeout or total timeout: kill the process group, collect the sandbox's violations, unregister routes, remove the temporary directory, finalise the cassette, derive the trace, run assertions and checkers, write `verdicts.json` and `meta.json`.
6. Replay differences: egress defaults to `block`. A strict miss returns a `bactrian_replay_miss` error body to the agent and records the divergence. `until-divergence` switches the run to live and marks the live tail.

Run outcomes form a closed type:
- `Passed`
- `Failed`: assertion failures, agent crash, non-zero exit, timeout, protocol violation. These are agent failures.
- `Diverged`: replay only.
- `Errored`: a fault inside bactrian, or a checker that could not give a verdict (timeout, crash, invalid output). Excluded from pass rates and listed separately.
- `Not_run`: spend cap reached. Treated as missing data.

Spend caps are enforced by reservation. Before a live request is forwarded, the ledger reserves its worst-case cost: input tokens estimated from the request size (bytes divided by 3, a deliberate overestimate), plus output at the request's own `max_tokens` (or the provider's equivalent) or, when the request sets none, the model's maximum output from the price table. If the reservation would cross the cap, the request is refused with a `bactrian_cap_reached` error and the run becomes `Not_run`. bactrian never edits the request to add a limit, because that would change its key. When the response finishes, the reservation is replaced by the actual cost from reported usage. A stream that ends without usage (interrupted, or a provider that omits it) keeps its full reservation and is listed in the report as a usage gap. The cap therefore cannot be exceeded.

### 5.1 Agent wire contract

Both modes speak version 1 of the same JSON messages. Every message has `"v": 1` and a `"type"`. Unknown types are a protocol violation.

bactrian to agent:
- `{"v":1,"type":"start","run_id":"…","scenario":"…","variant":"…","sim_now":"…","state_dir":"…"}` once, first. In HTTP mode it also has `"env":{"NAME":"value",…}`, see below. Command mode never sends `env`.
- `{"v":1,"type":"turn","turn":<n>,"content":"…"}` for each scripted user turn, `turn` counting from 1. The next turn is sent only after the agent's `reply` for the previous one.
- `{"v":1,"type":"end"}` after the last reply. The agent must then exit (command mode) or release the run's state (HTTP mode).

Agent to bactrian:
- `{"v":1,"type":"reply","turn":<n>,"content":"…"}` exactly once per turn, with the matching `turn`.
- `{"v":1,"type":"event","turn":<n>,"name":"…","data":{…}}` optionally, any number, before that turn's reply. Recorded in the trace, never interpreted.
- `{"v":1,"type":"error","turn":<n>,"message":"…"}` instead of a reply when the agent gives up on a turn. The run ends `Failed (agent_error)`.

Command mode: one JSON object per line on standard input and output. Standard error is captured to `agent.stderr` and never parsed. End of file on standard output before the final reply, a reply with the wrong `turn`, or a line that is not a valid message ends the run `Failed (protocol_violation)` with the offending line. After `end` the agent has 5 seconds to exit, then the process group gets SIGTERM and, 5 seconds later, SIGKILL. On a turn or total timeout, a protocol violation or an interrupt, SIGTERM goes to the group at once. In a Linux sandbox the group bactrian signals holds only `bwrap`, which starts the forwarder and the agent in a session of their own (`--new-session`) and does not pass the signal on: when `bwrap` exits, `--die-with-parent` and the end of its process namespace kill them with SIGKILL. A sandboxed Linux agent therefore gets no SIGTERM and no grace period, so state it writes only on SIGTERM is lost there, unlike on macOS and in unsandboxed runs.

HTTP mode (decision 0008): bactrian sends each bactrian-to-agent message as `POST <url>` with `Content-Type: application/json`, header `X-Bactrian-Run: <run-id>` and header `<namespace_header>: <run-id>`. The response body is a JSON array of agent-to-bactrian messages for that step (empty for `start` and `end`). A non-2xx status ends the run `Failed (agent_http_error)` with the status and the first 2 KiB of the body, and a body that is not an array of valid messages ends it `Failed (protocol_violation)`. The turn timeout bounds each request and the total timeout the whole run, as in command mode. Only `http://` URLs work until the TLS connector is built in, and an `https://` URL exits 69 with the same message as an `https` upstream.

The server was started by the user, so its process environment cannot carry the per-run values. The `env` object of the `start` message holds the per-run routing and identity variables that a command-mode agent gets in step 2 of section 5: the provider base URLs with the run token, the placeholder keys (never the real ones), `SIM_NOW`, `TZ`, `BACTRIAN_RUN_ID`, `BACTRIAN_STATE_DIR`, the MCP variables, and the variant's own variables. It leaves out what belongs to the agent's own process: `PATH`, `LANG`, `TERM`, the passthrough list, and the variables bactrian redirects into the run for a command agent (`HOME`, the `XDG_*` and npm cache directories, `TMPDIR`, `PYTHONDONTWRITEBYTECODE`). One function builds the values for both modes. The agent must use them for that run's calls and key its state by the run ID, because runs can overlap. `env` is optional when decoding, so agents that predate it keep working.

HTTP-mode runs are unsandboxed whatever `sandbox` says, because bactrian does not start the process. They carry the ordinary sandbox label of section 5.3 with `applied` false and the reason `http mode`, so reports and comparisons flag them like any other unsandboxed run, and the preflight probe is not run for the experiment. The experiment prints a warning once. `sandbox: strict` in any of its forms, or `--sandbox strict`, with `protocol: http` is a configuration error (located at `sandbox`, or at `agent.protocol` for the flag).

`examples/` contains a conformance agent in TypeScript and one in Python. Each runs in either mode (`--http PORT` for HTTP mode, where port 0 picks a free port, printed as the first line of standard output). A multi-turn transcript per mode that the test suite replays against the driver is planned.

### 5.2 Local security contract

- All listeners bind to `127.0.0.1` only.
- Each run gets a run token of 128 random bits from the operating system's generator, separate from its run ID. The model proxy requires it in the path and the egress proxy will require it as the proxy password. Requests without a live token get 403.
- The project CA (planned) lives in `.bactrian/ca/` (directory mode 0700, key mode 0600), is valid for one year and is replaced with `bactrian ca rotate`. Leaf certificates are valid for 7 days, live only in memory, and are cached per host.
- The per-run trust bundle (planned) is written into the run's temporary directory and deleted at teardown.
- bactrian's launch environment holds no provider keys. `record`, `replay` and `run` re-execute bactrian with only `PATH`, `HOME`, `USER`, `LOGNAME`, `SHELL`, `LANG`, `LC_*`, `TERM`, `TZ` and `TMPDIR` in it, and hand the full environment to the new image through an unlinked temporary file, from which it is restored with `putenv`. The launch-time copy that other processes can read (section 5.3) then holds none of the keys. No child inherits bactrian's environment: each gets an explicit one (decision record 0010 lists them). Nor does a child inherit a descriptor bactrian was started with: every command marks each open descriptor above 2 close-on-exec at start, so a child gets only the descriptors bactrian passes it (an inherited directory descriptor would lead out of the Linux sandbox's mount namespace).
- Checker processes run with a scrubbed environment: no provider keys, no proxy variables, no trust-bundle variables. They receive only `BACTRIAN_RUN_ID`, `PATH`, `HOME`, `LANG` and `TZ`.

### 5.3 Sandbox contract

Modes: `auto` (default), `strict`, `off`, set per experiment (`sandbox: auto`, or `sandbox: {mode, allow_write, deny_read}`) or with `--sandbox` on `record`, `replay` and `run`, which overrides the experiment. The sandbox applies in record, replay and live modes alike, because a recording that missed bypassed traffic is as wrong as a replay that leaked. The secrets, egress and environment guardrails apply in all three modes. MCP stdio servers are part of the world, not of the agent: they run outside its sandbox and never receive its environment.

Network:
- macOS: `sandbox-exec` with a Seatbelt profile generated per run (decision record 0007): `(deny default)`, then outbound connections allowed only to one loopback port, a listener of the model proxy that serves this run alone (it answers only the run's token, as the Linux socket below does), and Unix-socket connections only to the paths bactrian names. Another run's port, and the port every run shares, are refused. Every path reaches the profile as a parameter, never as text. Domain Name System (DNS) lookups fail, which is intended.
- Linux: `bwrap --unshare-net` (and `--unshare-pid`, `--unshare-ipc`, `--die-with-parent`, `--new-session`). The namespace has no route out. For each run the model proxy also listens on a Unix socket, which answers only that run's token, in a directory of its own under the system's temporary directory (the path must stay short) and removed at teardown. The socket is bind-mounted into the sandbox. A small forwarder inside the sandbox (bactrian re-executed as `bactrian __sandbox-init`, so no `socat` is needed) listens on the same loopback ports and relays each connection to the matching Unix socket, then starts the agent as its child.
- Egress policy defaults to `block` in every mode. Hosts must be listed to be recorded or passed through, so broad file reads cannot be shipped to arbitrary hosts.

Filesystem:
- Reads are allowed broadly so language runtimes work, except a deny-list that is always applied: `~/.ssh`, `~/.gnupg`, `~/.aws`, `~/.azure`, `~/.config/gcloud`, `~/.kube`, `~/.docker`, `~/.netrc`, `~/.npmrc`, `~/.pypirc`, `~/.git-credentials`, `~/Library/Keychains`, the project's `.env` files, `.bactrian/ca/`, and other runs' directories. Experiments can add paths under `sandbox.deny_read`.
- Writes are allowed only to the run's state directory, its temporary directory (`TMPDIR` points there), and paths the experiment lists under `sandbox.allow_write`. On Linux: `--ro-bind / /`, tmpfs over `/tmp`, `/dev/shm` and `/run`, the bactrian executable and the preloaded clock library bound read-only (they may live under one of those), read-write binds for the writable paths, then the deny-list masks (an empty tmpfs over a directory, made read-only once everything is mounted, and `/dev/null` over a file). Under the mask over `.bactrian/runs` only the run's own state and temporary directories are bound again, so an `allow_write` path inside another run stays hidden. On macOS: `(deny file-write*)` with `(allow file-write* (subpath …))` for each.
- Seatbelt matches resolved paths, so every path is resolved before it enters the profile (`/tmp` is `/private/tmp` on macOS). A deny-list path that is a symbolic link (a dotfile manager's `~/.aws`, a `.env` shared between projects) is denied at its target on both platforms. On macOS the link's own path is denied too. On Linux it is not, because a mount on a link to an absolute path would be resolved outside the sandbox's root.
- `SSH_AUTH_SOCK` and other agent-socket variables are never passed in. On Linux the host's `/run` is replaced by tmpfs, which hides `docker.sock` and similar sockets.
- The agent is started with pipes, never a terminal: the sandbox refuses `/dev/tty`, which would also allow keystroke injection.

Secrets:
- The agent receives placeholder keys of the form `bactrian-<run-token>`. The model proxy recognises the placeholder, checks it belongs to a live run, and substitutes the real key from bactrian's environment when forwarding upstream. Real keys never enter the sandbox, cannot be read from the agent's memory or files, and never appear in agent traffic, so they cannot leak into cassettes. Without the sandbox this still keeps keys out of the agent's environment, but cannot stop a hostile agent from finding them by other means. `agent.real_keys: true` (planned) opts out for agents that validate key formats, and is labelled in reports.

Preflight and failure:
- Once per experiment, and once per `record` or `replay`, bactrian probes the sandbox: it runs `bactrian __sandbox-probe` inside a sandbox built like a run's, with a probe listener of its own. The probe checks that an outbound TCP connection to `1.1.1.1:443` fails without sending any data, a connection to the probe listener succeeds, a write outside the state directory fails, a write inside it succeeds, and a read of a file under a directory added to the probe's deny-list fails or reads nothing. The outbound failure has to be the sandbox's own: on macOS Seatbelt's `EPERM`, on Linux a network namespace other than bactrian's, so a host that is merely offline is not taken for a sandbox. If any check gives the wrong result: under `auto` the runs go ahead unsandboxed, with a warning naming the cause and the fix. Under `strict` the command stops with exit 69 and the same message. `off` skips the probe.
- Linux requirements: bubblewrap 0.5 or later on `PATH`, and unprivileged user namespaces allowed. On Ubuntu 23.10 and later the AppArmor restriction blocks them by default, and the fix needs root once, either an AppArmor profile for `bwrap` or `sysctl kernel.apparmor_restrict_unprivileged_userns=0`. When the probe fails and that setting is 1, the message prints both commands. bactrian never runs `sudo` itself.

Labels and violations:
- Every run's `meta.json` has `sandbox: {mode, applied, reason?}`. The terminal output of `record`, `replay` and `run`, `compare` and every report flag the runs that were not sandboxed, with the reason.
- What the sandbox denied is attached to the run as `sandbox_violation` trace events and under `sandbox_violations` in `meta.json` (`{collected, coverage, complete, reason?, items}`). `coverage` says which kinds of denial `items` can hold: `seatbelt-log` on macOS, `relay-failures-only` on Linux. `complete` is false when `items` may miss some of those, and `reason` then says why. An unsandboxed run's violations are not collected, with the reason `the run was not sandboxed`. `record`, `replay` and `run --json` print the same object, the text output prints a one-line summary such as `at least 2 (the system log can drop denials, so this is a lower bound)`, and reports and `compare` group the runs by that line. On macOS every denial in the run's profile carries a tag naming the run (`(with message …)`), and after the agent exits bactrian reads the run's tagged lines with `log show`. The system log shows a denial a second or more after it happens, so bactrian first causes one more tagged denial as an end marker and reads until the marker is there and then until two reads a second apart give the same violations, because the marker does not prove that earlier denials from other processes have been flushed. All of this takes at most 30 seconds, and reads that have not settled by then are kept with a reason saying so. If `log` fails or the marker never shows, the run records the violations as not collected, with the reason, rather than as none. The system log also drops denials outright when it is under load (a run with three denied writes once logged only the last two, and the first never arrived), and neither the marker nor the settling detects that, so macOS violations are always a lower bound: `complete` is false with the reason `the system log can drop denials, so this is a lower bound`. The few lookups that every Node and Python process makes at start are refused without a log line. On Linux the violations are the forwarder's relay failures. The forwarder writes them, one line each, to a pipe whose read end bactrian holds and copies into `sandbox.log` in the run directory, and bactrian reads them back line by line, so a line it cannot read is skipped and counted as a gap rather than losing the rest. The agent does not inherit the pipe, and the forwarder makes itself not dumpable (`prctl(PR_SET_DUMPABLE, 0)`) before it starts the agent, so the agent, the same user in the same process namespace, cannot reopen the forwarder's descriptor through `/proc/<pid>/fd` either. Were that call to fail, the forwarder warns, and the residual is that the agent could add lines through the pipe: it still could not truncate or rewrite earlier ones, because a pipe cannot be rewound. Denied writes and reads surface there as the agent's own errors.
- A denial's process name and target are the agent's to choose, so a violation's kind is taken only from a known Seatbelt operation that follows `(PID) deny(N)`, and a tagged message without one is kept whole with the kind `unparsed`. The terminal output of `record`, `replay` and `run` writes every control character in such text as an escape (`\x1b`), so an agent cannot move the cursor or send terminal commands through it.
- What each kind of escape attempt yields, as exercised by the escape agents: on macOS, an outbound connection (HTTPS, raw TCP or WebSocket) is a `network-outbound` violation naming the port, a DNS lookup is a `network-outbound` violation on `/private/var/run/mDNSResponder` (Node also logs `mach-lookup` of `com.apple.SystemConfiguration.DNSConfiguration`, which Python's lookup does not log), a write is `file-write-create` and a read of a denied path is `file-read-data`. On Linux none of these is a violation. Each fails inside the agent (`ENETUNREACH`, `EROFS`, or `ENOENT` for a masked path).
- Launch environments: on macOS Seatbelt does not govern the `sysctl` that returns another process's launch-time arguments and environment, so an agent can read those of any process of the same user whose binary is not an Apple platform binary. bactrian therefore keeps the provider keys out of its own launch environment and out of every child's (section 5.2, decision record 0010). Residual risk: other processes of the user, such as a shell installed from Homebrew, `tmux`, `direnv` or the `uv run --env-file` that started bactrian, can hold keys in theirs, and the agent can read them. Run bactrian as a dedicated user or in a virtual machine when the agent is hostile. On Linux the agent has its own process namespace and sees no other process.
- Concurrent runs on macOS: another agent's launch environment holds that run's token (in its base URLs and placeholder keys), and is readable in the same way. The token is of no use from this sandbox, because the profile allows only the run's own port, which answers only the run's own token.
- MCP stdio servers on macOS: a server's launch environment holds the `env` the experiment declares for it, and its command line its arguments, and the agent can read both the same way. No sandbox setting hides them. bactrian warns once per command or experiment when the sandbox is Seatbelt and a stdio server it will start has arguments or `env`.
- Leftover processes: teardown signals the agent's process group. A process the agent starts in a new session or group (`setsid`, a daemon, a double fork) is not in it, so on macOS and in unsandboxed runs it can outlive the run and bactrian. On macOS it stays inside the run's profile: it can still write to the run's state directory and reach only the run's port, which closes when the run ends, so it cannot act as a later run. Unsandboxed, nothing confines it. On Linux the sandbox's process namespace ends it with the run. A bactrian killed with SIGKILL leaves its agents and MCP servers running on macOS and unsandboxed.

## 6. File formats

Human-written files are YAML (JSON accepted). Machine-written files are JSON Lines. Every format has a `version`. Hand-written JSON Schemas in `schemas/` are planned. Unknown fields are rejected. Project data lives in `.bactrian/` (runs, blobs, CA, ledger), which users are told to gitignore.

### Scenario

```yaml
version: 1
id: schedule-meeting
now: 2026-10-14T09:00:00+01:00
timezone: Europe/London
fixtures: ./fixtures/basic
turns:
  - user: "You promised Amy a meeting, can you set it up?"
  - user: "Wednesday works"
assertions:
  - tool_sequence: { mode: contains, tools: [calendar_read, calendar_create] }
  - tool_args: { tool: calendar_create, match: { attendees: ["amy@example.com"] } }
  - output: { turn: last, regex: "(?i)wednesday" }
  - max_steps: 12
  - max_cost_usd: 0.05
checkers:
  - command: ["python3", "checks/no_double_booking.py"]
golden:
  pass: [golden/ok.trace.jsonl]
  fail: [golden/double-booked.trace.jsonl]
```

Assertion kinds in v1: `tool_sequence` (`exact`, `in_order`, `contains`), `tool_args` (`exact`, `partial`, `schema`), `max_steps`, `output` (`regex`, `jsonpath`, `equals`), `max_cost_usd`, `max_tokens`, `max_latency` and `no_egress` (host list). A checker command receives the trace as JSON on standard input and prints `{"pass": bool, "score": number?, "message": string?}`.

### Experiment

```yaml
version: 1
name: luna-vs-flash
agent:
  protocol: command            # or: http { url, namespace_header }
  command: ["node", "dist/adapter.js"]
  timeouts: { turn: 120s, total: 600s }
scenarios: ["scenarios/*.yaml"]
variants:
  luna:  { env: { AGENT_MODEL: gpt-6-luna } }
  flash: { env: { AGENT_MODEL: gemini-3.8-flash } }
repeats: 20
concurrency: 4
intercept: [model, mcp, fixtures, egress]
egress: { default: block, hosts: { "www.googleapis.com": record } }
mcp:
  calendar: { command: ["python3", "tools/calendar_server.py"], policy: record }
normalise: ["\\d{4}-\\d{2}-\\d{2}"]
clock: { faketime: auto }      # auto | off
caps_usd: { openai: 5, gemini: 5, anthropic: 5 }
pricing: ./prices.yaml
seed: 1
compare:
  baseline: luna
  confidence: 0.95
  thresholds: { pass_rate: 0.05, cost: 0.20, latency_p50: 0.25 }
```

Record and replay policies belong to the experiment, not the scenario. How you run is not what you check.

### Cassette (`cassette.jsonl`)

The first line is a header: schema version, bactrian version, run ID, scenario, variant, recording time, `sim_now` and clock enforcement. Each following line is one exchange: sequence number, `t_ms` (monotonic offset), `kind` (`model`, `mcp` or `http`), protocol, canonical key, occurrence, redacted request, response (status, selected headers, then either a body or `stream: [{dt_ms, data}]`), and the error if any. Payloads over 64 KiB go to `blobs/blake2b-256/<hash>` and are referenced by hash.

### Trace (`trace.jsonl`)

Normalised events: turn start, model call, tool call, tool result, HTTP call, reply and done. Field names follow the OpenTelemetry GenAI conventions (`gen_ai.request.model`, `gen_ai.usage.input_tokens` and so on). Those conventions are still marked Development, so bactrian pins the names it uses in its own schema.

Tool calls are derived from model traffic (the call in a response, the result in the next request). Provider tool-call IDs and MCP JSON-RPC IDs are unrelated, so an MCP record is joined to a model tool call by tool name and canonical arguments within the same turn, taking the earliest unjoined candidate in order. The trace records this in the tool call's `mcp` field: `"joined"`, `"joined_by_order"` when several identical candidates existed and the join used their order, or `"unjoined"`. An MCP call with no matching model tool call appears on its own as an agent event named `mcp_call`, because a tool call there would be counted by assertions. Assertions count calls from the model view only, so a call is never counted twice. Assertions and checkers only ever see the trace.

## 7. Replay and matching

Canonical keys:
- Model calls: every request field is semantic, and therefore part of the key, unless the protocol adapter lists it as ignorable with a reason. Only fields that cannot change the response's content may be ignored: identifiers, attribution and billing hints. Routing and feature fields (OpenRouter's `provider`, `models`, `route`, `plugins` and `debug`, and Responses' `background` and `include`) stay in the key. Coverage holds by construction: unknown fields are kept, so only the listed fields are ever removed, and each adapter's tests check that every listed field is removed and an unknown one kept. Ignorable in v1: request and idempotency IDs, `user`, `metadata`, safety identifiers, prompt-cache keys and retention, `service_tier`, Anthropic thinking signatures and `cache_control`, Gemini `thoughtSignature` and `labels`, and OpenAI `include_obfuscation`. Tool-call IDs are replaced by position. Server-side state handles (`previous_response_id`, `conversation`) stay in the key. In replay they refer to recorded IDs, which the replayed responses themselves returned, so they match. `store: true` is kept in the key and flagged in `meta.json`, because server-side state cannot be reproduced in a live tail.
- The key also covers the method, the path, the query string (Gemini's `alt=sse` changes the response framing) and the behaviour headers `anthropic-version`, `anthropic-beta` and `openai-beta` (decision 0005).
- Experiment `normalise` patterns are masked before hashing. Each mask that fired is counted per run in `meta.json`, so over-broad patterns are visible.
- Egress HTTP (planned): method, host, path, sorted query, body (canonical if JSON, otherwise a byte hash), and `content-type` as the only header.
- MCP: server, method, tool name and canonical arguments. For other methods, the parameters without the JSON-RPC ID and without the `_meta` entries that change from one client or run to the next.
- Redaction replaces each distinct secret with a stable placeholder numbered in order of first replacement within the cassette (`<secret:1>`, `<secret:2>`), which is deterministic: literals first, then each pattern in order. Two different secrets never share a placeholder, so keys do not collide. Keys are computed on the placeholder form. In replay the agent receives placeholders in responses and sends them back, so its later requests still match. Secrets are found by the `authorization`, `x-api-key`, `x-goog-api-key` and cookie headers, the `key` query parameter, provider key formats, and the experiment's `redact` patterns.
- Stored headers follow an allowlist, not a denylist: request `content-type`, `accept`, `anthropic-version`, `anthropic-beta` and `openai-beta`, and response `content-type`, `mcp-session-id`, `retry-after`, `x-ratelimit-*`, `anthropic-ratelimit-*` and `x-request-id`. Cookies and `set-cookie` are never stored.
- Cassettes are plain text. Cassette encryption is listed under Later.

Matching layers:
1. `(key, occurrence)` lookup, where occurrence counts earlier identical requests in the run on the same channel.
2. Model and egress calls are ordered per channel. In `strict` mode a matched key that arrives out of recorded order is a `Reordered` divergence. `--allow-reorder` downgrades it to a warning. The lenient modes (`until-divergence`, `fuzzy`) always serve a reordered match and report the `Reordered` finding. Every request counts towards its key's occurrences, including refused and live ones. When a request is both out of order and of the wrong shape, the order check runs first and both findings are reported.
3. MCP calls are matched by `(key, occurrence)` regardless of order, because agents issue parallel tool calls. Identical concurrent calls are served in recorded order. Missing and unconsumed MCP calls are still divergences in `strict` mode.

Divergence type:
- `Missing_request` with the nearest recorded candidate and a structural diff path (for example `messages[3].tool_result.content`).
- `Unconsumed`: recorded calls the agent never made.
- `Reordered`.
- `Shape_mismatch`: streaming versus non-streaming.

Nearest means highest similarity among the next 64 unconsumed records on the same channel in sequence order. A bounded window keeps a miss cheap under the runtime's lock, and the record a changed agent was about to make is almost always near the front. Candidates whose leaf counts alone rule out beating the best similarity so far are skipped before diffing. Ties go to the lowest sequence number. The same search drives fuzzy mode.

Modes:
- `strict`: a miss returns `bactrian_replay_miss`. The run ends `Diverged`. Assertions still run, for information.
- `until-divergence`: the first miss on the model channel switches that run's model channel to live (this needs keys and counts against caps). The switch is a run-level flag set under a mutex. Requests that arrive after it go live, and requests already being replayed finish from the recording. MCP and egress stay in replay, because live tool servers never saw the replayed prefix and would hold different state. A miss on those channels ends the run `Diverged` and never triggers the switch. `--tail-tools live` lets tools go live as well, and labels the run `state unreconciled` in every report. The live tail is recorded and shown.
- `fuzzy`: serves the nearest candidate if similarity is at least the threshold (default 0.9) and records the score on every match.

Streams replay chunk by chunk. `--timing fast` (default) sends them without delay. `--timing recorded` reproduces the original pacing. Latency figures from a replay always come from recorded timing and are labelled as such.

Replay tests the agent's own code, never model variance. `compare` therefore computes statistics over live runs only, and replay reports show divergences, not pass rates.

## 8. Statistics and reports

### What is estimated

Two estimands, chosen per experiment with `compare.population`:
- `suite` (default): performance on exactly these scenarios. Scenarios are fixed. Uncertainty comes only from run-to-run variation. Valid with any number of scenarios, including one.
- `scenarios`: performance on scenarios like these, treating the suite as a sample. Uncertainty comes from both levels (hierarchical bootstrap: scenarios, then runs). Requires at least 10 scenarios, otherwise it is a configuration error.

The suite-level metric is the pass rate averaged over scenarios with equal weight. Reports state the estimand in their header.

### Per scenario and variant

Over n runs (`Errored` and `Not_run` excluded, but counted and shown per arm):
- pass rate with a Wilson score interval
- pass^k with the unbiased estimator C(c,k)/C(n,k) for every k up to n (tau-bench, arXiv 2406.12045)
- cost, tokens, total latency and time to first token per model call, as median and 90th percentile with bootstrap intervals

### Comparing variant B with baseline A

- Pass rate, suite level: the difference of the equal-weighted means. Under `suite`, a stratified Newcombe interval: a stratified Wilson interval per arm (Yan and Su) combined with Newcombe's hybrid rule. It is closed form, never zero-width (so an arm where every run passes cannot produce false certainty), and equal to the per-scenario Newcombe interval with one scenario. Under `scenarios`, the hull of a paired hierarchical bootstrap (scenarios resampled with the same draw for both arms, then runs, 10,000 resamples, seeded pseudo-random number generator, seed in the report) and the stratified Newcombe interval. A percentile bootstrap alone is not used because near 100% pass rates with few runs it gives zero-width intervals, and in simulation it returned false no-change verdicts up to 73% of the time.
- Pass rate, per scenario: Newcombe hybrid score interval for the difference of proportions.
- Cost and tokens, suite level: the log of the ratio of equal-weighted means of per-run values, log(mean_B / mean_A), where each mean averages the per-scenario means. Spend is additive, and pooled medians would call a 30% rise in spend confined to some scenarios no change. Latency, suite level: the equal-weighted average over scenarios of per-scenario log ratios of medians, so a few slow runs do not dominate. Both use a bootstrap interval that resamples within each scenario over the scenarios both arms have data for. For cost and tokens it is Hesterberg's expanded percentile interval, because the plain percentile interval undercovers ratios of skewed means at small n (0.83 to 0.88 at nominal 0.90 in simulation). Latency keeps the plain percentile interval. Results are shown back-transformed as a percentage. A scenario needs at least 5 values in each arm to count. Swapping A and B negates the log ratio. If either median is zero, or too few values remain, the metric is reported as `undefined`. An undefined gate metric counts as inconclusive.

### The gate

The experiment declares its gate family in `compare.gate`, a list of suite-level metrics with a threshold each. The default is `[pass_rate]` only. Every interval for a gate metric is computed at the Bonferroni-adjusted level 1 − α/m, where m is the number of gate metrics. A verdict uses one side of a two-sided interval, so the chance of any false regression across the gate is at most α/2 (deliberately conservative). Per-scenario results and metrics outside the gate are reported, with Bonferroni across scenarios for per-scenario intervals, but they never fail the gate. A threshold is never assumed: a gate metric without one is a usage error.

Verdict per metric with threshold δ (for cost and latency δ is a relative change):
- `regression`: the whole interval is worse than δ
- `improvement`: the whole interval is better than δ
- `no-change`: the whole interval lies within ±δ
- `inconclusive`: anything else, reported with its lean

`compare --gate` exit codes: 1 if any gate metric is `regression`, 2 if any gate metric is `inconclusive` and `--fail-on-inconclusive` is set, 3 when missing data could bias the comparison, otherwise 0. Missing data means more than 5% of runs in either arm are `Errored`, or the `Errored` plus `Not_run` share differs between the arms by more than 5 percentage points, or a scenario has no finished run in one arm (it is left out of the comparison and listed). The codes are checked in the order 1, 3, 2, so a regression wins over missing data, which wins over inconclusive.

### Planning

`plan` simulates the exact decision rule rather than using a closed-form formula. Its inputs are the baseline pass rate per scenario (from earlier runs, or one assumed figure), the assumed true effect (default 2δ, since an effect equal to δ can never be called a regression with certainty), δ, the number of scenarios, the number of gate metrics (`--gate-metrics`, default 1, for the Bonferroni level) and, under `scenarios`, the between-scenario standard deviation of the effect. That last input is always required: `plan --from` reads baseline rates from earlier runs, but one variant's runs say nothing about how the effect spreads. It returns the smallest repeat count per scenario and variant that reaches the correct verdict with the requested power (default 0.8), with the Monte Carlo standard error of that power estimate. It answers `unreachable` when no repeat count up to `--max-repeats` (default 200) gets there, which happens under `scenarios` when between-scenario variation dominates.

For an `inconclusive` verdict, the report runs the same simulator with the observed per-scenario rates as the truth and gives the extra repeats needed, or says that more repeats cannot resolve it (for example when the estimate sits right at the threshold).

### Behaviour without statistics

- Trajectory signature: the sequence of tool names in a run. Reports show each variant's signature distribution per scenario and flag new behaviours (a signature in at least 10% of B's runs and none of A's).
- Step-count distributions.

### Outputs

- A terminal verdict table by default.
- `--json` with a schema version on every command.
- `report` writes one self-contained HTML file with inline styles and charts and no scripts or external assets: a header with the estimand, gate family and missing-data counts per arm, an overview of verdicts, a per-scenario forest plot (difference, interval, threshold band) as inline SVG, cost, latency and token charts, the signature comparison, and failures with assertion messages and a collapsible trace. It works offline and follows the light or dark theme.

### Verification

Reference values come from SciPy and statsmodels, generated once by a small Python script run with uv and committed as fixtures. QCheck properties that hold:
- every interval contains its point estimate
- coverage: over many simulated datasets with a known truth, the interval covers it at close to the nominal rate (checked within Monte Carlo error)
- expected width shrinks as n grows (averaged over simulated samples, not per added observation)
- swapping A and B negates differences and log ratios
- verdicts agree with intervals and thresholds

## 9. Errors and failure modes

Modules return `result` with their own error types, chained with `let*`. Exceptions are for programming errors and the executable's top level only.

| Class | Handling |
|---|---|
| Configuration | Every error is collected, each with file, line, column, document path and a suggested fix. Unknown fields are rejected. A glob matching nothing is an error. Exit 64. |
| Environment | A sandbox preflight failure (section 5.3) is never silent: under `auto` it warns and labels every run `unsandboxed`, under `strict` it stops with exit 69. `doctor` and every run check: missing key variables for providers that will be called live (named), unbindable ports, an agent command that cannot be found, `libfaketime` being unavailable (a warning, and the clock becomes contract-only), and, once egress is intercepted, the CA (created if missing) and a Node version too old for `NODE_USE_ENV_PROXY` (a warning). Exit 69. `doctor [--experiment FILE] [--json]` runs the checks without an agent, one line each (`ok`, `warn`, `fail` or `skip`), and exits 69 when any is `fail`. A missing key is a warning, or a failure for a provider the experiment's `caps_usd` calls live. The sandbox check is the preflight probe of section 5.3, with the same cause and fix, and is a failure under `strict` and a warning under `auto`. The CA and Node version checks are `skip` while egress is not built. So is the HTTPS support check, unless the experiment needs HTTPS, which is a failure. |
| Agent | `Failed` with a reason: `Exit_nonzero`, `Timeout` (turn or total), `Protocol_violation` (with the offending line), `Crashed`. |
| Provider | 429 and 5xx responses pass through unchanged and are recorded. bactrian never retries on the agent's behalf. An unreachable provider gives the agent a 502 with a bactrian error body, recorded as an upstream error. |
| bactrian itself | `Errored` with an error ID and a stack trace in the run's `bactrian-error.log`. Other runs are unaffected. Exit 70 if the whole command fails. |
| Spend cap | Refused requests end their run `Not_run (cap reached)`, and remaining runs are not started. Usage gaps from interrupted streams are reported. |
| Interrupt | Ctrl-C cancels the experiment's switch: agents are killed, unfinished cassettes are marked `partial`, the ledger is flushed and a report is written for completed runs. Exit 130. `run --resume <id>` skips runs whose outcome is Passed, Failed or Diverged, re-runs `Not_run` and `Errored` runs, and refuses if the experiment, its scenarios, the agent command or the price file changed since the manifest was written. |

Crash safety: cassettes and the ledger are append-only JSON Lines, flushed per record. `meta.json` and `verdicts.json` are written to a temporary file and renamed. Shared state (the ledger, the CA's per-host certificate cache) is guarded by Eio mutexes. Authorisation headers are removed before storage, secrets are never logged, and an invalid redaction pattern is a configuration error.

## 10. Testing

1. Unit and snapshot tests. Snapshots cover parsers, canonicalisation, error messages and rendered reports.
2. QCheck properties:
   - `Sse`: print then parse is the identity, and parsing is invariant under any split of the input bytes
   - `Canon`: idempotent, and key order is irrelevant
   - `Matcher`: recording any generated sequence and replaying it matches perfectly. Changing one element yields `Missing_request` at exactly that index with the right diff path. MCP matching ignores order.
   - `Redact`: idempotent, and generated secrets never appear in the output
   - `Stats`: the properties in section 8
3. `qcheck-stm` model-based tests of the matcher and cassette session against a simple reference model.
4. Protocol fixtures: real streams captured once per provider, redacted and committed, plus hand-written edge cases: `[DONE]`, mid-stream error frames, keep-alive comments, OpenAI obfuscation fields and Anthropic thinking signatures. Every adapter parses every fixture. Because replay sends the recorded bytes unchanged, the fixtures also pin the chunk boundaries that replay must preserve.
5. Integration tests, local and free:
   - fake upstream servers in OCaml for the provider protocols, with streaming, slow chunks, 429, 5xx and mid-stream errors
   - toy agents in `examples/` (TypeScript with pnpm, Python with uv) run as real subprocesses through record, strict replay with zero divergence, and a prompt change with the exact expected divergence
   - MCP: fake servers for both protocol eras through `mcp-shim` and over Streamable HTTP
   - egress (planned): a fake HTTPS server reached through the interception proxy from Node `fetch` and from Python `httpx` and `requests`
   - clock: a Python and a Node agent print their time, in a time zone other than UTC and inside the sandbox too. On Linux with `libfaketime` it must equal `SIM_NOW` and the run is `enforced`. On macOS the run is flagged `contract-only` (`macos`). ELF, `#!`, library search and environment injection are unit-tested on constructed fixtures.
   - sandbox: escape-attempt agents in TypeScript and Python (`test/sandbox/`) make one model call through the proxy, then try a direct HTTPS request ignoring proxy variables, a raw TCP connection, a WebSocket, a DNS lookup, a write to `$HOME` and outside the state directory, and a read of a deny-listed path. Every attempt must fail, and each reports its own outcome. On macOS each must also appear as a `sandbox_violation` event. On Linux the operating system logs nothing, so the agent's reported failure is the evidence. The agent dumps its environment, and the test searches it and every file of the run's world for the real provider key, which is set in bactrian's own environment as well. It must find the placeholder and never the key.
6. Determinism: the same inputs and seed give a byte-identical JSON report.
7. Live smoke tests are opt-in, limited by spend caps, and never run in continuous integration, because a public repository's workflows get no secrets.
8. Continuous integration on GitHub Actions: Ubuntu and macOS arm64, strict warnings in the dev profile, an `ocamlformat` check, a locked opam install, the test layers above, and the example agents built with pnpm and uv.

## 11. Stack and verified facts

The facts below were checked in October 2026 against vendor documentation and local experiments. Re-check anything marked "re-check" before relying on it.

| Area | Choice | Notes |
|---|---|---|
| Compiler | OCaml 5.5.1, local opam switch | `js_of_ocaml-compiler` 6.4.1 caps OCaml below 5.6, so a browser toplevel build would stay on 5.5.x. |
| Build | dune 3.24.x, opam lock files | `dune pkg` is not used. |
| Concurrency | Eio 1.6 | `Eio.Process` does not document process-group kill, so the driver handles it explicitly. |
| HTTP | cohttp-eio 6.3.0, pinned to the master commit that includes PR #1149 | 6.3.0 corrupts partially delivered chunks, which breaks SSE. The pin is dropped at the next release. HTTP/1.1 only. |
| TLS (planned use) | tls-eio 2.1.3, ca-certs 1.0.3, x509 1.2.0, mirage-crypto | No Server Name Indication callback in `Tls.Config.server`, so one `Single` config per connection, built from the `CONNECT` host. `ca-certs` reads `SSL_CERT_FILE` and `OCAML_EXTRA_CA_CERTS`, hence the environment scrub of the egress proxy. |
| JSON | yojson, ppx_deriving_yojson 3.10.0 | 3.9.x cannot build on OCaml 5.5. |
| YAML | yaml 3.2.0 | Vendors libyaml, links statically. |
| CLI | cmdliner 2.1.1 | 2.x removed prefix matching. |
| Testing | Alcotest, QCheck 0.91, qcheck-stm 0.10 | |
| Schemas (planned) | hand-written JSON Schemas, validated in tests | OCaml JSON Schema validators are young, so they would be test-only. |
| HTML | tyxml 5.0.0 | |
| Storage | plain files, no SQLite in v1 | `sqlite3` links the system library, which complicates static builds. |
| Statistics | hand-rolled, tested against SciPy and statsmodels fixtures | Owl's last release was January 2025. `pareto` does not install on OCaml 5. |
| MCP | in-tree | No OCaml library targets 2026-07-28. The server side targets 2026-07-28 (stateless, `server/discover`, a tasks extension for long operations) with a 2025-11-25 fallback. The proxy and the shim pass either era through unchanged, so a recording replays its own handshake (`server/discover` or `initialize`). The HTTP route answers 405 to the legacy GET stream so older clients fail fast. Claude Code negotiates 2026-07-28. Cursor still speaks 2025-11-25. |
| Sandbox | `sandbox-exec` (macOS), bubblewrap (Linux) | The same model as Anthropic's Apache-2.0 sandbox runtime (`anthropic-experimental/sandbox-runtime`). Checked on macOS 26.6.1: external HTTPS and DNS blocked, loopback allowed, writes outside the allowed subpath denied. Ubuntu 24.04 needs the AppArmor user-namespace restriction lifted for `bwrap --unshare-net`. Landlock was considered and rejected as the network layer, because it filters TCP by port only, not by address, and needs Linux 6.7. |
| Clock | libfaketime 0.9.13 (Ubuntu ships 0.9.10 to 0.9.12, and 0.9.10 parses the same) | Works on Linux via `LD_PRELOAD`, with the multi-thread build `libfaketimeMT.so.1`. Not injected on macOS in v1: SIP strips `DYLD_*` variables from platform binaries and hardened-runtime ones ignore them. Go binaries and statically linked ones bypass it everywhere. The `@` start-at form is read by `mktime` in the process's own `TZ`, so bactrian passes a Unix time with `FAKETIME_FMT=%s` instead (decision record 0009). |

Agent-side interception facts:
- Node: `fetch` honours `HTTPS_PROXY` only with `NODE_USE_ENV_PROXY=1` (Node 22.21+ or 24.5+). `NODE_EXTRA_CA_CERTS` adds the CA. The official OpenAI and Anthropic TypeScript SDKs use global `fetch`. WebSocket clients built on `ws` (including Gemini Live in `@google/genai`) skip the proxy entirely. Under the sandbox their connections fail at the operating-system level.
- Python: the OpenAI and Anthropic SDKs now use httpx2, which honours `SSL_CERT_FILE` and proxy variables with `trust_env`. `requests` uses `REQUESTS_CA_BUNDLE`.
- Go: `ProxyFromEnvironment` by default. `SSL_CERT_FILE` behaviour on macOS changed in Go 1.27.
- Rust reqwest 0.13: rustls with the platform verifier, and it also reads macOS system proxy settings.

Provider facts:
- Streaming: OpenAI Chat ends with `data: [DONE]`, can send error frames after a 200, adds obfuscation fields by default, and reports usage in a final chunk with `stream_options.include_usage`. OpenAI Responses uses `event: response.*` lines and no `[DONE]`. Anthropic uses `message_start`, `content_block_delta`, `message_delta` (usage) and `ping`. Gemini Interactions uses `event:` lines and ends with `event: done` and `data: [DONE]`. The raw framing of Gemini `streamGenerateContent` is undocumented, so the adapter is tested against captured streams, and the framing depends on the `alt=sse` query parameter (decision 0005).
- Anthropic thinking blocks are accepted only while the system prompt, tools and earlier messages are unchanged. Otherwise the API returns 400 ("bound to a different conversation") on accounts created on or after 2026-08-31. This bites `until-divergence`: after a prefix change the agent may resend recorded thinking blocks to the live API. bactrian surfaces that 400 as an upstream error with an explanatory hint rather than hiding it. The matcher also strips signatures before hashing, which is bactrian's own choice and not a documented requirement.
- Opus 5.5, Sonnet 5.5 and Fable 5.1 reject forced `tool_choice`, prefill and disabled thinking.
- Rate limits: OpenAI sends `x-ratelimit-*` and `retry-after` (a minimum, absent on quota errors). OpenRouter free models allow 20 requests per minute and 50 per day (1,000 per day after buying 10 credits), and `GET /api/v1/key` reports the remaining free requests.

Built-in prices, in US dollars per million tokens. They are data and can be overridden or extended with an experiment's price file, whose entries carry effective dates. The OpenAI and Gemini figures should be re-checked against the vendors' pricing pages.

| Model | Input | Cached input | Output | Notes |
|---|---|---|---|---|
| gpt-6-luna | 0.10 | 0.01 | 0.50 | Cache write 0.125 |
| gpt-5-nano | 0.05 | 0.005 | 0.40 | |
| claude-haiku-5-5 | 0.10 | 0.01 | 0.50 | Cache write 0.125 (5 minutes) or 0.20 (1 hour). Above 100k prompt tokens, counting cache reads and writes, every rate is 5x. `inference_geo: "us"` adds 1.1x. |
| gemini-3.8-flash | 0.75 | 0.075 | 3.75 | From 2027-01-01: input 1.50, cached 0.15, output 7.50. Thinking tokens are billed as output. |
| gemini-3.1-pro-preview | 2.00 | 0.20 | 12.00 | Up to 200k context. Higher above. |
| OpenRouter `:free` | 0 | 0 | 0 | Rate-limited as above |

## Later (not v1)

- Cassette encryption at rest.
- Model substitution by protocol translation in the proxy.
- Language-model judges with calibration against human labels.
- A human evaluation interface.
- Simulated users driven by a language model (the interactive protocol already allows it).
- Fault injection (latency, errors, truncated streams) as an experiment option.
- OpenTelemetry Protocol (OTLP) export of traces.
- Transparent (non-proxy) interception, HTTP/2 upstreams, and WebSocket and gRPC capture.
- Clock enforcement on macOS and for Go and statically linked binaries.
- Reading and writing Inspect AI log files.
- Windows.

## Residual risks

| Risk | Mitigation |
|---|---|
| HTTPS interception breadth (old Node, certificate pinning, WebSockets, gRPC, macOS platform verifiers) | Fail loudly in replay, `doctor` warnings and a documented support matrix. |
| MCP specification churn (a breaking 2026-07-28 revision, split client support) | Dual-era code with a single internal message type. |
| Price drift | The price table is data with effective dates, and can be overridden per experiment. |
| OpenRouter free tier (50 requests per day) | Treat 429 responses as expected, and keep smoke runs short. |
| Sandbox portability (Ubuntu's AppArmor user-namespace restriction needs root once, bubblewrap missing, `sandbox-exec` deprecated by Apple though still working, `bwrap` failing inside unprivileged containers) | `auto` falls back to unsandboxed with a warning, the exact fix commands and a label on every run. `strict` for anyone who needs the guarantee. |
| Broad reads plus allowed hosts could still exfiltrate data | Egress `block` by default, a deny-list of credential paths and placeholder keys. This is documented as the residual risk. |
