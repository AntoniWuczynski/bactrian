# 0011: An MCP server over stdio

Date: 2026-10-13

## Context

Coding agents such as Claude Code, Codex and Cursor drive local tools through the Model Context Protocol (MCP). bactrian already speaks MCP as a proxy between an agent and its tool servers (decision 0006), and every command's work and `--json` output live in the command library (`bactrian.app`). `bactrian serve-mcp` puts those commands behind MCP tools, so a coding agent can record, replay, run experiments, compare variants and read divergences without parsing terminal output.

Two protocol eras are in use. Revision 2026-07-28 has no handshake. Every request carries its protocol version and the client's capabilities in `_meta`, every result says `resultType`, list results carry `ttlMs` and `cacheScope`, and `server/discover` reports what the server supports [1][2][7]. Revisions 2025-11-25 and earlier open a session with `initialize` [5]. Claude Code 2.1.292 negotiates 2026-07-28 with local stdio servers. Cursor still opens with `initialize` and has given no date for the newer revision.

`run`, `record` and `replay` can take minutes, longer than a client waits for one tool call.

## Decision

### Transport

stdio only. The client starts `bactrian serve-mcp` as a subprocess and exchanges one JSON-RPC message per line with it [4]. Streamable HTTP would add Origin validation, the header rules of 2026-07-28 and a listener that a page in a browser could reach, for no client that needs it now.

stdout carries nothing but MCP messages, and never a request [4]. Before anything else the server moves its protocol channel to descriptors of its own, puts `/dev/null` on descriptor 0 and standard error on descriptor 1. A child process that inherits the standard descriptors (a sandbox probe, `log show`) or a stray print therefore cannot read the client's messages or write between the replies. Replies go through one writer under a mutex, one encoded message per line. Warnings and progress go to standard error, which the client may show or ignore [4].

`serve-mcp` starts agents, so it scrubs its launch environment first, as `record`, `replay` and `run` do (decision 0010).

### Both eras on one connection

A server that supports both eras "selects its behavior from how the client opens" and "MAY serve both eras concurrently" [2]. This server fixes the era of a connection once and keeps it, which the page permits rather than asks for. The era is fixed by the first request that names one and is accepted: one that succeeds, or that gets the version error a dual-era client recognises as modern. After any other error such a client takes the server for a legacy one and falls back to `initialize` [4][9], which must then still work, so a rejected request fixes nothing.

- `initialize` selects the legacy revisions. The answer names the revision the client asked for when it is 2025-11-25, 2025-06-18 or 2025-03-26, and 2025-11-25 otherwise, since a server that does not support the requested version answers with the latest one it does [5]. `ping`, `tools/list` and `tools/call` follow. `notifications/initialized` is accepted but not waited for. A second `initialize` is an invalid request (-32600), and `server/discover` is not a method of this era (-32601).
- A request whose `_meta` holds `io.modelcontextprotocol/protocolVersion` selects 2026-07-28. Any other version is answered with `UnsupportedProtocolVersionError`, code -32022 with `data: {supported, requested}` [2][7]. The page says the server "MUST respond with an UnsupportedProtocolVersionError listing the versions it does support", and its example from a dual-era server lists `["2026-07-28", "2025-11-25"]` [2], so `supported` lists 2026-07-28 and the three legacy revisions. A request without the version or without `io.modelcontextprotocol/clientCapabilities`, both required in `_meta`, is invalid params (-32602), as the overview page requires [9]. `server/discover` answers with `resultType: "complete"`, `supportedVersions`, `capabilities: {"tools": {}}`, `instructions`, `ttlMs`, `cacheScope` and the server's name and version under `_meta["io.modelcontextprotocol/serverInfo"]` [1][7]. `supportedVersions` lists only 2026-07-28: the discovery page says the client "should choose one of these for subsequent requests" [1], which carry the version in `_meta`, where only 2026-07-28 is served. `tools/list` carries `resultType`, `ttlMs` (one hour) and `cacheScope: "public"` [3][7], since the list never changes while the server runs and holds nothing about a user. Every result of this era carries the server's name and version in its `_meta`, which servers SHOULD do [9]. `ping` was removed in 2026-07-28, so it is method not found there.
- Before the era is known, `ping` is answered and any other request gets -32600 with every supported revision in `data.supported`, naming both ways to open. The specification leaves this error to the implementation [4]. That includes a request with `clientCapabilities` but no version in `_meta`: it names no era, so the -32602 of a malformed modern request does not apply yet.

In both eras:

- Tools come back in a fixed order [3].
- An unknown method is -32601 [7]. An unknown tool, and `arguments` that are not an object, fail the `CallToolRequest` schema and are a protocol error -32602. Arguments that fail the tool's own schema are a tool error (`isError: true`) whose message names the argument, so the model can fix its call [3][6].
- `notifications/cancelled` stops the tool call it names, which then gets no reply, and a cancellation that names no call in flight is ignored [8]. It never stops a job, which is what `job_cancel` is for.
- A request whose id is already in flight gets -32600 with a `null` id, since an error carrying that id would be taken for the reply to the first request [9]. A line longer than 16 MiB gets -32600 with a `null` id and is skipped, and the session and its jobs go on.

### Tools and results

One tool per command: `run_start`, `record_start`, `replay_start`, `job_status`, `job_cancel`, `compare`, `plan`, `report`, `check`, `doctor` and `divergences`. Each `inputSchema` is a hand-written table rendered as JSON Schema 2020-12 with `additionalProperties: false` [3]. Arguments are checked against the same table (no unknown member, every required one, enumerations, non-empty arrays) and then decoded by typed `ppx_deriving_yojson` decoders.

A tool's JSON is the output of the same `bactrian.app` function and `to_json` that the command line's `--json` prints, so the two cannot drift apart. It goes into `structuredContent` and, serialised, into a text block, as the tools page recommends for older clients [3]. `check` and `report` gained `--json` for this. An error is a tool error whose JSON is what the command prints for that error under `--json`.

Each tool carries all four `ToolAnnotations` hints, whose names and defaults were checked against the schema page: `readOnlyHint` (default false), `destructiveHint` (default true), `idempotentHint` (default false) and `openWorldHint` (default true) [7]. The tools that start agents, which may call providers live, say `openWorldHint: true` and `destructiveHint: false`. The tools that only read say `readOnlyHint: true` and `openWorldHint: false`. `report` replaces its output file, so it says `destructiveHint: true`. `check` runs each scenario's checker commands, which can do anything the user can, so it says `readOnlyHint: false`, `destructiveHint: true` and `openWorldHint: true`, and its description says so. Clients must treat annotations as untrusted hints [3].

### Jobs, not Tasks

`run_start`, `record_start` and `replay_start` start a job and return `{"job_id"}` at once. `job_status` returns its state (`running`, `succeeded`, `failed` or `cancelled`), its progress in runs and, once it has finished, its exit status and either `result` (the command's `--json` output) or `error`. MCP has no sessions in 2026-07-28, and the tools page recommends explicit handles in tool arguments for state that spans calls [3]. Job IDs are 64 random bits. At most four jobs run at once, or the number `serve-mcp --max-jobs` gives, and a fifth is refused with a tool error until one ends, as the tools page asks servers to rate-limit tool invocations [3]. The job table lives in the server process, in its Eio switch. A finished job stays queryable for an hour, and only the newest hundred finished jobs are kept, because a coding agent's session keeps its server for hours and a replay's result holds every divergence. The descriptions of the three tools that start jobs say so, as the tools page asks of a tool that creates state, and asks that a handle have a bounded lifetime [3]. Results stay on disk under `.bactrian/`.

An exception other than cancellation from a job or a tool call, which is a fault of bactrian's, fails that job or call with an internal error and logs its backtrace to standard error. It does not end the server and the other jobs, as it would end a command on the command line.

The Tasks extension (`io.modelcontextprotocol/tasks`) is not used. Its field names beyond the overview are not verified, Codex does not support it, Claude Code's support is not confirmed, and start-and-poll works with every client.

`job_cancel` ends a job as SIGINT ends the command. On the command line a signal handler fires a stop trigger, and in the server each job has a trigger of its own, so cancelling one job touches no signal handler and no other job. Either way the command's fibre is cancelled: agents are killed with their process groups, unfinished cassettes stay partial, an experiment's manifest marks the runs in flight as interrupted, and `job_cancel` returns only once all of that has happened. A cancelled experiment's error holds its `experiment_id`, and `run_start` with `resume` continues it.

### Work that computes

`compare`, `report` and `plan` spend most of their time computing: the bootstrap, the simulated extra runs of an inconclusive gate metric and the power simulation, which can take minutes. Eio schedules fibres cooperatively, and none of that yields, so run inline it would stop the whole server: no message read, so no cancellation and no end of input noticed, and the jobs' model proxies silent towards their agents. That part runs with `Eio.Domain_manager.run` in a domain of its own, and reading the files and writing the report stay in the server's domain.

A systhread (`Eio_unix.run_in_systhread`) was the other choice. OCaml's threads of one domain share that domain's runtime lock, so a computing thread hands the event loop a turn only at each tick of the master lock, about every 50 milliseconds, which slows every job and every reply. A domain runs in parallel and costs about a millisecond to start per call. The work is pure and handed only its own values, so it is safe in another domain.

The simulation calls a check before each simulated experiment. In the server that check yields to the domain's own scheduler, which lets `Domain_manager.run` deliver the caller's cancellation and raises it, so a cancelled `plan` or `compare`, or one in flight when the input closes, stops within one simulated experiment rather than at its end. `plan` also refuses work it could not finish in reasonable time: more than 10,000 scenarios, or scenarios times simulations times `max_repeats` above 100,000,000, the same on the command line.

### Shutdown

When standard input closes, the server cancels the calls still in flight (they get no reply), cancels every running job as `job_cancel` would, waits until they have unwound and exits 0, as a server should exit promptly once its input closes [4][5]. SIGINT and SIGTERM do the same and exit 130.

### Root confinement

Every path argument (an experiment or scenario file, a report's output, an agent's working directory, a run directory) must resolve under the server's working directory. Relative paths start there, symbolic links are followed, and of a path that does not exist yet the longest existing prefix is resolved and the rest may not climb with `..`. A symbolic link that leads nowhere is refused, since a write would follow it. Run and experiment IDs may not be empty, `.` or `..`, or hold a `/`. Anything else is a tool error that names the argument.

What those files name stays inside too. The commands the tools call read and write through the server's working directory as an Eio directory capability rather than the whole filesystem, and the path arguments reach them relative to it. The capability resolves each path one component at a time and refuses an absolute path, `..` past the root and a symbolic link that leads out, at the moment the file is opened. So an experiment's scenario globs and price file, a scenario's golden traces and fixtures, the experiment file a manifest names and a report's output cannot reach elsewhere, and a directory that becomes a link after its argument was checked does not either. Paths that an experiment hands to a process as strings, which no capability covers (`sandbox.allow_write`, an MCP stdio server's `cwd`, a scenario's `fixtures`), are checked with the same rules as the arguments once the experiment is loaded, and one outside the root refuses the run with an error naming the key. An error never quotes a file it could not use: a golden trace line that is not JSON is reported by its number. A report is not written under `.bactrian` or `.git`, whose files bactrian and git own. A manifest written from the command line may name its experiment file by an absolute path, and one under the working directory is read by its path relative to it. The cost is that a symbolic link inside the project with an absolute target, even one inside the project, cannot be followed through the server.

Confinement is not a sandbox. It keeps the files that bactrian itself reads and writes inside the project. What the server runs is governed by its policy.

### Policy

The client is a coding agent that may be prompt-injected, by a web page, an issue or a file in the repository. It can usually write files in the project without asking and call the tools its user has allowed. The server should not turn that into running any program unsandboxed. `record_start` and `replay_start` run the agent command the client names, and an experiment names its own agents, checkers and MCP servers, so the server holds all of that to a policy that the user sets on the command line that registers it, never by a tool argument or an experiment file:

- Every job runs its agent in the operating-system sandbox in `strict` mode. A `sandbox` argument of `auto` or `off` is refused, an experiment's own `sandbox` setting is overridden, and an HTTP-mode agent, which bactrian cannot sandbox, fails to load. `serve-mcp --allow-unsandboxed` lifts this, and jobs then take the mode as the command line does: the argument, else the experiment's, else `auto`. Under `strict` the sandbox can contain the agent the client names: the network is closed except for the run's own proxy port, and writes are confined to the run's directories. Where the sandbox does not work (Linux without unprivileged user namespaces), every job fails until the flag is given.
- Scenario checkers (in `check` and in an experiment's scenarios), MCP servers (`mcp_stdio` and `mcp_http` in `record_start`, an experiment's `mcp`), passing variables of the server's environment to the agent (`env`, an experiment's `agent.env_passthrough`) and `sandbox.allow_write` paths outside the root are refused. Checkers and MCP stdio servers run outside the agent's sandbox (design 5.3), an HTTP MCP server at any URL receives the agent's tool traffic, which would carry what the agent read past the sandbox's closed network, and a passed variable may be a token that decision 0010 keeps out of the agent's reach. `serve-mcp --trust-project` lifts this, and makes the server as powerful as a shell in the project: anything that can write a file there and call a tool can run any program as the user.
- At most `--max-jobs` jobs run at once (4 by default).

Each refusal is a tool error, or a job that fails before it starts anything, whose message names the flag that would allow the action. The instructions of `server/discover` and `initialize` state the policy in force, so a client knows it before it calls a tool. Checkers are a core feature, so most users will add `--trust-project`, which turns that into a visible decision when the server is registered rather than a silent default. Running checkers in the sandbox is planned, after which they would need no flag.

### Money

Live calls need the spend caps (`BACTRIAN_CAP_<PROVIDER>`) in the server's own environment. No tool takes a cap, so no client can raise one. Concurrent jobs, and a terminal running `bactrian record` beside the server, share `.bactrian/ledger.jsonl`: each reservation takes a `lockf` lock on the file and reads what the others appended before it checks the cap, so the cap bounds their total rather than each job's own.

## Consequences

Claude Code connects in 2026-07-28, and Cursor, or any client that opens with `initialize`, in the legacy era, on the same binary.

A client polls a job. It gets no progress notification and no Tasks handle, and a job does not outlive the server. What a job wrote stays on disk, and a cancelled experiment resumes from its manifest.

A call in flight when the input closes gets no reply, because the client has said it is done.

Serving over Streamable HTTP and the Tasks extension are left for later.

## Sources

Checked on 2026-10-13, and again for the protocol details above on the same day.

1. Discovery, 2026-07-28: <https://modelcontextprotocol.io/specification/2026-07-28/server/discover>. `server/discover` is mandatory for servers, its result fields, and serverInfo under `_meta`.
2. Versioning and compatibility, 2026-07-28: <https://modelcontextprotocol.io/specification/2026-07-28/basic/versioning>. The version in every request's `_meta`, `UnsupportedProtocolVersionError`, and how a dual-era server selects its behaviour from how the client opens.
3. Tools, 2026-07-28: <https://modelcontextprotocol.io/specification/2026-07-28/server/tools>. Deterministic order, `ttlMs` and `cacheScope` on `tools/list`, `inputSchema` (2020-12 by default, `additionalProperties: false`), structured content with a text copy, protocol errors against tool execution errors, annotations as untrusted hints, and handles for stateful tools.
4. stdio transport, 2026-07-28: <https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/stdio>. Newline-delimited messages, only MCP messages on stdout, no server requests on stdout, stderr for logs, cancellation by notification, shutdown by closing standard input, implementation-defined errors before `initialize`, and a dual-era client's fallback to `initialize` after any error other than a recognised modern one.
5. Lifecycle, 2025-11-25: <https://modelcontextprotocol.io/specification/2025-11-25/basic/lifecycle>. `initialize`, version negotiation, `notifications/initialized` and stdio shutdown.
6. Tools, 2025-11-25: <https://modelcontextprotocol.io/specification/2025-11-25/server/tools>. The same tool, result and error shapes for the legacy era, with `structuredContent` an object.
7. Schema reference, 2026-07-28: <https://modelcontextprotocol.io/specification/2026-07-28/schema>. `ToolAnnotations` and their defaults, `ListToolsResult` and `DiscoverResult` with `ttlMs` and `cacheScope` (`"public"` or `"private"`), `RequestMetaObject`, `UnsupportedProtocolVersionError` (-32022) and the JSON-RPC error codes.
8. Cancellation, 2026-07-28: <https://modelcontextprotocol.io/specification/2026-07-28/basic/patterns/cancellation>. A cancelled request gets no response, and unknown or finished requests are ignored.
9. Overview, 2026-07-28: <https://modelcontextprotocol.io/specification/2026-07-28/basic/index>. Request ids unique among those in flight, an error response's id, the required `_meta` fields and the -32602 for a request without them, and `io.modelcontextprotocol/serverInfo` in every result's `_meta`.
