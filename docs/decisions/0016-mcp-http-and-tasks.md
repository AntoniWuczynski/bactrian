# 0016: Streamable HTTP serving and the Tasks extension for serve-mcp

Date: 2026-10-30

## Context

Decision 0011 put bactrian's commands behind a Model Context Protocol (MCP) server over stdio and left two things for later: serving over Streamable HTTP, and the Tasks extension (`io.modelcontextprotocol/tasks`) for the commands that take minutes. A client that does not start its servers as subprocesses, or one in a container or a browser, needs HTTP. A client that speaks Tasks can follow a long command without knowing bactrian's own job tools.

Streamable HTTP in 2026-07-28 is one endpoint that takes a POST per message and has no sessions [1]. The server must validate `Origin` against DNS rebinding and answer an invalid one with 403, should bind only to localhost when it runs locally, and should authenticate every connection [1]. Every request repeats fields of its body in headers: `MCP-Protocol-Version` its `_meta` protocol version, `Mcp-Method` its method, and `Mcp-Name` the tool of a `tools/call` [1]. A header that is missing, holds invalid characters or differs from the body is rejected with status 400 and the JSON-RPC error -32020 (HeaderMismatch) [1][3]. Authorization is optional in MCP, and an HTTP server that offers it uses `Authorization: Bearer` on every request and 401 for a missing or invalid token [4].

The Tasks extension lets a server answer a `tools/call` with a task handle instead of its result, for a client that declares the extension in that request's capabilities [7][9]. The client polls `tasks/get` until the task is `completed`, `failed` or `cancelled`, and `tasks/cancel` asks the server to stop it [9][10]. On the 2026-10-30 support matrix no client has a mark for Tasks, Claude Code included [8].

## Decision

### Transport

stdio stays the default. `serve-mcp --http PORT` serves Streamable HTTP at `http://127.0.0.1:PORT/mcp` instead, and `--http 0` picks a free port. The server prints the address on standard error, serves until SIGINT or SIGTERM, then cancels its calls and jobs as at the end of input over stdio and exits 130. The policy flags of decision 0011 apply unchanged.

It listens on `127.0.0.1` only, with no option to listen elsewhere. The transport is plain HTTP, since bactrian has no Transport Layer Security (TLS) stack yet, so on any other interface the bearer token would cross the network in the clear. A remote client can reach the server through a tunnel that ends on the machine.

Every request is checked in this order, before its message is read:

1. `Host` must be `127.0.0.1:PORT` or `localhost:PORT`, or the answer is 403. A page that rebinds its own name to `127.0.0.1` still sends that name in `Host`.
2. `Origin` must be absent, `http://127.0.0.1:PORT`, `http://localhost:PORT` or an origin given with `--http-allow-origin`, or the answer is 403 with a JSON-RPC error that has no id [1]. A browser sends `Origin` on every POST, so a page you visit cannot reach the server. This check comes before the token, so a page learns nothing about it.
3. The path must be `/mcp` (404) and the method POST. GET and DELETE, the stream and session ending of earlier revisions, are 405 [1][5], with `Allow: POST`. `OPTIONS` from an allowed origin is a browser's preflight, answered as below.
4. `Authorization: Bearer TOKEN` must hold the server's token, compared in constant time, or the answer is 401 with `WWW-Authenticate: Bearer` [4]. The token is the value of `BACTRIAN_MCP_TOKEN`, which must be at least 32 visible ASCII characters, or 32 random bytes in hex that the server makes and prints once on standard error. A static token, not the OAuth flow of the authorization page, because a local server for one user has no authorisation server to send a client to, and the page leaves authorization optional [4]. The variable reaches no agent: the launch environment is scrubbed as for stdio (decision 0010), and jobs pass no variable without `--trust-project`.
5. `Content-Type` must be `application/json` (415) and the body at most 16 MiB (413), the limit of a stdio line.

The body of a refused request is still read and dropped, up to 64 KiB. Left unread on a kept-alive connection, it would be parsed as the next request, one whose `Host` and `Origin` no check had seen, so a page could hide a request with the right `Host` and no `Origin` inside a refused one. A longer refused body, and a 413, end the connection after the answer instead. The server then stops sending and drops what still arrives for up to two seconds before it closes, since closing with bytes unread resets the connection, which can destroy the answer before the client reads it.

A page from an allowed origin is served as Cross-Origin Resource Sharing (CORS) asks [11]. Its POST carries `Authorization` and JSON, so the browser first sends a preflight `OPTIONS` request, which needs neither token nor JSON. It is answered with 204, `Access-Control-Allow-Methods: POST` and the headers it asks for in `Access-Control-Allow-Headers`, since the `Mcp-Param-*` names vary from tool to tool, cached for ten minutes. Every answer to a request from an allowed origin, refusals included, carries that origin in `Access-Control-Allow-Origin` with `Vary: Origin`, so the page can read it. An origin given with `--http-allow-origin` must be `http` or `https`, `://` and a host with an optional port and no path, as `Origin` carries one, and is compared in lower case. Anything else, such as a trailing slash or `*`, is a usage error rather than an origin that never matches.

Before the token is checked, a local process of any user can connect, so connections are bounded: at most 64 are open at once, a request's head holds at most 64 KiB, and a connection that has no request admitted within 30 seconds of its start, or of the answer to its last admitted request, is closed without an answer. A refusal does not restart that time, and the time bounds what the server writes as well as what it reads, so a client that sends refused requests and never reads the answers is closed at it too.

### Messages and eras

A POST holds one JSON-RPC message, answered on its own. A notification is accepted with 202 and no body [1]. 2026-07-28 defines no client notification over HTTP, since closing the stream is the cancellation [1], so `notifications/cancelled` is accepted and ignored. A response from the client, which it must not send, is 400.

There is no connection, so each request carries its own era, and the server never mints an `Mcp-Session-Id` [1].

- A request whose `_meta` names a protocol version is 2026-07-28. Its headers must match its body: `MCP-Protocol-Version` its version, `Mcp-Method` its method, and `Mcp-Name` the `name` of a `tools/call` or the `taskId` of a task method, as the extension asks [9]. An `Mcp-Name` in the form `=?base64?...?=` is decoded first, since the page allows that form for `Mcp-Name` and `Mcp-Param-*` only, and a value that holds a byte outside visible ASCII, space and tab is invalid [1]. No tool marks a parameter with `x-mcp-header`, so no `Mcp-Param-*` header is recognised, and unrecognised ones are ignored [1]. A failure is -32020 with status 400, carrying the request's id as the page's example does [1].
- `initialize` opens the legacy era as over stdio, and the answer is the same.
- Any other request follows its `MCP-Protocol-Version`: a legacy revision is that revision, 2026-07-28 is the modern era (where the missing `_meta` is -32602), an unknown value is 400, and no header is 2025-03-26, which a server that supports clients before 2025-06-18 may assume [1][5].

Statuses follow the 2026-07-28 page for the modern era: 404 for an unknown method, 400 for an unsupported version, a header mismatch, missing `_meta` fields and a missing capability [1][3], 500 for an internal error, and 400 for any other error, so that a dual-era client reads a recognised modern error in a 400 and does not fall back [1]. In the legacy era only a malformed request (-32700, -32600) is 400, and other JSON-RPC errors come with 200, which the client reads as JSON-RPC.

A `tools/call` is answered with an event stream (`text/event-stream`, `X-Accel-Buffering: no`) when the request's `Accept` lists it, and with JSON otherwise [1]. The head goes out at once. When the client closes the connection before the answer, the call is cancelled and nothing more is sent, as the page requires of a closed stream [1]. Over JSON the same holds, since the answer has nowhere to go. A connection that sends more bytes, the next request on a kept-alive connection, does not count as closed. Other requests are answered with JSON.

### Tasks

The Tasks extension is offered in 2026-07-28 only, where `server/discover` lists it under `capabilities.extensions` and a client declares it in each request [6][7][9]. The 2025-11-25 revision had a tasks utility of its own with other shapes, which bactrian does not implement, so a legacy client keeps the job tools and `tasks/get` is -32601 there.

A `tools/call` of `run_start`, `record_start` or `replay_start` whose request declares the extension starts the job as before and answers with a `CreateTaskResult` (`resultType: "task"`) [9]. Any other tool, or a request without the extension, answers as before [9]. The job is in the table before the answer goes out, so `tasks/get` finds it at once, as the extension requires [9].

The task ID is the job ID, so `job_status` and `job_cancel` work on a task too. Job IDs grew from 64 to 128 random bits, because the extension asks that a task ID cannot be guessed [9]. A job becomes a task like this [9][10]:

- running is `working`, with the runs finished so far in `statusMessage`.
- A command that produced its result is `completed`, and `result` is the tool result the call would have given: the command's `--json` output in `structuredContent` and a text block, with `isError: false`.
- A command that stopped with an error is `completed` too, with `isError: true` and the error as `--json` prints it, because `failed` is only for a JSON-RPC error during execution, and a tool's errors are results [9]. No task is ever `failed`: a fault of bactrian's in a job is already a tool error (decision 0011).
- A cancelled job is `cancelled`, with the error's message, which for an experiment names the ID to resume, in `statusMessage`.

`createdAt` is when the job started, and `lastUpdatedAt` when it started, when it ended, or when a poll first saw that another of its runs had finished, both in UTC to the second. The runs are counted only when a client asks, so a finished run moves `lastUpdatedAt` at the next poll, not at the moment it finished. `ttlMs` is `null` while the job runs, since a running job is never dropped, and once it has ended the time from its start until the table forgets it, an hour after it ended (decision 0011). The value may change over the life of a task [9]. `pollIntervalMs` is 1000.

`tasks/get` gives the task with `resultType: "complete"`, and the `result` once completed. `tasks/cancel` asks the job to stop and acknowledges at once with an empty result, which the extension allows, cancellation being cooperative [9]. The job then unwinds as on `job_cancel`. `tasks/update` is acknowledged and changes nothing, because no job asks for input. An unknown or forgotten task is -32602 for all three, and a request that does not declare the extension is -32021 with the extension in `data.requiredCapabilities` [9][3]. Task status notifications through `subscriptions/listen` are optional and not offered, so clients poll [9].

## Consequences

A client that cannot start a subprocess, or runs elsewhere on the machine, can drive bactrian over HTTP with the token, and a page in a browser cannot unless the user allowed its origin and it has the token. The token in `BACTRIAN_MCP_TOKEN` or on the terminal is the only secret: whoever has it can call every tool the policy allows, as a client over stdio can.

Until bactrian speaks TLS, nothing on another machine can reach the listener.

A client that speaks Tasks follows long commands through the protocol, and every other client keeps the job tools, which stay. A task does not outlive the server, as a job does not.

Calls in flight are tied to their connections over HTTP, while jobs and tasks belong to the server.

## Sources

Checked on 2026-10-30, from the raw pages of modelcontextprotocol.io and the ext-tasks repository at its main branch of 2026-10-20.

1. Streamable HTTP, 2026-07-28: <https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http>. One POST endpoint without sessions, Origin validation and 403, localhost binding, authentication, 202 for a notification, JSON or SSE answers and `X-Accel-Buffering`, cancellation by closing the stream, the `MCP-Protocol-Version`, `Mcp-Method` and `Mcp-Name` headers with their Base64 form, `x-mcp-header`, -32020 with 400, 404 for an unknown method, the missing-header fallback to 2025-03-26, and 405 for GET and DELETE.
2. Versioning, 2026-07-28: <https://modelcontextprotocol.io/specification/2026-07-28/basic/versioning>. Extension negotiation in `capabilities.extensions`, with the Tasks extension as its example.
3. Overview, 2026-07-28: <https://modelcontextprotocol.io/specification/2026-07-28/basic/index>. The error codes -32020, -32021 and -32022, and 400 for a request without its required `_meta` fields or a capability it needs.
4. Authorization, 2026-07-28: <https://modelcontextprotocol.io/specification/2026-07-28/basic/authorization>. Authorization is optional, the bearer token on every request, never in the query string, and 401 for an invalid one.
5. Transports, 2025-11-25: <https://modelcontextprotocol.io/specification/2025-11-25/basic/transports>. The legacy Streamable HTTP: an optional session, 405 for a GET stream the server does not offer, and the `MCP-Protocol-Version` header with 2025-03-26 when it is absent.
6. Extensions overview: <https://modelcontextprotocol.io/extensions/overview>. Negotiation per request in 2026-07-28 and in `initialize` before it.
7. Tasks overview: <https://modelcontextprotocol.io/extensions/tasks/overview>. Server-directed tasks for a client that declares the extension.
8. Extension support matrix: <https://modelcontextprotocol.io/extensions/client-matrix>. No client marked for Tasks.
9. Tasks specification, 2026-07-28: <https://github.com/modelcontextprotocol/ext-tasks/blob/main/specification/2026-07-28/tasks.md>. `CreateTaskResult`, the task fields and states, `completed` for a tool error and `failed` only for a JSON-RPC one, durable creation, `tasks/get`, `tasks/update` and `tasks/cancel` with their acknowledgements, -32602 and -32021, `Mcp-Name` as the task ID over HTTP, and unguessable task IDs.
10. Tasks schema, 2026-07-28: <https://github.com/modelcontextprotocol/ext-tasks/blob/main/schema/2026-07-28/schema.ts>. The TypeScript types of the above.
11. Cross-Origin Resource Sharing, MDN: <https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/CORS>. A JSON `Content-Type` or an `Authorization` header makes a request preflighted, the preflight answer's `Access-Control-Allow-Methods`, `-Allow-Headers` and `-Max-Age`, `Access-Control-Allow-Origin` on the answer a page reads, and `Vary: Origin` when that origin comes from an allowlist.
