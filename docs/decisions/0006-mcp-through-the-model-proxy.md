# 0006: MCP traffic goes through the model proxy

Date: 2026-10-13

## Context

bactrian records and replays an agent's Model Context Protocol (MCP) traffic for stdio servers (through the `bactrian mcp-shim` bridge) and for Streamable HTTP servers. Each MCP request with its response becomes a cassette exchange on the `mcp` channel. A run's model and MCP exchanges share one cassette, one sequence of `seq` numbers, one redaction state (so a secret gets the same placeholder in both), one set of `normalise` patterns and, in replay, one matcher state, whose `Unconsumed` findings cover both channels at the end of the run.

The model proxy already owns all of that per run: the loopback listener, the run registry keyed by the run token, the token check, the cassette writer, the redaction state and the matcher. A second listener for MCP would need its own copy of the registry and a way to share the rest across two servers.

## Decision

MCP shares the model proxy's listener and run registry. Two more routes sit beside the provider routes:

- `/r/<token>/mcp/<name>`: one JSON-RPC message per POST from the shim. The response body is the JSON-RPC reply, or empty with status 202 for a notification.
- `/r/<token>/mcp-http/<name>/...`: Streamable HTTP, forwarded to the server's real URL when recording or passing through and answered from the recording when replaying.

The 128-bit token in the path is the only credential on these routes, as it is for the model routes. The MCP logic lives in `Mcp_proxy`. The model proxy only routes, and hands `Mcp_proxy` a record of functions over the run's shared state (redaction, masking, occurrence counts, the matcher, the cassette writer and the run's error list) when the run first uses MCP. `Model_proxy.run` gains the run's MCP servers and their policies.

bactrian, not the shim, starts each real stdio server on its first use in a run, in its own process group and with only the environment the experiment declares (plus `PATH`, `HOME` and `LANG`), and kills the group when the run is unregistered. Tool servers are part of the world the agent acts on, so they live outside the agent's process group and, later, outside its sandbox.

A server's policy combines with the run's mode. `record` follows the run: live and recorded in a recording run, served from the recording in a replay, live and unrecorded in a passthrough run. `replay` is served from the recording, and a run without one refuses it. `passthrough` is always live and never recorded. `block` refuses every request: JSON-RPC error -32601 on the stdio route, HTTP 403 on the HTTP route.

## Consequences

One port and one token per run cover models and tools, so the sandbox has one loopback port to allow. Model and MCP exchanges interleave in one cassette in arrival order. `findings.exchanges`, `transport_errors` and `errors` count both channels.

A miss on the MCP channel never switches a run to live, because the matcher only does that on the model channel.

Server-to-client JSON-RPC requests from a legacy stdio server (sampling, roots) cannot reach the agent through the shim, whose transport is one POST per client message, and are dropped. 2026-07-28 servers must not send them. Notifications are forwarded when recording and dropped when replaying. They are not written to the cassette, because they have no response to replay and would otherwise be reported as unconsumed.

A run's `meta.json` lists each MCP server's name, transport (`stdio` or `http`) and policy, and never its command, environment or URL, which may hold secrets. A replay needs the transport only to tell the agent which servers exist (`BACTRIAN_MCP_URL`, and `BACTRIAN_MCP_HTTP_<NAME>` for an HTTP one), so `bactrian replay` reads the list from the recording and serves every server from it, with no flags. `bactrian record` declares its servers with `--mcp-stdio`, `--mcp-http` and `--mcp-policy`, checked as an experiment's are.
