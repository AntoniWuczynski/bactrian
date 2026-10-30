# 0018: HTTPS egress goes through the model proxy's listener

Date: 2026-10-30

## Context

An agent's tools call HTTPS services of their own, and a replay is only deterministic if those calls are recorded and served back too. The usual way to see inside them is a proxy that the client reaches with `CONNECT` and that terminates TLS with a certificate the client trusts. The clients that matter find the proxy in `HTTPS_PROXY`. Node's `fetch` does so only with `NODE_USE_ENV_PROXY=1`, which Node 24.0 and 22.21 added (Node's `cli.json`, "NODE_USE_ENV_PROXY"). httpx and requests read it by default (the httpx environment variables page and the requests advanced guide). Checked here with Node 24.3, httpx 0.28.1 and requests 2.34.2 against a bare listener: each sends `CONNECT host:port` with `Proxy-Authorization: Basic` taken from the proxy URL before any challenge, for loopback hosts too.

Two constraints shape where the proxy lives. The sandbox lets an agent reach one loopback port: a listener of the model proxy that answers only the run's token under Seatbelt, the same port relayed by the bubblewrap forwarder and by the virtual machine's relay (section 5.3). A second listener would need a second allowed port in all three, the relay in C included. And a run's egress exchanges belong in the run's one cassette, with its sequence numbers, redaction, `normalise` masks and matcher, which the model proxy already owns per run, as it does for MCP (decision 0006).

tls-eio has no Server Name Indication callback. `Tls.Config.server` takes `` `Single``, `` `Multiple`` or `` `Multiple_default`` certificates fixed when the configuration is made (`tls/config.mli`), so the certificate must be known before the handshake.

## Decision

The model proxy's listeners also take `CONNECT`. cohttp-eio parses it as any other request (it has no body, since `Http.Method.body_allowed` is false for `CONNECT`), and an expert response writes `HTTP/1.1 200 OK` with no framing header and then hands over the connection's buffered reader and writer (`cohttp-eio/server.ml`, `make_expert`). Bytes the agent sent after the head, a ClientHello in the same write, are still in that reader. `Egress_proxy` wraps the two as a flow, `Tls_eio.server_of_flow` runs the handshake on it with a fresh `` `Single`` configuration whose leaf names the `CONNECT` host, and `Cohttp_eio.Server.callback` serves the HTTP/1.1 requests inside. When the tunnel ends the handler raises, so the listener closes the connection rather than read HTTP from it again.

- The agent gets `HTTPS_PROXY` and `https_proxy` as `http://<run-id>:<run-token>@127.0.0.1:<port>`, the same port as its base URLs, and `NODE_USE_ENV_PROXY=1`. No `HTTP_PROXY` is set, so the plain-HTTP base URLs never go through the proxy and need no `NO_PROXY`.
- The run ID is the user name and the run token the password. With no `Proxy-Authorization` the answer is 407 with `Proxy-Authenticate: Basic`, so a client that sends credentials only when challenged still works. A token that is not live, the token of another run, or a run that does not intercept egress gets 403. A listener of one run answers only that run, as for model calls. None of these reaches TLS or mints a certificate.
- The host's policy is decided at `CONNECT`, from `host:port` and then `host`. `block` (the default) is a 403 before any TLS work, logged as an HTTP call of the run. `record`, `replay` and `passthrough` behave as they do for MCP servers.
- Exchanges go on the cassette's `http` channel, keyed on method, path and query plus a canonical request of the authority, method, path, query, `content-type` and body (canonical JSON, or a BLAKE2b-256 hash of other bytes). Bodies are read and passed on whole.
- The agent trusts a bundle of the system's root certificates followed by the project CA, named by `SSL_CERT_FILE`, `NODE_EXTRA_CA_CERTS` and `REQUESTS_CA_BUNDLE` and written to the run's temporary directory. The system roots are in it because `SSL_CERT_FILE` and `REQUESTS_CA_BUNDLE` replace a client's trust rather than add to it. A bundle of the CA alone would be stricter, since a client that skipped the proxy could then verify no real server, but it would also break every HTTPS call an unsandboxed agent makes outside the proxy on purpose. Every host reached through the proxy gets the CA's leaf either way.
- Leaf certificates are valid from 2 days before the earlier of the real time and the run's `sim_now` to 7 days after the later, and the CA from the Unix epoch on. Under `libfaketime` the agent checks the chain at `sim_now` (`FAKETIME=@<sim_now>`, decision 0009), which is a scenario's fixed `now` or, in a replay, the recording's, so leaves dated from the real time alone would fail every handshake of a replay more than 2 days old. One window covering both times was preferred to a leaf per clock, which would have to know which clock each agent reads (a static binary or a Go program ignores `libfaketime`). A `sim_now` after the CA expires still fails under `libfaketime`.
- An HTTP-mode agent is a server bactrian does not start, so it gets neither the proxy nor the bundle, and its runs list `egress` under `unenforced`.

## Consequences

The sandbox profiles, the forwarder and the relay are unchanged, and still let the agent reach exactly one port. One token covers models, tools and egress.

Blocked attempts, replay misses and passthrough calls are not exchanges, so the proxy keeps a list of every call of the run, and the trace gets an `http_call` for each, placed in its turn by time. A blocked attempt has method `CONNECT` and status 403. `no_egress` is checked against these, and counts an attempt that was refused as a call.

Not intercepted: plain `http://` egress, WebSockets (an `Upgrade` inside a tunnel gets 501), HTTP/2-only and gRPC clients (ALPN offers `http/1.1` only), and clients that ignore the proxy variables or bring their own roots. Under the sandbox these fail at the operating system. Without it they reach the network unrecorded. Streams inside a tunnel reach the agent whole, and only the response headers the model proxy stores are passed on.

Package managers honour `HTTPS_PROXY` as well, so a `uv run` or `npx` that downloads at start is refused unless its index is listed.
