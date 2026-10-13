# 0012: A base URL per provider

Date: 2026-10-13

## Context

The model proxy forwards a live request to the provider's own address, and every built-in address is `https`. This build has no TLS connector, so no experiment could run end to end, and nothing could be tested against a fake provider through the real binary. People also want to point bactrian at OpenAI-compatible servers they run themselves, such as Ollama, vLLM, LM Studio or LiteLLM, which listen on plain `http`. The libraries already took the upstream as a function from provider to URL (`?upstream` in `Run.execute` and `Executor.run_experiment`). Only a setting for a user was missing.

## Decision

`BACTRIAN_UPSTREAM_OPENAI`, `BACTRIAN_UPSTREAM_ANTHROPIC`, `BACTRIAN_UPSTREAM_GEMINI` and `BACTRIAN_UPSTREAM_OPENROUTER` replace a provider's address. An unset or empty variable keeps the built-in address. The four names come from the provider table, and `Upstream_env` in `bactrian.app` is the only code that reads them. `record`, `replay` (when it can go live), `run`, `doctor` and the MCP server's tools all take their upstream from it. The MCP tools read the server's environment at each call.

**Validation.** A value is accepted only as an absolute `http` or `https` URL (the scheme in any case) with a host, an optional port from 1 to 65535 and an optional path prefix. These are refused as a usage error (exit 64) that names the variable and the rule, and never repeats the value, because it may hold a credential:

- a relative URL, a scheme-relative URL, a bare `host:port` or any other scheme;
- user information (`user:password@`), because a URL with a password would reach logs, `meta.json` and `doctor`;
- a query or a fragment, because the proxy appends the request's own path and query to the base;
- no host, a bad port, whitespace or a control character.

The path prefix is joined to the provider path the SDK sends. The SDK's own base path, such as `/v1` for OpenAI, is part of that provider path, so it is left out of the variable: Ollama's `/v1/chat/completions` is reached with `BACTRIAN_UPSTREAM_OPENAI=http://127.0.0.1:11434`. A prefix is for a gateway that mounts a provider under a path.

**Plain HTTP.** An `http` upstream needs no TLS connector and works in this build. The real API key is added to the forwarded request, as for any upstream, so over plain HTTP to another machine it travels unencrypted. `doctor` warns for that case and not for the loopback addresses (`localhost`, `127.*`, `::1`).

**TLS refusal.** The exit 69 refusal ("this build has no TLS support") applies when a provider the run calls has an `https` upstream. A provider is called when its spend cap is above 0, and the message names those providers. An `https` upstream of a provider with no cap, or an `http` upstream, does not trigger it. `doctor`'s `tls` check counts the same way. It is `skip` without a need and `fail` when the experiment needs HTTPS for a provider's upstream or an HTTP-mode agent's URL.

**Labels.** `meta.json` has an `upstreams` object that maps the name of each provider whose upstream is not the built-in one to the URL, which carries no credentials after validation. It is `{}` when none is overridden, and for a strict or fuzzy replay, which calls nobody. The label sits beside `sandbox` and `clock`, so a run against a local server can be told from one against the real provider. Reports and `compare` do not show it yet.

**Keys.** The key variable of a provider is still required when its spend cap is above 0, whatever its upstream, because the proxy and the `doctor` key check have one rule. A local server that needs no key is given any placeholder value. The README says so.

**Prices.** The ledger prices a request from the model named in it. A model with no price is refused before it is sent (a 402 `bactrian_unknown_model` to the agent) and the run is `errored` with "no price for model X". It is never counted as free, so a local model cannot hide spend under a cap or run unmetered by accident. To run one, the user states its price in the experiment's price file, zero rates included. `record` and `replay` use the built-in table only, so they cannot price a local model: use an experiment. The cap must still be above 0, since it is what marks a provider as called live, and a model priced at zero reserves and spends nothing against it.

**Launch environment.** The allowlist of decision 0010 does not keep `BACTRIAN_*` variables: `scrub` drops them from the launch environment and the re-executed image restores them with `putenv` before it parses anything. The upstream variables hold no secret, so nothing is lost by that, and they survive the re-execution. A test pins both facts.

## Consequences

- Experiments run end to end against a fake upstream. The acceptance tests run the real binary and the example agents against a fake provider in its own process, and the MCP client tests drive `run_start`, `job_cancel`, a resume and `compare` through it.
- `Run.overridden_upstreams` compares a function with the built-in addresses, so a library caller that passes `?upstream` gets the same label.
- A gateway that rewrites model names must still be priced under the name the agent sends.
