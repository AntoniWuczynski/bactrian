# 0016: Real provider keys in the agent's environment

Date: 2026-10-30

## Context

An agent gets the placeholder `bactrian-<run-token>` in each provider key variable, and the model proxy swaps in the real key on the way upstream (section 5.3). Most SDKs accept any string. Some validate the key's format before they call (a prefix such as `sk-`, a length, a character set), and those agents fail before they make a request. Section 5.2 planned an opt-out, `agent.real_keys: true`, to be labelled in reports. Giving the agent the key undoes what the placeholder was for, so this record says what is placed where and what refuses it.

## Decision

`agent.real_keys` is a boolean in the experiment's `agent` block, `false` by default. When it is `true`, the variable of each provider that bactrian holds a key for (`Run.keys_from_environment`) carries that key instead of the placeholder, in both modes: in the process environment of a command agent, and in the `env` of the `start` message of an HTTP-mode agent. A provider without a key keeps the placeholder, so a replay without keys still works. The base URLs are unchanged, so every call still goes to the model proxy by way of the run's token, and the agent's variables, the proxy's check and the recording stay as they were. The proxy accepts the real key as the agent's credential as well as the placeholder, for that run only, in the provider's key header and in Gemini's `key` query parameter. Any other value is refused as before.

**What stays true.** The key never reaches the cassette: the request goes through the redaction that already scrubs the run's known keys, and the proxy never forwards the agent's own header. The `env` of a `start` message is not written to the trace. A variant cannot set a key variable either way (`reserved_env`). The launch environment stays scrubbed (decision 0010).

**What is lost.** An agent that holds the key can read it, write it into a reply, an event or its standard error, and so into `trace.jsonl` and `agent.stderr` (bactrian does not rewrite what the agent says). Without a sandbox it can send the key anywhere. Inside one it can connect only to the proxy, but the proxy forwards what the agent sends. A key written into a prompt reaches the provider, and one written into a tool call to an HTTP MCP server (`mcp_http`) reaches that server. bactrian scrubs the run's keys from what it records, not from what it forwards. Scrubbing the forwarded bodies would only catch the key as written, so an agent that encodes it would get past it, and the option exists for agents that are trusted with the key.

The sandbox also does not keep other processes of the user from reading the key:

- On macOS under Seatbelt, another process of the same user that is not an Apple binary, such as the agent of a concurrent run of another experiment, can read it from the agent's launch environment (decision 0010 explains the call).
- On Linux, bubblewrap's own environment and the agent's `/proc/PID/environ` are readable by a process of the same user outside the sandbox. In the same experiment that is an MCP stdio server, and a checker when `auto` fell back to no sandbox.
- In the virtual machine, the environment is written to `agent.env` in the run's directory (mode 0600) and given to the runtime with `--env-file`. The file is removed when the run ends, but a bactrian that is killed leaves it on disk. The container's configuration holds the key too: `docker inspect` shows it, and Apple's `container` keeps it under `~/Library/Application Support/com.apple.container` while the container exists (decision 0015). Any process of the user can read both. Refusing `real_keys` with the virtual machine was considered, and dropped because the agent that needs the option needs it there as much, and the exposure is to the same user's processes, as with the other backends.

**Refusals and warnings.**

- `sandbox: strict` with `real_keys: true` is a configuration error, located at `agent.real_keys`, and so is `--sandbox strict` over an experiment that sets it. `strict` is the mode that promises the whole sandbox contract, secrets included (the same reasoning refuses it for an HTTP-mode agent, decision 0008), and a user who wants the key in the agent's environment can say `auto` or `off`. A `strict` run that the sandbox cannot serve already stops, so no one finds out later that the key was in a process that was not sandboxed.
- `auto` and `off` run, with a warning once per experiment (also on the terminal of `run`) that names the key variables and says what the run's sandbox leaves open: nothing stops an unsandboxed agent (every HTTP-mode agent is one), a sandboxed agent can still send the key out through what the proxy forwards, and in the virtual machine the key is also in `agent.env` and the container's configuration.
- An HTTP-mode agent at an `http` URL that is neither `https` nor on this machine (`Live_preflight.is_cleartext`, the rule for `BACTRIAN_UPSTREAM_*` in decision 0012) gets a second warning, because the `start` message, with the keys in its `env`, crosses the network unencrypted. It is a warning and not a refusal, as it is for an upstream, since the network may be one the user trusts.
- `doctor --experiment` has a `real-keys` line for an experiment that sets the option: `warn` naming the key variables that are set in `doctor`'s environment and, for the case above, the plain-HTTP risk, and `skip` when no key is set, since the agent would then get placeholders.
- The MCP server needs `--trust-project` for an experiment that sets `real_keys`, as it does for `agent.env_passthrough`, the other way an experiment hands the agent the server's own environment. Its jobs run `strict` unless `--allow-unsandboxed`, so over MCP an experiment needs both flags, and the refusal names the one that is missing.
- There is no switch on `record` and `replay`. Only an experiment can set it.

**Labels.** Each run's `meta.json` has `real_keys: true|false`. `compare`, `report` and the JSON of both read it back: a warning counts the runs that had the key ("N of M runs had the real provider keys in the agent's environment"), each run has `real_keys` and the header has `real_keys_runs`. A run from before the field has none and counts as `false`. The trace and cassette headers do not carry it, because a cassette is shared and the label is about the run that made it. A replay does not need the key and is labelled by its own configuration.

## Consequences

An agent that insists on a key format is usable without touching the sandbox contract of any other experiment. Using the option costs the guarantee that a prompt-injected agent cannot read the key, and the report says so for every run that used it.
