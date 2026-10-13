# 0008: HTTP-mode agents get their per-run environment in the start message

Date: 2026-10-13

## Context

A command-mode agent is a process that bactrian starts for each run, so bactrian can give it exactly the environment it wants: the provider base URLs with the run token, placeholder API keys, `SIM_NOW`, `TZ`, `BACTRIAN_RUN_ID`, `BACTRIAN_STATE_DIR` and the MCP variables. An HTTP-mode agent is a long-lived server that the user starts. Nothing bactrian does can change its process environment, and several runs can be in flight against it at once, each with its own token and state directory.

## Decision

In HTTP mode the `start` message carries an optional `env` object, a map from variable name to string. It holds the per-run values a command-mode agent gets as environment variables, with the variant's own variables, and nothing else. It is for per-run routing and identity values. What belongs to the agent's own process stays out: `PATH`, `LANG`, `TERM`, the experiment's passthrough list, and the variables bactrian redirects into the run for a command agent so that its files stay inside it (`HOME`, `XDG_CACHE_HOME`, `XDG_CONFIG_HOME`, `XDG_DATA_HOME`, `npm_config_cache`, `TMPDIR`, `PYTHONDONTWRITEBYTECODE`). The server was started by the user with its own home and caches, bactrian does not sandbox it, and pointing a long-lived process at one run's directory would be wrong for the next run. Those variables are built apart from the per-run ones (`Run.home_env`), so the two modes still share one list of the values that matter. The agent must use these values for that run's calls and keep its state per run ID, which every request carries in `X-Bactrian-Run` and in the experiment's namespace header.

Both modes build the values with one function (`Run.run_variables`). A command agent's environment is those values behind the allowlisted process variables, and an HTTP agent's `env` is the values themselves, so the two lists cannot drift apart. The values are the same placeholders in both: the real provider keys are never in the environment and never in `env`. Command-mode `start` messages do not carry `env`, and the field is optional when decoding, so agents written before it existed keep working.

The driver sends one `POST` per message and reads the response body as a JSON array of the agent's messages for that step. It reuses the model proxy's upstream client, which already refuses `https` URLs in a build without the TLS connector, so an HTTP agent at an `https` URL stops the experiment with exit status 69 and the same explanation. A status outside 2xx ends the run as `agent_http_error` with the status and the first 2 KiB of the body. A body that is not a JSON array of valid messages is a protocol violation.

## Consequences

bactrian cannot reset a server it did not start, so its runs cannot be isolated. They are unsandboxed whatever `sandbox` says, and carry the ordinary sandbox label (decision 0007) with `applied` false and the reason `http mode`, so reports and comparisons flag them like any other unsandboxed run. No sandbox preflight is run for the agent of an HTTP-mode experiment, only for its checkers when a scenario has any (decision 0013), and it warns once. `sandbox: strict` in any of its forms, or `--sandbox strict`, is a configuration error with `protocol: http`.

An agent that reads its own process environment instead of `env` would call real providers with its own keys, or none, and bypass interception. bactrian cannot detect that, so the README says it plainly. Isolation between concurrent runs is the agent's job, and the example agents key their sessions by run ID.
