# 0013: Checkers in the sandbox

Date: 2026-10-13

## Context

A scenario's checkers are commands that the project names. They ran as the user with no sandbox, so whoever could write a scenario file could run any program by adding a checker to it. A coding agent that drives bactrian can write files in the project and may be prompt-injected. A checker only judges a trace. It reads the trace on standard input, perhaps a few files of its own, and prints a verdict. It needs no network and nowhere to write but scratch space.

## Decision

Checkers run inside the operating-system sandbox of section 5.3, under a policy of their own. `Sandbox_policy` writes it as one more shape of the agent's input: no run state directory, no listener and no `allow_write`. `Sandbox.checker` wraps the command, and `Checker_runner` keeps its process group, timeout and output limits.

- Network: none. On macOS the profile has no `network-outbound` rule and never allowed `network-bind`, so a connection to any address, loopback included, fails with `EPERM`. On Linux bubblewrap removes the network namespace as it does for the agent, but no Unix socket is mounted and the forwarder (`bactrian __sandbox-init`, kept so that the process tree is the agent's) has nothing to relay. A public address fails with `ENETUNREACH` and a loopback port with `ECONNREFUSED`.
- Writes: only a fresh temporary directory per checker, which is its `HOME` and `TMPDIR` and is removed when it ends. Not the scenario's directory, not `.bactrian`. `sandbox.allow_write` is the agent's and does not apply.
- Reads: the agent's deny-list (credential paths, the `.env` files at the project root, `.bactrian/ca`), the experiment's `deny_read`, and all of `.bactrian/runs`, the checker's own run included, because the trace arrives on standard input.
- Working directory: the scenario file's, as before. Environment: as before (decision 0010), except `HOME` and `TMPDIR`.

The temporary directory is `HOME` and `TMPDIR` in every mode, not only inside the sandbox, so a checker behaves the same on a host where `auto` falls back to running it unsandboxed. A checker that relied on the user's home directory, for a package installed there or a configuration file, has to find it in the project instead.

**Mode.** Checkers follow the sandbox mode resolved for the experiment (`--sandbox`, else the experiment's `sandbox`), and `check` takes `--sandbox` with `auto` by default. The preflight probe that decides for the agent decides for the checkers. Their policy is the agent's with fewer allowances, so a sandbox that passes the probe can enforce it, and a second probe would test nothing new. Two cases run the probe for the checkers alone. `check` starts no agent, so it probes once when a scenario has checkers, and not at all otherwise, so a scenario without checkers never depends on the host. An experiment with an HTTP-mode agent has no probe of its own (decision 0008), so it probes when a scenario has checkers. The warning under `auto` now says "running unsandboxed" rather than "runs are unsandboxed", since it serves both.

**Labels.** A run's `verdicts.json` carries `checker_sandbox: {mode, applied, reason?}` when its scenario has checkers, and `check --json` carries `sandbox` once at the top level, `null` when no scenario has a checker. The text output of `check` flags unsandboxed checkers as `record` flags an unsandboxed agent.

**Violations** are not collected for checkers. A denial shows as the checker's own error, so the run is `errored` with the checker's last line of standard error, which says more about what it tried than a log line would. On macOS the denials still carry a tag (`bactrian-sandbox:checker`) in the system log.

**Launch environment.** On macOS a sandboxed checker can read bactrian's launch environment as an agent can, so `check` now scrubs its own as `record`, `replay` and `run` do (decision 0010).

## Consequences

A checker that needs the network, for example one that asks a language model, or one that writes beside the scenario, fails in the sandbox. It has to run with `sandbox: off`, which the labels then show.

As for the agent, a checker on Linux sees an empty `/tmp`, so the checker of a scenario that lies under the system's temporary directory cannot read its own files.

Reads stay broad, so a checker can read files of the project and put them in its verdict's message, which reaches `verdicts.json` and the terminal. That is the agent's residual risk too, and the closed network keeps it from going anywhere else.
