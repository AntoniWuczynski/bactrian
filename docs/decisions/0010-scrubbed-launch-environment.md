# 0010: A scrubbed launch environment

Date: 2026-10-13

## Context

bactrian substitutes the real provider key for the agent's placeholder (section 5.3), so it holds the keys in its own environment. On macOS, the `sysctl` for a process's arguments (`PROCARGS2`), given a process ID, returns the arguments and environment that process was started with, for any process of the same user whose binary is not an Apple platform binary. Seatbelt does not govern this call: denying `sysctl-read` for the process-information names, or denying `process-info*`, leaves it working. A sandboxed agent could therefore read the launch environment of the bactrian process that started it, keys included.

That call reads the strings area set up at `exec`, not the live `environ`. A process started with `LAUNCH_SECRET` that then calls `putenv` for `LATER_SECRET` shows the first and not the second. On Linux `/proc/PID/environ` behaves the same way.

## Decision

`record`, `replay` and `run`, the commands that start agents, MCP servers and checkers, begin by re-executing bactrian when the launch environment holds any variable outside an allowlist: `PATH`, `HOME`, `USER`, `LOGNAME`, `SHELL`, `LANG`, `LC_*`, `TERM`, `TZ` and `TMPDIR`.

- bactrian creates a new temporary file under a random name in `TMPDIR` (mode 0600, `O_EXCL`), unlinks it, writes a random nonce and then the full environment into it as NUL-terminated entries and rewinds it. The descriptor stays open across `exec`, and is moved above 2 if bactrian was started with standard input, output or error closed.
- It then calls `execve` on its own executable, resolved the way the sandbox resolves it (absolute, symbolic links followed), with the same arguments and an environment of the allowlisted variables plus `BACTRIAN_ENV_FD=<descriptor>:<nonce>`. The process is replaced, not spawned, so its process ID, parent, process group, signals and exit status are unchanged.
- The new image, before parsing its arguments, reads the environment from the descriptor, closes it, sets each variable with `Unix.putenv` and sets `BACTRIAN_ENV_FD` to the empty string. It never re-executes again, so there is no loop. It reads only a regular file above descriptor 2 that starts with the nonce, so a `BACTRIAN_ENV_FD` left in a user's environment can neither block the start on a terminal or pipe nor be taken for an environment. Any other marker is an error with exit 70, as is a failure to write the file or to `exec`.
- Every platform does this, because `/proc/PID/environ` on Linux is the launch environment too.

Other commands (`compare`, `report`, `plan`, `check`, `mcp-shim`, `__sandbox-init`, `__sandbox-probe`) start no agent and are not re-executed. `check` runs checkers, which get an explicit environment (below).

Why a temporary file and not a pipe: the reader is the image that `exec` starts, so the writer is the same process before its `exec`. A pipe holds 16 KiB by default on macOS and 64 KiB on Linux, and a larger environment would block the write forever. A helper process that writes the pipe would be a fork of bactrian that carries the original strings area, keys included, for as long as it lives, and would need reaping. `memfd_create` exists only on Linux and needs a C stub. The file is unlinked before a byte is written, so no path ever leads to its contents. Its data blocks are freed when the descriptor closes, which happens before any child is started.

The restore puts the keys back into bactrian's live environment, so a child that inherited it would carry them in its own launch environment, readable in the same way if its binary is not an Apple platform binary (a Node MCP server, for instance). No child inherits it. What each one gets:

| Child | Environment |
| --- | --- |
| Agent, and the `sandbox-exec` or `bwrap` that wraps it | built explicitly: `PATH`, `LANG`, `TERM`, `--env` names, the variant's variables, bactrian's own variables with placeholder keys, `HOME` and `TMPDIR` in the run, and the clock's preload variables |
| Linux forwarder (`bactrian __sandbox-init`) | the agent's, from `bwrap`. It drops its log variable before starting the agent |
| MCP stdio server | `PATH`, `HOME`, `LANG` and the server's own `env` from the experiment |
| Checker | `PATH`, `HOME`, `LANG`, `TZ` and `BACTRIAN_RUN_ID` |
| Sandbox probe (`bactrian __sandbox-probe`) | `PATH`, and `HOME` and `TMPDIR` in the probe's directory |
| `bwrap --version`, the end-marker `sandbox-exec` and `touch`, `log show` | `PATH` only |

A provider key reaches a child only when the experiment names it for that child, in a variant's variables, an MCP server's `env` or `--env`. Descriptors follow the same rule: after the restore, bactrian marks every open descriptor above 2 close-on-exec, so one it inherited (a shell's `exec 5<file`, a CI runner's) reaches no child, and each child gets only the descriptors it is given.

## Consequences

Variables outside the allowlist, `OCAMLRUNPARAM` among them, are not seen by the OCaml runtime of the re-executed image at start, because the runtime reads them before bactrian restores the environment. They are restored for bactrian's own code.

Residual risk. Every other process of the same user whose binary is not an Apple platform binary can still be read the same way, and many hold keys in their launch environment: a shell installed from Homebrew, `tmux`, `direnv`, or `uv run --env-file` that started bactrian. The sandbox cannot hide them. Where an agent is hostile, run bactrian as a dedicated user that holds no other processes, or in a virtual machine. On Linux the agent has its own process namespace and sees none of these.

Other agents are such processes too, and their launch environments hold their runs' tokens. On macOS each sandboxed run therefore gets a model-proxy port of its own that answers only its token, and its profile allows no other port, so a token read from a concurrent run cannot be used. An MCP stdio server's launch environment holds the `env` the experiment gives it, and its command line is readable the same way: give such a server no credential you would not give the agent, or run bactrian as a separate user or in a virtual machine. bactrian warns once when the sandbox is Seatbelt and a stdio server it will start has arguments or `env`.
