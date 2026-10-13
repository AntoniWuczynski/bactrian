# 0007: The macOS Seatbelt profile

Date: 2026-10-13

## Context

On macOS the sandbox of section 5.3 is a Seatbelt profile run with `sandbox-exec` (deprecated by Apple, still working on macOS 26.6). `Sandbox_policy` writes the profile. The profile starts from `(deny default)`, so every rule below was added because something an agent does fails without it. Node 24 and Python 3.14 were run under candidate profiles doing what agents do: read their standard library, import modules, spawn children, write to the state and temporary directories, make HTTP requests to a loopback port, and connect to a Unix socket. Each rule was then removed again to see what broke.

## Decision

Paths reach the profile only as `(param "NAME")`, and the values are passed as `-D NAME=VALUE`. The profile is given with `-p`, so no file is written. `sandbox-exec` accepts parameter values with quotes, parentheses, backslashes and newlines unchanged. A parameter the profile names but the command line omits makes the profile fail to load, and an empty value is refused ("empty subpath pattern"), so a mistake fails closed. A relative value is accepted and then matches nothing useful, so `Sandbox_policy` rejects relative paths itself.

The rules, and what fails without each:

| Rule | Without it |
| --- | --- |
| `(allow process-exec)` | neither runtime starts |
| `(allow process-fork)` | `subprocess` and `child_process` fail with EPERM |
| `(allow signal (target same-sandbox))` | the agent cannot terminate its own children (EPERM). With the rule it still cannot signal its parent or `launchd` |
| `(allow sysctl-read)` | Node does not start |
| `(allow mach-lookup (global-name "com.apple.system.opendirectoryd.libinfo"))` | `os.userInfo()` in Node fails (`uv_os_get_passwd`). `getpwuid` is the only lookup granted |
| `(allow ipc-posix-sem)` | Python `multiprocessing` pools and `ProcessPoolExecutor` fail with EPERM |
| `(allow file-read*)` | neither runtime starts |
| `(allow file-write* …)` for the state directory, the temporary directory, `allow_write` paths and `/dev/null` | writes fail. `/dev/null` is opened for writing by `subprocess.DEVNULL` and by shells |
| `(allow file-write* (literal "/dev/dtracehelper"))` and `(allow file-ioctl (literal "/dev/dtracehelper"))` | nothing breaks, but every process logs two denials at start, which would bury real violations |
| `(allow network-outbound (remote ip (string-append "localhost:" (param "PORT_n"))))` per listener | connections to bactrian's listeners fail |
| `(allow network-outbound (remote unix-socket (path-literal (param "SOCKET_n"))))` per named socket | connecting to it fails |

Reads are denied with `(deny file-read-data (subpath …))`, not `file-read*`. A Seatbelt rule that comes later overrides an earlier one, which is how the run's own directories are readable again after `.bactrian/runs` is denied. With `file-read*`, Node's `fs.realpathSync` fails on the state directory, because it runs `lstat` on every parent, including `.bactrian/runs`. With `file-read-data` the contents and listings of denied paths are refused, but `stat` still works, so the existence and size of a denied file can be seen.

Deliberately not allowed, checked to fail:

- `network-bind`. The agent cannot connect to a listener of its own anyway, since only bactrian's ports are reachable.
- Every other `mach-lookup`. DNS, `configd`, `logd` and the notification centre are refused. DNS lookups fail (`getaddrinfo` and `dns.lookup`), and the denied lookups are harmless to both runtimes.
- `/dev/tty` and `/dev/ttysNNN`. Node cannot write to a terminal that is its standard output (`file-write-data /dev/ttys016`). Agents must be started with pipes, not a terminal. Allowing the terminal would also allow the `TIOCSTI` keystroke-injection ioctl.

Observed with the final profile:

- `1.1.1.1:443`, UDP to `1.1.1.1:53`, any other loopback port and `127.0.0.2` on a listener's port all fail with EPERM. `localhost`, `127.0.0.1`, `::1` and `0.0.0.0` on a listener's port pass the sandbox.
- A write to `$HOME`, `/tmp` or the project root fails, a read of a deny-listed path fails, and a read of `.env.example` works.
- `log show --predicate 'sender == "Sandbox"'` shows each denial as, for example, `deny(1) network-outbound remote:*:443` or `deny(1) file-write-create /path`. The remote host is shown as `*`. `/bin/ps` could not be executed at all.

## Consequences

Python writes `__pycache__` next to a module that has no cached bytecode, the profile denies that, and each denial shows up as a violation. The agent environment should set `PYTHONDONTWRITEBYTECODE=1` and point `HOME` and the cache variables into the state directory.

Only the `.env` files at the project root are denied, from a fixed list of names, because Seatbelt parameters are literal paths and a pure generator cannot expand a pattern. A `.env` in a subdirectory is readable. The same holds on Linux, where the masks are mounts.

Seatbelt matches resolved paths, so every path given to `Sandbox_policy` must have its symbolic links resolved (`/tmp` is `/private/tmp`, `/var` is `/private/var`).

The profile allows reads of everything not on the deny-list, as section 5.3 requires. What an agent can read, it can send only to bactrian's listeners, which is why the egress policy defaults to `block`.

Tools that need the macOS trust store or the keychain (for example `gh` or `gcloud`) need more `mach-lookup` services and are not covered. They are not Node or Python agents.
