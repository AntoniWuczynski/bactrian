# 0020: Readable paths outside the project

Date: 2026-10-30

## Context

The virtual machine sandbox (decisions 0014 and 0015) shows the container the project and nothing else of the host, so an agent that needs a shared data directory, a vendored library or a model cache outside the project could not read it. Seatbelt and bubblewrap never had that limit, because they read the whole host except the deny-list.

## Decision

`sandbox.allow_read` is a list of paths, relative to the experiment file like `allow_write`, in the experiment, in `Sandbox.settings` and in the policy input. It has no command-line flag, because `allow_write` has none either.

**Virtual machine.** Each path that exists is mounted read-only at its own absolute path, which is how every other mount is made, so the paths in the agent's arguments and environment work unchanged. Docker mounts a file or a directory. `container` mounts directories only, so a file is refused with the same message as an `allow_write` file. A path of `/` is refused, because it would replace the image's whole file system. A path that does not exist is skipped, as for `allow_write`.

Two more kinds of path are refused:

- At or under `/run`, `/var/run`, `/tmp`, `/proc`, `/sys` or `/dev`. A read-only mount does not stop `connect()` on a Unix socket, and the container runs as the host user's user ID. On Linux, a socket that user owns under one of these (the D-Bus session bus at `/run/user/UID/bus`, through which `systemd-run --user` starts host commands, a rootless Docker or Podman socket, an ssh-agent socket in `/tmp`) would give the agent a way to a daemon on the host. This follows from how the kernel treats sockets and was not run on Linux here. The limit that remains is a socket elsewhere inside an allowed tree, which stays reachable: name directories of files, not a directory that holds a socket. `sandbox.allow_write` is not refused under these directories (it predates this record, and writing under `/tmp` is a common need), so an `allow_write` of `/run/user/UID` or of an ssh-agent directory in `/tmp` exposes those sockets the same way, writable. The README says not to name one.
- At or under `.bactrian`. The virtual machine hides all of it except the run's own directories and the `allow_write` paths, and `allow_read` would otherwise bring back any other part of it that the deny-list does not name.

The mount takes part in the existing rules instead of adding its own:

- The deny-list is masked wherever a mount shows it, so an `allow_read` of the home directory still hides `~/.ssh` and the other credential paths. A path under a masked directory is not mounted, and a path equal to a masked one is replaced by the mask.
- One bind is made for a path. A writable mount of the same path (an `allow_write` path, the run's own directories) wins over a read-only one (the project root, a read-only path, an `allow_read` path), so `allow_write` of the project root mounts it writable, and the project is mounted once if `allow_read` names it.
- The agent's working directory may be in an `allow_read` path.

**The `.env` files.** The files that the deny-list names at the project root (`.env`, `.env.local`, `.env.production` and the others) are also named at the root of each `allow_read` path, on every backend. In the virtual machine that makes the directory of such a file a read-only replacement that holds its other entries, as for the project, and the rule that a masked file's directory has at most 1000 entries applies. Only the root of each path is covered, as only the root of the project is, because neither Seatbelt parameters nor mounts take patterns. A file that is `allow_read` itself and is a masked path is therefore hidden, and the deny-list wins over the allow.

**Seatbelt and bubblewrap.** They already read everything outside the deny-list, so `allow_read` changes nothing there except the `.env` masks above. That is the point of having it on every backend: an experiment means the same on each, and the `.env` of a shared directory is as hidden under Seatbelt as it is in the container. It is not a restriction, and the README says so.

**The MCP server.** An `allow_read` path outside the project root is refused unless the server was started with `--trust-project`, as an `allow_write` one is, because the experiment is a file in the project and `allow_read` reaches host files a client did not already choose.

**Checkers** keep the host's sandbox and get the same `.env` masks, with no `allow_write`, as before.

## Consequences

An agent can read a second tree in the virtual machine, and the hidden files of that tree stay hidden under any spelling of their names, on a case-insensitive volume too. What a mount shows is not copied, so a file that changes while a run reads it changes for the run, as in the project. A hard link to a masked file elsewhere in an `allow_read` tree is a different path and is not masked, the same limit the project has (decision 0014).
