# 0015: Apple's container as a runtime of the virtual machine sandbox

Date: 2026-10-21

## Context

Decision 0014 runs the agent in a Linux container through Docker and left Apple's `container` for later, because its documentation names no way to give a container no network. `container` 1.5.0 is now installed on the machine this was built on (macOS 26.6.1 on Apple silicon, with the recommended kernel), so its behaviour could be checked. It starts a small virtual machine for each container, so an agent inside sees none of the host's processes, as in Docker's virtual machine, and a user without OrbStack or Docker Desktop can still keep the agent away from the launch environments of decision 0010.

## Decision

`sandbox.runtime` is `docker` or `container`, and so are `--sandbox-runtime` on `record` and `replay` and `sandbox_runtime` in the Model Context Protocol (MCP) tools `record_start` and `replay_start`. When it is not set, the runtime is docker if docker answers, otherwise container. When neither works, the error gives the fix for both. The relay, its frames, the bridge, what the masks hide and the probe stay as decision 0014 describes them. This record gives what differs.

**What was checked.** These sources were read at the tag 1.5.0: `NetworkClient.swift` (`noNetworkName = "none"`, "a container should have no network attachment"), `Utility.swift` (`--network none` gives an empty list of networks and must be the only network), `Parser.swift` (the environment file reader, a port of docker/cli's `kvfile`), and `ProcessIO.swift` and `ContainersService.swift` (signals). These were checked with `alpine:3.24.2@sha256:294b683cb724975bec92580e1e685676bd4b50bda910ddb8c51d4cabeaec77e6`:

- `--network none` leaves only `lo` and an empty route table. A connection to `1.1.1.1` fails with "Network unreachable", and the host's own address cannot be reached either.
- Standard input without a terminal carries 3,000,000 random bytes there and back with the same Secure Hash Algorithm (SHA-256) hash, and the exit status comes back.
- `--read-only` makes the root fail with `EROFS`. `--user UID:GID` applies. `--cap-drop ALL` empties the effective and bounding capability sets. Each container is a virtual machine of its own, with three processes in `/proc`, no `/Users` and an empty `/home`.
- `--ulimit nproc=1024:1024` caps the processes (a fork beyond it fails with "Resource temporarily unavailable"), and `--memory 4g` applies (the default is 1 GiB). There is no `--pids-limit`, `--memory-swap`, `--log-driver`, `--pull` or `--security-opt`.
- `--mount type=bind` takes directories only. A file gives "is not a directory".
- `--masked-path` over `.env` hides `.env`, while `.ENV` and `.Env` still read the secret, the same hole as Docker's bind of an empty file over it.
- Every argument after the image reaches the entrypoint as given, a second `--` included.

**The run** is three steps, so the image cannot change between the check and the run:

1. `container create --rm --name bactrian-RUN --label bactrian.run=RUN --label bactrian.pid=PID --label bactrian.host=HOST --network none --read-only --cap-drop ALL --user UID:GID --tmpfs /tmp:rw,noexec,nosuid,nodev,size=1g,mode=1777 --ulimit nproc=1024:1024 --memory 4g --workdir DIR --env-file agent.env --entrypoint /bactrian/RELAY --mount type=bind,source=RELAY_DIR,target=/bactrian,readonly --mount ... -- IMAGE --forward PORT -- AGENT...`, run with the client's environment.
2. `container inspect -- bactrian-RUN`. The digest of the image it was created from (`configuration.image.descriptor.digest`) must equal the one `container image inspect -- IMAGE` gave at the preflight (`configuration.descriptor.digest`). Otherwise the container is removed and the run fails. `container` cannot run an image by its ID ("cannot specify 64 byte hex string as reference"), and its store is keyed by the full reference, so this check stands in for Docker's run by ID. The label's `image_id` is that digest.
3. The bridge runs `container start -a -i bactrian-RUN`, which attaches the container's standard input, output and error and exits with its status.

How the command differs from Docker's:

- There is no `--pids-limit`. In its own virtual machine the agent's user is the only one besides root, so `--ulimit nproc` caps the container's processes. There is no swap setting.
- `/tmp` gets `mode=1777`, because `container` would make it 0755 and root's.
- The relay's cache directory is mounted at `/bactrian`, since only directories can be mounted. The cached relay's name is the same for both runtimes (`relay-HASH-arm64`), so one build serves both. It is built with `container run --rm --progress none` on the default network.
- There is no `--log-driver none`. While a container exists, `container` keeps its configuration, which holds the agent's environment (placeholder keys and the run's token), and a log of its standard output and error under `~/Library/Application Support/com.apple.container/containers/NAME/`. The directory goes when the container does.
- There is no `--security-opt no-new-privileges`, so the relay sets `no_new_privs` itself (`prctl(PR_SET_NO_NEW_PRIVS)`) before it starts the agent, under both runtimes, and stops if it cannot. Without it a set-user-ID root program in the image would run with effective user ID 0: dropping every capability from the bounding set takes away capabilities, not the checks that rest on owning a file. The pinned uv image holds eleven such programs (`su`, `passwd`, `mount` among them). Checked in that image: `NoNewPrivs` is 1 in the agent, and `unix_chkpwd`, which is set-group-ID `shadow`, keeps the agent's group (saved group ID 20), where without the flag its saved group ID was 42.
- `container create` fetches a reference that is not in its store. The preflight checks that the image is present (it never pulls), and the digest is checked after `create`, so only an image the user pulled can run.

**Masks.** A directory mask is `--tmpfs PATH:rw,mode=0755,size=1m`. `container` cannot make a mount point in a read-only tmpfs ("mount failed with errno 30: failed to create directory"), and the mask over `.bactrian` needs mount points for the run's own directories. The tmpfs belongs to root, so the agent, which has no capabilities and cannot gain privileges, gets `EACCES` when it writes there. The agent may therefore not run as root (user ID 0) in `container`.

A file mask cannot be Docker's tmpfs with each entry bound back, because a file cannot be mounted. Instead bactrian makes a staging directory under the run's directory for each directory that holds a masked file, and binds it read-only over that directory. It holds a hard link of each other regular file the container shows, and for a symbolic link to such a file a hard link of the file it leads to. The listing records each file's device and inode, the link does not follow a symbolic link, and a staged link that is not the recorded file stops the run, so a neighbour swapped for a name of the masked file between the listing and the staging is not shown. It holds an empty directory for each subdirectory and for each path with a mount of its own, where that mount goes, and nothing else: not the masked file under any name, no other symbolic link, socket, pipe or device. Each subdirectory is bound again over its empty directory. A lookup in the staging directory finds only what was put there, so the masked file is not there under any spelling of its name. Checked: `.env`, `.ENV` and `.Env` give `ENOENT`, `other.txt` and `OTHER.TXT` read, `sub/a`, `SUB/a` and `Sub/a` read, and a container with 43 directory mounts starts. Each staging directory records the bactrian process and host that made it. bactrian removes it with the container. If bactrian is killed, its bridge removes both, and if the bridge is killed too, the next preflight removes every staging directory whose bactrian is no longer alive, checked by its process ID, as it does for containers.

Three cases are refused under `container`, each with its reason: a writable directory that holds a masked file (the staging directory would make it read-only), an `allow_write` path that is a file, and a hard link that fails (a file on another volume than the run's directory), naming the file. The preflight's probe keeps its masked file in its state directory, which is writable, so under `container` the probe shows that one directory read-only. Docker's probe keeps it writable, as a run's may be.

**The environment file.** `container`'s reader splits lines on Swift's `.newlines`, which are U+000A to U+000D, U+0085, U+2028 and U+2029. bactrian refuses any of them in a variable's name or value, for both runtimes.

**Signals.** In 1.5.0, `container start -a` cannot pass a signal on. The client sends it in a form the service refuses ("missing signal in xpc message") and keeps waiting. So bactrian starts the bridge with `signals=kill`, and the bridge sends SIGTERM and SIGINT itself with `container kill --signal SIGNAL bactrian-RUN`, which reaches the relay and so the agent. `container kill` refuses a container that has not started yet, so a refused signal is sent again every 50 milliseconds for at most half a second, which covers a signal that comes while `start` is still attaching. Checked: an agent that writes a file when it gets SIGINT or SIGTERM wrote it, and `record` ended with status 130 a second after SIGINT. A killed client leaves the container running, as with Docker, and the bridge's watch on its parent, the relay's heartbeat and the removal of stale containers cover that as before. Stale containers are found with `container list --all --format json` and their labels, and removed with `container delete --force`.

**The probe** expects `EACCES` instead of `EROFS` for a write outside the state directory, which lies under the root-owned mask over `.bactrian`. Any other error still fails the probe. Under both runtimes it also needs `NoNewPrivs` to be 1.

## Consequences

On a Mac with `container` the virtual machine backend needs neither OrbStack nor Docker Desktop. The whole acceptance suite of the virtual machine passes on both runtimes, with a new case for a masked file's neighbours. A whole `record` of a small Node agent, the preflight included, took about 1.9 to 2.2 seconds with `container` against 1.0 to 1.2 with OrbStack, because each container starts a virtual machine.

Continuous integration cannot run `container` (GitHub's runners do not have it), so the `container` suite runs on a developer's Mac, with `BACTRIAN_VM_RUNTIME=container dune build @acceptance`, and skips where the runtime is missing.

Residual risks, on top of decision 0014's:

- A subdirectory beside a masked file is bound again by its path when `create` runs. A swap of it for a symbolic link to another directory between the listing and `create` would show that directory instead, as with Docker's bind of each entry back. Hard links of files are checked, directories are not.
- The container's configuration (placeholder keys and the run's token) and its output log sit in the user's Library while it exists. Any process of the user can read them, as it can read the run's own directory.
- `create` would fetch a missing reference. The preflight checks that the image is present before, and the digest is checked after.
- `container`'s host-only networks (`--internal`) are not used. `--network none` leaves `lo` alone.
- A file beside a masked one is shown through a hard link, so the agent reads the same file, read-only. A hard link to the masked file elsewhere in the project is still shown, as with Docker and Seatbelt.
