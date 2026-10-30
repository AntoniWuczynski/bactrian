# 0016: A heartbeat file for the relay, a second look at bound-back entries, and tests for both

Date: 2026-10-30

## Context

Decisions 0014 and 0015 left three residual risks in the virtual machine sandbox and three missing tests.

1. An agent that stops reading holds the relay's liveness timer off. The relay does not count time spent waiting for the agent to take data that is already queued, because that wait is the agent's and not a sign that bactrian has gone. So an agent that reads nothing, with a frame waiting at its sink, outlives bactrian, the bridge and the runtime's client all being killed, and is ended only by the next preflight's removal of stale containers.
2. The entries beside a hidden file that the command binds again by their path (Docker's files and directories, `container`'s directories) are listed when the command line is built and resolved by the runtime when the container is created. A swap in between would show another path.
3. `vm/relay.c` had no tests of its own, and the error for a hard link across volumes (a hidden file's neighbour on another volume than the run's directory) had never been run.

## Decision

**Liveness goes out of band.** A heartbeat frame cannot do this job. It sits in the same pipe behind the frame that waits, and the bridge's own write blocks behind it too, so a live bactrian waiting on an agent that reads nothing and a dead one look the same from inside. Reading ahead into a bounded buffer does not help either, because once the buffer is full the relay is back to not knowing. So the signal is a file:

- Each run has a directory `heartbeat` under its run directory, holding an empty file `beat`. Both runtimes mount the directory read-only at `/bactrian-heartbeat` right after the relay, and start the relay with `--heartbeat /bactrian-heartbeat/beat`. The agent can read the file and cannot write it.
- The bridge (`bactrian __vm-bridge heartbeat=FILE`) sets the file's modification time to now every 2 seconds, from a fiber of its own that no blocked write holds up. The probe's bridge does the same.
- With `--heartbeat` the relay takes a change of the file's modification time since its last look as the sign of life, looking every 500 milliseconds. It compares for a change and never against its own clock, so a clock that differs between the host and the virtual machine does not matter. Frames no longer count, because they may have been sent before bactrian died and sit in the pipe behind a stuck frame. A file that is gone or cannot be read is silence, so a failure ends the run after 10 seconds and does not keep it for ever. A frame that waits for room no longer holds the timer off.
- The preflight probe checks that the file works. Run with the same `--heartbeat`, it looks at the file's modification time, waits up to 5 seconds for it to change while the probe's bridge touches it every 2, and reports `heartbeat` as `unchanged` if it does not (`Vm_probe.judge` then fails the probe with a message naming the file sharing). This covers a runtime whose file sharing caches attributes for longer than a few seconds (some sshfs or 9p set-ups), where the probe would otherwise pass and every run of more than about 10 seconds would be stopped for silence. It passes a lag of up to about 3 seconds, and a run tolerates one of about 8.
- Without `--heartbeat` the relay behaves as in decision 0014.

Checked on OrbStack 2.2.1 (Docker 29.4.0) and `container` 1.5.0 on macOS 26.6.1: a `touch` on the host showed as a new modification time in the container's `stat` within about a second, with the directory mounted read-only. The acceptance suite repeats it on every run (Docker took about 3 MB of data before it stopped taking more, and `container`'s client took the 4 MiB the test sends without passing it on, so the test stops there): the relay alone, with its input full of data for an agent that reads nothing and still open, stays up for twice the timeout while the file is touched from the host, and ends with status 70 after the touching stops, no sooner than the timeout less half a second after the last change of the file (measured from that change, not from the end of the test's touching loop) and within the timeout and its allowance.

**A second look just before `create`.** `Vm_args.argv` returns, with the command, the entries it binds again by path, each with the entry the listing gave. `Sandbox_vm.launch` looks at each again, as late as bactrian's own process can: for `container` after the staging directories are made and immediately before `create`, for Docker as the command is handed to the bridge. The kind (file, directory, symbolic link and where it leads) and the device and inode must be the same, and the entry must not have become another name of a masked file. Otherwise the run stops, naming the entry. The window is not closed. The runtime resolves the path itself, and neither Docker's `--mount` nor `container`'s takes a descriptor, so a swap after the check, within the bridge's start for Docker or `container`'s own `create`, is still not seen. That residual stays, narrower, and a directory bound by path remains the only way to show it.

**Tests.**

- `test/relay_c` has C tests of the relay, built and run by dune on Linux (`%{system}` is `linux`), where the relay compiles, so the Ubuntu runner of continuous integration runs them. `test_relay_unit.c` includes the relay's source with `main` renamed and calls its buffers, frame queueing, delivery and connection handling. `test_relay_proc.c` starts the compiled relay and drives it over pipes and loopback with frames spelt out in the test, so it checks the wire format and not the relay's own constants: echo and 300 KB each way, exit status, death by signal, SIGTERM and SIGINT passed on, `no_new_privs`, descriptors closed, two connections multiplexed and told apart, 64 connections at most, a frame split across reads, each kind of bad frame, the end of input, silence and heartbeats, a stuck frame with and without the file, output that bactrian reads late, and refused arguments. The liveness timeout is a macro (`LIVENESS_MS`) so the tests use 1.5 seconds. On a Mac the relay does not compile, so the same files are run inside the relay's Alpine build image.
- The relay's source is now two files, `relay.c` and `probe.inc` (the preflight probe), because `relay.c` reached 999 lines. Both are embedded in bactrian, written into the build directory and hashed into the cached relay's name.
- `test/vm_staging/test_vm_volume.ml` makes a 2 MiB HFS+ disk image with `hdiutil`, mounts it, puts a project with a hidden file and a neighbour on it and the run's directory on the host's own volume, and checks that `Sandbox_vm.make_stages` fails with the hard-link error naming the file and the reason (`EXDEV`), leaves nothing staged, and succeeds when both are on the volume. It is skipped where `hdiutil` is missing and always detaches the image.

## Consequences

An agent can no longer outlive a killed bactrian by leaving data unread: the relay ends it 10 seconds after the last change of the file, and the container goes with it. The same holds for an agent that exits while nobody drains the relay's standard output: the relay's last flush gives up 10 seconds after the last change of the file (without a file, when no byte is taken for 10 seconds), where it used to wait for ever as process 1. While the file changes the flush waits, so a live bactrian that reads late still gets the agent's last output and its exit status.

When the relay ends a run for silence it writes the log frame ("nothing from bactrian") only if nothing is queued ahead of it, so one write of less than `PIPE_BUF` bytes takes all of it or none, and a reader never gets a cut frame. Behind queued output the line on the relay's standard error is the record. A bridge that is alive but whose touching fails (the run directory removed under it) lets the relay stop the agent, which is the safe side.

The cost is one more read-only mount per container and a file the host has to keep touching. The relay reads one file's modification time every half second, which costs nothing measurable.

Residual risks:

- Docker's and `container`'s resolution of bind sources is out of reach, as above. A process of the same user that can rename directories in the project can still win the race between the second look and `create`.
- The agent can read the heartbeat file's modification time. It learns nothing that its own clock does not tell it.
- Time is only as good as the runtime's file sharing. The probe refuses a mount that shows no new modification time within about 5 seconds, and a mount whose lag is between about 3 and 8 seconds is refused though a run would survive it. OrbStack and `container` show the change within about a second.
- The C tests do not run natively on macOS, where the relay does not compile. They run on Ubuntu in continuous integration, and the relay inside a container is covered by the acceptance suite on both runtimes.
