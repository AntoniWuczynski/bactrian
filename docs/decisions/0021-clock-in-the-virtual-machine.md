# 0021: The agent's clock in the virtual machine

Date: 2026-10-30

## Context

Decision 0009 enforces the agent's clock on a Linux host by preloading `libfaketime`. In the virtual machine sandbox (decision 0014) every run was `contract-only` with the reason `vm`, because the host's library is for the host's C library, and bactrian could not know where an image keeps its own. The image decides three things the host cannot see: which C library the agent's programs load (Alpine's musl or Debian's glibc), what the agent's command leads to (a dynamic program, a static one, a Go one, a script), and whether a library is there at all.

## Decision

There are three parts: a build, a look at the image, and a relay that starts the clock.

**The build.** bactrian builds `libfaketimeMT.so.1` from source, as it builds the relay, on the first run that needs it, and caches it beside the relay as `~/.cache/bactrian/relay/faketime-HASH-ARCH/{musl,glibc}/libfaketimeMT.so.1`. It is built for one C library at a time, when an image needs it, so a user of Alpine images never builds the glibc one.

- Source: libfaketime 0.9.13, published 2026-08-31, which is older than the seven days that bactrian asks of a new dependency. The tarball GitHub serves for the tag has SHA-256 `8e56deeb805682b025107e095f1d94a6ea677472f05824cd475f5c5e6e1a5ddf` (github.com's archive URL and codeload give the same bytes). It is downloaded in the Alpine container that already builds the relay and checked there, kept in the cache, and checked again by each builder before it unpacks it. GitHub does not promise that an archive stays byte-for-byte the same. If it ever changes, the build stops with a checksum failure and the run is `contract-only`, which is the safe way to fail, and the pin is updated.
- musl: the relay's Alpine 3.24.2 image (pushed 2026-09-18), with `apk add make gcc musl-dev`.
- glibc: `debian:bookworm-20261005-slim` pinned by the digest of its multi-architecture index, `sha256:7c7b2c966bc9ee8cedfeef67e0e279108992c77681fa595db4a9d65c06ccc587` (pushed 2026-10-06), with `apt-get install gcc make libc6-dev`. Bookworm's glibc is 2.36, and a library built against it loads in any image with glibc 2.36 or later, which covers Debian 12 and 13 and Ubuntu 22.04 and later. As with the relay's packages, apk and apt check their own signatures and the package versions are not pinned, because Alpine keeps one build of a package and Debian replaces its security updates.
- Only `make libfaketimeMT.so.1` is run. The full `make` also builds a time64 object that does not compile with musl, and the single-thread library, which bactrian does not use (decision 0009).
- The cache is trusted as the relay's is: its directories and files must belong to this user, must not be symbolic links and must not be writable by others. It is mounted read-only (Docker at `/bactrian/faketime`, `container` through the mount of the relay's directory).
- A build that fails does not fail the run. It gives the label `contract-only` with the reason `vm`, and is not tried again in that process.

**The look at the image.** Before a run, `Sandbox_vm.clock` starts the relay with `--probe-exec NAME` in a container built like the run's (so the project and its files are where the agent expects them, and the working directory is the agent's), and the relay reports on one line:

- the image's C library, from the loader it carries (`ld-musl-*.so.1` or `ld-linux*.so.[12]` in `/lib`, `/lib64`, `/usr/lib` and `/usr/lib64`), or none
- whether `NAME` is found as the system would find it (a name with a slash is a path from the working directory, any other is looked up in the image's own `PATH`) and the first 256 KiB of that regular, executable file in hexadecimal

The host decodes the report (`Vm_exec_probe`) and classifies the prefix with the code that classifies a host executable (`Elf_probe`), so dynamic, static, Go and not-ELF mean the same in both places. A `#!` line is followed one level with `Shebang`, and the interpreter is looked up by a second probe, as on the host. The report for an image, a working directory and a command name is kept for the rest of the process, so the runs of an experiment look once. The look costs one extra container start the first time, about a second with Docker and two with `container`.

The label then follows `Agent_clock.decide ~vm`: `faketime-off`, `invalid-sim-now`, `vm` for a probe or a build that failed, the executable's own reason (`static-binary`, `go-binary`, `unreadable-executable`), and `library-not-found` when the image has no loader. The host's library, its `hidden` and `stripped` checks and the host operating system play no part, so a macOS host gets `enforced` for a Linux agent in a container.

**The relay starts the clock.** The relay is given `--faketime EPOCH DIR`. In the agent's process, just before it starts, it sets `LD_PRELOAD` to `DIR/LIBC/libfaketimeMT.so.1` (first, and once, ahead of any value the environment already holds) and `FAKETIME` to the offset `EPOCH - now` in seconds with microseconds, read from the container's own clock. The agent's child processes inherit both, so they share the clock. If the image has no loader or the library cannot be read, the relay fails the start with a message instead of running an agent whose clock was promised.

An offset replaces decision 0009's `@EPOCH` with `FAKETIME_FMT=%s`. musl's `strptime` does not implement `%s`, so on an Alpine image the clock started at 1899. The other `@` form needs the time as wall-clock text in the agent's `TZ`, which needs the image's time zone data and goes wrong in the repeated hour of a clock change. An offset needs neither. The price is that the clock starts at `EPOCH` a few milliseconds after the relay reads the container's clock, not at the library's first call, and the monotonic clock is not faked: libfaketime leaves `CLOCK_MONOTONIC` alone in offset mode (checked with Node on Alpine and Python 3.12 on Debian, where `time.monotonic()` read the container's uptime and `time.sleep` slept). That also avoids the sleep fault of libfaketime 0.9.10 that decision 0009 describes. Nothing is written to `agent.env` for the clock, so the placeholder-key file is unchanged.

## Consequences

An agent in the virtual machine on macOS now reads `SIM_NOW` from `time.time()`, `Date.now()` and the `date` it starts, when its program is dynamic and not Go. A replay of it matches its recording as before, because the clock label is never compared.

The first run of a new image pays for a look at the image, and the first run on a new machine pays for the build, which needs the network and, measured with a loaded network, took several minutes for the glibc image. `doctor` reports which libraries are built and does not build them. The wall clock of a Go or static agent, and a runtime that reads the vDSO directly, still shows real time, as on a Linux host.

Under `container` the same relay and libraries are used, and the clock cases of the acceptance suite (musl image, glibc image, a script, an image without the executable) pass on both runtimes. The offset is read from the container's own clock, so the host's clock and the virtual machine's need not agree.
