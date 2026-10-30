# 0016: Prebuilt release binaries

Date: 2026-10-30

## Context

Design section 1 promised prebuilt binaries: static musl on Linux and native on macOS. Building from source needs opam and an OCaml 5.5 switch, which is a lot to ask of someone who only wants to test an agent.

## Decision

A workflow, `.github/workflows/release.yml`, builds three archives: `linux-x86_64` and `linux-arm64` (static, musl) and `macos-arm64` (native). Each holds `bactrian`, `LICENSE` and `README.md`, and has a `.sha256` file. A tag matching `v*` also publishes them to a GitHub Release, with each `.sha256` file and a combined `SHA256SUMS`. Pull requests, pushes to `main` and manual runs build and verify without publishing, and a manual run on a tag does not publish either, because the publish job requires a `push` event.

**Static Linux.** The build runs in an Alpine container on a native runner of each architecture (no cross-compilation), through `dev/release/build-static.sh`. Alpine's C library is musl, and `BACTRIAN_STATIC=1` makes `bin/dune` add `-ccopt -static` to the link. The variable is read through `(env_var ...)`, so ordinary builds are unchanged. The dependencies have no C library beyond musl. The Eio backend is not `eio_posix` alone: `Eio_main.run` tries `eio_linux` first and falls back to `eio_posix` only where io_uring is unavailable (for example under a seccomp filter that blocks it), so the static binary links `eio_linux` and `uring`, which vendors liburing. The lock file is made on macOS and has neither, so the locked install takes them through `eio_main` at versions the solver picks. `build-static.sh` then requires `eio_linux.1.6` and `uring.2.15.0`. That install may add packages but must not move one already installed, so the script compares the installed packages before and after it and fails if one changed version or went away. A newer Eio backend therefore stops the build until these versions are raised, instead of shipping unnoticed. The base image is pinned by digest. The opam root lives in the runner's temporary directory, outside the checkout, because dune would otherwise read the `dune` executable inside it as a dune file. It is cached on the hashes of the lock file and `build-static.sh` and on the image, and pushes to `main` run the workflow so that the cache exists on the default branch for pull requests and tags.

**macOS.** Built with `ocaml/setup-ocaml` as in the main CI. Only the arm64 build is shipped, as macOS 15 runners are Apple silicon. The verification step fails if the binary links anything outside `/usr/lib` and `/System`.

**Verification.** A second job per target downloads the artefact on a fresh runner image with no OCaml and checks the checksum. On Linux it installs bubblewrap and lifts Ubuntu's AppArmor restriction on user namespaces, as the main CI does. It then runs `bactrian --version` and `bactrian doctor --json`. `doctor` exits 0 whatever it finds, so the job reads the JSON and requires the `sandbox` check to be `ok`, which runs the static binary's own bubblewrap path on Linux and Seatbelt on macOS. On Linux the job also requires that the program headers have no `INTERP` entry and the dynamic section no `NEEDED` entry. `ldd` is no test, because it exits 0 for a static-pie binary.

**Tags.** `bactrian --version` prints `Version.current` from `lib/core/version.ml`, which is also written into cassette headers. On a tag the verify job requires the output to equal the tag without its `v`, so the version must be raised in a commit before the tag is pushed. A separate `gate` job requires the tagged commit to be an ancestor of `origin/main` and a passing run of `ci.yml` for that commit, as the tag build itself runs no tests. A tag pushed together with its commit finds that run still going, so the gate waits for the newest `ci.yml` run of the commit to finish. A failed CI run means a fix on `main` and a new tag. A CI run that passes on a re-run needs the release workflow's failed jobs re-run too. A tag with a suffix, such as `v0.1.0-rc.1`, publishes a prerelease.

**Actions are pinned by commit.** The release jobs produce what users download, so every action is pinned to the commit of a release at least seven days old, with the release named in a comment. The main CI keeps its tags.

**Releases use `--profile release`.** That drops warnings-as-errors for the shipped build only. The main CI keeps the dev profile and gates every change.

## Consequences

No Intel macOS or Windows binary. The Homebrew tap and the first tag are separate owner decisions. The Linux static build cannot be run on a Mac, so its first proof is the pull request's CI.
