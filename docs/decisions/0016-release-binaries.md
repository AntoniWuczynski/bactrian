# 0016: Prebuilt release binaries

Date: 2026-10-30

## Context

Design section 1 promised prebuilt binaries: static musl on Linux and native on macOS. Building from source needs opam and an OCaml 5.5 switch, which is a lot to ask of someone who only wants to test an agent.

## Decision

A workflow, `.github/workflows/release.yml`, builds three archives: `linux-x86_64` and `linux-arm64` (static, musl) and `macos-arm64` (native). Each holds `bactrian`, `LICENSE` and `README.md`, and has a `.sha256` file. A tag matching `v*` also publishes them to a GitHub Release with a combined `SHA256SUMS`. Pull requests and manual runs build and verify without publishing.

**Static Linux.** The build runs in an Alpine container on a native runner of each architecture (no cross-compilation), through `dev/release/build-static.sh`. Alpine's C library is musl, and `BACTRIAN_STATIC=1` makes `bin/dune` add `-ccopt -static` to the link. The variable is read through `(env_var ...)`, so ordinary builds are unchanged. The dependencies have no C library beyond musl (the Eio backend is `eio_posix`, not io_uring). The opam root is cached on the lock file's hash.

**macOS.** Built with `ocaml/setup-ocaml` as in the main CI. Only the arm64 build is shipped, as macOS 15 runners are Apple silicon. The verification step fails if the binary links anything outside `/usr/lib` and `/System`.

**Verification.** A second job per target downloads the artefact on a fresh runner image with no OCaml, checks the checksum, runs `bactrian --version` and `bactrian doctor --json`, and on Linux requires `file` to report static linking and `ldd` to fail.

**Releases use `--profile release`.** That drops warnings-as-errors for the shipped build only. The main CI keeps the dev profile and gates every change.

## Consequences

No Intel macOS or Windows binary. The Homebrew tap and the first tag are separate owner decisions. The Linux static build cannot be run on a Mac, so its first proof is the pull request's CI.
