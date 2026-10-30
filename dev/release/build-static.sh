#!/bin/sh
# Build a static musl bactrian inside an Alpine container. Run as root with
# the repository mounted as the working directory and OPAMROOT on a mounted
# volume, so the compiler and dependencies are cached between runs.
set -eu

# A package index is needed, or opam reports system packages such as
# linux-headers (needed by uring) as unavailable.
apk update
apk add build-base git curl bash opam m4 patch unzip rsync linux-headers pkgconf
# The mounted checkout is owned by the runner user, not root, and git refuses it
# otherwise, which breaks opam's file listing.
git config --global --add safe.directory '*'
export OPAMROOT="${OPAMROOT:-/opam-root}"
export OPAMYES=1 OPAMCONFIRMLEVEL=unsafe-yes

if [ ! -d "$OPAMROOT/bactrian" ]; then
  opam init --bare --disable-sandboxing --no-setup
  opam switch create bactrian ocaml-base-compiler.5.5.1
fi
eval "$(opam env --switch=bactrian --set-switch)"
opam install . --deps-only --locked
# The lock file is made on macOS, so it has no Linux-only Eio backend. The
# locked install takes eio_linux and uring through eio_main, at versions the
# solver picks. Require the versions the release is built with, eio_linux 1.6
# and the uring it needs. The install may add packages but must not move one
# already installed, so if the solver picked others the build fails here and
# these versions need raising.
opam list --installed --short --columns=package | sort >/tmp/before
opam install eio_linux.1.6 uring.2.15.0
opam list --installed --short --columns=package | sort >/tmp/after
if comm -23 /tmp/before /tmp/after | grep .; then
  echo "the Eio backend install changed the locked packages above" >&2
  exit 1
fi
echo "added for the Eio backend:"
comm -13 /tmp/before /tmp/after
BACTRIAN_STATIC=1 dune build --profile release bin/main.exe
cp _build/default/bin/main.exe "${1:-bactrian}"
