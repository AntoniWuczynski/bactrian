#!/bin/sh
# Build a static musl bactrian inside an Alpine container. Run as root with
# the repository mounted as the working directory and OPAMROOT on a mounted
# volume, so the compiler and dependencies are cached between runs.
set -eu

apk add --no-cache build-base git curl bash opam m4 patch unzip rsync \
  linux-headers pkgconf
# --no-cache keeps no package index, and without one opam reports system
# packages such as linux-headers (needed by uring) as unavailable.
apk update
export OPAMROOT="${OPAMROOT:-/opam-root}"
export OPAMYES=1 OPAMCONFIRMLEVEL=unsafe-yes

if [ ! -d "$OPAMROOT/bactrian" ]; then
  opam init --bare --disable-sandboxing --no-setup
  opam switch create bactrian ocaml-base-compiler.5.5.1
fi
eval "$(opam env --switch=bactrian --set-switch)"
opam install . --deps-only --locked
# The lock file is made on macOS, so it has no Linux-only Eio backend. Pin it
# here, and the version of uring that eio_linux 1.6 needs, rather than let the
# solver pick the newest. The install may add packages but must not move a
# locked one, or the binary would drift from the lock.
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
