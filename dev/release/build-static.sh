#!/bin/sh
# Build a static musl bactrian inside an Alpine container. Run as root with
# the repository mounted as the working directory and OPAMROOT on a mounted
# volume, so the compiler and dependencies are cached between runs.
set -eu

apk add --no-cache build-base git curl bash opam m4 patch unzip rsync \
  linux-headers libffi-dev gmp-dev pkgconf
export OPAMROOT="${OPAMROOT:-/opam-root}"
export OPAMYES=1 OPAMCONFIRMLEVEL=unsafe-yes

if [ ! -d "$OPAMROOT/bactrian" ]; then
  opam init --bare --disable-sandboxing --no-setup
  opam switch create bactrian ocaml-base-compiler.5.5.1
fi
eval "$(opam env --switch=bactrian --set-switch)"
opam install . --deps-only --locked
BACTRIAN_STATIC=1 dune build --profile release bin/main.exe
cp _build/default/bin/main.exe "${1:-bactrian}"
