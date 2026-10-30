#!/bin/sh
# package.sh BINARY TARGET VERSION: write bactrian-VERSION-TARGET.tar.gz and
# its .sha256 file into the current directory.
set -eu
bin=$1 target=$2 version=$3
name="bactrian-$version-$target"
rm -rf "$name"
mkdir "$name"
install -m 755 "$bin" "$name/bactrian"
cp LICENSE README.md "$name/"
tar -czf "$name.tar.gz" "$name"
rm -rf "$name"
if command -v sha256sum >/dev/null 2>&1; then
  sha256sum "$name.tar.gz" >"$name.tar.gz.sha256"
else
  shasum -a 256 "$name.tar.gz" >"$name.tar.gz.sha256"
fi
