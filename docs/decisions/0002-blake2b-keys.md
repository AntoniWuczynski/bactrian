# 0002: BLAKE2b-256 for keys and blob addresses

Date: 2026-10-13

## Context

Replay matching keys requests by a hash of their canonical form, and large payloads are stored by content hash. The first design named SHA-256, which OCaml's standard library does not provide. Since OCaml 5.2 the standard library's `Digest` module provides BLAKE2b in 128, 256 and 512-bit sizes.

## Decision

Keys and blob addresses use `Digest.BLAKE256` (BLAKE2b with a 256-bit output), printed as 64 lowercase hex characters. Blobs live under `blobs/blake2b-256/`.

## Consequences

No hashing dependency in the pure core. BLAKE2b-256 offers the same collision resistance as SHA-256 for this purpose. Anything that compares bactrian's keys with external tools must use BLAKE2b, not SHA-256.
