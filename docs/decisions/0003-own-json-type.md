# 0003: bactrian's own canonical JSON type

Date: 2026-10-13

## Context

Provider request and response bodies are open-ended JSON. Matching needs two documents with the same meaning to compare equal and hash identically, regardless of key order or how a number was written. `Yojson.Safe.t` keeps key order and duplicate keys, and distinguishes `1` from `1.0`, so equal meaning does not imply equal values.

## Decision

`Canon.t` is a private variant with an invariant: object keys sorted and unique, integral floats stored as integers, only finite floats. Values are built only through smart constructors and the parser, which enforce the invariant. `Yojson.Safe.t` appears only at the parse boundary inside `Canon`.

## Consequences

Equality, ordering and hashing on `Canon.t` are structural and meaningful. Duplicate keys and non-finite numbers are rejected at parse time. Code that needs a specific shape still decodes into typed records. `Canon.t` is for payloads whose shape bactrian does not own.
