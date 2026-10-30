# 0020: JSON Schemas for the file formats

Date: 2026-10-30

## Context

Design section 6 promised hand-written JSON Schemas for the formats bactrian reads and writes, validated in tests only because OCaml JSON Schema validators are young. People writing scenarios and experiments in an editor, and tools that read a run's files, had only the prose of the design to go on. A schema that is written by hand and never run against real files drifts, so the work was to write the schemas and to make the test suite catch drift in either direction.

## Decision

**What has a schema.** Each format has a draft 2020-12 file in `schemas/`, named for what it describes: `scenario`, `experiment`, `price-file`, `cassette-line`, `trace-line`, `meta`, `verdicts`, `divergences`, `manifest`, `ledger-line`, `agent-input`, `agent-output`, `checker-input`, `checker-verdict`, `compare` and `plan`, with `defs` for the pieces they share. The line formats describe one line, because the files are JSON Lines. `compare` and `plan` are the `--json` outputs that carry a `schema_version`. The `--json` output of `run`, `record`, `replay`, `check` and `doctor` has none and no schema yet. `sandbox.log`, the JSON Lines file the Linux forwarder and the virtual machine's bridge write, has none either.

**What a schema says.** The schemas describe what the current bactrian writes, and for the files people write, what its decoders accept. Objects are closed (`additionalProperties` or `unevaluatedProperties` false), as the decoders are strict. The members that hold any JSON (`canonical`, `arguments`, `data`, `match`, `schema`, `equals`) are open. Rules the decoders check across fields are stated: exactly one key per assertion, `match` or `schema` by `mode`, `command` or `url` by `protocol`, a vm backend needs an image, `strict` is refused with an HTTP agent, `tool_args.schema` holds only the keywords `Json_schema.check` supports, a cassette exchange is a response or an error, usage tokens come in pairs, a `done` run has an outcome. Rules the decoders check by running code are not: that a regular expression compiles, that a JSONPath parses, that an environment variable is not reserved, that variant names are distinct. The schema says the member is a string and the decoder judges it. Where a field can be `null` on the wire and is absent when written, the schema accepts both, because traces are hand-writable as golden files.

**No version field on four formats.** `meta.json`, `verdicts.json`, `ledger.jsonl` and a checker's verdict have none. Adding one changes files that older runs already hold and that `compare` and `report` read, so it is a separate change with its own reading rule, listed as a deferral. Their schemas say so. `meta.json` also has older shapes that are still read (a bare string for the clock, no sandbox label), which the schema does not describe.

**Validation is a test, written in Python.** `dev/schemas/validate.py` is a uv project (`jsonschema` 4.26.0 with `referencing`, `pyyaml` 6.0.3, locked, nothing newer than seven days at the time of locking) checked with `mypy --strict` and `pyright` strict. It was chosen over a TypeScript script because the repository already has a uv project beside its Python agent, and `jsonschema` is the reference implementation with full 2020-12 support (`unevaluatedProperties`, `dependentRequired`, `if`/`then`). Its YAML reader follows bactrian's own scalar rules: `true` and `false` are the only booleans (no `yes`, `no` or `on`), a timestamp stays a string, and a mapping key is its text as written. The schemas' patterns are ECMA-262 regular expressions, where a final `$` matches only at the end, so the script reads it as Python's `\Z` (Python's `$` also matches before a trailing newline).

**What it validates.** `dune build @schemas` runs the real binary: an experiment of two variants with checkers and assertions, a replay that diverges, `compare`, `plan` and `check`, all against the fake upstream in a process of its own. Everything left in the project is validated, with the run's golden trace also validated as a checker's input. Then come the hand-written fixtures for what one run does not reach (an HTTP agent, MCP servers, the virtual machine label, streamed and base64 bodies, a transport error, every trace event, a partial manifest, an inconclusive comparison), then the invalid fixtures, which must each be rejected. Last, every valid document is mutated: an unknown field added to each object in turn, and the version removed or changed. All of these must be rejected. A schema that validates no document, or a format with no sample, fails the run, and so does an invalid fixture that names no schema or holds no document, since neither proves a rejection.

**Agreement with the decoders.** The same alias feeds every scenario, experiment and price file fixture to bactrian (`check`, `doctor --experiment`, and `run` for a price file, which only `run` loads, from an experiment that names it and stops at the missing spend cap). It must exit with the configuration error status for the invalid fixtures and not for the valid ones. A schema that is looser or stricter than the decoder on a fixture fails there.

**Where it runs.** It has its own alias and is not part of `dune test`, because it needs uv and python3. Continuous integration runs it after `dune test` on both operating systems, after type-checking the script.

## Consequences

- A change to a writer or a decoder that the schemas do not follow fails `dune build @schemas`, either on a real file or on a fixture.
- The schemas are published as files in the repository. Their `$id` is the raw URL on the main branch, so editors can fetch them.
- A new format needs a schema, a rule in `validate.py` for where its files sit in a project, and a sample.
- The unknown-field check only reaches members that a sample or fixture holds. A member that no document uses is not exercised.
- The `--json` output of `run`, `record`, `replay`, `check`, `doctor` and `divergences`, the results of the Model Context Protocol tools and the HTML report's embedded data are not covered.
