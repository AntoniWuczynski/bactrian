"""Validates the files bactrian writes and reads against the schemas in schemas/.

Usage:
    uv run --project dev/schemas python dev/schemas/validate.py \\
        --schemas schemas --samples DIR [--fixtures dev/schemas/fixtures]

DIR holds a real project that the test suite built (see
test/acceptance/gen_schema_samples.ml): scenarios, an experiment, a price file,
and the .bactrian directory with its runs. The hand-written fixtures cover the
variants a single run does not reach, and the invalid ones must be rejected.

Besides validating, it mutates every valid document: an unknown field added to
any object must be rejected, and removing or changing the version must be
rejected. The schemas are checked, never the OCaml decoders.
"""

from __future__ import annotations

import argparse
import copy
import fnmatch
import json
import re
import sys
from collections.abc import Iterator
from dataclasses import dataclass
from pathlib import Path
from typing import Literal, cast

import yaml
from jsonschema import Draft202012Validator
from jsonschema.protocols import Validator
from referencing import Registry, Resource
from referencing.jsonschema import DRAFT202012, Schema, SchemaRegistry

type JSON = dict[str, JSON] | list[JSON] | str | int | float | bool | None
type Path_ = tuple[str | int, ...]
type Kind = Literal["document", "lines"]

DIALECT = "https://json-schema.org/draft/2020-12/schema"

# Keys that name the version of a format, by schema. A document of a versioned
# format must be rejected without it and with another number.
VERSION_KEYS: dict[str, str] = {
    "scenario": "version",
    "experiment": "version",
    "price-file": "version",
    "cassette-line": "v",
    "trace-line": "v",
    "divergences": "version",
    "manifest": "version",
    "agent-input": "v",
    "agent-output": "v",
    "compare": "schema_version",
    "plan": "schema_version",
}

# Members whose value is any JSON, or a map with arbitrary keys over numbers: an
# unknown field inside one is not an error.
FREE_FORM = {"canonical", "arguments", "data", "match", "schema", "equals", "thresholds"}

UNKNOWN_FIELD = "bactrian_unknown_field"


@dataclass(frozen=True)
class Rule:
    pattern: str
    schema: str
    kind: Kind


# Where each format sits in a project the test suite built.
RULES: tuple[Rule, ...] = (
    Rule("scenarios/*.yaml", "scenario", "document"),
    Rule("scenarios/*.json", "scenario", "document"),
    Rule("scenarios/golden/*.jsonl", "trace-line", "lines"),
    Rule("exp.yaml", "experiment", "document"),
    Rule("exp.json", "experiment", "document"),
    Rule("prices.yaml", "price-file", "document"),
    Rule("prices.json", "price-file", "document"),
    Rule(".bactrian/runs/*/cassette.jsonl", "cassette-line", "lines"),
    Rule(".bactrian/runs/*/trace.jsonl", "trace-line", "lines"),
    Rule(".bactrian/runs/*/meta.json", "meta", "document"),
    Rule(".bactrian/runs/*/verdicts.json", "verdicts", "document"),
    Rule(".bactrian/runs/*/divergences.json", "divergences", "document"),
    Rule(".bactrian/experiments/*/manifest.json", "manifest", "document"),
    Rule(".bactrian/experiments/*/ledger.jsonl", "ledger-line", "lines"),
    Rule(".bactrian/ledger.jsonl", "ledger-line", "lines"),
    Rule("outputs/compare.json", "compare", "document"),
    Rule("outputs/plan.json", "plan", "document"),
)

# What a project must have produced, so a rule that matches nothing is noticed.
EXPECTED = {rule.schema for rule in RULES}


PLAIN_NULL = {"", "~", "null", "Null", "NULL"}
PLAIN_TRUE = {"true", "True", "TRUE"}
PLAIN_FALSE = {"false", "False", "FALSE"}
INTEGER = re.compile(r"^[-+]?[0-9]+$")
NUMBER = re.compile(r"^[-+0-9.eE]+$")


def plain_scalar(text: str) -> JSON:
    """A plain scalar the way bactrian's loader reads it (config_loader.ml):
    only the core-schema null, true and false, decimal integers and decimals.
    There are no yes, no, on or off booleans and no timestamps."""
    if text in PLAIN_NULL:
        return None
    if text in PLAIN_TRUE:
        return True
    if text in PLAIN_FALSE:
        return False
    if INTEGER.match(text):
        return int(text)
    if NUMBER.match(text) and any(c.isdigit() for c in text):
        try:
            return float(text)
        except ValueError:
            return text
    return text


def narrow(value: object) -> JSON:
    """What json.loads returned, checked to be JSON rather than trusted."""
    if value is None or isinstance(value, (str, bool, int, float)):
        return value
    if isinstance(value, list):
        return [narrow(item) for item in cast(list[object], value)]
    if isinstance(value, dict):
        return {str(key): narrow(item) for key, item in cast(dict[object, object], value).items()}
    raise SystemExit(f"not JSON: {value!r}")


def node_json(node: yaml.Node) -> JSON:
    if isinstance(node, yaml.ScalarNode):
        value = str(node.value)
        return value if node.style else plain_scalar(value)
    if isinstance(node, yaml.SequenceNode):
        return [node_json(child) for child in node.value]
    if isinstance(node, yaml.MappingNode):
        return {mapping_key(key): node_json(value) for key, value in node.value}
    raise SystemExit(f"unsupported YAML node {node!r}")


def mapping_key(node: yaml.Node) -> str:
    """A mapping key is its text as written, whatever it would resolve to as a
    value: bactrian keeps the raw key (Config_loader.fields), so `true:` is "true"."""
    if isinstance(node, yaml.ScalarNode):
        return str(node.value)
    raise SystemExit(f"unsupported YAML mapping key {node!r}")


def load_text(text: str, suffix: str) -> JSON:
    if suffix in (".yaml", ".yml"):
        root = yaml.SafeLoader(text).get_single_node()
        return None if root is None else node_json(root)
    return narrow(json.loads(text))


def load_lines(text: str) -> list[JSON]:
    return [narrow(json.loads(line)) for line in text.split("\n") if line != ""]


@dataclass(frozen=True)
class Document:
    """One JSON value to validate, and where it came from."""

    origin: str
    schema: str
    value: JSON


def documents(path: Path, schema: str, kind: Kind, origin: str) -> list[Document]:
    text = path.read_text(encoding="utf-8")
    if kind == "lines":
        return [Document(f"{origin}:{n}", schema, v) for n, v in enumerate(load_lines(text), 1)]
    return [Document(origin, schema, load_text(text, path.suffix))]


def sample_documents(root: Path) -> list[Document]:
    found: list[Document] = []
    for path in sorted(p for p in root.rglob("*") if p.is_file()):
        relative = path.relative_to(root).as_posix()
        for rule in RULES:
            if fnmatch.fnmatchcase(relative, rule.pattern):
                found.extend(documents(path, rule.schema, rule.kind, relative))
                break
    return found


def checker_inputs(root: Path) -> list[Document]:
    """What a checker reads on standard input: a golden trace as one array."""
    return [
        Document(f"{path.relative_to(root).as_posix()} (as a checker's input)", "checker-input", load_lines(path.read_text(encoding="utf-8")))
        for path in sorted(root.glob("scenarios/golden/*.jsonl"))
    ]


@dataclass(frozen=True)
class Fixture:
    origin: str
    schema: str
    expected_valid: bool
    documents: list[Document]


def fixtures(root: Path) -> list[Fixture]:
    """fixtures/valid/<schema>.<name>.<ext> must pass and fixtures/invalid/... must
    fail (a lines file fails when any line does)."""
    found: list[Fixture] = []
    for verdict, expected in (("valid", True), ("invalid", False)):
        for path in sorted((root / verdict).glob("*")):
            schema = path.name.split(".", 1)[0]
            kind: Kind = "lines" if path.suffix == ".jsonl" else "document"
            origin = f"fixtures/{verdict}/{path.name}"
            found.append(Fixture(origin, schema, expected, documents(path, schema, kind, origin)))
    return found


def ecma_anchors(schema: JSON) -> JSON:
    """The schemas' patterns as ECMA-262 reads them, which they are written in: a
    final `$` matches only at the end. Python's `$` also matches before a
    trailing newline, so "2026-01-01\\n" would pass a date pattern the decoder
    rejects. `\\Z` is Python's end of string."""
    if isinstance(schema, list):
        return [ecma_anchors(item) for item in schema]
    if isinstance(schema, dict):
        return {
            key: item.removesuffix("$") + r"\Z"
            if key == "pattern" and isinstance(item, str) and item.endswith("$") and not item.endswith("\\$")
            else ecma_anchors(item)
            for key, item in schema.items()
        }
    return schema


class Schemas:
    def __init__(self, directory: Path) -> None:
        self.names: list[str] = []
        contents: dict[str, Schema] = {}
        resources: list[tuple[str, Resource[Schema]]] = []
        for path in sorted(directory.glob("*.schema.json")):
            schema = ecma_anchors(narrow(json.loads(path.read_text(encoding="utf-8"))))
            if not isinstance(schema, dict) or not isinstance(schema.get("$id"), str):
                raise SystemExit(f"{path.name}: needs an object with an $id")
            if schema.get("$schema") != DIALECT:
                raise SystemExit(f"{path.name}: $schema must be {DIALECT}")
            Draft202012Validator.check_schema(schema)
            name = path.name.removesuffix(".schema.json")
            self.names.append(name)
            contents[name] = schema
            resources.append((str(schema["$id"]), DRAFT202012.create_resource(schema)))
        registry: SchemaRegistry = Registry[Schema]().with_resources(resources)
        self.validators: dict[str, Validator] = {
            name: Draft202012Validator(schema, registry=registry) for name, schema in contents.items()
        }

    def errors(self, document: Document) -> list[str]:
        validator = self.validators.get(document.schema)
        if validator is None:
            return [f"no schema named {document.schema}"]
        return [
            f"/{'/'.join(str(p) for p in e.absolute_path)}: {e.message[:200]}"
            for e in validator.iter_errors(document.value)
        ]


def objects(value: JSON, path: Path_ = ()) -> Iterator[tuple[Path_, dict[str, JSON]]]:
    if isinstance(value, dict):
        yield path, value
        for key, child in value.items():
            yield from objects(child, (*path, key))
    elif isinstance(value, list):
        for index, child in enumerate(value):
            yield from objects(child, (*path, index))


def at(value: JSON, path: Path_) -> dict[str, JSON]:
    node = value
    for step in path:
        if isinstance(node, dict) and isinstance(step, str):
            node = node[step]
        elif isinstance(node, list) and isinstance(step, int):
            node = node[step]
    if not isinstance(node, dict):
        raise SystemExit(f"{path} is not an object")
    return node


def mutations(document: Document) -> Iterator[tuple[str, Document]]:
    """Copies of the document that the schema must reject."""
    for path, _ in objects(document.value):
        if any(isinstance(step, str) and step in FREE_FORM for step in path):
            continue
        copied = copy.deepcopy(document.value)
        at(copied, path)[UNKNOWN_FIELD] = 1
        yield f"unknown field at /{'/'.join(map(str, path))}", Document(document.origin, document.schema, copied)
    key = VERSION_KEYS.get(document.schema)
    if key is not None and isinstance(document.value, dict) and key in document.value:
        without = {k: v for k, v in document.value.items() if k != key}
        yield f"no {key}", Document(document.origin, document.schema, without)
        other = dict(document.value)
        other[key] = 2
        yield f"{key} 2", Document(document.origin, document.schema, other)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--schemas", type=Path, required=True)
    parser.add_argument("--samples", type=Path, required=True)
    parser.add_argument("--fixtures", type=Path, default=Path(__file__).parent / "fixtures")
    parser.add_argument("--verbose", action="store_true", help="say why each invalid fixture is rejected")
    args = parser.parse_args()

    schemas = Schemas(args.schemas)
    failures: list[str] = []
    valid = [*sample_documents(args.samples), *checker_inputs(args.samples)]
    covered: dict[str, int] = {}

    for document in valid:
        covered[document.schema] = covered.get(document.schema, 0) + 1
        failures.extend(f"{document.origin}: {e}" for e in schemas.errors(document))

    for fixture in fixtures(args.fixtures):
        origin = fixture.origin
        # An invalid fixture only proves something if it names a schema and has a
        # document to judge: "no schema named X" would count as a rejection.
        if fixture.schema not in schemas.names:
            failures.append(f"{origin}: no schema named {fixture.schema}")
            continue
        if not fixture.documents:
            failures.append(f"{origin}: holds no documents")
            continue
        rejected = [d for d in fixture.documents if schemas.errors(d)]
        if fixture.expected_valid:
            for document in fixture.documents:
                covered[document.schema] = covered.get(document.schema, 0) + 1
                failures.extend(f"{origin}: {e}" for e in schemas.errors(document))
                valid.append(document)
        elif not rejected:
            failures.append(f"{origin}: expected to be rejected, but every document is valid")
        elif args.verbose:
            print(f"{origin}: {schemas.errors(rejected[0])[0]}")

    mutated = 0
    for document in valid:
        for what, mutant in mutations(document):
            mutated += 1
            if not schemas.errors(mutant):
                failures.append(f"{document.origin}: accepted with {what}")

    for name in sorted(EXPECTED | {"checker-input"}):
        if name not in covered:
            failures.append(f"no document of {name} was found in the samples")
    for name in schemas.names:
        if name != "defs" and name not in covered:
            failures.append(f"schema {name} validated no document")

    for name in sorted(covered):
        print(f"{name:16} {covered[name]:4} documents")
    print(f"{mutated} mutated documents rejected" if not failures else f"{len(failures)} problem(s)")
    for failure in failures:
        print(f"FAIL {failure}", file=sys.stderr)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
