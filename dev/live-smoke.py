# /// script
# requires-python = ">=3.12"
# ///
# pyright: strict
"""Opt-in live smoke test of the example agents against a real provider.

Never run in CI. It spends real money, so it refuses to start unless every
variable below is set explicitly:

    BACTRIAN_LIVE_SMOKE=1       confirms you want to spend
    OPENAI_API_KEY              the real key (the example agents speak the
                                OpenAI chat completions protocol)
    BACTRIAN_CAP_OPENAI         spend cap in US dollars, above 0 and at most
                                0.10. It is cumulative: bactrian replays the
                                whole .bactrian/ledger.jsonl of this checkout,
                                so earlier spend counts against it, and so
                                does every until-divergence replay that goes
                                live
    BACTRIAN_SMOKE_MODEL        the model the agents call (AGENT_MODEL)
    BACTRIAN_SMOKE_LEDGER       a JSON-lines file the spend is appended to,
                                one line per model call, in the format of
                                dev/capture: date, provider, model,
                                input_tokens, output_tokens, usd

It also refuses to start when BACTRIAN_UPSTREAM_OPENAI is set. The smoke would
then pass without reaching OpenAI, and over an http override bactrian would
send the real key in cleartext.

Usage, from the repository root with `bactrian` on PATH, after building the
TypeScript agent (`pnpm build` in examples/agents/typescript) and syncing the
Python one (`uv sync --locked` in examples/agents/python):

    BACTRIAN_LIVE_SMOKE=1 BACTRIAN_CAP_OPENAI=0.05 BACTRIAN_SMOKE_MODEL=<model> \\
        BACTRIAN_SMOKE_LEDGER=<file> uv run dev/live-smoke.py

For each agent it records one short turn, replays it offline, and replays it
with `--mode until-divergence`. It checks that every run exits 0, that the
record made at least one exchange and no request was refused for using a key
other than bactrian's placeholder, that the cost in `meta.json` matches the
settled spend in `.bactrian/ledger.jsonl`, and that no run directory and none
of bactrian's output holds the first 12 characters of the real key. The spend
of every bactrian call is read from the ledger and logged whatever the exit
code.

Caveat: this runs under bactrian's default sandbox. On macOS that is Seatbelt,
under which the agent can read the launch environment of your other processes
(docs/decisions/0010), and both this script and bactrian hold the real key.
The key check shows only that the key was not written to disk or printed, not
that the agent could not see it. The example agents are trusted code. Use a
throwaway key with a low limit."""

from __future__ import annotations

import json
import math
import os
import subprocess
import sys
from dataclasses import dataclass, field
from datetime import UTC, datetime
from pathlib import Path
from typing import cast

ROOT = Path(__file__).resolve().parents[1]
BACTRIAN_LEDGER = ROOT / ".bactrian" / "ledger.jsonl"
TURN = "What is the weather in Paris?"
MAX_CAP_USD = 0.10
KEY_PREFIX_LENGTH = 12
AGENTS: dict[str, list[str]] = {
    "typescript": ["node", "examples/agents/typescript/dist/agent.js"],
    "python": [
        "uv",
        "run",
        "--no-sync",
        "--project",
        "examples/agents/python",
        "examples/agents/python/agent.py",
    ],
}


def require_env(name: str) -> str:
    value = os.environ.get(name, "")
    if not value:
        sys.exit(f"live-smoke: {name} is not set")
    return value


def cap_from_env() -> float:
    raw = require_env("BACTRIAN_CAP_OPENAI")
    try:
        cap = float(raw)
    except ValueError:
        sys.exit(f"live-smoke: BACTRIAN_CAP_OPENAI is not a number: {raw}")
    if not math.isfinite(cap) or cap <= 0 or cap > MAX_CAP_USD:
        sys.exit(f"live-smoke: BACTRIAN_CAP_OPENAI must be above 0 and at most {MAX_CAP_USD}")
    return cap


def string_keyed(value: object) -> dict[str, object] | None:
    if not isinstance(value, dict):
        return None
    pairs = cast(dict[object, object], value)
    return {k: v for k, v in pairs.items() if isinstance(k, str)}


def parse_object(text: str) -> dict[str, object] | None:
    try:
        parsed: object = json.loads(text)
    except json.JSONDecodeError:
        return None
    return string_keyed(parsed)


def object_at(obj: dict[str, object] | None, key: str) -> dict[str, object] | None:
    return None if obj is None else string_keyed(obj.get(key))


def int_at(obj: dict[str, object] | None, key: str) -> int | None:
    value = None if obj is None else obj.get(key)
    return value if isinstance(value, int) and not isinstance(value, bool) else None


def number_at(obj: dict[str, object] | None, key: str) -> float | None:
    value = None if obj is None else obj.get(key)
    if isinstance(value, bool) or not isinstance(value, int | float):
        return None
    return float(value)


def str_at(obj: dict[str, object] | None, key: str) -> str | None:
    value = None if obj is None else obj.get(key)
    return value if isinstance(value, str) else None


def json_lines(text: str) -> list[dict[str, object]]:
    return [o for line in text.splitlines() if (o := parse_object(line)) is not None]


@dataclass(frozen=True)
class Spend:
    """One model call of the ledger. A gap or an unsettled reservation counts
    at its reserved amount, as bactrian charges it."""

    model: str
    usd: float


def spends_since(offset: int, owner: str | None) -> list[Spend]:
    """The spend of the reservations appended after byte `offset` by `owner`,
    the run id. Other processes can share the ledger. Without a run id, every
    reservation counts."""
    if not BACTRIAN_LEDGER.exists():
        return []
    with BACTRIAN_LEDGER.open("rb") as f:
        f.seek(offset)
        events = json_lines(f.read().decode())
    reserved: dict[int, Spend] = {}
    ended: dict[int, float] = {}
    for event in events:
        ident = int_at(event, "id")
        kind = str_at(event, "event")
        if ident is None:
            continue
        if kind == "reserve" and owner in (None, str_at(event, "owner")):
            reserved[ident] = Spend(
                str_at(event, "model") or "", number_at(event, "reserved_usd") or 0.0
            )
        elif kind in ("settle", "gap"):
            ended[ident] = number_at(event, "usd") or 0.0
    return [Spend(r.model, ended.get(i, r.usd)) for i, r in reserved.items()]


def tokens_of(run_dir: Path | None) -> list[tuple[int, int] | None]:
    """Input and output tokens of each model call in the run's trace."""
    if run_dir is None or not (run_dir / "trace.jsonl").exists():
        return []
    calls: list[tuple[int, int] | None] = []
    for entry in json_lines((run_dir / "trace.jsonl").read_text()):
        if entry.get("type") != "model_call":
            continue
        tokens_in = int_at(entry, "gen_ai.usage.input_tokens")
        tokens_out = int_at(entry, "gen_ai.usage.output_tokens")
        calls.append(None if tokens_in is None or tokens_out is None else (tokens_in, tokens_out))
    return calls


def cost_of(run_dir: Path) -> float | None:
    meta = run_dir / "meta.json"
    return number_at(parse_object(meta.read_text()), "cost_usd") if meta.exists() else None


@dataclass(frozen=True)
class Call:
    code: int
    report: dict[str, object] | None
    run_dir: Path | None
    spends: list[Spend]


@dataclass
class Session:
    env: dict[str, str]
    ledger: Path
    needle: bytes
    outputs: list[str] = field(default_factory=lambda: list[str]())
    run_dirs: list[Path] = field(default_factory=lambda: list[Path]())
    problems: list[str] = field(default_factory=lambda: list[str]())

    def invoke(self, label: str, sub: str, options: list[str], command: list[str]) -> Call:
        """Runs one bactrian command and logs the spend it made, whatever its
        exit code. `--json` and `--env` go before the `--` that ends bactrian's
        options, because everything after it is the agent's command line."""
        offset = BACTRIAN_LEDGER.stat().st_size if BACTRIAN_LEDGER.exists() else 0
        done = subprocess.run(
            ["bactrian", sub, "--json", "--env", "AGENT_MODEL", *options, "--", *command],
            cwd=ROOT,
            env=self.env,
            capture_output=True,
            text=True,
            check=False,
        )
        self.outputs += [done.stdout, done.stderr]
        report = parse_object(done.stdout)
        dir_field = str_at(report, "run_dir")
        run_dir = ROOT / dir_field if dir_field else None
        if run_dir is not None:
            self.run_dirs.append(run_dir)
        spends = spends_since(offset, str_at(report, "run_id"))
        self.log(spends, tokens_of(run_dir))
        if report is None:
            self.problems.append(f"{label}: no JSON report, exit {done.returncode}")
        return Call(done.returncode, report, run_dir, spends)

    def log(self, spends: list[Spend], tokens: list[tuple[int, int] | None]) -> None:
        # An until-divergence replay that goes live made only the last calls of
        # its trace live.
        live = len(tokens) - len(spends)
        known = tokens[live:] if live >= 0 else [None] * len(spends)
        with self.ledger.open("a") as f:
            for spend, used in zip(spends, known, strict=True):
                entry = {
                    "date": datetime.now(UTC).isoformat(timespec="seconds"),
                    "provider": "openai",
                    "model": spend.model,
                    "input_tokens": None if used is None else used[0],
                    "output_tokens": None if used is None else used[1],
                    "usd": spend.usd,
                }
                f.write(json.dumps(entry) + "\n")

    def leaks(self) -> list[str]:
        found = [
            str(p)
            for d in self.run_dirs
            if d.is_dir()
            for p in d.rglob("*")
            if p.is_file() and self.needle in p.read_bytes()
        ]
        if any(self.needle in o.encode() for o in self.outputs):
            found.append("bactrian's output")
        return found


def check_record(name: str, call: Call, session: Session, model: str) -> str | None:
    """Checks the record and returns its run id, or None when it cannot be replayed."""
    if call.code != 0:
        session.problems.append(f"{name}: record exited {call.code}")
    findings = object_at(call.report, "findings")
    exchanges = int_at(findings, "exchanges")
    if exchanges is None or exchanges < 1:
        session.problems.append(f"{name}: record made {exchanges} exchanges, expected at least 1")
    refused = int_at(findings, "refused_keys")
    if refused != 0:
        session.problems.append(
            f"{name}: {refused} requests were refused for a key that is not the placeholder"
        )
    if any(s.model != model for s in call.spends):
        models = sorted({s.model for s in call.spends})
        session.problems.append(f"{name}: the ledger priced {models}, expected {model}")
    cost = None if call.run_dir is None else cost_of(call.run_dir)
    settled = sum(s.usd for s in call.spends)
    if cost is None or not math.isclose(cost, settled, rel_tol=1e-9, abs_tol=1e-12):
        session.problems.append(f"{name}: meta.json cost {cost} differs from the ledger's {settled}")
    print(f"{name}: record cost {settled} over {len(call.spends)} calls")
    return str_at(call.report, "run_id")


def smoke(name: str, command: list[str], session: Session, model: str) -> None:
    record = session.invoke(name, "record", ["--turn", TURN], command)
    run_id = check_record(name, record, session, model)
    if run_id is None or record.code != 0:
        return
    for label, extra in (("replay", []), ("until-divergence", ["--mode", "until-divergence"])):
        replay = session.invoke(f"{name} {label}", "replay", [run_id, *extra], command)
        if replay.code != 0:
            session.problems.append(f"{name}: {label} exited {replay.code}")
        if replay.spends:
            print(f"{name}: {label} went live, cost {sum(s.usd for s in replay.spends)}")


def main() -> None:
    if os.environ.get("BACTRIAN_LIVE_SMOKE") != "1":
        sys.exit("live-smoke: set BACTRIAN_LIVE_SMOKE=1 to spend real money")
    upstream = os.environ.get("BACTRIAN_UPSTREAM_OPENAI", "")
    if upstream:
        sys.exit(
            f"live-smoke: unset BACTRIAN_UPSTREAM_OPENAI ({upstream}), the smoke must reach OpenAI"
        )
    key = require_env("OPENAI_API_KEY")
    cap_from_env()
    model = require_env("BACTRIAN_SMOKE_MODEL")
    ledger = Path(require_env("BACTRIAN_SMOKE_LEDGER"))
    ledger.open("a").close()
    session = Session({**os.environ, "AGENT_MODEL": model}, ledger, key[:KEY_PREFIX_LENGTH].encode())
    for name, command in AGENTS.items():
        smoke(name, command, session, model)
    session.problems += [f"the real key is in {leak}" for leak in session.leaks()]
    for p in session.problems:
        print(f"FAIL {p}", file=sys.stderr)
    sys.exit(1 if session.problems else 0)


if __name__ == "__main__":
    main()
