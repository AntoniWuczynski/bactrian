# /// script
# requires-python = ">=3.12"
# ///
# pyright: strict
"""Opt-in live smoke test of the example agents against a real provider.

Never run in CI. It spends real money, so it refuses to start unless every
variable below is set explicitly (with --compare, see the end of this text):

    BACTRIAN_LIVE_SMOKE=1       confirms you want to spend
    OPENAI_API_KEY              the real key (the example agents speak the
                                OpenAI chat completions protocol)
    BACTRIAN_CAP_OPENAI         spend cap in US dollars, above 0 and at most
                                0.10. It is cumulative: bactrian replays the
                                whole .bactrian/ledger.jsonl of this checkout,
                                so earlier spend counts against it, and so
                                does every until-divergence replay that goes
                                live
    BACTRIAN_SMOKE_MODEL        the model the agents call (AGENT_MODEL). It
                                must be in bactrian's OpenAI price table
    AGENT_MAX_TOKENS            optional, default 256: the output limit the
                                agents send as max_completion_tokens
    BACTRIAN_SMOKE_LEDGER       a JSON-lines file the spend is appended to,
                                one line per model call, in the format of
                                dev/capture: date, provider, model,
                                input_tokens, output_tokens, usd

bactrian reserves the worst-case cost of a call before it forwards it: the
input estimate plus the request's own output limit. Without a limit that is the
model's maximum output, 65,536 tokens, which would not fit a cap of a few cents
for any but the cheapest models, so the example agents send AGENT_MAX_TOKENS.
The script refuses a model whose worst-case reservation for one call, with
that limit and an input allowance of 1,000 tokens, is above the cap. The rates
it uses are in OPENAI_RATES, which test/live_smoke checks against
lib/core/pricing.ml.

It also refuses to start when BACTRIAN_UPSTREAM_OPENAI is set. The smoke would
then pass without reaching OpenAI, and over an http override bactrian would
send the real key in cleartext.

Usage, from the repository root with `bactrian` on PATH, after building the
TypeScript agent (`pnpm build` in examples/agents/typescript) and syncing the
Python one (`uv sync --locked` in examples/agents/python):

    BACTRIAN_LIVE_SMOKE=1 BACTRIAN_CAP_OPENAI=0.05 BACTRIAN_SMOKE_MODEL=<model> \\
        BACTRIAN_SMOKE_LEDGER=<file> uv run dev/live-smoke.py

First it runs a probe agent (dev/live-smoke-probe.py) under bactrian and
requires that its OPENAI_API_KEY is bactrian's "bactrian-" placeholder, not the
real key. Then, for each agent, it records one short turn, replays it offline, and replays it
with `--mode until-divergence`. It checks that every run exits 0, that the
record made at least one exchange and no request was refused for using a key
other than bactrian's placeholder, that the cost in `meta.json` of every run,
replays included, matches its settled spend in `.bactrian/ledger.jsonl`, and
that no run directory and none of bactrian's output holds the first 12
characters of the real key. The spend
of every bactrian call is read from the ledger and logged whatever the exit
code, and also when the call is interrupted or raises. Then no run id is known,
so every reservation appended meanwhile counts.

With --compare it runs the comparison of dev/live-compare/experiment.yaml
instead: gpt-6-luna against claude-haiku-5-5, both through the TypeScript
agent (AGENT_PROVIDER picks the API), 3 repeats each. It needs both
OPENAI_API_KEY and ANTHROPIC_API_KEY, refuses when BACTRIAN_UPSTREAM_OPENAI or
BACTRIAN_UPSTREAM_ANTHROPIC is set, and takes BACTRIAN_CAP_OPENAI,
BACTRIAN_SMOKE_MODEL (it must be gpt-6-luna, the experiment's) and
BACTRIAN_SMOKE_LEDGER as above. AGENT_MAX_TOKENS is not read: the experiment
file sets 256, and the caps of 0.05 dollars for each provider are in it too.
The script refuses when that file does not set the agent, the variants' models
and limit, and the caps it holds in COMPARE_*, and when one call of either model could reserve more than its
cap, using ANTHROPIC_RATES as well as OPENAI_RATES. It runs the probe, which
checks the placeholder for both keys, then `bactrian check` of the scenario,
`bactrian run` of the experiment, `bactrian compare` of its result and an
offline `bactrian replay` of one run per variant. It checks every exit code
(`run` may exit 1, because a model can fail the scenario's assertion, which is
a result), that every run finished as passed or failed, that the cost in
`meta.json` of every run matches its spend in the experiment's ledger, that
the replays made no live call, and that no run directory and none of
bactrian's output holds the first 12 characters of either real key. Each
spend is logged with the provider of bactrian's ledger event.

    BACTRIAN_LIVE_SMOKE=1 BACTRIAN_CAP_OPENAI=0.05 BACTRIAN_SMOKE_MODEL=gpt-6-luna \\
        BACTRIAN_SMOKE_LEDGER=<file> uv run dev/live-smoke.py --compare

Caveat: this runs under bactrian's default sandbox. On macOS that is Seatbelt,
under which the agent can read the launch environment of your other processes
(docs/decisions/0010), and both this script and bactrian hold the real key.
The key check shows only that the key was not written to disk or printed, not
that the agent could not see it. The example agents are trusted code. Use a
throwaway key with a low limit."""

from __future__ import annotations

import argparse
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
BACTRIAN_DIR = ROOT / ".bactrian"
BACTRIAN_LEDGER = BACTRIAN_DIR / "ledger.jsonl"
RUNS = BACTRIAN_DIR / "runs"
EXPERIMENTS = BACTRIAN_DIR / "experiments"
TURN = "What is the weather in Paris?"
MAX_CAP_USD = 0.10
KEY_PREFIX_LENGTH = 12
DEFAULT_MAX_TOKENS = 256
INPUT_ALLOWANCE_TOKENS = 1_000
# Dollars per million input and output tokens of bactrian's OpenAI and
# Anthropic price tables.
OPENAI_RATES: dict[str, tuple[float, float]] = {
    "gpt-6-luna": (0.10, 0.50),
    "gpt-5-nano": (0.05, 0.40),
    "gpt-6-sol": (2.00, 10.00),
    "gpt-6-astra": (10.00, 50.00),
}
ANTHROPIC_RATES: dict[str, tuple[float, float]] = {
    "claude-haiku-5-5": (0.10, 0.50),
    "claude-sonnet-5-5": (2.00, 10.00),
    "claude-opus-5-5": (4.00, 20.00),
    "claude-fable-5-1": (10.00, 50.00),
}
RATES = {"openai": OPENAI_RATES, "anthropic": ANTHROPIC_RATES}
PROVIDER_NAMES = {"openai": "OpenAI", "anthropic": "Anthropic"}
# What dev/live-compare/experiment.yaml must set, which main checks before
# anything runs: the variants' environment, and the cap for each provider.
COMPARE_EXPERIMENT = "dev/live-compare/experiment.yaml"
COMPARE_SCENARIO = "dev/live-compare/scenarios/weather.yaml"
COMPARE_VARIANTS: dict[str, dict[str, str]] = {
    "luna": {"AGENT_PROVIDER": "openai", "AGENT_MODEL": "gpt-6-luna"},
    "haiku": {"AGENT_PROVIDER": "anthropic", "AGENT_MODEL": "claude-haiku-5-5"},
}
COMPARE_MAX_TOKENS = 256
COMPARE_CAP_USD = 0.05
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
PROBE = [
    "uv",
    "run",
    "--no-sync",
    "--project",
    "examples/agents/python",
    "dev/live-smoke-probe.py",
]
KEY_VARIABLES = {"openai": "OPENAI_API_KEY", "anthropic": "ANTHROPIC_API_KEY"}


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


def max_tokens_from_env() -> int:
    raw = os.environ.get("AGENT_MAX_TOKENS", str(DEFAULT_MAX_TOKENS))
    if not raw.isdecimal() or int(raw) < 1:
        sys.exit(f"live-smoke: AGENT_MAX_TOKENS is not a positive integer: {raw}")
    return int(raw)


def check_reservation(
    provider: str, model: str, max_tokens: int, cap: float, cap_name: str
) -> None:
    """Refuses a model that bactrian would not let make one call under the cap."""
    rates = RATES[provider]
    # As bactrian finds a price: the longest name that is the model or a prefix
    # of it followed by a dash.
    names = [n for n in rates if model == n or model.startswith(n + "-")]
    if not names:
        sys.exit(
            f"live-smoke: no price is known for {model}, so bactrian would refuse it. "
            f"Known models: {', '.join(rates)}"
        )
    rate_in, rate_out = rates[max(names, key=len)]
    worst = (INPUT_ALLOWANCE_TOKENS * rate_in + max_tokens * rate_out) / 1_000_000
    if worst > cap:
        sys.exit(
            f"live-smoke: {model} could reserve ${worst:.4f} for one call with "
            f"AGENT_MAX_TOKENS={max_tokens}, above the {cap_name} cap of ${cap}. "
            "Lower AGENT_MAX_TOKENS, raise the cap (at most 0.10) or pick a cheaper model"
        )


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


def objects_at(obj: dict[str, object] | None, key: str) -> list[dict[str, object]]:
    value = None if obj is None else obj.get(key)
    if not isinstance(value, list):
        return []
    items = cast(list[object], value)
    return [o for item in items if (o := string_keyed(item)) is not None]


def json_lines(text: str) -> list[dict[str, object]]:
    return [o for line in text.splitlines() if (o := parse_object(line)) is not None]


@dataclass(frozen=True)
class Spend:
    """One model call of a ledger: the provider, model and run (`owner`) of its
    reservation. A gap or an unsettled reservation counts at its reserved
    amount, as bactrian charges it."""

    provider: str
    model: str
    owner: str
    usd: float


def spends_since(ledger: Path, offset: int, owner: str | None) -> list[Spend]:
    """The spend of the reservations appended to `ledger` after byte `offset`
    by `owner`, the run id. Other processes can share the ledger. Without a run
    id, every reservation counts."""
    if not ledger.exists():
        return []
    with ledger.open("rb") as f:
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
                str_at(event, "provider") or "unknown",
                str_at(event, "model") or "",
                str_at(event, "owner") or "",
                number_at(event, "reserved_usd") or 0.0,
            )
        elif kind in ("settle", "gap"):
            ended[ident] = number_at(event, "usd") or 0.0
    return [Spend(r.provider, r.model, r.owner, ended.get(i, r.usd)) for i, r in reserved.items()]


def experiment_dirs() -> set[Path]:
    return {d for d in EXPERIMENTS.iterdir() if d.is_dir()} if EXPERIMENTS.is_dir() else set()


def tokens_of(run_dir: Path) -> list[tuple[int, int] | None]:
    """Input and output tokens of each model call in the run's trace."""
    if not (run_dir / "trace.jsonl").exists():
        return []
    calls: list[tuple[int, int] | None] = []
    for entry in json_lines((run_dir / "trace.jsonl").read_text()):
        if entry.get("type") != "model_call":
            continue
        tokens_in = int_at(entry, "gen_ai.usage.input_tokens")
        tokens_out = int_at(entry, "gen_ai.usage.output_tokens")
        calls.append(None if tokens_in is None or tokens_out is None else (tokens_in, tokens_out))
    return calls


def replies_of(run_dir: Path | None) -> list[str]:
    """The content of each reply the agent sent, from the run's trace."""
    if run_dir is None or not (run_dir / "trace.jsonl").exists():
        return []
    entries = json_lines((run_dir / "trace.jsonl").read_text())
    return [c for e in entries if e.get("type") == "reply" and (c := str_at(e, "content"))]


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
    needles: tuple[bytes, ...]
    outputs: list[str] = field(default_factory=lambda: list[str]())
    run_dirs: list[Path] = field(default_factory=lambda: list[Path]())
    problems: list[str] = field(default_factory=lambda: list[str]())
    passed_env: tuple[str, ...] = ("AGENT_MODEL", "AGENT_MAX_TOKENS")

    def invoke(
        self,
        label: str,
        sub: str,
        options: list[str],
        command: list[str] | None,
        env: dict[str, str] | None = None,
    ) -> Call:
        """Runs one bactrian command and logs the spend it made, whatever its
        exit code. `--json` and `--env` go before the `--` that ends bactrian's
        options, because everything after it is the agent's command line. A
        command without an agent (check, run, compare) has neither `--env` nor
        `--`. The spend of an experiment is in its own ledger, and one the
        command made is found among the experiments that did not exist before."""
        offset = BACTRIAN_LEDGER.stat().st_size if BACTRIAN_LEDGER.exists() else 0
        experiments = experiment_dirs()
        done: subprocess.CompletedProcess[str] | None = None
        report: dict[str, object] | None = None
        run_dir: Path | None = None
        env_flags = [] if command is None else [a for n in self.passed_env for a in ("--env", n)]
        agent_tail = [] if command is None else ["--", *command]
        try:
            done = subprocess.run(
                ["bactrian", sub, "--json", *env_flags, *options, *agent_tail],
                cwd=ROOT,
                env=self.env | (env or {}),
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
            for entry in objects_at(report, "runs"):
                if (entry_id := str_at(entry, "run_id")) is not None:
                    self.run_dirs.append(RUNS / entry_id)
        finally:
            # Also on an interrupt or an exception, which leave no report and so
            # no run id: every reservation since `offset` is then logged.
            fresh = sorted(experiment_dirs() - experiments)
            self.run_dirs += fresh
            spends = spends_since(BACTRIAN_LEDGER, offset, str_at(report, "run_id"))
            for directory in fresh:
                spends += spends_since(directory / "ledger.jsonl", 0, None)
            self.log(spends)
        if report is None:
            self.problems.append(f"{label}: no JSON report, exit {done.returncode}")
        return Call(done.returncode, report, run_dir, spends)

    def log(self, spends: list[Spend]) -> None:
        by_owner: dict[str, list[Spend]] = {}
        for spend in spends:
            by_owner.setdefault(spend.owner, []).append(spend)
        with self.ledger.open("a") as f:
            for owner, owned in by_owner.items():
                # An until-divergence replay that goes live made only the last
                # calls of its trace live.
                tokens = tokens_of(RUNS / owner)
                live = len(tokens) - len(owned)
                known = tokens[live:] if live >= 0 else [None] * len(owned)
                for spend, used in zip(owned, known, strict=True):
                    entry = {
                        "date": datetime.now(UTC).isoformat(timespec="seconds"),
                        "provider": spend.provider,
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
            if p.is_file() and any(n in p.read_bytes() for n in self.needles)
        ]
        if any(n in o.encode() for o in self.outputs for n in self.needles):
            found.append("bactrian's output")
        return found


def check_cost(label: str, call: Call, session: Session) -> float:
    """Checks that the run's meta.json cost is its settled spend, and returns that."""
    cost = None if call.run_dir is None else cost_of(call.run_dir)
    settled = sum(s.usd for s in call.spends)
    if cost is None or not math.isclose(cost, settled, rel_tol=1e-9, abs_tol=1e-12):
        session.problems.append(f"{label}: meta.json cost {cost} differs from the ledger's {settled}")
    return settled


def check_findings(
    name: str,
    findings: dict[str, object] | None,
    call: Call,
    session: Session,
    provider: str,
    model: str,
) -> float:
    """Checks what a recorded run found and made, and returns its settled spend."""
    exchanges = int_at(findings, "exchanges")
    if exchanges is None or exchanges < 1:
        session.problems.append(f"{name}: record made {exchanges} exchanges, expected at least 1")
    refused = int_at(findings, "refused_keys")
    if refused != 0:
        session.problems.append(
            f"{name}: {refused} requests were refused for a key that is not the placeholder"
        )
    if any((s.provider, s.model) != (provider, model) for s in call.spends):
        priced = sorted({(s.provider, s.model) for s in call.spends})
        session.problems.append(f"{name}: the ledger priced {priced}, expected {(provider, model)}")
    return check_cost(name, call, session)


def check_record(name: str, call: Call, session: Session, model: str) -> str | None:
    """Checks the record and returns its run id, or None when it cannot be replayed."""
    if call.code != 0:
        session.problems.append(f"{name}: record exited {call.code}")
    findings = object_at(call.report, "findings")
    settled = check_findings(name, findings, call, session, "openai", model)
    print(f"{name}: record cost {settled} over {len(call.spends)} calls")
    return str_at(call.report, "run_id")


def check_placeholder(session: Session, keys: list[str]) -> bool:
    """Runs the probe agent and returns whether each of these key variables of
    the agent is the placeholder."""
    call = session.invoke("probe", "record", ["--turn", TURN], [*PROBE, *keys])
    replies = replies_of(call.run_dir)
    if call.code == 0 and replies == [" ".join(["placeholder"] * len(keys))]:
        return True
    session.problems.append(
        f"probe: exit {call.code}, the agent's {', '.join(keys)} is {replies}, expected the "
        "bactrian- placeholder. Not running the agents"
    )
    return False


def smoke(name: str, command: list[str], session: Session, model: str) -> None:
    record = session.invoke(name, "record", ["--turn", TURN], command)
    run_id = check_record(name, record, session, model)
    if run_id is None or record.code != 0:
        return
    for label, extra in (("replay", []), ("until-divergence", ["--mode", "until-divergence"])):
        replay = session.invoke(f"{name} {label}", "replay", [run_id, *extra], command)
        if replay.code != 0:
            session.problems.append(f"{name}: {label} exited {replay.code}")
        settled = check_cost(f"{name} {label}", replay, session)
        if replay.spends:
            print(f"{name}: {label} went live, cost {settled}")


def compare_experiment_missing(text: str) -> list[str]:
    """The lines of COMPARE_EXPERIMENT, given as `text`, that are not as this
    script assumes: the agent, each variant's environment and the caps, which
    bactrian enforces. caps_usd must be named once, so no other value of it
    applies."""
    _, script = AGENTS["typescript"]
    caps = ", ".join(f"{p}: {COMPARE_CAP_USD}" for p in RATES)
    wanted = [
        f'\n  command: ["node", "../../{script}"]\n',
        *(
            f"\n  {name}:\n    env: {{ AGENT_PROVIDER: {c['AGENT_PROVIDER']}, "
            f'AGENT_MODEL: {c["AGENT_MODEL"]}, AGENT_MAX_TOKENS: "{COMPARE_MAX_TOKENS}" }}\n'
            for name, c in COMPARE_VARIANTS.items()
        ),
        f"\ncaps_usd: {{ {caps}, gemini: 0, openrouter: 0 }}\n",
    ]
    missing = [w.strip() for w in wanted if w not in text]
    if text.count("caps_usd") != 1:
        missing.append("caps_usd named once")
    return missing


def compare_variant_env(variant: str) -> dict[str, str]:
    return COMPARE_VARIANTS[variant] | {"AGENT_MAX_TOKENS": str(COMPARE_MAX_TOKENS)}


def check_experiment_run(session: Session, call: Call) -> dict[str, str]:
    """Checks the experiment's runs against its ledger and returns the id of the
    first run that passed, for each variant."""
    if call.code not in (0, 1):
        session.problems.append(f"run: exited {call.code}, expected 0 or 1")
    by_run: dict[str, list[Spend]] = {}
    for spend in call.spends:
        by_run.setdefault(spend.owner, []).append(spend)
    first_passed: dict[str, str] = {}
    for entry in objects_at(call.report, "runs"):
        variant = str_at(entry, "variant") or "?"
        run_id = str_at(entry, "run_id")
        status = str_at(entry, "status")
        name = f"{variant} repeat {int_at(entry, 'repeat')}"
        if run_id is None or status not in ("passed", "failed"):
            session.problems.append(f"{name}: status {status}, expected passed or failed")
            continue
        run_dir = RUNS / run_id
        meta_file = run_dir / "meta.json"
        meta = parse_object(meta_file.read_text()) if meta_file.exists() else None
        config = COMPARE_VARIANTS.get(variant)
        if config is None:
            session.problems.append(f"{name}: not a variant of the experiment")
            continue
        run = Call(0, None, run_dir, by_run.get(run_id, []))
        provider, model = config["AGENT_PROVIDER"], config["AGENT_MODEL"]
        settled = check_findings(name, object_at(meta, "findings"), run, session, provider, model)
        print(f"{name}: {status}, cost {settled} over {len(run.spends)} calls")
        if status == "passed":
            first_passed.setdefault(variant, run_id)
    return first_passed


def print_arms(report: dict[str, object] | None) -> None:
    for row in objects_at(object_at(report, "arms"), "rows"):
        cost = number_at(object_at(row, "cost_usd"), "median")
        tokens = number_at(object_at(row, "tokens"), "median")
        print(
            f"compare: {str_at(row, 'variant')}: {int_at(row, 'passes')} of "
            f"{int_at(row, 'finished')} passed, median cost {cost}, median tokens {tokens}"
        )


def compare(session: Session) -> None:
    """The comparison of dev/live-compare: check, run, compare, offline replays."""
    checked = session.invoke("check", "check", [COMPARE_SCENARIO], None)
    if checked.code != 0:
        session.problems.append(
            f"check: exited {checked.code}, the golden traces do not hold. Not running"
        )
        return
    ran = session.invoke("run", "run", [COMPARE_EXPERIMENT], None)
    first_passed = check_experiment_run(session, ran)
    by_provider: dict[str, float] = {}
    for spend in ran.spends:
        by_provider[spend.provider] = by_provider.get(spend.provider, 0.0) + spend.usd
    print(f"run: spend by provider {by_provider}")
    experiment = str_at(ran.report, "experiment_id")
    if experiment is not None:
        compared = session.invoke("compare", "compare", [experiment], None)
        if compared.code != 0:
            session.problems.append(f"compare: exited {compared.code}")
        print_arms(compared.report)
    else:
        session.problems.append("run: no experiment id in the report, nothing to compare")
    for variant in COMPARE_VARIANTS:
        run_id = first_passed.get(variant)
        if run_id is None:
            session.problems.append(f"{variant}: no run passed, nothing to replay")
            continue
        label = f"{variant} replay"
        env = compare_variant_env(variant)
        replay = session.invoke(label, "replay", [run_id], AGENTS["typescript"], env)
        if replay.code != 0:
            session.problems.append(f"{label}: exited {replay.code}")
        if replay.spends:
            session.problems.append(f"{label}: went live, {len(replay.spends)} calls")
        check_cost(label, replay, session)


def refuse_upstream_overrides(providers: list[str]) -> None:
    for provider in providers:
        if os.environ.get(f"BACTRIAN_UPSTREAM_{provider.upper()}", ""):
            sys.exit(
                f"live-smoke: unset BACTRIAN_UPSTREAM_{provider.upper()}, "
                f"the smoke must reach {PROVIDER_NAMES[provider]}"
            )


def main() -> None:
    parser = argparse.ArgumentParser(description="Opt-in live smoke test, see the file's header")
    parser.add_argument(
        "--compare", action="store_true", help="run dev/live-compare/experiment.yaml instead"
    )
    comparing = parser.parse_args().compare
    providers = list(RATES) if comparing else ["openai"]
    if os.environ.get("BACTRIAN_LIVE_SMOKE") != "1":
        sys.exit("live-smoke: set BACTRIAN_LIVE_SMOKE=1 to spend real money")
    refuse_upstream_overrides(providers)
    keys = {p: require_env(KEY_VARIABLES[p]) for p in providers}
    cap = cap_from_env()
    model = require_env("BACTRIAN_SMOKE_MODEL")
    if comparing:
        compared = COMPARE_VARIANTS["luna"]["AGENT_MODEL"]
        if model != compared:
            sys.exit(
                f"live-smoke: with --compare BACTRIAN_SMOKE_MODEL must be {compared}, "
                "the experiment's"
            )
        if missing := compare_experiment_missing((ROOT / COMPARE_EXPERIMENT).read_text()):
            sys.exit(f"live-smoke: {COMPARE_EXPERIMENT} does not hold {'; '.join(missing)}")
        max_tokens = COMPARE_MAX_TOKENS
        for config in COMPARE_VARIANTS.values():
            provider, variant_model = config["AGENT_PROVIDER"], config["AGENT_MODEL"]
            limits = [(COMPARE_CAP_USD, f"{COMPARE_EXPERIMENT} {provider}")]
            if provider == "openai":
                limits.append((cap, "BACTRIAN_CAP_OPENAI"))
            for limit, name in limits:
                check_reservation(provider, variant_model, max_tokens, limit, name)
    else:
        max_tokens = max_tokens_from_env()
        check_reservation("openai", model, max_tokens, cap, "BACTRIAN_CAP_OPENAI")
    ledger = Path(require_env("BACTRIAN_SMOKE_LEDGER"))
    ledger.open("a").close()
    env = {**os.environ, "AGENT_MODEL": model, "AGENT_MAX_TOKENS": str(max_tokens)}
    needles = tuple(k[:KEY_PREFIX_LENGTH].encode() for k in keys.values())
    session = Session(env, ledger, needles)
    if comparing:
        session.passed_env = (*session.passed_env, "AGENT_PROVIDER")
    if check_placeholder(session, [KEY_VARIABLES[p] for p in providers]):
        if comparing:
            compare(session)
        else:
            for name, command in AGENTS.items():
                smoke(name, command, session, model)
    session.problems += [f"the real key is in {leak}" for leak in session.leaks()]
    for p in session.problems:
        print(f"FAIL {p}", file=sys.stderr)
    sys.exit(1 if session.problems else 0)


if __name__ == "__main__":
    main()
