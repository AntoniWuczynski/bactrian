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
    BACTRIAN_CAP_OPENAI         spend cap in US dollars for each record
    BACTRIAN_SMOKE_MODEL        the model the agents call (AGENT_MODEL)
    BACTRIAN_SMOKE_LEDGER       optional: a JSON-lines file to append each
                                record's spend to

Usage, from the repository root with `bactrian` on PATH, after building the
TypeScript agent (`pnpm build` in examples/agents/typescript) and syncing the
Python one (`uv sync --locked` in examples/agents/python):

    BACTRIAN_LIVE_SMOKE=1 BACTRIAN_CAP_OPENAI=0.05 BACTRIAN_SMOKE_MODEL=<model> \\
        uv run dev/live-smoke.py

For each agent it records one short turn, replays it offline, replays it with
`--mode until-divergence`, and checks that the replays pass, that the run's
`meta.json` carries a cost, and that the real key appears nowhere in the run
directory. Everything runs under the default sandbox."""

from __future__ import annotations

import json
import os
import subprocess
import sys
from datetime import UTC, datetime
from pathlib import Path
from typing import cast

ROOT = Path(__file__).resolve().parents[1]
TURN = "What is the weather in Paris?"
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


def bactrian(args: list[str], env: dict[str, str]) -> tuple[int, str]:
    done = subprocess.run(
        ["bactrian", *args, "--json"],
        cwd=ROOT,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    return done.returncode, done.stdout


def run_id_of(output: str) -> str:
    parsed = cast(dict[str, object], json.loads(output))
    run_id = parsed.get("run_id")
    if not isinstance(run_id, str):
        sys.exit(f"live-smoke: no run_id in output: {output[:200]}")
    return run_id


def run_dir_leaks(run_dir: Path, secret: str) -> list[str]:
    needle = secret.encode()
    return [
        str(p.relative_to(run_dir))
        for p in run_dir.rglob("*")
        if p.is_file() and needle in p.read_bytes()
    ]


def cost_of(run_dir: Path) -> float | None:
    meta = cast(dict[str, object], json.loads((run_dir / "meta.json").read_text()))
    cost = meta.get("cost_usd")
    return float(cost) if isinstance(cost, int | float) else None


def smoke(name: str, command: list[str], env: dict[str, str], key: str) -> list[str]:
    problems: list[str] = []
    code, out = bactrian(["record", "--turn", TURN, "--", *command], env)
    if code != 0:
        return [f"{name}: record exited {code}"]
    run_id = run_id_of(out)
    run_dir = ROOT / ".bactrian" / "runs" / run_id
    for label, extra in (("replay", []), ("until-divergence", ["--mode", "until-divergence"])):
        replay_code, _ = bactrian(["replay", run_id, *extra, "--", *command], env)
        if replay_code != 0:
            problems.append(f"{name}: {label} exited {replay_code}")
    cost = cost_of(run_dir)
    if cost is None:
        problems.append(f"{name}: meta.json has no cost_usd")
    leaks = run_dir_leaks(run_dir, key)
    if leaks:
        problems.append(f"{name}: the real key is in {', '.join(leaks)}")
    ledger = os.environ.get("BACTRIAN_SMOKE_LEDGER")
    if ledger and cost is not None:
        entry = {
            "date": datetime.now(UTC).isoformat(timespec="seconds"),
            "provider": "openai",
            "model": env["AGENT_MODEL"],
            "usd": cost,
            "run_id": run_id,
        }
        with Path(ledger).open("a") as f:
            f.write(json.dumps(entry) + "\n")
    print(f"{name}: run {run_id} cost {cost}")
    return problems


def main() -> None:
    if os.environ.get("BACTRIAN_LIVE_SMOKE") != "1":
        sys.exit("live-smoke: set BACTRIAN_LIVE_SMOKE=1 to spend real money")
    key = require_env("OPENAI_API_KEY")
    require_env("BACTRIAN_CAP_OPENAI")
    env = {**os.environ, "AGENT_MODEL": require_env("BACTRIAN_SMOKE_MODEL")}
    problems = [p for n, c in AGENTS.items() for p in smoke(n, c, env, key)]
    for p in problems:
        print(f"FAIL {p}", file=sys.stderr)
    sys.exit(1 if problems else 0)


if __name__ == "__main__":
    main()
