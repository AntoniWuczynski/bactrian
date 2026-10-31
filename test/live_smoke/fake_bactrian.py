# pyright: strict
"""A stand-in for bactrian that test_live_smoke.py puts on PATH.

It writes what bactrian writes for one run: the --json report, meta.json,
trace.jsonl and the reserve and settle lines of .bactrian/ledger.jsonl, in the
current directory. A record makes one live call and, as if another process
shared the ledger, a reservation of a different owner. An offline replay
replays that call. An until-divergence replay replays it and makes one more
live call. The probe agent (dev/live-smoke-probe.py) makes no call and
replies FAKE_PROBE_REPLY, "placeholder" by default.

The experiment commands are faked too: `check` passes, `run` makes the six runs
of dev/live-compare (luna on openai, haiku on anthropic, 3 repeats each) in an
experiment of its own, with its own ledger, `compare` prints the arms and a
`replay` with AGENT_PROVIDER set replays one run on that provider.

Faults, by environment variable: FAKE_REPLAY_COST_ERROR is added to the cost
in meta.json of an until-divergence replay. FAKE_LEAK=file writes the first 12
characters of the real key into the run directory, FAKE_LEAK=output prints
them, and FAKE_LEAK=anthropic-file writes those of the Anthropic key. FAKE_INTERRUPT=1
interrupts the parent, as Ctrl-C does, after the first agent record has made its
call, or in the middle of the experiment. FAKE_RUN_STATUS is the status of every
run of the experiment ("passed" by default), FAKE_RUN_EXIT its exit status,
FAKE_RUN_COST_ERROR is added to the cost in meta.json of each of its runs and
FAKE_CHECK_EXIT is the exit status of `check`."""

from __future__ import annotations

import json
import os
import signal
import sys
import time
from pathlib import Path
from typing import TextIO

RECORDED = (100, 10, 0.001)
LIVE_TAIL = (200, 20, 0.002)
OTHER_OWNER = "another-process"


def main() -> None:
    args = sys.argv[1:]
    sub = args[0]
    runs = Path(".bactrian") / "runs"
    runs.mkdir(parents=True, exist_ok=True)
    if sub in ("check", "run", "compare"):
        sys.exit(experiment_command(sub, args, runs))
    options = args[: args.index("--")]
    passed = [options[i + 1] for i, a in enumerate(options) if a == "--env"]
    if sub == "replay" and "AGENT_PROVIDER" in passed:
        with (Path(".bactrian") / "replay-env.jsonl").open("a") as f:
            f.write(json.dumps({n: os.environ.get(n) for n in passed}) + "\n")
    model = os.environ["AGENT_MODEL"]
    provider = os.environ.get("AGENT_PROVIDER", "openai")
    run_id = f"{sub}-{len(list(runs.iterdir()))}"
    run_dir = runs / run_id
    run_dir.mkdir()
    probe = any(a.endswith("dev/live-smoke-probe.py") for a in args)
    replayed: list[tuple[int, int, float]] = []
    live: list[tuple[int, int, float]] = []
    if probe:
        pass
    elif sub == "record":
        live = [RECORDED]
    elif "until-divergence" in options:
        replayed, live = [RECORDED], [LIVE_TAIL]
    else:
        replayed = [RECORDED]
    charges = [(run_id, usd) for _, _, usd in live]
    if sub == "record" and not probe:
        charges.append((OTHER_OWNER, 0.5))
    ledger = Path(".bactrian") / "ledger.jsonl"
    first_id = len(ledger.read_text().splitlines()) if ledger.exists() else 0
    with ledger.open("a") as f:
        for ident, (owner, usd) in enumerate(charges, start=first_id):
            common = {"id": ident, "model": model, "provider": provider}
            reserve = {"event": "reserve", "owner": owner, "reserved_usd": 0.01}
            f.write(json.dumps(common | reserve) + "\n")
            f.write(json.dumps(common | {"event": "settle", "usd": usd}) + "\n")
    if os.environ.get("FAKE_INTERRUPT") and not probe:
        os.kill(os.getppid(), signal.SIGINT)
        time.sleep(30)
    key_prefix = os.environ["OPENAI_API_KEY"][:12]
    if os.environ.get("FAKE_LEAK") == "file":
        (run_dir / "leak.txt").write_text(key_prefix)
    if os.environ.get("FAKE_LEAK") == "anthropic-file":
        (run_dir / "leak.txt").write_text(os.environ["ANTHROPIC_API_KEY"][:12])
    if os.environ.get("FAKE_LEAK") == "output":
        print(key_prefix, file=sys.stderr)
    error = float(os.environ.get("FAKE_REPLAY_COST_ERROR", "0")) if replayed and live else 0.0
    cost = sum(usd for owner, usd in charges if owner == run_id) + error
    (run_dir / "meta.json").write_text(json.dumps({"cost_usd": cost}))
    with (run_dir / "trace.jsonl").open("w") as f:
        for tokens_in, tokens_out, _ in replayed + live:
            call = {
                "type": "model_call",
                "gen_ai.usage.input_tokens": tokens_in,
                "gen_ai.usage.output_tokens": tokens_out,
            }
            f.write(json.dumps(call) + "\n")
        if probe:
            keys = args[[a.endswith("live-smoke-probe.py") for a in args].index(True) + 1 :]
            reply = os.environ.get("FAKE_PROBE_REPLY", " ".join(["placeholder"] * len(keys)))
            f.write(json.dumps({"type": "reply", "content": reply}) + "\n")
    findings = {"exchanges": len(replayed) + len(live), "refused_keys": 0}
    report = {"run_id": run_id, "run_dir": str(run_dir), "findings": findings}
    print(json.dumps(report))


VARIANTS = (("luna", "openai", "gpt-6-luna"), ("haiku", "anthropic", "claude-haiku-5-5"))
REPEATS = 3


def compare_report(experiment_id: str) -> dict[str, object]:
    rows = [
        {
            "variant": variant,
            "passes": REPEATS,
            "finished": REPEATS,
            "cost_usd": {"median": RECORDED[2]},
            "tokens": {"median": float(RECORDED[0] + RECORDED[1])},
        }
        for variant, _, _ in VARIANTS
    ]
    return {"experiment_id": experiment_id, "arms": {"rows": rows}}


def write_experiment_run(runs: Path, ledger: TextIO, index: int, provider: str, model: str) -> str:
    """One run of the experiment: its directory and its two lines of the ledger."""
    run_id = f"run-{index}"
    run_dir = runs / run_id
    run_dir.mkdir()
    common = {"id": index, "model": model, "provider": provider}
    reserve = {"event": "reserve", "owner": run_id, "reserved_usd": 0.01}
    ledger.write(json.dumps(common | reserve) + "\n")
    ledger.write(json.dumps(common | {"event": "settle", "usd": RECORDED[2]}) + "\n")
    ledger.flush()
    cost = RECORDED[2] + float(os.environ.get("FAKE_RUN_COST_ERROR", "0"))
    findings = {"exchanges": 2, "refused_keys": 0}
    (run_dir / "meta.json").write_text(json.dumps({"cost_usd": cost, "findings": findings}))
    call = {
        "type": "model_call",
        "gen_ai.usage.input_tokens": RECORDED[0],
        "gen_ai.usage.output_tokens": RECORDED[1],
    }
    (run_dir / "trace.jsonl").write_text(json.dumps(call) + "\n")
    if os.environ.get("FAKE_LEAK") == "experiment":
        (run_dir / "leak.txt").write_text(os.environ["ANTHROPIC_API_KEY"][:12])
    return run_id


def experiment_command(sub: str, args: list[str], runs: Path) -> int:
    if sub == "check":
        print(json.dumps({"passed": True}))
        return int(os.environ.get("FAKE_CHECK_EXIT", "0"))
    if sub == "compare":
        print(json.dumps(compare_report(args[args.index("--json") + 1])))
        return 0
    experiments = Path(".bactrian") / "experiments"
    experiment_id = f"experiment-{len(list(experiments.iterdir())) if experiments.exists() else 0}"
    directory = experiments / experiment_id
    directory.mkdir(parents=True)
    status = os.environ.get("FAKE_RUN_STATUS", "passed")
    entries: list[dict[str, object]] = []
    with (directory / "ledger.jsonl").open("w") as ledger:
        for repeat in range(1, REPEATS + 1):
            for variant, provider, model in VARIANTS:
                index = len(entries)
                run_id = write_experiment_run(runs, ledger, index, provider, model)
                entries.append(
                    {
                        "index": index,
                        "variant": variant,
                        "repeat": repeat,
                        "status": status,
                        "run_id": run_id,
                    }
                )
                if os.environ.get("FAKE_INTERRUPT") and len(entries) == 2:
                    os.kill(os.getppid(), signal.SIGINT)
                    time.sleep(30)
    print(json.dumps({"experiment_id": experiment_id, "runs": entries}))
    return int(os.environ.get("FAKE_RUN_EXIT", "0"))


if __name__ == "__main__":
    main()
