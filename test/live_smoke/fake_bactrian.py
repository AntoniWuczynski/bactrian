# pyright: strict
"""A stand-in for bactrian that test_live_smoke.py puts on PATH.

It writes what bactrian writes for one run: the --json report, meta.json,
trace.jsonl and the reserve and settle lines of .bactrian/ledger.jsonl, in the
current directory. A record makes one live call and, as if another process
shared the ledger, a reservation of a different owner. An offline replay
replays that call. An until-divergence replay replays it and makes one more
live call. The probe agent (dev/live-smoke-probe.py) makes no call and
replies FAKE_PROBE_REPLY, "placeholder" by default.

Faults, by environment variable: FAKE_REPLAY_COST_ERROR is added to the cost
in meta.json of an until-divergence replay. FAKE_LEAK=file writes the first 12
characters of the real key into the run directory, FAKE_LEAK=output prints
them. FAKE_INTERRUPT=1 interrupts the parent, as Ctrl-C does, after the first
agent record has made its call."""

from __future__ import annotations

import json
import os
import signal
import sys
import time
from pathlib import Path

RECORDED = (100, 10, 0.001)
LIVE_TAIL = (200, 20, 0.002)
OTHER_OWNER = "another-process"


def main() -> None:
    args = sys.argv[1:]
    sub = args[0]
    options = args[: args.index("--")]
    model = os.environ["AGENT_MODEL"]
    runs = Path(".bactrian") / "runs"
    runs.mkdir(parents=True, exist_ok=True)
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
            common = {"id": ident, "model": model, "provider": "openai"}
            reserve = {"event": "reserve", "owner": owner, "reserved_usd": 0.01}
            f.write(json.dumps(common | reserve) + "\n")
            f.write(json.dumps(common | {"event": "settle", "usd": usd}) + "\n")
    if os.environ.get("FAKE_INTERRUPT") and not probe:
        os.kill(os.getppid(), signal.SIGINT)
        time.sleep(30)
    key_prefix = os.environ["OPENAI_API_KEY"][:12]
    if os.environ.get("FAKE_LEAK") == "file":
        (run_dir / "leak.txt").write_text(key_prefix)
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
            reply = os.environ.get("FAKE_PROBE_REPLY", "placeholder")
            f.write(json.dumps({"type": "reply", "content": reply}) + "\n")
    findings = {"exchanges": len(replayed) + len(live), "refused_keys": 0}
    report = {"run_id": run_id, "run_dir": str(run_dir), "findings": findings}
    print(json.dumps(report))


if __name__ == "__main__":
    main()
