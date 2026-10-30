# pyright: strict
"""dev/live-smoke.py against fake_bactrian.py on PATH, in a scratch copy of the
repository layout, with a fake key. No provider is reached."""

from __future__ import annotations

import importlib.util
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from typing import cast

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
SCRIPT = REPO / "dev" / "live-smoke.py"
PROBE = REPO / "dev" / "live-smoke-probe.py"
PRICING = REPO / "lib" / "core" / "pricing.ml"
MODEL = "gpt-6-luna"


class LiveSmoke(unittest.TestCase):
    def setUp(self) -> None:
        scratch = tempfile.TemporaryDirectory()
        self.addCleanup(scratch.cleanup)
        self.root = Path(scratch.name)
        (self.root / "dev").mkdir()
        shutil.copy(SCRIPT, self.root / "dev" / "live-smoke.py")
        bin_dir = self.root / "bin"
        bin_dir.mkdir()
        fake = bin_dir / "bactrian"
        fake.write_text(
            "#!/bin/sh\n"
            f'exec {shlex.quote(sys.executable)} {shlex.quote(str(HERE / "fake_bactrian.py"))} "$@"\n'
        )
        fake.chmod(0o755)
        self.smoke_ledger = self.root / "smoke.jsonl"
        self.env: dict[str, str] = {
            "PATH": f"{bin_dir}{os.pathsep}{os.environ['PATH']}",
            "BACTRIAN_LIVE_SMOKE": "1",
            "OPENAI_API_KEY": "sk-test-not-a-real-key",
            "BACTRIAN_CAP_OPENAI": "0.05",
            "BACTRIAN_SMOKE_MODEL": MODEL,
            "BACTRIAN_SMOKE_LEDGER": str(self.smoke_ledger),
        }

    def smoke(
        self, without: tuple[str, ...] = (), **extra: str
    ) -> subprocess.CompletedProcess[str]:
        env = {k: v for k, v in (self.env | extra).items() if k not in without}
        return subprocess.run(
            [sys.executable, "-I", str(self.root / "dev" / "live-smoke.py")],
            env=env,
            capture_output=True,
            text=True,
            check=False,
        )

    def assert_refused(self, done: subprocess.CompletedProcess[str], message: str) -> None:
        """The script refused with `message` before it ran bactrian or logged anything."""
        self.assertNotEqual(done.returncode, 0)
        self.assertIn(message, done.stderr)
        self.assertFalse((self.root / ".bactrian").exists(), "bactrian ran")
        self.assertFalse(self.smoke_ledger.exists(), "the spend log was created")

    def test_refuses_an_upstream_override(self) -> None:
        done = self.smoke(BACTRIAN_UPSTREAM_OPENAI="http://127.0.0.1:9")
        self.assert_refused(done, "unset BACTRIAN_UPSTREAM_OPENAI")
        self.assertNotIn("127.0.0.1", done.stderr)

    def test_refuses_a_cap_outside_the_range(self) -> None:
        for cap in ("0", "-0.01", "0.11", "nan", "inf", "abc", ""):
            with self.subTest(cap=cap):
                self.assert_refused(self.smoke(BACTRIAN_CAP_OPENAI=cap), "BACTRIAN_CAP_OPENAI")

    def test_refuses_a_missing_cap(self) -> None:
        done = self.smoke(without=("BACTRIAN_CAP_OPENAI",))
        self.assert_refused(done, "BACTRIAN_CAP_OPENAI is not set")

    def test_accepts_the_largest_cap(self) -> None:
        done = self.smoke(BACTRIAN_CAP_OPENAI="0.10")
        self.assertEqual(done.returncode, 0, done.stderr)

    def test_refuses_a_model_that_cannot_fit_the_cap(self) -> None:
        # gpt-6-astra reserves (1000 * 10 + 256 * 50) / 1e6 = 0.0228 for one call.
        done = self.smoke(BACTRIAN_SMOKE_MODEL="gpt-6-astra", BACTRIAN_CAP_OPENAI="0.02")
        self.assert_refused(done, "gpt-6-astra could reserve $0.0228")
        # gpt-6-sol with a large output limit reserves about 1.0.
        done = self.smoke(BACTRIAN_SMOKE_MODEL="gpt-6-sol", AGENT_MAX_TOKENS="100000")
        self.assert_refused(done, "AGENT_MAX_TOKENS=100000")

    def test_accepts_a_model_that_fits_the_cap(self) -> None:
        done = self.smoke(BACTRIAN_SMOKE_MODEL="gpt-6-astra", BACTRIAN_CAP_OPENAI="0.03")
        self.assertEqual(done.returncode, 0, done.stderr)

    def test_refuses_a_model_without_a_price(self) -> None:
        for model in ("nonesuch", "gpt-6-lunar"):
            with self.subTest(model=model):
                self.assert_refused(self.smoke(BACTRIAN_SMOKE_MODEL=model), "no price is known")

    def test_refuses_a_bad_output_limit(self) -> None:
        for limit in ("0", "-5", "abc", "2.5", ""):
            with self.subTest(limit=limit):
                self.assert_refused(self.smoke(AGENT_MAX_TOKENS=limit), "AGENT_MAX_TOKENS")

    def test_the_rates_are_those_of_the_price_table(self) -> None:
        spec = importlib.util.spec_from_file_location("live_smoke_script", SCRIPT)
        assert spec is not None and spec.loader is not None
        module = importlib.util.module_from_spec(spec)
        sys.modules[spec.name] = module  # dataclasses look their module up here
        sys.dont_write_bytecode = True  # no __pycache__ next to the script
        spec.loader.exec_module(module)
        rates = cast(dict[str, tuple[float, float]], module.OPENAI_RATES)
        table = re.findall(
            r'openai "([^"]+)" \(r ([0-9.]+) [0-9.]+ [0-9.]+ ([0-9.]+)\)', PRICING.read_text()
        )
        self.assertEqual(rates, {m: (float(i), float(o)) for m, i, o in table})

    def test_fails_when_the_real_key_is_in_a_run_directory(self) -> None:
        done = self.smoke(FAKE_LEAK="file")
        self.assertEqual(done.returncode, 1, done.stderr)
        self.assertIn("the real key is in", done.stderr)
        self.assertIn("leak.txt", done.stderr)
        self.assertNotIn(self.env["OPENAI_API_KEY"], done.stderr)

    def test_fails_when_the_real_key_is_in_bactrians_output(self) -> None:
        done = self.smoke(FAKE_LEAK="output")
        self.assertEqual(done.returncode, 1, done.stderr)
        self.assertIn("the real key is in bactrian's output", done.stderr)

    def test_does_not_run_the_agents_when_the_key_is_not_the_placeholder(self) -> None:
        for reply in ("other", "unset"):
            with self.subTest(reply=reply):
                done = self.smoke(FAKE_PROBE_REPLY=reply)
                self.assertEqual(done.returncode, 1, done.stderr)
                self.assertIn(f"probe: exit 0, the agent's OPENAI_API_KEY is ['{reply}']", done.stderr)
                self.assertEqual(len(list((self.root / ".bactrian" / "runs").iterdir())), 1)
                shutil.rmtree(self.root / ".bactrian")

    def test_the_probe_names_the_state_of_the_key_without_printing_it(self) -> None:
        turn = json.dumps({"v": 1, "type": "turn", "turn": 3, "content": "hi"})
        for key, expected in (
            ("bactrian-0123abcd", "placeholder"),
            ("sk-test-not-a-real-key", "other"),
            (None, "unset"),
        ):
            with self.subTest(key=key):
                env = {"PATH": os.environ["PATH"]} | ({} if key is None else {"OPENAI_API_KEY": key})
                done = subprocess.run(
                    [sys.executable, "-I", str(PROBE)],
                    input=f"{turn}\n",
                    env=env,
                    capture_output=True,
                    text=True,
                    check=False,
                )
                self.assertEqual(done.returncode, 0, done.stderr)
                reply: object = json.loads(done.stdout)
                self.assertEqual(reply, {"v": 1, "type": "reply", "turn": 3, "content": expected})

    def test_logs_the_spend_of_an_interrupted_call(self) -> None:
        done = self.smoke(FAKE_INTERRUPT="1")
        self.assertNotEqual(done.returncode, 0)
        self.assertIn(0.001, [e["usd"] for e in self.logged()])

    def logged(self) -> list[dict[str, object]]:
        entries: list[dict[str, object]] = []
        for line in self.smoke_ledger.read_text().splitlines():
            entry: object = json.loads(line)
            if not isinstance(entry, dict):
                self.fail(f"not a JSON object: {line}")
            entries.append(cast(dict[str, object], entry))
        return entries

    def test_logs_only_its_own_runs_spend(self) -> None:
        done = self.smoke()
        self.assertEqual(done.returncode, 0, done.stderr)
        records = [e for e in self.logged() if e["usd"] == 0.001]
        self.assertEqual(len(records), 2, self.logged())
        for entry in records:
            self.assertEqual(entry["model"], MODEL)
            self.assertEqual((entry["input_tokens"], entry["output_tokens"]), (100, 10))
        self.assertNotIn(0.5, [e["usd"] for e in self.logged()])

    def test_logs_the_tokens_of_a_replays_live_calls(self) -> None:
        done = self.smoke()
        self.assertEqual(done.returncode, 0, done.stderr)
        tails = [e for e in self.logged() if e["usd"] == 0.002]
        self.assertEqual(len(tails), 2, self.logged())
        for entry in tails:
            self.assertEqual((entry["input_tokens"], entry["output_tokens"]), (200, 20))

    def test_compares_a_replays_cost_with_its_spend(self) -> None:
        done = self.smoke(FAKE_REPLAY_COST_ERROR="0.5")
        self.assertEqual(done.returncode, 1, done.stderr)
        self.assertIn("python until-divergence: meta.json cost", done.stderr)
        self.assertIn("typescript until-divergence: meta.json cost", done.stderr)


if __name__ == "__main__":
    unittest.main()
