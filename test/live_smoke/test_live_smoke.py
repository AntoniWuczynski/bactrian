# pyright: strict
"""dev/live-smoke.py against fake_bactrian.py on PATH, in a scratch copy of the
repository layout, with a fake key. No provider is reached."""

from __future__ import annotations

import json
import os
import shlex
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from typing import cast

HERE = Path(__file__).resolve().parent
SCRIPT = HERE.parents[1] / "dev" / "live-smoke.py"


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
        self.env = {
            "PATH": f"{bin_dir}{os.pathsep}{os.environ['PATH']}",
            "BACTRIAN_LIVE_SMOKE": "1",
            "OPENAI_API_KEY": "sk-test-not-a-real-key",
            "BACTRIAN_CAP_OPENAI": "0.05",
            "BACTRIAN_SMOKE_MODEL": "test-model",
            "BACTRIAN_SMOKE_LEDGER": str(self.smoke_ledger),
        }

    def smoke(self, **extra: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, "-I", str(self.root / "dev" / "live-smoke.py")],
            env=self.env | extra,
            capture_output=True,
            text=True,
            check=False,
        )

    def test_refuses_an_upstream_override(self) -> None:
        done = self.smoke(BACTRIAN_UPSTREAM_OPENAI="http://127.0.0.1:9")
        self.assertNotEqual(done.returncode, 0)
        self.assertIn("unset BACTRIAN_UPSTREAM_OPENAI", done.stderr)
        self.assertFalse((self.root / ".bactrian").exists(), "bactrian ran")

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
            self.assertEqual(entry["model"], "test-model")
            self.assertEqual((entry["input_tokens"], entry["output_tokens"]), (100, 10))
        self.assertNotIn(0.5, [e["usd"] for e in self.logged()])


if __name__ == "__main__":
    unittest.main()
