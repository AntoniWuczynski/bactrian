# pyright: strict
"""dev/live-smoke.py against fake_bactrian.py on PATH, in a scratch copy of the
repository layout, with a fake key. No provider is reached."""

from __future__ import annotations

import os
import shlex
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

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


if __name__ == "__main__":
    unittest.main()
