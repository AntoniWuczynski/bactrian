# pyright: strict
"""Probe agent for dev/live-smoke.py. It speaks bactrian's command-mode wire
contract and answers every turn with one word for each environment variable
named on its command line (OPENAI_API_KEY when none is), separated by spaces:
"placeholder" when the variable starts with "bactrian-", "other" when it holds
anything else (the real key, say) and "unset" when it is missing. It never
prints a key."""

from __future__ import annotations

import json
import os
import sys
from typing import cast

PLACEHOLDER_PREFIX = "bactrian-"


def key_state(name: str) -> str:
    key = os.environ.get(name)
    if key is None:
        return "unset"
    return "placeholder" if key.startswith(PLACEHOLDER_PREFIX) else "other"


def main() -> None:
    names = sys.argv[1:] or ["OPENAI_API_KEY"]
    for line in sys.stdin:
        if not line.strip():
            continue
        message: object = json.loads(line)
        if not isinstance(message, dict):
            continue
        fields = cast(dict[str, object], message)
        if fields.get("type") == "end":
            return
        if fields.get("type") == "turn":
            content = " ".join(key_state(n) for n in names)
            reply = {"v": 1, "type": "reply", "turn": fields.get("turn"), "content": content}
            sys.stdout.write(json.dumps(reply) + "\n")
            sys.stdout.flush()


if __name__ == "__main__":
    main()
