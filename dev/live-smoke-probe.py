# pyright: strict
"""Probe agent for dev/live-smoke.py. It speaks bactrian's command-mode wire
contract and answers every turn with one word: "placeholder" when OPENAI_API_KEY
in its environment starts with "bactrian-", "other" when it holds anything else
(the real key, say) and "unset" when it is missing. It never prints the key."""

from __future__ import annotations

import json
import os
import sys
from typing import cast

PLACEHOLDER_PREFIX = "bactrian-"


def key_state() -> str:
    key = os.environ.get("OPENAI_API_KEY")
    if key is None:
        return "unset"
    return "placeholder" if key.startswith(PLACEHOLDER_PREFIX) else "other"


def main() -> None:
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
            reply = {"v": 1, "type": "reply", "turn": fields.get("turn"), "content": key_state()}
            sys.stdout.write(json.dumps(reply) + "\n")
            sys.stdout.flush()


if __name__ == "__main__":
    main()
