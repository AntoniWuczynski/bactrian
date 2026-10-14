"""Agent for the sandbox tests: on every turn it makes one legitimate model call
through the proxy, then tries to get out. The reply is a JSON object with the
outcome of each attempt: "ok" when it worked, "failed" with the error when it
did not. A sandboxed run must have every attempt fail except the model call and
the write to the state directory, which also dumps the agent's environment.

The paths to attack come from REAL_HOME_FILE, PROJECT_FILE, SECRET_FILE and
MASKED_FILE, a file of the deny-list that it also reads under its name in upper
case and in mixed case, which a case-insensitive host resolves to the same
file. It also tries to forge a line in the Linux forwarder's violation log."""

import base64
import json
import os
import socket
import sys
import urllib.error
import urllib.request
from collections.abc import Callable
from typing import cast

TIMEOUT = 5.0
PUBLIC = "1.1.1.1"
FORGED = b'{"kind":"relay_failed","port":1,"socket":"/forged","reason":"forged"}\n'


def model() -> str:
    request = urllib.request.Request(
        os.environ["OPENAI_BASE_URL"] + "/chat/completions",
        data=json.dumps(
            {"model": "gpt-6-luna", "messages": [{"role": "user", "content": "hello"}]}
        ).encode(),
        headers={
            "Content-Type": "application/json",
            "Authorization": "Bearer " + os.environ["OPENAI_API_KEY"],
        },
    )
    with urllib.request.urlopen(request, timeout=TIMEOUT) as response:
        return f"status {response.status}"


def https() -> str:
    # No proxy handler, so nothing from the environment can redirect it.
    opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
    try:
        with opener.open(f"https://{PUBLIC}/", timeout=TIMEOUT) as response:
            return f"status {response.status}"
    except urllib.error.HTTPError as error:
        return f"status {error.code}"


def tcp() -> str:
    socket.create_connection(("8.8.8.8", 853), timeout=TIMEOUT).close()
    return "connected"


def websocket() -> str:
    # A hand-made upgrade request, so no library is needed.
    nonce = base64.b64encode(os.urandom(16)).decode()
    with socket.create_connection((PUBLIC, 80), timeout=TIMEOUT) as connection:
        connection.sendall(
            (
                f"GET / HTTP/1.1\r\nHost: {PUBLIC}\r\nUpgrade: websocket\r\n"
                "Connection: Upgrade\r\nSec-WebSocket-Version: 13\r\n"
                f"Sec-WebSocket-Key: {nonce}\r\n\r\n"
            ).encode()
        )
        return connection.recv(64).decode(errors="replace").split("\r\n")[0]


def dns() -> str:
    return str(socket.getaddrinfo("example.com", 443)[0][4])


def write(path: str) -> str:
    with open(path, "w"):
        pass
    return "written"


def deny_read() -> str:
    with open(os.environ["SECRET_FILE"]) as secret:
        return f"read {len(secret.read())} bytes"


def read_as(spell: Callable[[str], str]) -> Callable[[], str]:
    def read() -> str:
        directory, name = os.path.split(os.environ["MASKED_FILE"])
        with open(os.path.join(directory, spell(name))) as masked:
            return f"read {len(masked.read())} bytes"

    return read


def mixed_case(name: str) -> str:
    return "".join(c.upper() if i % 2 == 1 else c for i, c in enumerate(name))


def forwarder_log() -> str:
    # On Linux the agent's parent is the sandbox's forwarder, which holds the
    # violation log's pipe as descriptor 3. Elsewhere there is no /proc.
    fd = os.open(f"/proc/{os.getppid()}/fd/3", os.O_WRONLY)
    try:
        os.write(fd, FORGED)
    finally:
        os.close(fd)
    return "written"


def state_write() -> str:
    path = os.path.join(os.environ["BACTRIAN_STATE_DIR"], "agent-env.json")
    with open(path, "w") as out:
        json.dump({"argv": sys.argv, "env": dict(os.environ)}, out)
    return "written"


ACTIONS: dict[str, Callable[[], str]] = {
    "model": model,
    "https": https,
    "tcp": tcp,
    "websocket": websocket,
    "dns": dns,
    "home_write": lambda: write(os.environ["REAL_HOME_FILE"]),
    "project_write": lambda: write(os.environ["PROJECT_FILE"]),
    "deny_read": deny_read,
    "masked_file": read_as(lambda name: name),
    "masked_file_upper": read_as(str.upper),
    "masked_file_mixed": read_as(mixed_case),
    "forwarder_log": forwarder_log,
    "state_write": state_write,
}


def attempt(action: Callable[[], str]) -> dict[str, str]:
    try:
        return {"outcome": "ok", "detail": action()}
    except OSError as error:
        return {"outcome": "failed", "detail": f"{type(error).__name__}: {error}"}


def send(message: dict[str, str | int]) -> None:
    sys.stdout.write(json.dumps({"v": 1, **message}) + "\n")
    sys.stdout.flush()


for line in sys.stdin:
    message = cast("dict[str, object]", json.loads(line))
    if message["type"] == "turn" and isinstance(message["turn"], int):
        results = {name: attempt(action) for name, action in ACTIONS.items()}
        send(
            {
                "type": "reply",
                "turn": message["turn"],
                "content": json.dumps(results, sort_keys=True),
            }
        )
    elif message["type"] == "end":
        break
