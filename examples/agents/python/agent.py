"""Toy bactrian agent (design §5.1): model calls through OPENAI_BASE_URL with a
get_weather tool.

Command mode (the default): one JSON object per line on stdin and stdout, the
configuration in the process environment.

HTTP mode (--http PORT, 0 for any free port): a server that prints its port as
the first line of stdout. Each POST carries one message, the run in the
X-Bactrian-Run header, and is answered with a JSON array of messages. The
configuration of a run is the env of its start message, never the process
environment, so runs in flight at once stay apart.

When AGENT_MCP_COMMAND holds a JSON array (for example
["bactrian", "mcp-shim", "weather"]), get_weather runs on that stdio MCP server
instead of returning a canned answer. AGENT_MCP_PROTOCOL picks the protocol
revision: 2026-07-28 (the default) or 2025-11-25, which opens with initialize."""

import json
import os
import subprocess
import sys
import threading
from collections.abc import Mapping
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from typing import Annotated, Literal

from openai import OpenAI
from openai.types.chat import (
    ChatCompletionMessageParam,
    ChatCompletionToolParam,
)
from pydantic import BaseModel, Field, JsonValue, TypeAdapter, ValidationError

WEATHER_RESULT = "18 degrees Celsius, cloudy"
MAX_MODEL_CALLS = 5
DEFAULT_SYSTEM_PROMPT = (
    "You are a concise assistant. Use the get_weather tool for weather questions."
)

TOOLS: list[ChatCompletionToolParam] = [
    {
        "type": "function",
        "function": {
            "name": "get_weather",
            "description": "Get the current weather for a city.",
            "parameters": {
                "type": "object",
                "properties": {"city": {"type": "string", "description": "City name."}},
                "required": ["city"],
                "additionalProperties": False,
            },
        },
    }
]


class Start(BaseModel):
    type: Literal["start"]
    env: dict[str, str] | None = None


class Turn(BaseModel):
    type: Literal["turn"]
    turn: int
    content: str


class End(BaseModel):
    type: Literal["end"]


ToAgent = Annotated[Start | Turn | End, Field(discriminator="type")]
TO_AGENT: TypeAdapter[Start | Turn | End] = TypeAdapter(ToAgent)


Message = dict[str, str | int]


def send(message: Message) -> None:
    sys.stdout.write(json.dumps({"v": 1, **message}) + "\n")
    sys.stdout.flush()


class RpcError(BaseModel):
    code: int
    message: str


class RpcReply(BaseModel):
    id: int | str | None = None
    result: JsonValue = None
    error: RpcError | None = None


class Content(BaseModel):
    type: str
    text: str = ""


class ToolResult(BaseModel):
    content: list[Content]


Arguments = TypeAdapter(dict[str, JsonValue])


class McpClient:
    """A minimal MCP client for a stdio server: one JSON-RPC message per line,
    one request in flight at a time."""

    def __init__(self, argv: list[str], env: Mapping[str, str], protocol: str) -> None:
        self.protocol = protocol
        self.process = subprocess.Popen(
            argv, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, env=dict(env)
        )
        self.next_id = 0
        if protocol == "2025-11-25":
            self.request(
                "initialize",
                {
                    "protocolVersion": protocol,
                    "capabilities": {},
                    "clientInfo": {"name": "toy-agent", "version": "0"},
                },
            )
            self.write({"jsonrpc": "2.0", "method": "notifications/initialized"})

    def write(self, message: dict[str, JsonValue]) -> None:
        if self.process.stdin is None:
            raise RuntimeError("the MCP shim has no stdin")
        self.process.stdin.write(json.dumps(message) + "\n")
        self.process.stdin.flush()

    def request(self, method: str, params: dict[str, JsonValue]) -> JsonValue:
        self.next_id += 1
        if self.protocol != "2025-11-25":
            params = {
                **params,
                "_meta": {"io.modelcontextprotocol/protocolVersion": self.protocol},
            }
        self.write({"jsonrpc": "2.0", "id": self.next_id, "method": method, "params": params})
        if self.process.stdout is None:
            raise RuntimeError("the MCP shim has no stdout")
        line = self.process.stdout.readline()
        if not line:
            raise RuntimeError("the MCP shim closed its output")
        reply = RpcReply.model_validate_json(line)
        if reply.error is not None:
            raise RuntimeError(f"MCP error {reply.error.code}: {reply.error.message}")
        return reply.result

    def call_tool(self, name: str, arguments: dict[str, JsonValue]) -> str:
        result = ToolResult.model_validate(
            self.request("tools/call", {"name": name, "arguments": arguments})
        )
        return "".join(item.text for item in result.content if item.type == "text")

    def close(self) -> None:
        if self.process.stdin is not None:
            self.process.stdin.close()
        self.process.wait(timeout=10)


class Session:
    """One conversation. [env] is the whole configuration: the process
    environment in command mode, the start message's env in HTTP mode."""

    def __init__(self, env: Mapping[str, str]) -> None:
        self.env = env
        self.model = env.get("AGENT_MODEL", "gpt-6-luna")
        self.mcp_command = env.get("AGENT_MCP_COMMAND")
        self.mcp_protocol = env.get("AGENT_MCP_PROTOCOL", "2026-07-28")
        self.mcp: McpClient | None = None
        self.client = OpenAI(
            max_retries=0,
            api_key=env.get("OPENAI_API_KEY"),
            base_url=env.get("OPENAI_BASE_URL"),
        )
        self.history: list[ChatCompletionMessageParam] = [
            {
                "role": "system",
                "content": env.get("AGENT_SYSTEM_PROMPT", DEFAULT_SYSTEM_PROMPT),
            }
        ]

    def weather(self, name: str, arguments_json: str) -> str:
        if self.mcp_command is None:
            return WEATHER_RESULT
        if self.mcp is None:
            # The shim finds bactrian through BACTRIAN_MCP_URL, which is in the
            # run's env and not necessarily in this process's.
            self.mcp = McpClient(
                TypeAdapter(list[str]).validate_json(self.mcp_command),
                {**os.environ, **self.env},
                self.mcp_protocol,
            )
        return self.mcp.call_tool(name, Arguments.validate_json(arguments_json))

    def answer(self, content: str) -> str:
        self.history.append({"role": "user", "content": content})
        for _ in range(MAX_MODEL_CALLS):
            completion = self.client.chat.completions.create(
                model=self.model,
                messages=self.history,
                tools=TOOLS,
                reasoning_effort="none",
            )
            if not completion.choices:
                raise RuntimeError("model returned no choices")
            message = completion.choices[0].message
            tool_calls = message.tool_calls or []
            if not tool_calls:
                self.history.append({"role": "assistant", "content": message.content or ""})
                return message.content or ""
            self.history.append(
                {
                    "role": "assistant",
                    "content": message.content,
                    "tool_calls": [
                        {
                            "id": call.id,
                            "type": "function",
                            "function": {
                                "name": call.function.name,
                                "arguments": call.function.arguments,
                            },
                        }
                        for call in tool_calls
                        if call.type == "function"
                    ],
                }
            )
            for call in tool_calls:
                result = (
                    self.weather(call.function.name, call.function.arguments)
                    if call.type == "function" and call.function.name == "get_weather"
                    else "unknown tool"
                )
                self.history.append({"role": "tool", "tool_call_id": call.id, "content": result})
        raise RuntimeError(f"no final answer after {MAX_MODEL_CALLS} model calls")

    def reply_to(self, turn: Turn) -> Message:
        try:
            return {"type": "reply", "turn": turn.turn, "content": self.answer(turn.content)}
        except Exception as error:  # report any model failure to bactrian as a turn error
            return {"type": "error", "turn": turn.turn, "message": str(error)}

    def close(self) -> None:
        if self.mcp is not None:
            self.mcp.close()


def main() -> int:
    session = Session(os.environ)
    for line in sys.stdin:
        if not line.strip():
            continue
        try:
            message = TO_AGENT.validate_json(line)
        except ValidationError:
            sys.stderr.write(f"unparseable message: {line}")
            return 1
        if isinstance(message, End):
            session.close()
            return 0
        if isinstance(message, Start):
            continue
        send(session.reply_to(message))
    return 0


sessions: dict[str, Session] = {}
sessions_lock = threading.Lock()


def handle_http(run_id: str, body: bytes) -> tuple[int, list[Message]]:
    """The status and the messages for one POST."""
    try:
        message = TO_AGENT.validate_json(body)
    except ValidationError:
        return 400, []
    with sessions_lock:
        if isinstance(message, Start):
            sessions[run_id] = Session(message.env or {})
            return 200, []
        session = sessions.get(run_id)
        if session is None:
            return 404, []
        if isinstance(message, End):
            del sessions[run_id]
    if isinstance(message, End):
        session.close()
        return 200, []
    return 200, [session.reply_to(message)]


class Handler(BaseHTTPRequestHandler):
    def do_POST(self) -> None:
        body = self.rfile.read(int(self.headers.get("Content-Length", "0")))
        status, messages = handle_http(self.headers.get("X-Bactrian-Run", ""), body)
        payload = json.dumps([{"v": 1, **m} for m in messages]).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(payload)))
        self.end_headers()
        self.wfile.write(payload)

    def log_message(self, format: str, *args: object) -> None:
        sys.stderr.write(f"{self.address_string()} {format % args}\n")


def serve(port: int) -> int:
    server = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    print(server.server_address[1], flush=True)
    server.serve_forever()
    return 0


if __name__ == "__main__":
    if len(sys.argv) == 3 and sys.argv[1] == "--http":
        sys.exit(serve(int(sys.argv[2])))
    sys.exit(main())
