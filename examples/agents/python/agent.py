"""Toy bactrian agent (design §5.1, command mode): one JSON object per line on
stdin and stdout, model calls through OPENAI_BASE_URL with a get_weather tool.

When AGENT_MCP_COMMAND holds a JSON array (for example
["bactrian", "mcp-shim", "weather"]), get_weather runs on that stdio MCP server
instead of returning a canned answer. AGENT_MCP_PROTOCOL picks the protocol
revision: 2026-07-28 (the default) or 2025-11-25, which opens with initialize."""

import json
import os
import subprocess
import sys
from typing import Annotated, Literal

from openai import OpenAI
from openai.types.chat import (
    ChatCompletionMessageParam,
    ChatCompletionToolParam,
)
from pydantic import BaseModel, Field, JsonValue, TypeAdapter, ValidationError

MODEL = os.environ.get("AGENT_MODEL", "gpt-6-luna")
WEATHER_RESULT = "18 degrees Celsius, cloudy"
MAX_MODEL_CALLS = 5
SYSTEM_PROMPT = os.environ.get(
    "AGENT_SYSTEM_PROMPT",
    "You are a concise assistant. Use the get_weather tool for weather questions.",
)

MCP_COMMAND = os.environ.get("AGENT_MCP_COMMAND")
MCP_PROTOCOL = os.environ.get("AGENT_MCP_PROTOCOL", "2026-07-28")

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


class Turn(BaseModel):
    type: Literal["turn"]
    turn: int
    content: str


class End(BaseModel):
    type: Literal["end"]


ToAgent = Annotated[Start | Turn | End, Field(discriminator="type")]
TO_AGENT: TypeAdapter[Start | Turn | End] = TypeAdapter(ToAgent)


def send(message: dict[str, str | int]) -> None:
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

    def __init__(self, argv: list[str]) -> None:
        self.process = subprocess.Popen(
            argv, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True
        )
        self.next_id = 0
        if MCP_PROTOCOL == "2025-11-25":
            self.request(
                "initialize",
                {
                    "protocolVersion": MCP_PROTOCOL,
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
        if MCP_PROTOCOL != "2025-11-25":
            params = {
                **params,
                "_meta": {"io.modelcontextprotocol/protocolVersion": MCP_PROTOCOL},
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


mcp: McpClient | None = None


def weather(name: str, arguments_json: str) -> str:
    global mcp
    if MCP_COMMAND is None:
        return WEATHER_RESULT
    if mcp is None:
        mcp = McpClient(TypeAdapter(list[str]).validate_json(MCP_COMMAND))
    return mcp.call_tool(name, Arguments.validate_json(arguments_json))


client = OpenAI(max_retries=0)
history: list[ChatCompletionMessageParam] = [
    {
        "role": "system",
        "content": SYSTEM_PROMPT,
    }
]


def answer(content: str) -> str:
    history.append({"role": "user", "content": content})
    for _ in range(MAX_MODEL_CALLS):
        completion = client.chat.completions.create(
            model=MODEL,
            messages=history,
            tools=TOOLS,
            reasoning_effort="none",
        )
        if not completion.choices:
            raise RuntimeError("model returned no choices")
        message = completion.choices[0].message
        tool_calls = message.tool_calls or []
        if not tool_calls:
            history.append({"role": "assistant", "content": message.content or ""})
            return message.content or ""
        history.append(
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
                weather(call.function.name, call.function.arguments)
                if call.type == "function" and call.function.name == "get_weather"
                else "unknown tool"
            )
            history.append({"role": "tool", "tool_call_id": call.id, "content": result})
    raise RuntimeError(f"no final answer after {MAX_MODEL_CALLS} model calls")


def main() -> int:
    for line in sys.stdin:
        if not line.strip():
            continue
        try:
            message = TO_AGENT.validate_json(line)
        except ValidationError:
            sys.stderr.write(f"unparseable message: {line}")
            return 1
        if isinstance(message, End):
            if mcp is not None:
                mcp.close()
            return 0
        if isinstance(message, Start):
            continue
        try:
            send({"type": "reply", "turn": message.turn, "content": answer(message.content)})
        except Exception as error:  # report any model failure to bactrian as a turn error
            send({"type": "error", "turn": message.turn, "message": str(error)})
    return 0


if __name__ == "__main__":
    sys.exit(main())
