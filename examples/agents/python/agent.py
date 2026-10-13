"""Toy bactrian agent (spec 5.1, command mode): one JSON object per line on
stdin and stdout, model calls through OPENAI_BASE_URL with a get_weather tool."""

import json
import os
import sys
from typing import Annotated, Literal

from openai import OpenAI
from openai.types.chat import (
    ChatCompletionMessageParam,
    ChatCompletionToolParam,
)
from pydantic import BaseModel, Field, TypeAdapter, ValidationError

MODEL = os.environ.get("AGENT_MODEL", "gpt-6-luna")
WEATHER_RESULT = "18 degrees Celsius, cloudy"
MAX_MODEL_CALLS = 5

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


client = OpenAI(max_retries=0)
history: list[ChatCompletionMessageParam] = [
    {
        "role": "system",
        "content": "You are a concise assistant. Use the get_weather tool for weather questions.",
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
                WEATHER_RESULT
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
