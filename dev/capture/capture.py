# /// script
# requires-python = ">=3.12"
# dependencies = ["httpx"]
# ///
# pyright: strict
"""Capture live provider traffic as protocol fixtures.

Usage (keys come from the environment, loaded by uv):
    uv run --env-file .env dev/capture/capture.py models
    uv run --env-file .env dev/capture/capture.py <protocol>/<case>
"""

from __future__ import annotations

import json
import os
import sys
import time
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import Callable, Literal, TypedDict, cast

import httpx

type JSON = dict[str, JSON] | list[JSON] | str | int | float | bool | None

ROOT = Path(__file__).resolve().parents[2]
REQUESTS = ROOT / "dev" / "capture" / "requests"
FIXTURES = ROOT / "test" / "fixtures" / "protocols"
LEDGER = Path(os.environ.get("CAPTURE_LEDGER", str(ROOT / ".bactrian" / "capture-ledger.jsonl")))

Provider = Literal["openai", "anthropic", "gemini", "openrouter"]

BASES: dict[Provider, str] = {
    "openai": "https://api.openai.com",
    "anthropic": "https://api.anthropic.com",
    "gemini": "https://generativelanguage.googleapis.com",
    "openrouter": "https://openrouter.ai",
}
KEY_VARS: dict[Provider, str] = {
    "openai": "OPENAI_API_KEY",
    "anthropic": "ANTHROPIC_API_KEY",
    "gemini": "GEMINI_API_KEY",
    "openrouter": "OPENROUTER_API_KEY",
}


@dataclass(frozen=True)
class Price:
    """USD per million tokens, spec section 11."""

    input: float
    cached: float
    cache_write: float
    output: float


@dataclass(frozen=True)
class Usage:
    """Same meaning as Trace.usage: input is the total prompt, including the
    cache reads and cache writes, which are subsets of it. Output includes
    reasoning tokens."""

    input: int
    cached: int
    cache_write: int
    output: int


# Cache writes are priced at the 5-minute rate for Anthropic. Models with no
# cache-write price bill them at the input rate.
PRICES: dict[str, Price] = {
    "gpt-6-luna": Price(0.10, 0.01, 0.125, 0.50),
    "gpt-5-nano": Price(0.05, 0.005, 0.05, 0.40),
    "claude-haiku-5-5": Price(0.10, 0.01, 0.125, 0.50),
    "gemini-3.8-flash": Price(0.75, 0.075, 0.75, 3.75),
    "gemini-3.1-pro-preview": Price(2.00, 0.20, 2.00, 12.00),
}


def cost_usd(price: Price, u: Usage) -> float:
    plain = u.input - u.cached - u.cache_write
    total = (plain * price.input + u.cached * price.cached
             + u.cache_write * price.cache_write + u.output * price.output)
    return round(total / 1_000_000, 8)
RESPONSE_HEADER_ALLOW = ("content-type", "retry-after", "x-request-id")
RESPONSE_HEADER_PREFIXES = ("x-ratelimit-", "anthropic-ratelimit-")
TOOL_RESULT = "18 degrees Celsius, cloudy"


# ---------- JSON narrowing helpers ----------


def as_obj(x: JSON) -> dict[str, JSON]:
    return x if isinstance(x, dict) else {}


def as_list(x: JSON) -> list[JSON]:
    return x if isinstance(x, list) else []


def as_str(x: JSON) -> str:
    return x if isinstance(x, str) else ""


def as_int(x: JSON) -> int:
    if isinstance(x, bool):
        return 0
    return int(x) if isinstance(x, (int, float)) else 0


def get(x: JSON, *path: str | int) -> JSON:
    for key in path:
        if isinstance(key, int):
            items = as_list(x)
            x = items[key] if key < len(items) else None
        else:
            x = as_obj(x).get(key)
    return x


def narrow(x: object) -> JSON:
    """Check a json.loads result against JSON instead of casting it."""
    if x is None or isinstance(x, (str, bool, int, float)):
        return x
    if isinstance(x, list):
        return [narrow(item) for item in cast(list[object], x)]
    if isinstance(x, dict):
        out: dict[str, JSON] = {}
        for key, value in cast(dict[object, object], x).items():
            if not isinstance(key, str):
                raise ValueError(f"non-string JSON key: {key!r}")
            out[key] = narrow(value)
        return out
    raise ValueError(f"not JSON: {type(x).__name__}")


def parse_json(raw: bytes) -> JSON:
    return narrow(json.loads(raw))


class Chunk(TypedDict):
    offset: int
    length: int
    ms: float


class Meta(TypedDict):
    protocol: str
    method: str
    path: str
    query: str
    status: int
    request_headers: dict[str, str]
    response_headers: dict[str, str]
    model: str
    captured: str


@dataclass(frozen=True)
class Case:
    provider: Provider
    protocol: str
    name: str
    model: str
    path: str
    query: str = ""
    # For second-turn cases: the case whose non-streamed response seeds the history.
    seed: str | None = None


def case_id(c: Case) -> str:
    return f"{c.protocol}/{c.name}"


CASES: list[Case] = [
    Case("openai", "openai_chat", "luna_stream", "gpt-6-luna", "/v1/chat/completions"),
    Case("openai", "openai_chat", "luna_nonstream", "gpt-6-luna", "/v1/chat/completions"),
    Case("openai", "openai_chat", "nano_stream", "gpt-5-nano", "/v1/chat/completions"),
    Case("openai", "openai_chat", "luna_turn2", "gpt-6-luna", "/v1/chat/completions",
         seed="openai_chat/luna_nonstream"),
    Case("openrouter", "openai_chat", "openrouter_gemma_stream", "google/gemma-4-31b-it:free",
         "/api/v1/chat/completions"),
    Case("openrouter", "openai_chat", "openrouter_nemotron_stream",
         "nvidia/nemotron-3-super-120b-a12b:free", "/api/v1/chat/completions"),
    Case("openai", "openai_responses", "luna_stream", "gpt-6-luna", "/v1/responses"),
    Case("openai", "openai_responses", "luna_nonstream", "gpt-6-luna", "/v1/responses"),
    Case("openai", "openai_responses", "luna_turn2", "gpt-6-luna", "/v1/responses",
         seed="openai_responses/luna_nonstream"),
    Case("anthropic", "anthropic_messages", "haiku_stream", "claude-haiku-5-5", "/v1/messages"),
    Case("anthropic", "anthropic_messages", "haiku_nonstream", "claude-haiku-5-5", "/v1/messages"),
    Case("anthropic", "anthropic_messages", "haiku_thinking_stream", "claude-haiku-5-5",
         "/v1/messages"),
    Case("anthropic", "anthropic_messages", "haiku_turn2", "claude-haiku-5-5", "/v1/messages",
         seed="anthropic_messages/haiku_nonstream"),
    Case("gemini", "gemini_generate", "flash_stream_sse", "gemini-3.8-flash",
         "/v1beta/models/gemini-3.8-flash:streamGenerateContent", "alt=sse"),
    Case("gemini", "gemini_generate", "flash_stream_json", "gemini-3.8-flash",
         "/v1beta/models/gemini-3.8-flash:streamGenerateContent"),
    Case("gemini", "gemini_generate", "flash_generate", "gemini-3.8-flash",
         "/v1beta/models/gemini-3.8-flash:generateContent"),
    Case("gemini", "gemini_generate", "pro_generate", "gemini-3.1-pro-preview",
         "/v1beta/models/gemini-3.1-pro-preview:generateContent"),
    Case("gemini", "gemini_generate", "flash_turn2", "gemini-3.8-flash",
         "/v1beta/models/gemini-3.8-flash:generateContent",
         seed="gemini_generate/flash_generate"),
    Case("gemini", "gemini_interactions", "flash_stream", "gemini-3.8-flash",
         "/v1beta/interactions", "alt=sse"),
    Case("gemini", "gemini_interactions", "flash_text_stream", "gemini-3.8-flash",
         "/v1beta/interactions", "alt=sse"),
    Case("gemini", "gemini_interactions", "flash_turn2", "gemini-3.8-flash",
         "/v1beta/interactions", "alt=sse", seed="gemini_interactions/flash_stream"),
    Case("openrouter", "openai_chat", "openrouter_gemma26_stream", "google/gemma-4-26b-a4b-it:free",
         "/api/v1/chat/completions"),
]


# ---------- second-turn request builders ----------


def load_seed_request(seed: Case) -> dict[str, JSON]:
    return as_obj(parse_json((REQUESTS / f"{seed.protocol}__{seed.name}.json").read_bytes()))


def load_seed_response(seed: Case) -> JSON:
    if seed.protocol == "gemini_interactions":
        return sse_events((FIXTURES / seed.protocol / seed.name / "response.raw").read_bytes())
    return parse_json((FIXTURES / seed.protocol / seed.name / "response.raw").read_bytes())


def build_turn2_chat(req: dict[str, JSON], resp: JSON) -> dict[str, JSON]:
    msg = as_obj(get(resp, "choices", 0, "message"))
    calls = as_list(msg.get("tool_calls"))
    call_id = as_str(get(calls, 0, "id"))
    out = dict(req)
    out["messages"] = [
        *as_list(req["messages"]),
        {"role": "assistant", "content": msg.get("content"), "tool_calls": calls},
        {"role": "tool", "tool_call_id": call_id, "content": TOOL_RESULT},
    ]
    return out


def build_turn2_responses(req: dict[str, JSON], resp: JSON) -> dict[str, JSON]:
    items = as_list(get(resp, "output"))
    call_ids = [as_str(get(i, "call_id")) for i in items if get(i, "type") == "function_call"]
    out = dict(req)
    out["input"] = [
        {"role": "user", "content": as_str(req["input"])},
        *items,
        {"type": "function_call_output", "call_id": call_ids[0], "output": TOOL_RESULT},
    ]
    return out


def build_turn2_anthropic(req: dict[str, JSON], resp: JSON) -> dict[str, JSON]:
    content = as_list(get(resp, "content"))
    tool_ids = [as_str(get(b, "id")) for b in content if get(b, "type") == "tool_use"]
    out = dict(req)
    out["messages"] = [
        *as_list(req["messages"]),
        {"role": "assistant", "content": content},
        {"role": "user",
         "content": [{"type": "tool_result", "tool_use_id": tool_ids[0], "content": TOOL_RESULT}]},
    ]
    return out


def build_turn2_gemini(req: dict[str, JSON], resp: JSON) -> dict[str, JSON]:
    model_content = as_obj(get(resp, "candidates", 0, "content"))
    parts = as_list(model_content.get("parts"))
    call = next(as_obj(as_obj(p).get("functionCall")) for p in parts if "functionCall" in as_obj(p))
    fr: dict[str, JSON] = {"name": call.get("name"), "response": {"result": TOOL_RESULT}}
    if "id" in call:
        fr["id"] = call["id"]
    out = dict(req)
    out["contents"] = [
        *as_list(req["contents"]),
        model_content,
        {"role": "user", "parts": [{"functionResponse": fr}]},
    ]
    return out


def build_turn2_interactions(req: dict[str, JSON], events: JSON) -> dict[str, JSON]:
    """Stateless history: user_input, the model's steps rebuilt from the stream, function_result."""
    steps: dict[int, dict[str, JSON]] = {}
    for e in as_list(events):
        idx = as_int(get(e, "index"))
        kind = get(e, "event_type")
        if kind == "step.start":
            steps[idx] = dict(as_obj(get(e, "step")))
        elif kind == "step.delta":
            d = as_obj(get(e, "delta"))
            if d.get("type") == "thought_signature":
                steps[idx]["signature"] = d.get("signature")
            elif d.get("type") == "arguments_delta":
                steps[idx]["arguments"] = parse_json(as_str(d.get("arguments")).encode())
    model_steps: list[JSON] = [steps[i] for i in sorted(steps)]
    call = next(s for s in model_steps if get(s, "type") == "function_call")
    out = dict(req)
    out["store"] = False
    out["input"] = [
        {"type": "user_input", "content": [{"type": "text", "text": as_str(req["input"])}]},
        *model_steps,
        {"type": "function_result", "name": get(call, "name"), "call_id": get(call, "id"),
         "result": [{"type": "text", "text": TOOL_RESULT}]},
    ]
    return out


TURN2: dict[str, Callable[[dict[str, JSON], JSON], dict[str, JSON]]] = {
    "openai_chat": build_turn2_chat,
    "openai_responses": build_turn2_responses,
    "anthropic_messages": build_turn2_anthropic,
    "gemini_generate": build_turn2_gemini,
    "gemini_interactions": build_turn2_interactions,
}


def request_body(c: Case) -> dict[str, JSON]:
    if c.seed is None:
        return as_obj(parse_json((REQUESTS / f"{c.protocol}__{c.name}.json").read_bytes()))
    seed = next(s for s in CASES if case_id(s) == c.seed)
    return TURN2[c.protocol](load_seed_request(seed), load_seed_response(seed))


# ---------- usage extraction ----------


def sse_events(raw: bytes) -> list[JSON]:
    events: list[JSON] = []
    for line in raw.decode("utf-8", "replace").splitlines():
        if line.startswith("data:"):
            data = line[5:].strip()
            if data and data != "[DONE]":
                events.append(parse_json(data.encode()))
    return events


def events_of(raw: bytes, content_type: str) -> list[JSON]:
    if "text/event-stream" in content_type:
        return sse_events(raw)
    parsed = parse_json(raw)
    return parsed if isinstance(parsed, list) else [parsed]


def usage_of(protocol: str, events: list[JSON]) -> Usage:
    inp = cached = write = out = 0
    for e in events:
        if protocol == "openai_chat":
            u = as_obj(get(e, "usage"))
            if u:
                inp, out = as_int(u.get("prompt_tokens")), as_int(u.get("completion_tokens"))
                cached = as_int(get(u, "prompt_tokens_details", "cached_tokens"))
                write = as_int(get(u, "prompt_tokens_details", "cache_write_tokens"))
        elif protocol == "openai_responses":
            u = as_obj(get(e, "response", "usage")) or as_obj(get(e, "usage"))
            if u:
                inp, out = as_int(u.get("input_tokens")), as_int(u.get("output_tokens"))
                cached = as_int(get(u, "input_tokens_details", "cached_tokens"))
                write = as_int(get(u, "input_tokens_details", "cache_write_tokens"))
        elif protocol == "anthropic_messages":
            u = as_obj(get(e, "message", "usage")) or as_obj(get(e, "usage"))
            if u:
                if "input_tokens" in u:
                    cached = as_int(u.get("cache_read_input_tokens"))
                    write = as_int(u.get("cache_creation_input_tokens"))
                    inp = as_int(u.get("input_tokens")) + cached + write
                if get(e, "type") != "message_start":
                    out = as_int(u.get("output_tokens"))
        elif protocol == "gemini_generate":
            u = as_obj(get(e, "usageMetadata"))
            if u:
                inp = as_int(u.get("promptTokenCount")) + as_int(u.get("toolUsePromptTokenCount"))
                cached = as_int(u.get("cachedContentTokenCount"))
                out = as_int(u.get("candidatesTokenCount")) + as_int(u.get("thoughtsTokenCount"))
        elif protocol == "gemini_interactions":
            u = as_obj(get(e, "interaction", "usage"))
            if u:
                inp = as_int(u.get("total_input_tokens")) + as_int(u.get("total_tool_use_tokens"))
                cached = as_int(u.get("total_cached_tokens"))
                out = as_int(u.get("total_output_tokens")) + as_int(u.get("total_thought_tokens"))
    return Usage(inp, cached, write, out)


# ---------- HTTP ----------


def auth_headers(p: Provider) -> dict[str, str]:
    key = os.environ[KEY_VARS[p]]
    if p in ("openai", "openrouter"):
        return {"Authorization": f"Bearer {key}"}
    if p == "anthropic":
        return {"x-api-key": key, "anthropic-version": "2023-06-01"}
    return {"x-goog-api-key": key}


def list_models() -> None:
    wanted = ("luna", "nano", "haiku", "flash", "3.1-pro", "gemma-4-31b", "gemma-4-26b", "nemotron-3-super")
    specs: list[tuple[Provider, str]] = [
        ("openai", "/v1/models"),
        ("anthropic", "/v1/models?limit=1000"),
        ("gemini", "/v1beta/models?pageSize=1000"),
        ("openrouter", "/api/v1/models"),
    ]
    with httpx.Client(timeout=60) as client:
        for p, path in specs:
            r = client.get(BASES[p] + path, headers=auth_headers(p))
            print(f"== {p} HTTP {r.status_code}")
            if r.status_code != 200:
                print(r.text[:300])
                continue
            body = r.json()
            entries = as_list(get(body, "data") or get(body, "models"))
            ids = sorted(
                as_str(get(e, "id")) or as_str(get(e, "name")) for e in entries
            )
            for i in ids:
                if any(w in i for w in wanted):
                    print("  ", i)


def request_headers(stream: bool) -> dict[str, str]:
    h = {"content-type": "application/json"}
    if stream:
        h["accept"] = "text/event-stream"
    return h


def run_case(c: Case) -> int:
    body = request_body(c)
    out_dir = FIXTURES / c.protocol / c.name
    streamed = body.get("stream") is True or "alt=sse" in c.query
    sent = request_headers(streamed)
    if c.protocol == "anthropic_messages":
        sent["anthropic-version"] = "2023-06-01"
    stored_req = dict(sent)
    headers = {**sent, **auth_headers(c.provider), "accept-encoding": "identity"}
    url = BASES[c.provider] + c.path + (f"?{c.query}" if c.query else "")
    raw = bytearray()
    chunks: list[Chunk] = []
    with httpx.Client(timeout=120) as client:
        start = time.perf_counter()
        with client.stream("POST", url, headers=headers, content=json.dumps(body).encode()) as r:
            for part in r.iter_raw():
                chunks.append({"offset": len(raw), "length": len(part),
                               "ms": round((time.perf_counter() - start) * 1000, 1)})
                raw.extend(part)
            status = r.status_code
            resp_headers = {
                k.lower(): v for k, v in r.headers.items()
                if k.lower() in RESPONSE_HEADER_ALLOW
                or k.lower().startswith(RESPONSE_HEADER_PREFIXES)
            }
    ctype = resp_headers.get("content-type", "")
    if status != 200:
        print(f"FAILED {case_id(c)} HTTP {status}: {bytes(raw)[:600].decode('utf-8', 'replace')}")
        return 1
    out_dir.mkdir(parents=True, exist_ok=True)
    (out_dir / "request.json").write_text(json.dumps(body, indent=2) + "\n")
    (out_dir / "response.raw").write_bytes(bytes(raw))
    (out_dir / "chunks.json").write_text(json.dumps(chunks, indent=2) + "\n")
    now = datetime.now(UTC).isoformat(timespec="seconds")
    meta: Meta = {"protocol": c.protocol, "method": "POST", "path": c.path, "query": c.query,
                  "status": status, "request_headers": stored_req,
                  "response_headers": resp_headers, "model": c.model, "captured": now}
    (out_dir / "meta.json").write_text(json.dumps(meta, indent=2) + "\n")
    usage = usage_of(c.protocol, events_of(bytes(raw), ctype))
    usd = cost_usd(PRICES.get(c.model, Price(0.0, 0.0, 0.0, 0.0)), usage)
    line = {"date": now, "provider": c.provider, "model": c.model,
            "input_tokens": usage.input, "cached_input_tokens": usage.cached,
            "cache_write_tokens": usage.cache_write, "output_tokens": usage.output,
            "usd": usd}
    LEDGER.parent.mkdir(parents=True, exist_ok=True)
    with LEDGER.open("a") as f:
        f.write(json.dumps(line) + "\n")
    print(f"OK {case_id(c)} HTTP {status} chunks={len(chunks)} bytes={len(raw)} "
          f"in={usage.input} out={usage.output} usd={usd}")
    return 0


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print(__doc__)
        return 2
    if argv[1] == "models":
        list_models()
        return 0
    match = [c for c in CASES if case_id(c) == argv[1]]
    if not match:
        print("unknown case; known:", *[case_id(c) for c in CASES], sep="\n  ")
        return 2
    return run_case(match[0])


if __name__ == "__main__":
    sys.exit(main(sys.argv))
