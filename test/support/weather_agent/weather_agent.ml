let script =
  {|
import json, os, sys, urllib.request

base = os.environ["OPENAI_BASE_URL"]
key = os.environ["OPENAI_API_KEY"]
system = sys.argv[1] if len(sys.argv) > 1 else None
TOOLS = [{"type": "function", "function": {"name": "get_weather", "description": "Get the current weather for a city.", "parameters": {"type": "object", "properties": {"city": {"type": "string", "description": "City name"}}, "required": ["city"]}}}]

def send(m):
    sys.stdout.write(json.dumps(m) + "\n")
    sys.stdout.flush()

def chat(messages):
    body = {"model": "gpt-6-luna", "messages": messages, "tools": TOOLS, "max_completion_tokens": 400, "reasoning_effort": "none"}
    req = urllib.request.Request(base + "/chat/completions", data=json.dumps(body).encode(), headers={"Content-Type": "application/json", "Authorization": "Bearer " + key})
    with urllib.request.urlopen(req) as r:
        return json.load(r)["choices"][0]["message"]

for line in sys.stdin:
    m = json.loads(line)
    if m["type"] == "start":
        with open(os.path.join(m["state_dir"], "env.json"), "w") as f:
            json.dump(dict(os.environ), f)
    elif m["type"] == "turn":
        try:
            messages = [{"role": "user", "content": m["content"]}]
            if system:
                messages.insert(0, {"role": "system", "content": system})
            msg = chat(messages)
            while msg.get("tool_calls"):
                messages.append({"role": "assistant", "content": None, "tool_calls": msg["tool_calls"]})
                for call in msg["tool_calls"]:
                    send({"v": 1, "type": "event", "turn": m["turn"], "name": "tool", "data": {"name": call["function"]["name"]}})
                    messages.append({"role": "tool", "tool_call_id": call["id"], "content": "18 degrees Celsius, cloudy"})
                msg = chat(messages)
            send({"v": 1, "type": "reply", "turn": m["turn"], "content": msg["content"]})
        except Exception as e:
            send({"v": 1, "type": "error", "turn": m["turn"], "message": str(e)})
    elif m["type"] == "end":
        break
|}
