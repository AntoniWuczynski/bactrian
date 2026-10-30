bactrian serve-mcp speaks MCP on standard input and output. A small client
discovers it in the 2026-07-28 revision, lists the tools, calls doctor and
closes its input, and only JSON-RPC responses come back.

  $ cat > client.py <<'PY'
  > import json, subprocess, sys
  > server = subprocess.Popen(["bactrian", "serve-mcp"], stdin=subprocess.PIPE,
  >                           stdout=subprocess.PIPE, text=True)
  > meta = {"io.modelcontextprotocol/protocolVersion": "2026-07-28",
  >         "io.modelcontextprotocol/clientCapabilities": {}}
  > def call(id, method, params={}):
  >     server.stdin.write(json.dumps({"jsonrpc": "2.0", "id": id, "method": method,
  >                                    "params": dict(params, _meta=meta)}) + "\n")
  >     server.stdin.flush()
  >     reply = json.loads(server.stdout.readline())
  >     assert reply["id"] == id and "method" not in reply, reply
  >     return reply["result"]
  > found = call(1, "server/discover")
  > print(found["supportedVersions"], found["resultType"], found["_meta"]["io.modelcontextprotocol/serverInfo"]["name"])
  > print(" ".join(t["name"] for t in call(2, "tools/list")["tools"]))
  > doctor = call(3, "tools/call", {"name": "doctor", "arguments": {}})
  > assert json.loads(doctor["content"][0]["text"]) == doctor["structuredContent"]
  > json.dump(doctor["structuredContent"], open("tool.json", "w"))
  > server.stdin.close()
  > print("left over:", repr(server.stdout.read()), "exit", server.wait())
  > PY
  $ env -i PATH="$PATH" OPENAI_API_KEY=sk-SENTINEL python3 client.py
  ['2026-07-28'] complete bactrian
  run_start record_start replay_start job_status job_cancel compare plan report check doctor divergences
  left over: '' exit 0

The doctor tool's result is what bactrian doctor --json prints, and neither
holds the key.

  $ env -i PATH="$PATH" OPENAI_API_KEY=sk-SENTINEL bactrian doctor --json > cli.json
  $ python3 -c 'import json; print(json.load(open("tool.json")) == json.load(open("cli.json")))'
  True
  $ grep -c SENTINEL tool.json cli.json
  tool.json:0
  cli.json:0
  [1]

Over Streamable HTTP it needs a port, and a token of at least 32 characters
when one is given.

  $ bactrian serve-mcp --http 70000
  bactrian serve-mcp: --http needs a port from 0 to 65535
  [64]
  $ bactrian serve-mcp --http-allow-origin http://localhost:5173
  bactrian serve-mcp: --http-allow-origin needs --http
  [64]
  $ bactrian serve-mcp --http 0 --http-allow-origin http://localhost:5173/
  bactrian serve-mcp: --http-allow-origin: http://localhost:5173/ is not an origin: give scheme://host or scheme://host:port, with http or https and no path, such as http://localhost:5173
  [64]
  $ BACTRIAN_MCP_TOKEN=short bactrian serve-mcp --http 0
  bactrian serve-mcp: BACTRIAN_MCP_TOKEN must be at least 32 visible ASCII characters, without spaces
  [64]
