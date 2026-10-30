With the sandbox off, the checkers share the agent's (lack of) sandbox, so the
summary says so once, on the line for every run. A checker line of its own would
repeat the same cause.

  $ cat > agent.sh <<'SH'
  > #!/bin/sh
  > while IFS= read -r line; do
  >   case "$line" in
  >     *'"type":"turn"'*) echo '{"v":1,"type":"reply","turn":1,"content":"ok"}' ;;
  >     *'"type":"end"'*) exit 0 ;;
  >   esac
  > done
  > SH
  $ chmod +x agent.sh
  $ mkdir scenarios
  $ cat > scenarios/s.yaml <<'YAML'
  > version: 1
  > id: s
  > turns:
  >   - user: "hi"
  > checkers:
  >   - command: ["python3", "-I", "-c", "import json; print(json.dumps({'pass': True}))"]
  > YAML
  $ cat > exp.yaml <<'YAML'
  > version: 1
  > name: demo
  > agent: { protocol: command, command: ["./agent.sh"] }
  > scenarios: ["scenarios/*.yaml"]
  > variants:
  >   a: { env: {} }
  > caps_usd: { openai: 1 }
  > sandbox: off
  > YAML
  $ export OPENAI_API_KEY=k BACTRIAN_UPSTREAM_OPENAI=http://127.0.0.1:9
  $ bactrian run exp.yaml > out.txt 2> err.txt; echo "exit $?"
  exit 0
  $ grep -e 'every' -e 'passed' out.txt | sed 's/  [0-9TZ]*-[0-9a-f]*$//'
  every run: warning: UNSANDBOXED (sandbox: off, sandbox: off)
    0  scenarios/s.yaml  a  #1  passed
  1 passed

An HTTP-mode agent has no sandbox of its own, so its checkers run in the
host's. Under `sandbox: off` the two labels give different causes, and the
summary has a line for each.

  $ cat > http_agent.py <<'PY'
  > import http.server, json, sys
  > class H(http.server.BaseHTTPRequestHandler):
  >     def do_POST(self):
  >         m = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
  >         out = [{"v": 1, "type": "reply", "turn": m["turn"], "content": "ok"}] if m["type"] == "turn" else []
  >         body = json.dumps(out).encode()
  >         self.send_response(200)
  >         self.send_header("Content-Type", "application/json")
  >         self.send_header("Content-Length", str(len(body)))
  >         self.end_headers()
  >         self.wfile.write(body)
  >     def log_message(self, *args):
  >         pass
  > s = http.server.HTTPServer(("127.0.0.1", 0), H)
  > import os
  > with open("agent.pid", "w") as f:
  >     f.write(str(os.getpid()))
  > with open("port.tmp", "w") as f:
  >     f.write(str(s.server_address[1]))
  > os.rename("port.tmp", "port")
  > s.serve_forever()
  > PY
  $ (python3 -I http_agent.py &)
  $ trap 'kill "$(cat agent.pid 2>/dev/null)" 2>/dev/null' EXIT
  $ for _ in $(seq 100); do [ -f port ] && break; sleep 0.1; done; [ -f port ] || { echo "agent did not start"; exit 1; }
  $ cat > http.yaml <<YAML
  > version: 1
  > name: demo
  > agent: { protocol: http, url: "http://127.0.0.1:$(cat port)/agent", namespace_header: X-Run }
  > scenarios: ["scenarios/*.yaml"]
  > variants:
  >   a: { env: {} }
  > caps_usd: { openai: 1 }
  > sandbox: off
  > YAML
  $ bactrian run http.yaml > out.txt 2> err.txt; echo "exit $?"; kill "$(cat agent.pid)"
  exit 0
  $ grep -e 'every' -e 'passed' out.txt | sed 's/  [0-9TZ]*-[0-9a-f]*$//'
  every run: warning: UNSANDBOXED (sandbox: off, http mode)
  every checker: warning: UNSANDBOXED (sandbox: off, sandbox: off)
    0  scenarios/s.yaml  a  #1  passed
  1 passed
