The doctor runs in a controlled environment: only PATH, and key variables set
to sentinels whose text must never be printed. The sandbox and clock lines
depend on the host (macOS or Linux), so they are filtered out here, and the
unit tests cover them with the host stated.

  $ run() { env -i PATH="$PATH" "$@"; }

With no experiment, every known provider is listed and a missing key is a
warning. Only the variable name is printed.

  $ run OPENAI_API_KEY=sk-SENTINEL-one ANTHROPIC_API_KEY=sk-SENTINEL-two bactrian doctor > out.txt; echo "exit $?"
  exit 0
  $ grep -v -e ' sandbox:' -e ' clock:' out.txt
  ok    key-openai: OPENAI_API_KEY is set
  ok    key-anthropic: ANTHROPIC_API_KEY is set
  warn  key-gemini: GEMINI_API_KEY is not set
  warn  key-openrouter: OPENROUTER_API_KEY is not set
  ok    loopback: a port on 127.0.0.1 can be bound
  skip  tls: HTTPS support is not built in
  skip  ca: egress interception is not built yet
  skip  node: egress interception is not built yet
  $ grep -c -e ' sandbox:' -e ' clock:' out.txt
  2
  $ grep -c SENTINEL out.txt
  0
  [1]

The same as JSON, with the same shape every time.

  $ run OPENAI_API_KEY=sk-SENTINEL-one bactrian doctor --json > out.json; echo "exit $?"
  exit 0
  $ grep -c SENTINEL out.json
  0
  [1]
  $ sed -e 's/{"name"/\n&/g' out.json | grep -v -e '"name":"sandbox"' -e '"name":"clock"'
  {"checks":[
  {"name":"key-openai","status":"ok","message":"OPENAI_API_KEY is set"},
  {"name":"key-anthropic","status":"warn","message":"ANTHROPIC_API_KEY is not set"},
  {"name":"key-gemini","status":"warn","message":"GEMINI_API_KEY is not set"},
  {"name":"key-openrouter","status":"warn","message":"OPENROUTER_API_KEY is not set"},
  {"name":"loopback","status":"ok","message":"a port on 127.0.0.1 can be bound"},
  {"name":"tls","status":"skip","message":"HTTPS support is not built in"},
  {"name":"ca","status":"skip","message":"egress interception is not built yet"},
  {"name":"node","status":"skip","message":"egress interception is not built yet"}],"ok":true}

An empty key counts as missing.

  $ run OPENAI_API_KEY= bactrian doctor | grep key-openai
  warn  key-openai: OPENAI_API_KEY is not set

An experiment with no spend cap calls no provider live, so a missing key is
skipped, and the agent command is looked up. `run` refuses such an
experiment, so the caps check fails.

  $ cat > exp.yaml <<'YAML'
  > version: 1
  > name: demo
  > agent: { protocol: command, command: ["sh"] }
  > scenarios: ["s/*.yaml"]
  > variants:
  >   a: { env: {} }
  > sandbox: off
  > YAML
  $ run OPENAI_API_KEY=sk-SENTINEL-one bactrian doctor --experiment exp.yaml > out.txt; echo "exit $?"
  exit 69
  $ grep -v ' clock:' out.txt | sed 's/ resolves to .*/ resolves to FILE/'
  ok    key-openai: OPENAI_API_KEY is set
  skip  key-anthropic: ANTHROPIC_API_KEY is not set, and the experiment has no spend cap for anthropic
  skip  key-gemini: GEMINI_API_KEY is not set, and the experiment has no spend cap for gemini
  skip  key-openrouter: OPENROUTER_API_KEY is not set, and the experiment has no spend cap for openrouter
  fail  caps: no provider has a spend cap above 0: set caps_usd in the experiment
  ok    loopback: a port on 127.0.0.1 can be bound
  ok    agent: sh resolves to FILE
  skip  sandbox: sandbox: off
  skip  tls: HTTPS support is not built in
  skip  ca: egress interception is not built yet
  skip  node: egress interception is not built yet

A provider with a spend cap is called live, so its missing key fails, and so
does HTTPS support while this build has none.

  $ cat exp.yaml > capped.yaml
  $ echo 'caps_usd: { openai: 1, gemini: 1 }' >> capped.yaml
  $ run GEMINI_API_KEY=sk-SENTINEL-three bactrian doctor --experiment capped.yaml > out.txt; echo "exit $?"
  exit 69
  $ grep -e key- -e caps: -e tls: out.txt
  fail  key-openai: OPENAI_API_KEY is not set, and the experiment has a spend cap for openai
  skip  key-anthropic: ANTHROPIC_API_KEY is not set, and the experiment has no spend cap for anthropic
  ok    key-gemini: GEMINI_API_KEY is set
  skip  key-openrouter: OPENROUTER_API_KEY is not set, and the experiment has no spend cap for openrouter
  ok    caps: a spend cap above 0 for openai, gemini
  fail  tls: HTTPS support is not built in, but the experiment needs it for openai, gemini
  $ grep -c SENTINEL out.txt
  0
  [1]

An agent command that cannot be found fails.

  $ sed 's/\["sh"\]/["no-such-agent"]/' exp.yaml > missing.yaml
  $ run bactrian doctor --experiment missing.yaml | grep ' agent:' | sed 's/ relative to .*/ relative to DIR/'
  fail  agent: no-such-agent is not an executable file on PATH or relative to DIR
  $ run bactrian doctor --experiment missing.yaml > /dev/null; echo "exit $?"
  exit 69

A relative command is looked up from the experiment file's directory.

  $ mkdir bin
  $ printf '#!/bin/sh\n' > bin/agent.sh
  $ sed 's#\["sh"\]#["bin/agent.sh"]#' exp.yaml > relative.yaml
  $ run bactrian doctor --experiment relative.yaml | grep ' agent:'
  fail  agent: bin/agent.sh is not an executable file on PATH or relative to $TESTCASE_ROOT
  $ chmod +x bin/agent.sh
  $ run bactrian doctor --experiment relative.yaml | grep ' agent:' | sed 's/ resolves to .*/ resolves to FILE/'
  ok    agent: bin/agent.sh resolves to FILE

An HTTP-mode agent is checked for a well-formed URL and never contacted: nothing
listens on this one.

  $ cat > http.yaml <<'YAML'
  > version: 1
  > name: demo
  > agent: { protocol: http, url: "http://127.0.0.1:9/agent", namespace_header: X-Run }
  > scenarios: ["s/*.yaml"]
  > variants:
  >   a: { env: {} }
  > YAML
  $ run bactrian doctor --experiment http.yaml | grep -e ' agent:' -e ' sandbox:' -e ' clock:' -e ' tls:'
  ok    agent: http://127.0.0.1:9/agent is a well-formed URL (not contacted)
  skip  sandbox: an HTTP-mode agent is not started by bactrian
  skip  clock: contract-only (http-mode): bactrian does not start an HTTP-mode agent
  skip  tls: HTTPS support is not built in
  $ sed 's#http://127.0.0.1:9/agent#http:///agent#' http.yaml > nohost.yaml
  $ run bactrian doctor --experiment nohost.yaml | grep ' agent:'
  fail  agent: http:///agent has no host
  $ sed 's#http://127.0.0.1:9/agent#https://agent.example/run#' http.yaml > tls.yaml
  $ run bactrian doctor --experiment tls.yaml | grep -e ' agent:' -e ' tls:'
  ok    agent: https://agent.example/run is a well-formed URL (not contacted)
  fail  tls: HTTPS support is not built in, but the experiment needs it for the agent at https://agent.example/run

A configuration error in the experiment is reported with its location, as
for run, and exits 64.

  $ printf 'version: 1\nsurprise: 1\n' > bad.yaml
  $ run bactrian doctor --experiment bad.yaml
  bad.yaml:1:1: name: missing required field
  bad.yaml:1:1: agent: missing required field
  bad.yaml:1:1: scenarios: missing required field
  bad.yaml:1:1: variants: missing required field
  bad.yaml:2:1: surprise: unknown field
  [64]
  $ run bactrian doctor --experiment nope.yaml
  nope.yaml:1:1: no such file
  [64]

The doctor documents its exit statuses like the other commands.

  $ bactrian doctor --help=plain | sed -n '/^EXIT STATUS/,/^[A-Z]/p' | grep -E '^ +69'
         69  when the environment is not ready, for example a spend cap of 0, a
