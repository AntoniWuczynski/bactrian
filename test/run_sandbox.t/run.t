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
