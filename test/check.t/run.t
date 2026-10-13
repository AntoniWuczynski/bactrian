Golden traces check a scenario's assertions and checkers before they judge real
runs. Set up a scenario with a checker and two golden traces.

  $ mkdir golden checks
  $ cat > checks/no_double.py <<'PY'
  > import json, sys
  > trace = json.load(sys.stdin)
  > n = sum(1 for e in trace if e["type"] == "tool_call" and e["name"] == "calendar_create")
  > print(json.dumps({"pass": n < 2, "message": "double booked"}))
  > PY
  $ start='{"clock":"contract_only","run_id":"g","scenario":"s","seq":0,"sim_now":"2026-10-14T09:00:00Z","t_ms":0,"type":"run_start","v":1,"variant":"v"}
  > {"content":"hi","seq":1,"t_ms":1,"turn":1,"type":"turn_start","v":1}'
  $ call() { echo '{"arguments":{},"index":'$1',"mcp":"joined","name":"calendar_create","seq":'$(($1+2))',"t_ms":2,"turn":1,"type":"tool_call","v":1}'; }
  $ end='{"content":"done","seq":9,"t_ms":3,"turn":1,"type":"reply","v":1}
  > {"seq":10,"t_ms":4,"type":"done","v":1}'
  $ { echo "$start"; call 0; echo "$end"; } > golden/ok.jsonl
  $ { echo "$start"; call 0; call 1; echo "$end"; } > golden/double.jsonl
  $ cat > good.yaml <<'YAML'
  > version: 1
  > id: schedule
  > turns:
  >   - user: "hi"
  > assertions:
  >   - tool_sequence: { mode: contains, tools: [calendar_create] }
  >   - max_steps: 2
  > checkers:
  >   - command: ["python3", "-I", "checks/no_double.py"]
  > golden:
  >   pass: [golden/ok.jsonl]
  >   fail: [golden/double.jsonl]
  > YAML

Every pass trace passes and every fail trace fails: exit 0, and no colour codes
because stdout is not a terminal.

  $ bactrian check good.yaml
  ok good.yaml
  $ bactrian check good.yaml | grep -c "$(printf '\033')"
  0
  [1]

A deliberately broken checker (here: removed) lets the double booking through,
and its golden fail trace gives it away.

  $ grep -v 'checks/no_double' good.yaml | grep -v '^checkers:' > lax.yaml
  $ bactrian check lax.yaml
  FAIL lax.yaml
    golden/double.jsonl (golden fail): expected to fail but every assertion and checker passed
  [1]

A broken assertion is caught the same way: this one is too strict for the pass
trace.

  $ sed 's/tools: \[calendar_create\]/tools: [calendar_delete]/' good.yaml > strict.yaml
  $ bactrian check strict.yaml
  FAIL strict.yaml
    golden/ok.jsonl (golden pass): expected to pass but assertions[0] tool_sequence: tool_sequence contains: missing calendar_delete, called: calendar_create
  [1]

Several scenarios are checked in one go, and every offender is listed.

  $ bactrian check good.yaml lax.yaml strict.yaml
  ok good.yaml
  FAIL lax.yaml
    golden/double.jsonl (golden fail): expected to fail but every assertion and checker passed
  FAIL strict.yaml
    golden/ok.jsonl (golden pass): expected to pass but assertions[0] tool_sequence: tool_sequence contains: missing calendar_delete, called: calendar_create
  [1]

A scenario with assertions or checkers and no golden trace at all proves
nothing, so it is not ok.

  $ grep -v '^golden:\|^  pass:\|^  fail:' good.yaml > bare.yaml
  $ bactrian check bare.yaml
  FAIL bare.yaml
    no golden traces: add golden.pass and golden.fail traces so the assertions and checkers are exercised
  [1]

With only pass traces an assertion that always passes would go unnoticed. That
is a warning, and the check still succeeds.

  $ grep -v '^  fail:' good.yaml > passonly.yaml
  $ bactrian check passonly.yaml
  ok passonly.yaml
    warning: no golden.fail trace, so an assertion or checker that always passes would go unnoticed

A scenario with nothing to check needs no golden traces.

  $ printf 'version: 1\nid: bare\nturns:\n  - user: hi\n' > empty.yaml
  $ bactrian check empty.yaml
  ok empty.yaml

A trace that cannot be read is an offender too.

  $ sed 's/golden\/ok.jsonl/golden\/nope.jsonl/' good.yaml > missing.yaml
  $ bactrian check missing.yaml
  FAIL missing.yaml
    golden/nope.jsonl (golden pass): cannot read the trace
  [1]

Configuration errors are all reported with file, line, column and document
path, and exit 64.

  $ cat > broken.yaml <<'YAML'
  > version: 1
  > id: broken
  > turns: []
  > assertions:
  >   - max_steps: lots
  > surprise: true
  > YAML
  $ bactrian check broken.yaml
  broken.yaml:3:1: turns: must contain at least one item
  broken.yaml:5:5: assertions[0].max_steps: expected an integer, found a string
  broken.yaml:6:1: surprise: unknown field
  [64]
  $ bactrian check nothing-here.yaml
  nothing-here.yaml:1:1: no such file
  [64]
  $ bactrian check 2> /dev/null
  [64]
