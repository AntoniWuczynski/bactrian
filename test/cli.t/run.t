  $ bactrian --version
  0.1.0-dev

With no arguments bactrian prints its help and succeeds.

  $ bactrian | grep -c "bactrian - Record, replay and statistically compare AI agent runs"
  1

  $ bactrian frobnicate 2> /dev/null
  [64]

  $ bactrian --no-such-flag 2> /dev/null
  [64]

The documented exit statuses are the ones the spec defines.

  $ bactrian --help=plain | sed -n '/^EXIT STATUS/,/^[A-Z]/p' | grep -E '^ +[0-9]+ +on'
         0   on success: the run passed.
         64  on a usage or configuration error.
         70  on an internal error.

The record and replay commands document the same statuses.

  $ bactrian record --help=plain | sed -n '/^EXIT STATUS/,/^[A-Z]/p' | grep -E '^ +[0-9]+ +(on|when)' | cut -c1-30
         0   on success: the run
         1   when the run failed
         2   when the run did no
         64  on a usage or confi
         69  when the environmen
         70  on an internal erro
         130 when interrupted by

Usage errors exit 64.

  $ bactrian record -- true 2> err.txt
  [64]
  $ grep -v '^Usage' err.txt
  bactrian: record needs at least one --turn
  $ bactrian record --turn hi 2> /dev/null
  [64]
  $ bactrian replay 2> /dev/null
  [64]
  $ bactrian replay some-run --threshold 0.5 -- true 2> err.txt
  [64]
  $ grep -v '^Usage' err.txt
  bactrian: --threshold only applies to --mode fuzzy
  $ bactrian replay some-run --mode fuzzy --threshold 2 -- true 2> err.txt
  [64]
  $ grep -v '^Usage' err.txt
  bactrian: --threshold must be between 0 and 1
  $ bactrian replay some-run --mode sideways -- true 2> /dev/null
  [64]

A malformed spend cap is a configuration error.

  $ BACTRIAN_CAP_OPENAI=lots bactrian record --turn hi -- true
  bactrian: BACTRIAN_CAP_OPENAI="lots" is not a non-negative amount in USD
  [64]

A live recording needs a cap above zero and the provider's key first. The
caller's own caps and keys must not leak into these checks.

  $ unset BACTRIAN_CAP_OPENAI BACTRIAN_CAP_ANTHROPIC BACTRIAN_CAP_GEMINI BACTRIAN_CAP_OPENROUTER OPENAI_API_KEY ANTHROPIC_API_KEY GEMINI_API_KEY OPENROUTER_API_KEY

  $ bactrian record --turn hi -- true
  bactrian: no provider has a spend cap above 0: set BACTRIAN_CAP_OPENAI, BACTRIAN_CAP_ANTHROPIC, BACTRIAN_CAP_GEMINI, BACTRIAN_CAP_OPENROUTER (USD) for the providers this run calls
  [69]
  $ env -u OPENAI_API_KEY BACTRIAN_CAP_OPENAI=1 bactrian record --turn hi -- true
  bactrian: openai has a spend cap but OPENAI_API_KEY is not set
  [69]

Replaying a run that does not exist is a configuration error.

  $ bactrian replay no-such-run -- true
  bactrian: no cassette for run "no-such-run"
  [64]

The run and check commands document the same statuses.

  $ bactrian run --help=plain | sed -n '/^EXIT STATUS/,/^[A-Z]/p' | grep -E '^ +[0-9]+ +(on|when)' | cut -c1-30
         0   on success: the run
         1   when the run failed
         2   when the run did no
         64  on a usage or confi
         69  when the environmen
         70  on an internal erro
         130 when interrupted by
  $ bactrian check --help=plain | sed -n '/^EXIT STATUS/,/^[A-Z]/p' | grep -cE '^ +[0-9]+ +(on|when)'
  7

Missing arguments are usage errors.

  $ bactrian run 2> /dev/null
  [64]
  $ bactrian check 2> /dev/null
  [64]

An experiment with several mistakes reports all of them with file, line, column
and path, and exits 64.

  $ cat > exp.yaml <<'YAML'
  > version: 1
  > name: demo
  > agent: { protocol: command, command: ["true"] }
  > scenarios: ["scenarios/*.yaml"]
  > variants:
  >   a: { env: {} }
  > repeats: 0
  > surprise: 1
  > YAML
  $ bactrian run exp.yaml
  exp.yaml:7:1: repeats: must be at least 1, found 0
  exp.yaml:8:1: surprise: unknown field
  [64]
  $ bactrian run exp.yaml --json
  {"errors":[{"file":"exp.yaml","line":7,"column":1,"path":"repeats","message":"must be at least 1, found 0"},{"file":"exp.yaml","line":8,"column":1,"path":"surprise","message":"unknown field"}],"exit":64}
  [64]

A glob that matches nothing is a configuration error too.

  $ sed -e '/^repeats/d' -e '/^surprise/d' exp.yaml > nomatch.yaml
  $ bactrian run nomatch.yaml
  nomatch.yaml:4:13: scenarios[0]: glob "scenarios/*.yaml" matches no files
  [64]

An experiment with scenarios still needs a spend cap and a key, and an unknown
experiment cannot be resumed.

  $ mkdir scenarios
  $ cat > scenarios/s.yaml <<'YAML'
  > version: 1
  > id: s
  > turns: [{ user: hi }]
  > YAML
  $ bactrian run nomatch.yaml
  bactrian: no provider has a spend cap above 0: set caps_usd in the experiment
  [69]
  $ sed 's/^repeats.*//' exp.yaml | sed -e '/^surprise/d' > capped.yaml
  $ echo 'caps_usd: { openai: 1 }' >> capped.yaml
  $ bactrian run capped.yaml
  bactrian: openai has a spend cap but OPENAI_API_KEY is not set
  [69]
  $ OPENAI_API_KEY=k bactrian run capped.yaml
  bactrian: this build has no TLS support (bactrian.tls), so it cannot call providers over HTTPS
  [69]
  $ OPENAI_API_KEY=k bactrian run capped.yaml --resume nope
  bactrian: no experiment "nope"
  [64]
