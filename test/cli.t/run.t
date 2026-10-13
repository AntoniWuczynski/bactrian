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
         0   on success.
         64  on a usage or configuration error.
         70  on an internal error.
