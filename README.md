# bactrian

bactrian tests AI agents the way you would test any other program. It runs your agent as a subprocess, whatever language it is written in and whichever model provider it calls. It records every model call, tool call and network request the agent makes, and it can replay a recording exactly, so you can see the precise step where a new version of the agent started behaving differently. It also runs agents live against several models or configurations, repeats each scenario enough times to separate a real change from noise, and tells you whether a change is a regression, an improvement, no change, or still too close to call.

The name comes from the two-humped camel. Most of what bactrian does is compare two things.

## Status

Early development. Nothing is usable yet.

## Building from source

You need [opam](https://opam.ocaml.org). From the repository root:

```sh
opam switch create . ocaml-base-compiler.5.5.1 --deps-only --with-test --locked
opam exec -- dune build
opam exec -- dune test
```

The design decisions behind the code are recorded in [`docs/decisions/`](docs/decisions/).

## Licence

Apache-2.0. See [LICENSE](LICENSE).
