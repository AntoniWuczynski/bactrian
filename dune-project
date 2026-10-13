(lang dune 3.24)
(name bactrian)
(generate_opam_files true)
(source (github AntoniWuczynski/bactrian))
(authors "Antoni Wuczynski")
(maintainers "Antoni Wuczynski <antoni.wuczynski@gmail.com>")
(license Apache-2.0)

(package
 (name bactrian)
 (synopsis "Record, replay and statistically compare AI agent runs")
 (description
  "bactrian runs AI agents written in any language as black-box subprocesses, records their model, tool and network traffic, replays it deterministically, and compares agent versions with confidence intervals.")
 (depends
  (ocaml (>= 5.4))
  (cmdliner (>= 2.1.1))
  (alcotest :with-test)))
