open Cmdliner

let exit_ok = 0
let exit_usage = 64
let exit_internal = 70

let exits =
  [
    Cmd.Exit.info exit_ok ~doc:"on success.";
    Cmd.Exit.info exit_usage ~doc:"on a usage or configuration error.";
    Cmd.Exit.info exit_internal ~doc:"on an internal error.";
  ]

let cmd =
  let info =
    Cmd.info "bactrian" ~version:Bactrian_core.Version.current ~exits
      ~doc:"Record, replay and statistically compare AI agent runs"
  in
  let default = Term.(ret (const (`Help (`Auto, None)))) in
  Cmd.group ~default info []

let () =
  let code =
    match Cmd.eval_value cmd with
    | Ok (`Ok () | `Version | `Help) -> exit_ok
    | Error (`Parse | `Term) -> exit_usage
    | Error `Exn -> exit_internal
  in
  exit code
