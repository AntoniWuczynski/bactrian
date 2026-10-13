open Cmdliner

let exit_usage = 64
let exit_internal = 70

let cmd =
  let info =
    Cmd.info "bactrian" ~version:Bactrian_core.Version.current
      ~doc:"Record, replay and statistically compare AI agent runs"
  in
  let default = Term.(ret (const (`Help (`Auto, None)))) in
  Cmd.group ~default info []

let () =
  let code =
    match Cmd.eval_value cmd with
    | Ok (`Ok () | `Version | `Help) -> 0
    | Error (`Parse | `Term) -> exit_usage
    | Error `Exn -> exit_internal
  in
  exit code
