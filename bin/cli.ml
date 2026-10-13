open Cmdliner
open Bactrian_runtime

let exits =
  Output.
    [
      Cmd.Exit.info exit_ok ~doc:"on success: the run passed.";
      Cmd.Exit.info exit_failed
        ~doc:"when the run failed (the agent) or diverged (a replay).";
      Cmd.Exit.info exit_not_run
        ~doc:"when the run did not happen because a spend cap was reached.";
      Cmd.Exit.info exit_usage ~doc:"on a usage or configuration error.";
      Cmd.Exit.info exit_environment
        ~doc:
          "when the environment is not ready, for example a spend cap of 0, a \
           missing API key or no TLS support for a live call.";
      Cmd.Exit.info exit_internal ~doc:"on an internal error.";
      Cmd.Exit.info Interrupt.exit_status
        ~doc:"when interrupted by SIGINT or SIGTERM.";
    ]

(* Under [--json] a failure before the run is one JSON object on stdout. *)
let fail ~json code message =
  if json then
    print_endline
      (Yojson.Safe.to_string
         (`Assoc [ ("error", `String message); ("exit", `Int code) ]))
  else prerr_endline ("bactrian: " ^ message);
  code

let located_json (e : Config_loader.located_error) =
  `Assoc
    [
      ("file", `String e.file);
      ("line", `Int e.line);
      ("column", `Int e.column);
      ("path", `String e.path);
      ("message", `String e.message);
    ]

(* Configuration errors, all of them, with their locations. *)
let fail_located ~json errors =
  if json then
    print_endline
      (Yojson.Safe.to_string
         (`Assoc
            [
              ("errors", `List (List.map located_json errors));
              ("exit", `Int Output.exit_usage);
            ]))
  else List.iter (fun e -> prerr_endline (Config_loader.located_line e)) errors;
  Output.exit_usage

let json =
  Arg.(
    value & flag
    & info [ "json" ] ~doc:"Print one JSON object instead of a summary.")
