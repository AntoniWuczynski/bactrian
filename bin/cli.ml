open Cmdliner
open Bactrian_app
open Bactrian_runtime

let exits =
  Exit_code.
    [
      Cmd.Exit.info ok ~doc:"on success: the run passed.";
      Cmd.Exit.info failed
        ~doc:"when the run failed (the agent) or diverged (a replay).";
      Cmd.Exit.info not_run
        ~doc:"when the run did not happen because a spend cap was reached.";
      Cmd.Exit.info usage ~doc:"on a usage or configuration error.";
      Cmd.Exit.info environment
        ~doc:
          "when the environment is not ready, for example a spend cap of 0, a \
           missing API key or no TLS support for a live call.";
      Cmd.Exit.info internal ~doc:"on an internal error.";
      Cmd.Exit.info Interrupt.exit_status
        ~doc:"when interrupted by SIGINT or SIGTERM.";
    ]

let fail_with ~json ?(resume = fun _ -> "") (e : Command_error.t) =
  (match e with
  | Interrupted { signal; experiment_id } ->
      prerr_endline
        (Printf.sprintf "bactrian: interrupted by signal %d%s" signal
           (match experiment_id with Some id -> resume id | None -> ""))
  | Configuration errors when not json ->
      List.iter (fun e -> prerr_endline (Config_loader.located_line e)) errors
  | Usage _ | Environment _ | Internal _ | Configuration _ ->
      if json then
        print_endline (Yojson.Safe.to_string (Command_error.to_json e))
      else prerr_endline ("bactrian: " ^ Command_error.message e));
  Command_error.exit_code e

let print_progress : Progress.t -> unit = function
  | Warning w -> prerr_endline ("bactrian: warning: " ^ w)
  | Experiment_started id -> prerr_endline ("bactrian: experiment " ^ id)

let json =
  Arg.(
    value & flag
    & info [ "json" ] ~doc:"Print one JSON object instead of a summary.")
