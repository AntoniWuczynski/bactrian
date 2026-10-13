open Cmdliner
open Bactrian_app

let doctor_command ~json ~experiment ~bactrian_exe =
  Eio_main.run @@ fun env ->
  match Doctor_command.run ~bactrian_exe ~env { experiment } with
  | Error e -> Cli.fail_with ~json e
  | Ok report ->
      print_string
        (if json then
           Yojson.Safe.to_string (Doctor_command.to_json report) ^ "\n"
         else Bactrian_runtime.Doctor.render_text report);
      Doctor_command.exit_code report

let cmd ~bactrian_exe =
  let experiment =
    Arg.(
      value
      & opt (some string) None
      & info [ "experiment" ] ~docv:"FILE"
          ~doc:
            "Check what $(docv) needs: the providers its spend caps call, its \
             agent, its sandbox mode and its clock setting. Without it, check \
             the host and every provider key.")
  in
  let run json experiment = doctor_command ~json ~experiment ~bactrian_exe in
  let info =
    Cmd.info "doctor" ~exits:Cli.exits
      ~doc:"Check that this host is ready to record and run"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Prints one line per check: $(b,ok), $(b,warn), $(b,fail) or \
             $(b,skip), the check's name and a short message. The checks are \
             the provider keys (the variable names, never the values), a \
             loopback port, the agent command or HTTP URL, the sandbox, the \
             clock and HTTPS support. A missing key is a warning, or a failure \
             when $(b,--experiment) calls that provider live. No agent is \
             started and no URL is contacted. Exits 69 when any check fails, \
             else 0.";
        ]
  in
  Cmd.v info Term.(const run $ Cli.json $ experiment)
