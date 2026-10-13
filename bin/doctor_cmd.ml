open Cmdliner
open Bactrian_runtime

let doctor_command ~json ~experiment ~tls_available ~bactrian_exe =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let target =
    match experiment with
    | None -> Ok None
    | Some file ->
        let dir =
          match Filename.dirname file with
          | "." -> Sys.getcwd ()
          | dir when Filename.is_relative dir ->
              Filename.concat (Sys.getcwd ()) dir
          | dir -> dir
        in
        Result.map
          (fun e -> Some (Doctor.target ~dir e))
          (Config_loader.load_experiment Eio.Path.(Eio.Stdenv.fs env / file))
  in
  match target with
  | Error errors -> Cli.fail_located ~json errors
  | Ok target ->
      let report =
        Doctor.report
          (Doctor.run ~env ~bactrian_exe ~tls_available
             ~keys:(Run.keys_from_environment ())
             ~path:(Sys.getenv_opt "PATH") target)
      in
      print_string
        (if json then Yojson.Safe.to_string (Doctor.to_json report) ^ "\n"
         else Doctor.render_text report);
      if report.ok then Output.exit_ok else Output.exit_environment

let cmd ~tls_available ~bactrian_exe =
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
  let run json experiment =
    doctor_command ~json ~experiment ~tls_available ~bactrian_exe
  in
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
