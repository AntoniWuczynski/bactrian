open Cmdliner
open Bactrian_app

let ca_command ~json action =
  Eio_main.run @@ fun env ->
  let root = Eio.Path.native_exn Eio.Path.(Eio.Stdenv.cwd env / ".bactrian") in
  match Ca_command.run ~root action with
  | Error e -> Cli.fail_with ~json e
  | Ok t ->
      print_string
        (if json then Yojson.Safe.to_string (Ca_command.to_json t) ^ "\n"
         else Ca_command.render_text action t);
      Exit_code.ok

let sub name action ~doc =
  Cmd.v
    (Cmd.info name ~exits:Cli.exits ~doc)
    Term.(const (fun json -> ca_command ~json action) $ Cli.json)

let cmd =
  let info =
    Cmd.info "ca" ~exits:Cli.exits
      ~doc:"The project certificate authority of egress interception"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "bactrian terminates an agent's HTTPS egress with leaf \
             certificates signed by a certificate authority of the project, \
             kept in ./.bactrian/ca (the key readable by you alone) and valid \
             for one year. Every run trusts it through the bundle named by \
             SSL_CERT_FILE, NODE_EXTRA_CA_CERTS and REQUESTS_CA_BUNDLE.";
        ]
  in
  Cmd.group info
    [
      sub "path" Ca_command.Path
        ~doc:
          "Print the path of the CA's certificate, making the CA when it is \
           missing";
      sub "rotate" Ca_command.Rotate
        ~doc:"Replace the CA with a new one, valid for a year from now";
    ]
