open Cmdliner

let era =
  let eras = [ ("modern", Fake_mcp.Modern); ("legacy", Fake_mcp.Legacy) ] in
  Arg.(
    value
    & opt (enum eras) Fake_mcp.Modern
    & info [ "era" ] ~docv:"ERA"
        ~doc:"Protocol era to speak: $(b,modern) (2026-07-28) or $(b,legacy).")

let log =
  Arg.(
    value
    & opt (some string) None
    & info [ "log" ] ~docv:"FILE"
        ~env:(Cmd.Env.info "FAKE_MCP_LOG")
        ~doc:"Append every request line to $(docv).")

let main era log = Fake_mcp.serve_stdio ~era ~log stdin stdout

let () =
  exit
    (Cmd.eval
       (Cmd.v
          (Cmd.info "fake_mcp_server" ~doc:"A fake stdio MCP server.")
          Term.(const main $ era $ log)))
