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

let crash_on =
  Arg.(
    value
    & opt (some string) None
    & info [ "crash-on" ] ~docv:"CITY"
        ~doc:"Exit, unanswered, when asked for $(docv)'s weather.")

let hang_on =
  Arg.(
    value & opt_all string []
    & info [ "hang-on" ] ~docv:"CITY"
        ~doc:"Never answer a call for $(docv)'s weather. Repeatable.")

let null_id_on =
  Arg.(
    value
    & opt (some string) None
    & info [ "null-id-error-on" ] ~docv:"CITY"
        ~doc:
          "Answer a call for $(docv)'s weather with an error whose id is null.")

let reverse_pairs =
  Arg.(
    value & flag
    & info [ "reverse-pairs" ]
        ~doc:"Hold each tools/call for the next and answer the two in reverse.")

let ask_client =
  Arg.(
    value & flag
    & info [ "ask-client" ]
        ~doc:"Send the client a roots/list request before answering a call.")

let no_read =
  Arg.(
    value & flag
    & info [ "no-read" ]
        ~doc:
          "Never read stdin, as a server stuck elsewhere would, until killed.")

let env_log =
  Arg.(
    value
    & opt (some string) None
    & info [ "env-log" ] ~docv:"FILE"
        ~doc:"Write the server's environment to $(docv), one entry per line.")

let pid_file =
  Arg.(
    value
    & opt (some string) None
    & info [ "pid-file" ] ~docv:"FILE" ~doc:"Write the server's pid to $(docv).")

let main era log crash_on hang_on null_id_on reverse_pairs ask_client no_read
    env_log pid_file =
  Option.iter
    (fun path ->
      Out_channel.with_open_text path (fun oc ->
          output_string oc (string_of_int (Unix.getpid ()))))
    pid_file;
  Option.iter
    (fun path ->
      Out_channel.with_open_text path (fun oc ->
          Array.iter
            (fun entry -> output_string oc (entry ^ "\n"))
            (Unix.environment ())))
    env_log;
  if no_read then
    while true do
      Unix.sleep 3600
    done
  else
    Fake_mcp.serve_stdio
      ~options:{ crash_on; hang_on; null_id_on; reverse_pairs; ask_client }
      ~era ~log stdin stdout

let () =
  exit
    (Cmd.eval
       (Cmd.v
          (Cmd.info "fake_mcp_server" ~doc:"A fake stdio MCP server.")
          Term.(
            const main $ era $ log $ crash_on $ hang_on $ null_id_on
            $ reverse_pairs $ ask_client $ no_read $ env_log $ pid_file)))
