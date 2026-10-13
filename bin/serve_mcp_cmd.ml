open Cmdliner
open Bactrian_app
open Bactrian_runtime
open Bactrian_server

(* The protocol moves to descriptors of its own. Descriptor 0 becomes
   /dev/null and descriptor 1 standard error, so neither a child that inherits
   them nor a stray print can read the client's messages or write between the
   replies. *)
let private_channel () =
  let input = Unix.dup ~cloexec:true Unix.stdin
  and output = Unix.dup ~cloexec:true Unix.stdout in
  let null = Unix.openfile "/dev/null" [ O_RDONLY; O_CLOEXEC ] 0 in
  Unix.dup2 ~cloexec:false null Unix.stdin;
  Unix.close null;
  Unix.dup2 ~cloexec:false Unix.stderr Unix.stdout;
  (input, output)

let fail m =
  prerr_endline ("bactrian serve-mcp: " ^ m);
  Exit_code.internal

let serve ~bactrian_exe (policy : Policy.t) =
  if policy.max_jobs < 1 then (
    prerr_endline "bactrian serve-mcp: --max-jobs must be at least 1";
    Exit_code.usage)
  else
    match Launch_env.scrub ~exe:bactrian_exe with
    | Error m -> fail m
    | Ok () -> (
        (* A call or job that ends in an exception logs its backtrace to
           standard error. *)
        Printexc.record_backtrace true;
        match (Confine.create (Sys.getcwd ()), private_channel ()) with
        | exception Unix.Unix_error (e, _, _) -> fail (Unix.error_message e)
        | Error m, _ -> fail m
        | Ok root, (input, output) -> (
            Eio_main.run @@ fun env ->
            Eio.Switch.run @@ fun sw ->
            (* A plain read and write flow over each descriptor, pipe or not. *)
            let flow fd =
              Eio_unix.Net.import_socket_stream ~sw ~close_unix:true fd
            in
            let stop = Interrupt.trigger () in
            match
              Interrupt.with_handlers
                (List.map (fun s -> (s, Interrupt.fire stop)) Interrupt.signals)
                (fun () ->
                  Mcp_server.run ~env ~root ~stop
                    {
                      policy;
                      bactrian_exe;
                      on_progress = Cli.print_progress;
                      log =
                        (fun m -> prerr_endline ("bactrian serve-mcp: " ^ m));
                    }
                    ~input:(flow input) ~output:(flow output))
            with
            | Input_closed -> Exit_code.ok
            | Stopped _ -> Interrupt.exit_status))

let cmd ~bactrian_exe =
  let info =
    Cmd.info "serve-mcp"
      ~exits:
        [
          Cmd.Exit.info Exit_code.ok
            ~doc:"when the client closed standard input.";
          Cmd.Exit.info Exit_code.usage ~doc:"when an option is invalid.";
          Cmd.Exit.info Exit_code.internal
            ~doc:"when the server could not start.";
          Cmd.Exit.info Interrupt.exit_status
            ~doc:"when stopped by SIGINT or SIGTERM.";
        ]
      ~doc:"Serve bactrian's commands to a coding agent over MCP"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Speaks the Model Context Protocol (MCP) on standard input and \
             output, one JSON-RPC message per line, in the 2026-07-28 revision \
             or, for a client that opens with $(b,initialize), in 2025-11-25 \
             and earlier. Each command is a tool whose result is the JSON the \
             command prints with $(b,--json). $(b,run), $(b,record) and \
             $(b,replay) start jobs: $(b,run_start), $(b,record_start) and \
             $(b,replay_start) return a job ID at once, $(b,job_status) polls \
             it and $(b,job_cancel) stops it as Ctrl-C would. Paths must lie \
             inside the current directory. Unless the options below allow \
             more, every job runs its agent in the sandbox in $(b,strict) \
             mode, and checkers, MCP servers and passed environment variables \
             are refused. Spend is capped by the \
             $(b,BACTRIAN_CAP_)$(i,PROVIDER) variables of the server's \
             environment, which no tool can change.";
          `P
            "Warnings go to standard error. When standard input closes, \
             running jobs are cancelled (an experiment stays resumable) and \
             the server exits once their agents are gone. SIGINT and SIGTERM \
             do the same and exit 130.";
        ]
  in
  let max_jobs =
    Arg.(
      value
      & opt int Policy.default.max_jobs
      & info [ "max-jobs" ] ~docv:"N"
          ~doc:
            "Run at most $(docv) jobs at once. A further $(b,run_start), \
             $(b,record_start) or $(b,replay_start) is refused until one \
             finishes.")
  in
  let allow_unsandboxed =
    Arg.(
      value & flag
      & info [ "allow-unsandboxed" ]
          ~doc:
            "Let jobs run their agent outside the operating-system sandbox: \
             $(b,sandbox) $(b,auto) or $(b,off) in a tool argument or an \
             experiment, and HTTP-mode agents. Without it every job is \
             $(b,strict), and fails where the sandbox does not work.")
  in
  let trust_project =
    Arg.(
      value & flag
      & info [ "trust-project" ]
          ~doc:
            "Let the project's files run commands and reach outside the \
             sandbox: scenario checkers, MCP servers ($(b,mcp_stdio), \
             $(b,mcp_http) and an experiment's $(b,mcp)), passing the server's \
             environment variables to an agent ($(b,env), \
             $(b,agent.env_passthrough)) and $(b,sandbox.allow_write) paths \
             outside the working directory. This makes the server as powerful \
             as a shell in this project: anything that can write a file here \
             and call a tool can run any program as you.")
  in
  Cmd.v info
    Term.(
      const (fun allow_unsandboxed trust_project max_jobs ->
          serve ~bactrian_exe
            { Policy.allow_unsandboxed; trust_project; max_jobs })
      $ allow_unsandboxed $ trust_project $ max_jobs)
