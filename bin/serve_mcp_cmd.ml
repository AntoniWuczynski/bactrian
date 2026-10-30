open Cmdliner
open Bactrian_app
open Bactrian_runtime
open Bactrian_server

(* The protocol moves to descriptors of its own. Descriptor 0 becomes
   /dev/null and descriptor 1 standard error, so neither a child that inherits
   them nor a stray print can read the client's messages or write between the
   replies. *)
let quiet_standard_descriptors () =
  let null = Unix.openfile "/dev/null" [ O_RDONLY; O_CLOEXEC ] 0 in
  Unix.dup2 ~cloexec:false null Unix.stdin;
  Unix.close null;
  Unix.dup2 ~cloexec:false Unix.stderr Unix.stdout

let private_channel () =
  let input = Unix.dup ~cloexec:true Unix.stdin
  and output = Unix.dup ~cloexec:true Unix.stdout in
  quiet_standard_descriptors ();
  (input, output)

type transport = Stdio | Http of { port : int; allowed_origins : string list }

let token_variable = "BACTRIAN_MCP_TOKEN"

(* The bearer token of the HTTP listener: the user's, or a fresh one printed
   once, which only the terminal that started the server sees. *)
let token () =
  match Sys.getenv_opt token_variable with
  | Some t
    when String.length t >= 32
         && String.for_all (fun c -> c > ' ' && c <= '~') t ->
      Ok (t, false)
  | Some _ ->
      Error
        (token_variable
       ^ " must be at least 32 visible ASCII characters, without spaces")
  | None -> Ok (Proc_util.hex (Mirage_crypto_rng.generate 32), true)

let fail m =
  prerr_endline ("bactrian serve-mcp: " ^ m);
  Exit_code.internal

let log m = prerr_endline ("bactrian serve-mcp: " ^ m)

let usage m =
  log m;
  Exit_code.usage

let serve ~bactrian_exe (policy : Policy.t) transport =
  if policy.max_jobs < 1 then usage "--max-jobs must be at least 1"
  else
    match Launch_env.scrub ~exe:bactrian_exe with
    | Error m -> fail m
    | Ok () -> (
        (* A call or job that ends in an exception logs its backtrace to
           standard error. *)
        Printexc.record_backtrace true;
        match Confine.create (Sys.getcwd ()) with
        | exception Unix.Unix_error (e, _, _) -> fail (Unix.error_message e)
        | Error m -> fail m
        | Ok root -> (
            let config =
              {
                Mcp_server.policy;
                bactrian_exe;
                on_progress = Cli.print_progress;
                log;
              }
            in
            let stop = Interrupt.trigger () in
            let serving f =
              Interrupt.with_handlers
                (List.map (fun s -> (s, Interrupt.fire stop)) Interrupt.signals)
                f
            in
            let exit_status : Mcp_server.ended -> int = function
              | Input_closed -> Exit_code.ok
              | Stopped _ -> Interrupt.exit_status
            in
            match transport with
            | Stdio -> (
                match private_channel () with
                | exception Unix.Unix_error (e, _, _) ->
                    fail (Unix.error_message e)
                | input, output ->
                    Eio_main.run @@ fun env ->
                    Eio.Switch.run @@ fun sw ->
                    (* A plain read and write flow over each descriptor, pipe or
                       not. *)
                    let flow fd =
                      Eio_unix.Net.import_socket_stream ~sw ~close_unix:true fd
                    in
                    exit_status
                      (serving (fun () ->
                           Mcp_server.run ~env ~root config
                             (fun server handlers ->
                               Mcp_server.serve ~server ~handlers ~stop ~log
                                 ~input:(flow input) ~output:(flow output)))))
            | Http { port; allowed_origins } -> (
                Mirage_crypto_rng_unix.use_default ();
                match (token (), quiet_standard_descriptors ()) with
                | exception Unix.Unix_error (e, _, _) ->
                    fail (Unix.error_message e)
                | Error m, () -> usage m
                | Ok (token, generated), () -> (
                    Eio_main.run @@ fun env ->
                    Eio.Switch.run @@ fun sw ->
                    match
                      Mcp_http.listen ~sw ~net:(Eio.Stdenv.net env) port
                    with
                    | Error m -> fail ("cannot listen: " ^ m)
                    | Ok (socket, port) ->
                        log
                          (Printf.sprintf "listening on http://127.0.0.1:%d%s"
                             port Mcp_http.endpoint);
                        if generated then
                          log
                            (Printf.sprintf
                               "bearer token: %s (set %s to choose one)" token
                               token_variable);
                        exit_status
                          (serving (fun () ->
                               Mcp_server.run ~env ~root config
                                 (fun server handlers ->
                                   Mcp_http.serve ~server ~handlers ~stop ~log
                                     ~clock:(Eio.Stdenv.clock env)
                                     { port; token; allowed_origins }
                                     socket)))))))

let cmd ~bactrian_exe =
  let info =
    Cmd.info "serve-mcp"
      ~exits:
        [
          Cmd.Exit.info Exit_code.ok
            ~doc:"when the client closed standard input (stdio only).";
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
             environment, which no tool can change. A client that declares the \
             $(b,io.modelcontextprotocol/tasks) extension (2026-07-28 only) \
             gets the jobs as tasks, polled with $(b,tasks/get).";
          `P
            "With $(b,--http), the server speaks Streamable HTTP instead, at \
             $(b,http://127.0.0.1:)$(i,PORT)$(b,/mcp), and serves until SIGINT \
             or SIGTERM. It listens on the loopback interface only. Every \
             request must carry $(b,Authorization: Bearer) $(i,TOKEN): the \
             value of $(b,BACTRIAN_MCP_TOKEN) (at least 32 characters), or a \
             token the server makes and prints on standard error when that is \
             not set. A request from a browser page whose $(b,Origin) is not \
             this server's own, or one allowed with $(b,--http-allow-origin), \
             is refused with status 403.";
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
  let http =
    Arg.(
      value
      & opt (some int) None
      & info [ "http" ] ~docv:"PORT"
          ~doc:
            "Serve Streamable HTTP on 127.0.0.1 at $(docv) (0 picks a free \
             port, which the server prints) instead of standard input and \
             output.")
  in
  let allowed_origins =
    Arg.(
      value & opt_all string []
      & info [ "http-allow-origin" ] ~docv:"ORIGIN"
          ~doc:
            "Also accept requests whose $(b,Origin) header is $(docv), \
             $(i,SCHEME)$(b,://)$(i,HOST) with an optional $(b,:)$(i,PORT) and \
             no path, such as $(b,http://localhost:5173) for a client that \
             runs in a browser. The server answers the browser's CORS \
             preflight and lets a page from that origin read its answers, so \
             the page can drive the server if it has the token. Repeatable.")
  in
  Cmd.v info
    Term.(
      const
        (fun allow_unsandboxed trust_project max_jobs http allowed_origins ->
          match (http, allowed_origins) with
          | Some port, _ when port < 0 || port > 65535 ->
              usage "--http needs a port from 0 to 65535"
          | None, _ :: _ -> usage "--http-allow-origin needs --http"
          | Some port, _ -> (
              match
                List.fold_right
                  (fun o acc ->
                    Result.bind acc (fun os ->
                        Result.map (fun o -> o :: os) (Mcp_http.origin o)))
                  allowed_origins (Ok [])
              with
              | Error m -> usage ("--http-allow-origin: " ^ m)
              | Ok allowed_origins ->
                  serve ~bactrian_exe
                    { Policy.allow_unsandboxed; trust_project; max_jobs }
                    (Http { port; allowed_origins }))
          | None, [] ->
              serve ~bactrian_exe
                { Policy.allow_unsandboxed; trust_project; max_jobs }
                Stdio)
      $ allow_unsandboxed $ trust_project $ max_jobs $ http $ allowed_origins)
