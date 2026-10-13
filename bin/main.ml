open Cmdliner
open Bactrian_app
open Bactrian_runtime
module P = Model_proxy

(* The executable the sandbox re-runs as its probe and its Linux forwarder. *)
let bactrian_exe = Sys.executable_name

(* A command that starts agents first drops secrets from its launch
   environment (design §5.3). *)
let scrubbed ~json f =
  match Launch_env.scrub ~exe:bactrian_exe with
  | Error m -> Cli.fail_with ~json (Internal m)
  | Ok () -> f ()

(* A single run, recorded or replayed, printed as text or JSON. *)
let single_run ~json run =
  scrubbed ~json @@ fun () ->
  Eio_main.run @@ fun env ->
  match run ~on_progress:Cli.print_progress ~bactrian_exe ~env with
  | Error e -> Cli.fail_with ~json e
  | Ok result ->
      print_string
        (if json then Yojson.Safe.to_string (Single_run.to_json result) ^ "\n"
         else Output.render_text result);
      Single_run.exit_code result

let experiment_command ~json ~experiment ~resume ~sandbox =
  scrubbed ~json @@ fun () ->
  Eio_main.run @@ fun env ->
  match
    Run_command.run ~on_progress:Cli.print_progress ~bactrian_exe ~env
      { experiment; resume; sandbox }
  with
  | Error e ->
      Cli.fail_with ~json
        ~resume:(fun id ->
          Printf.sprintf ", resume with: bactrian run %s --resume %s" experiment
            id)
        e
  | Ok result ->
      print_string
        (if json then Yojson.Safe.to_string (Run_command.to_json result) ^ "\n"
         else
           Output.render_summary_text ~colour:(Lazy.force Output.colour) result);
      Run_command.exit_code result

let check_command json scenarios =
  Eio_main.run @@ fun env ->
  match Check_command.run ~env { scenarios } with
  | Error e -> Cli.fail_with ~json e
  | Ok reports ->
      if json then
        print_endline (Yojson.Safe.to_string (Check_command.to_json reports))
      else
        List.iter
          (fun r ->
            print_string
              (Output.render_golden_text ~colour:(Lazy.force Output.colour) r))
          reports;
      Check_command.exit_code reports

(* Arguments *)

let scenario =
  Arg.(
    value & opt string "adhoc"
    & info [ "scenario" ] ~docv:"NAME"
        ~doc:"Scenario name recorded with the run.")

let variant =
  Arg.(
    value & opt string "default"
    & info [ "variant" ] ~docv:"NAME" ~doc:"Variant name recorded with the run.")

let cwd =
  Arg.(
    value & opt string "."
    & info [ "cwd" ] ~docv:"DIR" ~doc:"Working directory of the agent.")

let env_passthrough =
  Arg.(
    value & opt_all string []
    & info [ "env" ] ~docv:"NAME"
        ~doc:
          "Pass the variable $(docv) from bactrian's environment to the agent. \
           The agent otherwise gets only PATH, HOME, LANG, TERM and bactrian's \
           own variables.")

let timezone =
  Arg.(
    value & opt string "UTC"
    & info [ "timezone" ] ~docv:"TZ" ~doc:"Value of TZ for the agent.")

let sim_now =
  Arg.(
    value & opt string ""
    & info [ "sim-now" ] ~docv:"RFC3339"
        ~doc:
          "Simulated time given to the agent. Defaults to now, or in a replay \
           to the recording's.")

let turn_timeout =
  Arg.(
    value & opt float 120.
    & info [ "turn-timeout" ] ~docv:"SECONDS"
        ~doc:"Longest wait for one agent message.")

let total_timeout =
  Arg.(
    value & opt float 600.
    & info [ "total-timeout" ] ~docv:"SECONDS"
        ~doc:"Longest the whole run may take.")

let sandbox_doc =
  "Run the agent inside the operating-system sandbox: $(b,auto) when the host \
   supports it (otherwise unsandboxed, with a warning and a label), \
   $(b,strict) to stop with exit status 69 when it does not, $(b,off) to skip \
   it."

let sandbox_mode =
  Arg.(
    value
    & opt (enum Sandbox.modes) `Auto
    & info [ "sandbox" ] ~docv:"MODE" ~doc:(sandbox_doc ^ " Default $(b,auto)."))

let sandbox_override =
  Arg.(
    value
    & opt (some (enum Sandbox.modes)) None
    & info [ "sandbox" ] ~docv:"MODE"
        ~doc:(sandbox_doc ^ " Overrides the experiment's $(b,sandbox) setting."))

let mcp_flag name ~docv ~doc =
  Arg.(value & opt_all string [] & info [ name ] ~docv ~doc)

let mcp_stdio =
  mcp_flag Mcp_flags.stdio_name ~docv:"NAME=JSON"
    ~doc:
      "Declare the stdio MCP server $(i,NAME), started with the command given \
       as a JSON array of strings, for example \
       $(b,'weather=[\"python3\",\"weather.py\"]'). It runs in the current \
       directory with an empty environment beyond PATH, HOME and LANG. Repeat \
       for several servers."

let mcp_http =
  mcp_flag Mcp_flags.http_name ~docv:"NAME=URL"
    ~doc:
      "Declare the Streamable HTTP MCP server $(i,NAME) at $(i,URL) (http:// \
       or https://). Repeat for several servers."

let mcp_policy =
  mcp_flag Mcp_flags.policy_name ~docv:"NAME=POLICY"
    ~doc:
      "Set the policy of the declared server $(i,NAME) to $(b,record) \
       (default), $(b,passthrough) (live, not recorded) or $(b,block) (every \
       request refused). Repeat for several servers."

let agent_argv ~pos_after =
  let docv = "AGENT" in
  let doc = "The agent command and its arguments, after $(b,--)." in
  match pos_after with
  | `Start -> Arg.(non_empty & pos_all string [] & info [] ~docv ~doc)
  | `After n ->
      Arg.(non_empty & pos_right (n - 1) string [] & info [] ~docv ~doc)

let common ~agent ~cwd ~scenario ~variant ~sim_now ~timezone ~env_passthrough
    ~turn_timeout ~total_timeout ~sandbox : Single_run.common =
  {
    agent;
    cwd;
    scenario;
    variant;
    sim_now;
    timezone;
    env_passthrough;
    turn_timeout;
    total_timeout;
    sandbox;
  }

let record_cmd =
  let turns =
    Arg.(
      value & opt_all string []
      & info [ "turn" ] ~docv:"TEXT"
          ~doc:"A scripted user turn. Repeat for several turns, in order.")
  in
  let run json agent cwd turns scenario variant sim_now timezone env_passthrough
      turn_timeout total_timeout mcp_stdio mcp_http mcp_policy sandbox =
    let request : Record_command.request =
      {
        common =
          common ~agent ~cwd ~scenario ~variant ~sim_now ~timezone
            ~env_passthrough ~turn_timeout ~total_timeout ~sandbox;
        turns;
        mcp_stdio;
        mcp_http;
        mcp_policy;
      }
    in
    match Record_command.validate request with
    | Error m -> `Error (false, m)
    | Ok () -> `Ok (single_run ~json (Record_command.run request))
  in
  let info =
    Cmd.info "record" ~exits:Cli.exits
      ~doc:"Run an agent against the real providers and record the run"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Runs $(i,AGENT) as a subprocess, sends it each $(b,--turn), and \
             records its model traffic under ./.bactrian/runs. Spend is capped \
             per provider by $(b,BACTRIAN_CAP_OPENAI), \
             $(b,BACTRIAN_CAP_ANTHROPIC), $(b,BACTRIAN_CAP_GEMINI) and \
             $(b,BACTRIAN_CAP_OPENROUTER) (USD, default 0). Real keys come \
             from $(b,OPENAI_API_KEY), $(b,ANTHROPIC_API_KEY), \
             $(b,GEMINI_API_KEY) and $(b,OPENROUTER_API_KEY).";
          `P
            "The agent's MCP servers are declared with $(b,--mcp-stdio) and \
             $(b,--mcp-http). The agent reaches a stdio server through \
             $(b,bactrian mcp-shim) and an HTTP one through its \
             $(b,BACTRIAN_MCP_HTTP_)$(i,NAME) variable. Their traffic is \
             recorded with the model's, and $(b,bactrian replay) serves it \
             from the recording.";
        ]
  in
  Cmd.v info
    Term.(
      ret
        (const run $ Cli.json
        $ agent_argv ~pos_after:`Start
        $ cwd $ turns $ scenario $ variant $ sim_now $ timezone
        $ env_passthrough $ turn_timeout $ total_timeout $ mcp_stdio $ mcp_http
        $ mcp_policy $ sandbox_mode))

let replay_cmd =
  let from =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"RUN" ~doc:"Run ID or run directory to replay.")
  in
  let mode =
    Arg.(
      value
      & opt
          (enum
             [
               ("strict", Replay_command.Strict);
               ("until-divergence", Until_divergence);
               ("fuzzy", Fuzzy);
             ])
          Replay_command.Strict
      & info [ "mode" ] ~docv:"MODE"
          ~doc:
            "$(b,strict) refuses any request the recording lacks, \
             $(b,until-divergence) goes live at the first one, $(b,fuzzy) \
             serves the nearest recorded exchange above $(b,--threshold).")
  in
  let threshold =
    Arg.(
      value
      & opt (some float) None
      & info [ "threshold" ] ~docv:"F"
          ~doc:"Minimum similarity for $(b,--mode fuzzy), from 0 to 1 (0.9).")
  in
  let allow_reorder =
    Arg.(
      value & flag
      & info [ "allow-reorder" ]
          ~doc:"Serve recorded exchanges out of order, as a finding only.")
  in
  let timing =
    Arg.(
      value
      & opt (enum [ ("fast", P.Fast); ("recorded", P.Recorded) ]) P.Fast
      & info [ "timing" ] ~docv:"TIMING"
          ~doc:"Serve streams $(b,fast), or with their $(b,recorded) pacing.")
  in
  let run json from agent mode threshold allow_reorder timing cwd scenario
      variant sim_now timezone env_passthrough turn_timeout total_timeout
      sandbox =
    let request : Replay_command.request =
      {
        common =
          common ~agent ~cwd ~scenario ~variant ~sim_now ~timezone
            ~env_passthrough ~turn_timeout ~total_timeout ~sandbox;
        from;
        mode;
        threshold;
        allow_reorder;
        timing;
      }
    in
    match Replay_command.validate request with
    | Error m -> `Error (false, m)
    | Ok () -> `Ok (single_run ~json (Replay_command.run request))
  in
  let info =
    Cmd.info "replay" ~exits:Cli.exits
      ~doc:"Replay a recorded run against an agent without calling providers"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Runs $(i,AGENT) with the turns of the recording and answers its \
             model requests from the recording in $(i,RUN), looked up in \
             ./.bactrian/runs. A request the recording lacks is a divergence. \
             The MCP servers the recording used are served from it too, with \
             no flags: the real servers are never started.";
        ]
  in
  Cmd.v info
    Term.(
      ret
        (const run $ Cli.json $ from
        $ agent_argv ~pos_after:(`After 1)
        $ mode $ threshold $ allow_reorder $ timing $ cwd $ scenario $ variant
        $ sim_now $ timezone $ env_passthrough $ turn_timeout $ total_timeout
        $ sandbox_mode))

let run_cmd =
  let experiment =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"EXPERIMENT" ~doc:"The experiment file (YAML or JSON).")
  in
  let resume =
    Arg.(
      value
      & opt (some string) None
      & info [ "resume" ] ~docv:"ID"
          ~doc:
            "Continue the experiment $(docv) (see ./.bactrian/experiments): \
             runs that finished are not repeated.")
  in
  let run json experiment resume sandbox =
    experiment_command ~json ~experiment ~resume ~sandbox
  in
  let info =
    Cmd.info "run" ~exits:Cli.exits
      ~doc:"Run an experiment: its scenarios, variants and repeats"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Runs every scenario of $(i,EXPERIMENT) with every variant, \
             $(b,repeats) times, at most $(b,concurrency) at a time, in a \
             seeded blocked and interleaved order. Each run is judged by its \
             scenario's assertions and checkers. Spend is capped per provider \
             by $(b,caps_usd). Progress is kept in \
             ./.bactrian/experiments/ID/manifest.json. Interrupt with Ctrl-C \
             and continue with $(b,--resume). The exit status is 70 if a run \
             errored, else 1 if a run failed, else 2 if a run did not happen, \
             else 0.";
        ]
  in
  Cmd.v info
    Term.(const run $ Cli.json $ experiment $ resume $ sandbox_override)

let check_cmd =
  let scenarios =
    Arg.(
      non_empty & pos_all string []
      & info [] ~docv:"SCENARIO" ~doc:"A scenario file (YAML or JSON).")
  in
  let info =
    Cmd.info "check" ~exits:Cli.exits
      ~doc:"Check scenarios' assertions and checkers against golden traces"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Evaluates each $(i,SCENARIO)'s assertions and checkers on its \
             golden traces. Every $(b,golden.pass) trace must pass them all \
             and every $(b,golden.fail) trace must fail at least one. Exits 1 \
             and lists the offending traces otherwise, and 64 on a \
             configuration error, printed as file:line:column.";
        ]
  in
  Cmd.v info Term.(const check_command $ Cli.json $ scenarios)

let mcp_shim_cmd =
  let name =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"NAME" ~doc:"The MCP server's name in the experiment.")
  in
  let run name =
    match Sys.getenv_opt Mcp_proxy.url_variable with
    | None | Some "" ->
        prerr_endline
          ("bactrian mcp-shim: " ^ Mcp_proxy.url_variable
         ^ " is not set: run the agent under bactrian");
        Exit_code.usage
    | Some url ->
        Eio_main.run (fun env ->
            Mcp_shim.run ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
              ~stdin:(Eio.Stdenv.stdin env) ~stdout:(Eio.Stdenv.stdout env) ~url
              ~name);
        Exit_code.ok
  in
  let info =
    Cmd.info "mcp-shim" ~exits:Cli.exits
      ~doc:"Stand in for a stdio MCP server inside an agent run"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Point the agent's MCP configuration at $(b,bactrian mcp-shim) \
             $(i,NAME) instead of the server's own command. The shim passes \
             each JSON-RPC line from its stdin to bactrian, which starts, \
             records or replays the real server $(i,NAME), and writes the \
             replies to its stdout. It reads $(b,BACTRIAN_MCP_URL), which \
             bactrian sets for the agent, and exits when its stdin closes.";
        ]
  in
  Cmd.v info Term.(const run $ name)

(* cmdliner drops the [--] between the pairs and the agent, so the forwarder
   reads the raw arguments after its own name. *)
let sandbox_init_arguments () =
  match
    List.drop_while
      (fun a -> not (String.equal a "__sandbox-init"))
      (Array.to_list Sys.argv)
  with
  | _ :: args -> args
  | [] -> []

let sandbox_init_cmd =
  let args =
    Arg.(value & pos_all string [] & info [] ~docv:"PORT=SOCKET... -- AGENT...")
  in
  let run (_ : string list) =
    match Sandbox_init.parse (sandbox_init_arguments ()) with
    | Error e ->
        prerr_endline
          ("bactrian __sandbox-init: " ^ Sandbox_init.describe_parse_error e);
        Exit_code.usage
    | Ok config -> (
        if not (Sandbox_init.set_undumpable ()) then
          prerr_endline
            "bactrian __sandbox-init: cannot make the forwarder undumpable: \
             the agent may add lines to the violation log";
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        (* Standard input, output and error are the agent's own. *)
        let fd_number =
          Option.bind (Sys.getenv_opt Sandbox_init.log_variable) (fun v ->
              match int_of_string_opt v with
              | Some n when n > 2 -> Some n
              | Some _ | None -> None)
        in
        (* Reopened through /dev/fd with close-on-exec, because OCaml cannot
           name an inherited descriptor by its number. The inherited one was
           marked close-on-exec at start, and the agent gets /dev/null in its
           place as well. *)
        let log =
          Option.bind fd_number (fun n ->
              match
                Unix.openfile
                  (Printf.sprintf "/dev/fd/%d" n)
                  [ O_WRONLY; O_APPEND; O_CLOEXEC ]
                  0
              with
              | fd -> Some (Eio_unix.Fd.of_unix ~sw ~close_unix:true fd)
              | exception Unix.Unix_error (e, _, _) ->
                  prerr_endline
                    ("bactrian __sandbox-init: no violation log: "
                   ^ Unix.error_message e);
                  None)
        in
        match
          Sandbox_init.run ?log ?hidden_fd:fd_number ~net:(Eio.Stdenv.net env)
            ~process_mgr:(Eio.Stdenv.process_mgr env)
            config
        with
        | Ok code -> code
        | Error e ->
            prerr_endline
              ("bactrian __sandbox-init: " ^ Sandbox_init.describe_error e);
            Exit_code.environment)
  in
  let info =
    Cmd.info "__sandbox-init" ~docs:Manpage.s_none ~exits:Cli.exits
      ~doc:"Internal: the forwarder bactrian runs inside the Linux sandbox"
  in
  Cmd.v info Term.(const run $ args)

let sandbox_probe_cmd =
  let required name docv =
    Arg.(required & opt (some string) None & info [ name ] ~docv)
  in
  let port =
    Arg.(required & opt (some int) None & info [ "port" ] ~docv:"PORT")
  in
  let run port outside inside denied =
    print_endline
      (Sandbox_probe.to_line (Sandbox_probe.run ~port ~outside ~inside ~denied));
    Exit_code.ok
  in
  let info =
    Cmd.info "__sandbox-probe" ~docs:Manpage.s_none ~exits:Cli.exits
      ~doc:"Internal: the check bactrian runs inside a new sandbox"
  in
  Cmd.v info
    Term.(
      const run $ port $ required "outside" "PATH" $ required "inside" "PATH"
      $ required "denied" "PATH")

let cmd =
  let info =
    Cmd.info "bactrian" ~version:Bactrian_core.Version.current ~exits:Cli.exits
      ~doc:"Record, replay and statistically compare AI agent runs"
  in
  let default = Term.(ret (const (`Help (`Auto, None)))) in
  Cmd.group ~default info
    [
      record_cmd;
      replay_cmd;
      run_cmd;
      check_cmd;
      Compare_cmd.cmd;
      Plan_cmd.cmd;
      Report_cmd.cmd;
      Doctor_cmd.cmd ~bactrian_exe;
      mcp_shim_cmd;
      sandbox_init_cmd;
      sandbox_probe_cmd;
    ]

(* The image {!Launch_env.scrub} started takes its environment back first, and
   nothing it inherited reaches a child it starts. *)
let () =
  match Launch_env.restore () with
  | Ok () -> Launch_env.close_inherited_on_exec ()
  | Error m ->
      prerr_endline ("bactrian: " ^ m);
      exit Exit_code.internal

let () =
  let code =
    match Cmd.eval_value cmd with
    | Ok (`Ok n) -> n
    | Ok (`Version | `Help) -> Exit_code.ok
    | Error (`Parse | `Term) -> Exit_code.usage
    | Error `Exn -> Exit_code.internal
  in
  exit code
