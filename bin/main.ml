open Cmdliner
open Bactrian_runtime
module P = Model_proxy

(* Whether this build can call a provider over HTTPS. The TLS connector is a
   separate library (bactrian.tls) that is not part of this build yet. *)
let tls_available = false
let ( let* ) = Result.bind

(* Configuration *)

let cap_variable name = "BACTRIAN_CAP_" ^ String.uppercase_ascii name

let read_caps () =
  List.fold_left
    (fun acc (_, name) ->
      Result.bind acc (fun caps ->
          match Sys.getenv_opt (cap_variable name) with
          | None | Some "" -> Ok ((name, 0.) :: caps)
          | Some raw -> (
              match float_of_string_opt raw with
              | Some usd when Float.is_finite usd && usd >= 0. ->
                  Ok ((name, usd) :: caps)
              | Some _ | None ->
                  Error
                    (Printf.sprintf "%s=%S is not a non-negative amount in USD"
                       (cap_variable name) raw))))
    (Ok []) Run.providers
  |> Result.map List.rev

(* A live call needs a spend cap above zero, the key behind it and TLS. *)
let live_problem caps =
  let funded =
    List.filter
      (fun (_, name) ->
        match List.assoc_opt name caps with
        | Some usd -> usd > 0.
        | None -> false)
      Run.providers
  in
  match funded with
  | [] ->
      Some
        (Printf.sprintf
           "no provider has a spend cap above 0: set %s (USD) for the \
            providers this run calls"
           (String.concat ", "
              (List.map (fun (_, n) -> cap_variable n) Run.providers)))
  | _ -> (
      match
        List.find_opt
          (fun (p, _) ->
            match Sys.getenv_opt (Run.key_variable p) with
            | None | Some "" -> true
            | Some _ -> false)
          funded
      with
      | Some (p, name) ->
          Some
            (Printf.sprintf "%s has a spend cap but %s is not set" name
               (Run.key_variable p))
      | None ->
          if tls_available then None
          else
            Some
              "this build has no TLS support (bactrian.tls), so it cannot call \
               providers over HTTPS")

(* Running *)

(* The executable the sandbox re-runs as its probe and its Linux forwarder. *)
let bactrian_exe = Sys.executable_name
let sandbox_warning w = prerr_endline ("bactrian: warning: " ^ w)

(* A command that starts agents first drops secrets from its launch
   environment (design §5.3). *)
let scrubbed ~json f =
  match Launch_env.scrub ~exe:bactrian_exe with
  | Error m -> Cli.fail ~json Output.exit_internal m
  | Ok () -> f ()

(* [config] gets the store, because a replay reads the recording's MCP servers
   from it, and what the sandbox preflight decided. *)
let run_command ~json ~live ~sandbox
    (config : Store.t -> Sandbox.t -> (Run.config, string) result) =
  scrubbed ~json @@ fun () ->
  match read_caps () with
  | Error m -> Cli.fail ~json Output.exit_usage m
  | Ok caps -> (
      match if live then live_problem caps else None with
      | Some m -> Cli.fail ~json Output.exit_environment m
      | None -> (
          Mirage_crypto_rng_unix.use_default ();
          Eio_main.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          let root = Eio.Path.(Eio.Stdenv.cwd env / ".bactrian") in
          match Ledger.create ~path:Eio.Path.(root / "ledger.jsonl") ~caps with
          | exception Ledger.Corrupt { line; reason } ->
              Cli.fail ~json Output.exit_environment
                (Printf.sprintf
                   "the spend ledger .bactrian/ledger.jsonl is corrupt (line \
                    %d: %s)"
                   line reason)
          | ledger -> (
              match
                let* sandbox =
                  Sandbox.preflight ~env ~warn:sandbox_warning
                    {
                      mode = sandbox;
                      allow_write = [];
                      deny_read = [];
                      bactrian_exe;
                      bactrian_dir = Eio.Path.native_exn root;
                    }
                in
                let* proxy =
                  P.start ~sw ~net:(Eio.Stdenv.net env)
                    ~clock:(Eio.Stdenv.clock env)
                    ~process_mgr:(Eio.Stdenv.process_mgr env)
                in
                Ok (sandbox, proxy)
              with
              | Error m -> Cli.fail ~json Output.exit_environment m
              | Ok (sandbox, proxy) -> (
                  let store = Store.create ~root in
                  match
                    Interrupt.run (fun () ->
                        Result.bind (config store sandbox) (fun config ->
                            Run.execute ~env ~proxy ~store ~ledger config))
                  with
                  | Error signal ->
                      prerr_endline
                        (Printf.sprintf "bactrian: interrupted by signal %d"
                           signal);
                      Interrupt.exit_status
                  | Ok (Error m) -> Cli.fail ~json Output.exit_usage m
                  | Ok (Ok result) ->
                      print_string
                        (if json then Output.render_json result ^ "\n"
                         else Output.render_text result);
                      Output.exit_code result.outcome))))

(* Experiments *)

let experiment_command ~json ~experiment ~resume ~sandbox =
  scrubbed ~json @@ fun () ->
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let announced = ref None in
  let label = ref None in
  match
    Interrupt.run (fun () ->
        Executor.run_experiment ?sandbox
          ~on_sandbox:(fun l -> label := Some l)
          ~bactrian_exe ~env ~experiment_file:experiment ~resume
          ~announce:(fun id ->
            announced := Some id;
            prerr_endline ("bactrian: experiment " ^ id))
          ~warn:(fun w -> prerr_endline ("bactrian: warning: " ^ w))
          ())
  with
  | Error signal ->
      prerr_endline
        (Printf.sprintf "bactrian: interrupted by signal %d%s" signal
           (match !announced with
           | Some id ->
               Printf.sprintf ", resume with: bactrian run %s --resume %s"
                 experiment id
           | None -> ""));
      Interrupt.exit_status
  | Ok (Error (Configuration errors)) -> Cli.fail_located ~json errors
  | Ok (Error (Usage m)) -> Cli.fail ~json Output.exit_usage m
  | Ok (Error (Environment m)) -> Cli.fail ~json Output.exit_environment m
  | Ok (Ok summary) ->
      print_string
        (if json then Output.render_summary_json ?sandbox:!label summary ^ "\n"
         else
           Output.render_summary_text ?sandbox:!label
             ~colour:(Lazy.force Output.colour) summary);
      Output.summary_exit_code summary

let check_command scenarios =
  Eio_main.run @@ fun env ->
  let results =
    List.map (fun scenario_file -> Golden.check ~env ~scenario_file) scenarios
  in
  match List.concat_map (function Ok _ -> [] | Error e -> e) results with
  | _ :: _ as errors -> Cli.fail_located ~json:false errors
  | [] ->
      let reports = List.filter_map Result.to_option results in
      List.iter
        (fun r ->
          print_string
            (Output.render_golden_text ~colour:(Lazy.force Output.colour) r))
        reports;
      if List.for_all Golden.passed reports then Output.exit_ok
      else Output.exit_failed

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

let build_config ~agent ~cwd ~turns ~scenario ~variant ~sim_now ~timezone
    ~replay ~env_passthrough ~turn_timeout ~total_timeout ~mcp ~sandbox :
    Run.config =
  {
    agent = Command agent;
    cwd;
    turns;
    scenario;
    variant;
    sim_now;
    timezone;
    replay;
    env_passthrough;
    variant_env = [];
    fixtures = None;
    normalise = [];
    redact = [];
    pricing = Bactrian_core.Pricing.default;
    checks = None;
    unenforced = [];
    turn_timeout;
    total_timeout;
    mcp;
    sandbox;
    faketime = true;
  }

let positive name v =
  if Float.is_finite v && v > 0. then Ok ()
  else Error (Printf.sprintf "%s must be a positive number of seconds" name)

let record_cmd =
  let turns =
    Arg.(
      value & opt_all string []
      & info [ "turn" ] ~docv:"TEXT"
          ~doc:"A scripted user turn. Repeat for several turns, in order.")
  in
  let run json agent cwd turns scenario variant sim_now timezone env_passthrough
      turn_timeout total_timeout mcp_stdio mcp_http mcp_policy sandbox =
    match
      let* () =
        match turns with
        | [] -> Error "record needs at least one --turn"
        | _ :: _ ->
            let* () = positive "--turn-timeout" turn_timeout in
            positive "--total-timeout" total_timeout
      in
      Mcp_flags.parse ~stdio:mcp_stdio ~http:mcp_http ~policy:mcp_policy
    with
    | Error m -> `Error (false, m)
    | Ok mcp ->
        `Ok
          (run_command ~json ~live:true ~sandbox (fun _ sandbox ->
               Ok
                 (build_config ~agent ~cwd ~turns ~scenario ~variant ~sim_now
                    ~timezone ~replay:None ~env_passthrough ~turn_timeout
                    ~total_timeout ~mcp ~sandbox)))
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

type matcher_choice = Strict | Until_divergence | Fuzzy

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
               ("strict", Strict);
               ("until-divergence", Until_divergence);
               ("fuzzy", Fuzzy);
             ])
          Strict
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
    let matcher : (Bactrian_core.Matcher.mode, string) result =
      match (mode, threshold) with
      | Strict, None -> Ok Strict
      | Until_divergence, None -> Ok Until_divergence
      | Fuzzy, None -> Ok (Fuzzy 0.9)
      | Fuzzy, Some t when Float.is_finite t && t >= 0. && t <= 1. ->
          Ok (Fuzzy t)
      | Fuzzy, Some _ -> Error "--threshold must be between 0 and 1"
      | (Strict | Until_divergence), Some _ ->
          Error "--threshold only applies to --mode fuzzy"
    in
    match
      Result.bind matcher (fun matcher ->
          Result.bind (positive "--turn-timeout" turn_timeout) (fun () ->
              Result.map
                (fun () -> matcher)
                (positive "--total-timeout" total_timeout)))
    with
    | Error m -> `Error (false, m)
    | Ok matcher ->
        let live =
          match matcher with
          | Until_divergence -> true
          | Strict | Fuzzy _ -> false
        in
        `Ok
          (run_command ~json ~live ~sandbox (fun store sandbox ->
               let* mcp = Run.recorded_mcp store ~from in
               Ok
                 (build_config ~agent ~cwd ~turns:[] ~scenario ~variant ~sim_now
                    ~timezone
                    ~replay:(Some { Run.from; matcher; allow_reorder; timing })
                    ~env_passthrough ~turn_timeout ~total_timeout ~mcp ~sandbox)))
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
  Cmd.v info Term.(const check_command $ scenarios)

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
        Output.exit_usage
    | Some url ->
        Eio_main.run (fun env ->
            Mcp_shim.run ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
              ~stdin:(Eio.Stdenv.stdin env) ~stdout:(Eio.Stdenv.stdout env) ~url
              ~name);
        Output.exit_ok
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
        Output.exit_usage
    | Ok config -> (
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
           name an inherited descriptor by its number. *)
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
            Output.exit_environment)
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
  let run port outside inside =
    print_endline
      (Sandbox_probe.to_line (Sandbox_probe.run ~port ~outside ~inside));
    Output.exit_ok
  in
  let info =
    Cmd.info "__sandbox-probe" ~docs:Manpage.s_none ~exits:Cli.exits
      ~doc:"Internal: the check bactrian runs inside a new sandbox"
  in
  Cmd.v info
    Term.(
      const run $ port $ required "outside" "PATH" $ required "inside" "PATH")

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
      Doctor_cmd.cmd ~tls_available ~bactrian_exe;
      mcp_shim_cmd;
      sandbox_init_cmd;
      sandbox_probe_cmd;
    ]

(* The image {!Launch_env.scrub} started takes its environment back first. *)
let () =
  match Launch_env.restore () with
  | Ok () -> ()
  | Error m ->
      prerr_endline ("bactrian: " ^ m);
      exit Output.exit_internal

let () =
  let code =
    match Cmd.eval_value cmd with
    | Ok (`Ok n) -> n
    | Ok (`Version | `Help) -> Output.exit_ok
    | Error (`Parse | `Term) -> Output.exit_usage
    | Error `Exn -> Output.exit_internal
  in
  exit code
