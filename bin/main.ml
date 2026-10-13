open Cmdliner
open Bactrian_core
open Bactrian_runtime
module P = Model_proxy

let exits =
  Output.
    [
      Cmd.Exit.info exit_ok ~doc:"on success: the run passed.";
      Cmd.Exit.info exit_failed
        ~doc:"when the run failed (the agent) or diverged (a replay).";
      Cmd.Exit.info exit_not_run
        ~doc:"when the run did not happen because a spend cap was reached.";
      Cmd.Exit.info exit_usage ~doc:"on a usage or configuration error.";
      Cmd.Exit.info exit_environment
        ~doc:
          "when the environment is not ready, for example a spend cap of 0, a \
           missing API key or no TLS support for a live call.";
      Cmd.Exit.info exit_internal ~doc:"on an internal error.";
      Cmd.Exit.info Interrupt.exit_status
        ~doc:"when interrupted by SIGINT or SIGTERM.";
    ]

(* Whether this build can call a provider over HTTPS. The TLS connector is a
   separate library (bactrian.tls) that is not part of this build yet. *)
let tls_available = false

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

(* Under [--json] a failure before the run is one JSON object on stdout. *)
let fail ~json code message =
  if json then
    print_endline
      (Yojson.Safe.to_string
         (`Assoc [ ("error", `String message); ("exit", `Int code) ]))
  else prerr_endline ("bactrian: " ^ message);
  code

(* Running *)

let run_command ~json ~live (config : Run.config) =
  match read_caps () with
  | Error m -> fail ~json Output.exit_usage m
  | Ok caps -> (
      match if live then live_problem caps else None with
      | Some m -> fail ~json Output.exit_environment m
      | None -> (
          Mirage_crypto_rng_unix.use_default ();
          Eio_main.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          let root = Eio.Path.(Eio.Stdenv.cwd env / ".bactrian") in
          match Ledger.create ~path:Eio.Path.(root / "ledger.jsonl") ~caps with
          | exception Ledger.Corrupt { line; reason } ->
              fail ~json Output.exit_environment
                (Printf.sprintf
                   "the spend ledger .bactrian/ledger.jsonl is corrupt (line \
                    %d: %s)"
                   line reason)
          | ledger -> (
              match
                P.start ~sw ~net:(Eio.Stdenv.net env)
                  ~clock:(Eio.Stdenv.clock env)
                  ~process_mgr:(Eio.Stdenv.process_mgr env)
              with
              | Error m -> fail ~json Output.exit_environment m
              | Ok proxy -> (
                  match
                    Interrupt.run (fun () ->
                        Run.execute ~env ~proxy ~store:(Store.create ~root)
                          ~ledger config)
                  with
                  | Error signal ->
                      prerr_endline
                        (Printf.sprintf "bactrian: interrupted by signal %d"
                           signal);
                      Interrupt.exit_status
                  | Ok (Error m) -> fail ~json Output.exit_usage m
                  | Ok (Ok result) ->
                      print_string
                        (if json then Output.render_json result ^ "\n"
                         else Output.render_text result);
                      Output.exit_code result.outcome))))

(* Experiments *)

let located_line (e : Config_loader.located_error) =
  Printf.sprintf "%s:%d:%d: %s%s" e.file e.line e.column
    (if String.equal e.path "" then "" else e.path ^ ": ")
    e.message

let located_json (e : Config_loader.located_error) =
  `Assoc
    [
      ("file", `String e.file);
      ("line", `Int e.line);
      ("column", `Int e.column);
      ("path", `String e.path);
      ("message", `String e.message);
    ]

(* Configuration errors, all of them, with their locations. *)
let fail_located ~json errors =
  if json then
    print_endline
      (Yojson.Safe.to_string
         (`Assoc
            [
              ("errors", `List (List.map located_json errors));
              ("exit", `Int Output.exit_usage);
            ]))
  else List.iter (fun e -> prerr_endline (located_line e)) errors;
  Output.exit_usage

let experiment_command ~json ~experiment ~resume =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let announced = ref None in
  match
    Interrupt.run (fun () ->
        Executor.run_experiment ~env ~experiment_file:experiment ~resume
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
  | Ok (Error (Configuration errors)) -> fail_located ~json errors
  | Ok (Error (Usage m)) -> fail ~json Output.exit_usage m
  | Ok (Error (Environment m)) -> fail ~json Output.exit_environment m
  | Ok (Ok summary) ->
      print_string
        (if json then Output.render_summary_json summary ^ "\n"
         else
           Output.render_summary_text ~colour:(Lazy.force Output.colour) summary);
      Output.summary_exit_code summary

let check_command scenarios =
  Eio_main.run @@ fun env ->
  let results =
    List.map (fun scenario_file -> Golden.check ~env ~scenario_file) scenarios
  in
  match List.concat_map (function Ok _ -> [] | Error e -> e) results with
  | _ :: _ as errors -> fail_located ~json:false errors
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

let json =
  Arg.(
    value & flag
    & info [ "json" ] ~doc:"Print one JSON object instead of a summary.")

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

let agent_argv ~pos_after =
  let docv = "AGENT" in
  let doc = "The agent command and its arguments, after $(b,--)." in
  match pos_after with
  | `Start -> Arg.(non_empty & pos_all string [] & info [] ~docv ~doc)
  | `After n ->
      Arg.(non_empty & pos_right (n - 1) string [] & info [] ~docv ~doc)

let build_config ~agent ~cwd ~turns ~scenario ~variant ~sim_now ~timezone
    ~replay ~env_passthrough ~turn_timeout ~total_timeout : Run.config =
  {
    agent;
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
    mcp = [];
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
      turn_timeout total_timeout =
    match
      match turns with
      | [] -> Error "record needs at least one --turn"
      | _ :: _ ->
          Result.bind (positive "--turn-timeout" turn_timeout) (fun () ->
              positive "--total-timeout" total_timeout)
    with
    | Error m -> `Error (false, m)
    | Ok () ->
        `Ok
          (run_command ~json ~live:true
             (build_config ~agent ~cwd ~turns ~scenario ~variant ~sim_now
                ~timezone ~replay:None ~env_passthrough ~turn_timeout
                ~total_timeout))
  in
  let info =
    Cmd.info "record" ~exits
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
        ]
  in
  Cmd.v info
    Term.(
      ret
        (const run $ json
        $ agent_argv ~pos_after:`Start
        $ cwd $ turns $ scenario $ variant $ sim_now $ timezone
        $ env_passthrough $ turn_timeout $ total_timeout))

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
      variant sim_now timezone env_passthrough turn_timeout total_timeout =
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
          (run_command ~json ~live
             (build_config ~agent ~cwd ~turns:[] ~scenario ~variant ~sim_now
                ~timezone
                ~replay:(Some { Run.from; matcher; allow_reorder; timing })
                ~env_passthrough ~turn_timeout ~total_timeout))
  in
  let info =
    Cmd.info "replay" ~exits
      ~doc:"Replay a recorded run against an agent without calling providers"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Runs $(i,AGENT) with the turns of the recording and answers its \
             model requests from the recording in $(i,RUN), looked up in \
             ./.bactrian/runs. A request the recording lacks is a divergence.";
        ]
  in
  Cmd.v info
    Term.(
      ret
        (const run $ json $ from
        $ agent_argv ~pos_after:(`After 1)
        $ mode $ threshold $ allow_reorder $ timing $ cwd $ scenario $ variant
        $ sim_now $ timezone $ env_passthrough $ turn_timeout $ total_timeout))

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
  let run json experiment resume =
    experiment_command ~json ~experiment ~resume
  in
  let info =
    Cmd.info "run" ~exits
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
  Cmd.v info Term.(const run $ json $ experiment $ resume)

let check_cmd =
  let scenarios =
    Arg.(
      non_empty & pos_all string []
      & info [] ~docv:"SCENARIO" ~doc:"A scenario file (YAML or JSON).")
  in
  let info =
    Cmd.info "check" ~exits
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

(* Statistics *)

let ( let* ) = Result.bind

let compare_exits =
  Output.
    [
      Cmd.Exit.info exit_ok
        ~doc:
          "on success, or when $(b,--gate) is not given and the comparison ran.";
      Cmd.Exit.info 1
        ~doc:"with $(b,--gate), when a gate metric is a regression.";
      Cmd.Exit.info 2
        ~doc:
          "with $(b,--gate) and $(b,--fail-on-inconclusive), when a gate \
           metric is inconclusive.";
      Cmd.Exit.info 3
        ~doc:
          "with $(b,--gate), when more than 5% of the runs in an arm errored, \
           or the share of errored and unrun runs differs between the arms by \
           more than 5 percentage points.";
      Cmd.Exit.info exit_usage
        ~doc:"on a usage or configuration error, or an unknown experiment.";
      Cmd.Exit.info exit_internal
        ~doc:"on an internal error, such as unreadable results.";
    ]

let results_error ~json (e : Results.error) =
  let code =
    match e with
    | No_such_experiment _ -> Output.exit_usage
    | Bad_manifest _ | Bad_run _ -> Output.exit_internal
  in
  fail ~json code (Format.asprintf "%a" Results.pp_error e)

(* The experiment's results, the comparison and the report data, or the exit
   status of the failure already printed. *)
let analyse ~json ~baseline ~candidate ~fail_on_inconclusive ~experiment_id env
    =
  let* loaded =
    Results.load ~fs:(Eio.Stdenv.fs env)
      ~root:Eio.Path.(Eio.Stdenv.cwd env / ".bactrian")
      ~experiment_id
    |> Result.map_error (results_error ~json)
  in
  let* settings =
    Results.settings loaded ~baseline ~candidate ~fail_on_inconclusive
    |> Result.map_error (fail ~json Output.exit_usage)
  in
  let* result =
    Compare.run settings |> Result.map_error (fail ~json Output.exit_usage)
  in
  Ok
    (Report_data.build ~experiment_id ~experiment:loaded.manifest.name ~settings
       ~result loaded.runs)

let compare_command ~json ~gate ~fail_on_inconclusive ~baseline ~candidate
    experiment_id =
  Eio_main.run @@ fun env ->
  match
    analyse ~json ~baseline ~candidate ~fail_on_inconclusive ~experiment_id env
  with
  | Error code -> code
  | Ok data ->
      print_string
        (if json then Stats_output.render_compare_json data
         else
           Stats_output.render_compare_text ~colour:(Lazy.force Output.colour)
             data);
      if gate then data.exit else Output.exit_ok

let report_command ~baseline ~candidate ~output experiment_id =
  Eio_main.run @@ fun env ->
  match
    analyse ~json:false ~baseline ~candidate ~fail_on_inconclusive:false
      ~experiment_id env
  with
  | Error code -> code
  | Ok data -> (
      match
        Store.write_atomic
          Eio.Path.(Eio.Stdenv.fs env / output)
          (Report_html.render data)
      with
      | () ->
          print_endline ("wrote " ^ output);
          Output.exit_ok
      | exception Eio.Io _ ->
          fail ~json:false Output.exit_environment
            (Printf.sprintf "cannot write %s" output))

let plan_command ~json ~rates ~scenarios ~true_effect ~threshold ~population
    ~between_sd ~power ~confidence ~max_repeats ~simulations ~seed =
  let count = Option.value scenarios ~default:(List.length rates) in
  let in_unit name v =
    if Float.is_finite v && v > 0. && v < 1. then Ok ()
    else Error (Printf.sprintf "%s must be strictly between 0 and 1" name)
  in
  let non_negative name v =
    if Float.is_finite v && v >= 0. then Ok ()
    else Error (Printf.sprintf "%s must be a non-negative number" name)
  in
  let* () =
    match rates with
    | [] -> Error "plan needs at least one --baseline-rate"
    | _ :: _ ->
        if List.for_all (fun r -> Float.is_finite r && r >= 0. && r <= 1.) rates
        then Ok ()
        else Error "--baseline-rate must be between 0 and 1"
  in
  let* () =
    if count < 1 then Error "--scenarios must be at least 1"
    else if
      List.compare_length_with rates 1 > 0
      && List.compare_length_with rates count <> 0
    then
      Error
        "give one --baseline-rate for every scenario, or a single one for all \
         of them"
    else Ok ()
  in
  let* () = non_negative "--threshold" threshold in
  let* () =
    if Float.is_finite true_effect then Ok ()
    else Error "--effect must be a number"
  in
  let* () = in_unit "--power" power in
  let* () = in_unit "--confidence" confidence in
  let* () =
    if max_repeats < 1 || simulations < 1 then
      Error "--max-repeats and --simulations must be at least 1"
    else Ok ()
  in
  let* () =
    match (population, between_sd) with
    | `Suite, _ -> Ok ()
    | `Scenarios, None -> Error "--population scenarios needs --between-sd"
    | `Scenarios, Some sd ->
        if count < 10 then
          Error "--population scenarios needs at least 10 --scenarios"
        else non_negative "--between-sd" sd
  in
  let rate i =
    match rates with
    | [ r ] -> r
    | _ -> Option.value (List.nth_opt rates i) ~default:0.
  in
  let assumption : Plan_sim.assumption =
    {
      baseline_rates =
        List.init count (fun i ->
            (Printf.sprintf "scenario-%d" (i + 1), rate i));
      true_effect;
      threshold;
      between_sd;
      population;
      confidence;
      power;
      max_repeats;
      seed;
      simulations;
    }
  in
  let answer = Plan_sim.plan assumption in
  print_string
    (if json then Stats_output.render_plan_json assumption answer
     else Stats_output.render_plan_text assumption answer);
  Ok Output.exit_ok

let experiment_id =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"EXPERIMENT-ID"
        ~doc:"An experiment under ./.bactrian/experiments.")

let baseline_flag =
  Arg.(
    value
    & opt (some string) None
    & info [ "baseline" ] ~docv:"VARIANT"
        ~doc:
          "The baseline variant. Defaults to $(b,compare.baseline) in the \
           experiment file the manifest records.")

let candidate_flag =
  Arg.(
    value
    & opt (some string) None
    & info [ "candidate" ] ~docv:"VARIANT"
        ~doc:
          "The candidate variant. Defaults to the other variant when the \
           experiment has exactly two.")

let comparison_man =
  [
    `S Manpage.s_description;
    `P
      "Compares the candidate with the baseline over the runs of \
       $(i,EXPERIMENT-ID). The estimand, confidence, gate and thresholds come \
       from the $(b,compare) section of the experiment file that the manifest \
       records. Every gate metric needs a threshold there, under \
       $(b,compare.thresholds): none is ever assumed, and a missing one is a \
       usage error naming the metric. The file must therefore still exist and \
       have a $(b,compare) section. $(b,--baseline) and $(b,--candidate) \
       override its baseline and pick the candidate.";
  ]

let compare_cmd =
  let gate =
    Arg.(
      value & flag
      & info [ "gate" ]
          ~doc:"Exit with the gate's status (see EXIT STATUS), not 0.")
  in
  let fail_on_inconclusive =
    Arg.(
      value & flag
      & info [ "fail-on-inconclusive" ]
          ~doc:"Make an inconclusive gate metric exit 2 under $(b,--gate).")
  in
  let run json gate fail_on_inconclusive baseline candidate id =
    compare_command ~json ~gate ~fail_on_inconclusive ~baseline ~candidate id
  in
  let info =
    Cmd.info "compare" ~exits:compare_exits
      ~doc:"Compare two variants of an experiment with confidence intervals"
      ~man:comparison_man
  in
  Cmd.v info
    Term.(
      const run $ json $ gate $ fail_on_inconclusive $ baseline_flag
      $ candidate_flag $ experiment_id)

let report_cmd =
  let output =
    Arg.(
      required
      & opt (some string) None
      & info [ "o"; "output" ] ~docv:"FILE"
          ~doc:"Where to write the HTML report.")
  in
  let run baseline candidate output id =
    report_command ~baseline ~candidate ~output id
  in
  let info =
    Cmd.info "report" ~exits
      ~doc:"Write a self-contained HTML report of an experiment"
      ~man:
        (comparison_man
        @ [
            `P
              "The report is one file with inline styles and charts, no \
               scripts and no external assets, and works offline.";
          ])
  in
  Cmd.v info
    Term.(const run $ baseline_flag $ candidate_flag $ output $ experiment_id)

let plan_cmd =
  let rates =
    Arg.(
      value & opt_all float []
      & info [ "baseline-rate" ] ~docv:"R"
          ~doc:
            "The baseline pass rate of a scenario, from 0 to 1. Repeat it for \
             several scenarios, or give one for all of them.")
  in
  let scenarios =
    Arg.(
      value
      & opt (some int) None
      & info [ "scenarios" ] ~docv:"N"
          ~doc:"The number of scenarios (default: the number of rates).")
  in
  let effect_flag =
    Arg.(
      value
      & opt (some float) None
      & info [ "effect" ] ~docv:"E"
          ~doc:
            "The assumed true drop in pass rate of the candidate (default \
             twice the threshold).")
  in
  let threshold =
    Arg.(
      required
      & opt (some float) None
      & info [ "threshold" ] ~docv:"T" ~doc:"The regression threshold.")
  in
  let population =
    Arg.(
      value
      & opt (enum [ ("suite", `Suite); ("scenarios", `Scenarios) ]) `Suite
      & info [ "population" ] ~docv:"POP"
          ~doc:"The estimand, $(b,suite) or $(b,scenarios).")
  in
  let between_sd =
    Arg.(
      value
      & opt (some float) None
      & info [ "between-sd" ] ~docv:"S"
          ~doc:
            "The standard deviation of the effect between scenarios (required \
             for $(b,--population scenarios)).")
  in
  let power =
    Arg.(
      value & opt float 0.8
      & info [ "power" ] ~docv:"P"
          ~doc:"The probability of the correct verdict to reach.")
  in
  let confidence =
    Arg.(
      value & opt float 0.95
      & info [ "confidence" ] ~docv:"C" ~doc:"The confidence level.")
  in
  let max_repeats =
    Arg.(
      value & opt int 200
      & info [ "max-repeats" ] ~docv:"N" ~doc:"The largest repeat count to try.")
  in
  let simulations =
    Arg.(
      value & opt int 500
      & info [ "simulations" ] ~docv:"N"
          ~doc:"Simulated experiments per repeat count.")
  in
  let seed =
    Arg.(value & opt int 0 & info [ "seed" ] ~docv:"N" ~doc:"The random seed.")
  in
  let run json rates scenarios effect_opt threshold population between_sd power
      confidence max_repeats simulations seed =
    let true_effect = Option.value effect_opt ~default:(2. *. threshold) in
    match
      plan_command ~json ~rates ~scenarios ~true_effect ~threshold ~population
        ~between_sd ~power ~confidence ~max_repeats ~simulations ~seed
    with
    | Ok code -> `Ok code
    | Error m -> `Error (false, m)
  in
  let info =
    Cmd.info "plan" ~exits ~doc:"Say how many repeats an experiment needs"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Simulates the exact decision rule of $(b,compare) and prints the \
             smallest repeat count per scenario and variant that reaches the \
             correct verdict with the requested power, with the Monte Carlo \
             standard error. It says $(b,unreachable) when no count up to \
             $(b,--max-repeats) gets there.";
        ]
  in
  Cmd.v info
    Term.(
      ret
        (const run $ json $ rates $ scenarios $ effect_flag $ threshold
       $ population $ between_sd $ power $ confidence $ max_repeats
       $ simulations $ seed))

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
    Cmd.info "mcp-shim" ~exits
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

let cmd =
  let info =
    Cmd.info "bactrian" ~version:Bactrian_core.Version.current ~exits
      ~doc:"Record, replay and statistically compare AI agent runs"
  in
  let default = Term.(ret (const (`Help (`Auto, None)))) in
  Cmd.group ~default info
    [
      record_cmd;
      replay_cmd;
      run_cmd;
      check_cmd;
      compare_cmd;
      plan_cmd;
      report_cmd;
      mcp_shim_cmd;
    ]

let () =
  let code =
    match Cmd.eval_value cmd with
    | Ok (`Ok n) -> n
    | Ok (`Version | `Help) -> Output.exit_ok
    | Error (`Parse | `Term) -> Output.exit_usage
    | Error `Exn -> Output.exit_internal
  in
  exit code
