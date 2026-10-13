open Cmdliner
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

let fail code message =
  prerr_endline ("bactrian: " ^ message);
  code

(* Running *)

let run_command ~json ~live (config : Run.config) =
  match read_caps () with
  | Error m -> fail Output.exit_usage m
  | Ok caps -> (
      match if live then live_problem caps else None with
      | Some m -> fail Output.exit_environment m
      | None -> (
          Mirage_crypto_rng_unix.use_default ();
          Eio_main.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          let root = Eio.Path.(Eio.Stdenv.cwd env / ".bactrian") in
          let ledger =
            Ledger.create ~path:Eio.Path.(root / "ledger.jsonl") ~caps
          in
          match
            P.start ~sw ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
          with
          | Error m -> fail Output.exit_environment m
          | Ok proxy -> (
              match
                Run.execute ~env ~proxy ~store:(Store.create ~root) ~ledger
                  config
              with
              | Error m -> fail Output.exit_usage m
              | Ok result ->
                  print_string
                    (if json then Output.render_json result ^ "\n"
                     else Output.render_text result);
                  Output.exit_code result.outcome)))

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
        ~doc:
          "Longest the whole run may take, and the time to exit after the end.")

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
    turn_timeout;
    total_timeout;
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

let cmd =
  let info =
    Cmd.info "bactrian" ~version:Bactrian_core.Version.current ~exits
      ~doc:"Record, replay and statistically compare AI agent runs"
  in
  let default = Term.(ret (const (`Help (`Auto, None)))) in
  Cmd.group ~default info [ record_cmd; replay_cmd ]

let () =
  let code =
    match Cmd.eval_value cmd with
    | Ok (`Ok n) -> n
    | Ok (`Version | `Help) -> Output.exit_ok
    | Error (`Parse | `Term) -> Output.exit_usage
    | Error `Exn -> Output.exit_internal
  in
  exit code
