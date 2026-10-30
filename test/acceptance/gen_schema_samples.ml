(* Real files for the schema validator (dev/schemas). The binary under test runs
   an experiment, a replay that diverges, compare, plan and check against a fake
   upstream in a process of its own, and everything it leaves behind is kept in
   the directory named by the first argument. Nothing leaves this machine.

   The second argument is the validator's fixtures directory. Each scenario,
   experiment and price file fixture goes through the binary's own decoders too,
   which must reject exactly the invalid ones, each at the path of its fault, so
   the schemas and the decoders cannot drift apart unnoticed. *)

open Bactrian_core
open Acceptance_world
open Cli_world

let variable = "BACTRIAN_UPSTREAM_OPENAI"
let argv = [ "python3"; "-I"; "-c"; Weather_agent.script ]

let argv_json =
  Yojson.Safe.to_string (`List (List.map (fun s -> `String s) argv))

(* The scenario and the experiment are YAML on purpose: the validator reads the
   same dialect bactrian does. *)
let scenario =
  Printf.sprintf
    {|version: 1
id: weather
now: 2026-10-14T09:00:00+01:00
timezone: Europe/London
turns:
  - user: %S
assertions:
  - tool_sequence: { mode: exact, tools: [get_weather] }
  - tool_args: { tool: get_weather, match: { city: Paris } }
  - output: { turn: last, regex: "(?i)paris" }
  - max_steps: 2
  - max_tokens: 100000
  - max_cost_usd: 1
  - max_latency: 60s
checkers:
  - command: ["python3", "-I", "checks/ok.py"]
golden:
  pass: [golden/ok.trace.jsonl]
|}
    first_turn

let checker =
  {|import json, sys
trace = json.load(sys.stdin)
print(json.dumps({"pass": len(trace) > 0, "score": 1.0, "message": "fine"}))
|}

let prices =
  {|version: 1
prices:
  - model: gpt-6-luna
    provider: openai
    effective_from: 2026-01-01
    rates: { input: 1, cached_input: 0.1, cache_write: 0, output: 4 }
    long_context:
      above: 200000
      rates: { input: 2, cached_input: 0.2, cache_write: 0, output: 8 }
    max_output: 128000
    max_input: 400000
|}

let experiment =
  Printf.sprintf
    {|version: 1
name: weather
agent:
  protocol: command
  command: %s
  timeouts: { turn: 120s, total: 600s }
scenarios: ["scenarios/*.yaml"]
variants:
  base: {}
  terse: { env: { %s: "You are a terse assistant." } }
repeats: 2
concurrency: 1
seed: 3
sandbox: { mode: "off" }
caps_usd: { openai: 100 }
normalise: ["\\d{4}-\\d{2}-\\d{2}"]
redact: ["secret-[a-z]+"]
pricing: prices.yaml
compare:
  baseline: base
  thresholds: { pass_rate: 0.1 }
|}
    argv_json prompt_variable

let save dir name (printed : Canon.t) =
  write
    (Filename.concat dir ("outputs/" ^ name))
    (Canon.to_string printed ^ "\n")

let expect_code what wanted got =
  if wanted <> got then (
    Printf.eprintf "%s exited %d, expected %d\n" what got wanted;
    exit 1)

let absolute d =
  if Filename.is_relative d then Filename.concat (Sys.getcwd ()) d else d

(* The binary's exit status 64 is a configuration error. *)
let config_error = 64

(* Fixtures are copied beside an empty fixtures/ directory, which the valid
   scenario refers to, and run from there. A price file is only read as an
   experiment's pricing, and only [run] loads it, so it gets an experiment of
   its own that names it and a scenario to find. That run stops at the missing
   spend cap and agent, long before anything starts or costs. *)
let price_experiment price_file =
  Printf.sprintf
    {|version: 1
name: pricing
agent: { protocol: command, command: ["x"] }
scenarios: ["s.yaml"]
variants: { a: {} }
pricing: %S
|}
    price_file

(* Where bactrian must report each invalid fixture's fault. Rejecting a fixture
   for another reason, say in the shared s.yaml, would not show that the schema
   and the decoder agree. *)
let expected_paths =
  [
    ("experiment.command-with-url.yaml", "agent.url");
    ("experiment.empty-command.yaml", "agent.command");
    ("experiment.http-strict-mapping.yaml", "sandbox");
    ("experiment.http-strict.yaml", "sandbox");
    ("experiment.http-with-command.yaml", "agent.command");
    ("experiment.http-without-header.yaml", "agent.namespace_header");
    ("experiment.mcp-command-and-url.yaml", "mcp.s");
    ("experiment.mcp-replay-policy.yaml", "mcp.s.policy");
    ("experiment.no-variants.yaml", "variants");
    ("experiment.no-version.yaml", "version");
    ( "experiment.pass-rate-threshold-above-one.yaml",
      "compare.thresholds.pass_rate" );
    ("experiment.unknown-provider-cap.yaml", "caps_usd.mistral");
    ("experiment.vm-without-image.yaml", "sandbox.image");
    ("experiment.zero-timeout.yaml", "agent.timeouts.turn");
    ("price-file.date-trailing-newline.json", "prices[0].effective_from");
    ("price-file.negative-rate.json", "prices[0].rates.input");
    ("price-file.no-max-output.json", "prices[0].max_output");
    ("scenario.exact-without-match.yaml", "assertions[0].tool_args.match");
    ("scenario.negative-limit.yaml", "assertions[0].max_steps");
    ("scenario.no-turns.yaml", "turns");
    ("scenario.output-turn-zero.yaml", "assertions[0].output.turn");
    ("scenario.output-two-checks.yaml", "assertions[0].output");
    ("scenario.schema-and-match.yaml", "assertions[0].tool_args.match");
    ( "scenario.schema-unsupported-keyword.yaml",
      "assertions[0].tool_args.schema.properties.city.minLength" );
    ("scenario.two-keys-assertion.yaml", "assertions[0]");
    ("scenario.unknown-assertion.yaml", "assertions[0].sing");
  ]

let errors_of printed =
  Option.value ~default:[] (Canon.list_member "errors" printed)

let reports ~file ~path printed =
  let is key want e =
    Option.equal String.equal (Canon.str_member key e) (Some want)
  in
  List.exists
    (fun e -> is "file" file e && is "path" path e)
    (errors_of printed)

let check_fixtures_agree ~dir ~fixtures ~env =
  let copy = Filename.concat dir "agreement" in
  write (Filename.concat copy "valid/fixtures/basic/.keep") "";
  List.iter
    (fun verdict ->
      write
        (Filename.concat copy (verdict ^ "/s.yaml"))
        "version: 1\nid: s\nturns: [{ user: hi }]\n")
    [ "valid"; "invalid" ];
  List.iter
    (fun (verdict, should_reject) ->
      let from = Filename.concat fixtures verdict in
      Array.iter
        (fun name ->
          let relative = Filename.concat verdict name in
          let args =
            match String.split_on_char '.' name with
            | "experiment" :: _ ->
                Some [ "doctor"; "--experiment"; relative; "--json" ]
            | "scenario" :: _ ->
                Some [ "check"; relative; "--json"; "--sandbox"; "off" ]
            | "price-file" :: _ ->
                let experiment = relative ^ ".experiment.yaml" in
                write (Filename.concat copy experiment) (price_experiment name);
                Some [ "run"; experiment; "--sandbox"; "off"; "--json" ]
            | _ -> None
          in
          Option.iter
            (fun args ->
              write
                (Filename.concat copy relative)
                (read (Filename.concat from name));
              let code, printed = cli ~dir:copy ~env args in
              let fail why =
                Printf.eprintf "%s: %s (exit %d)\n" relative why code;
                exit 1
              in
              match (should_reject, List.assoc_opt name expected_paths) with
              | false, _ ->
                  if
                    code = config_error
                    || not (List.is_empty (errors_of printed))
                  then fail "bactrian rejected the file"
              | true, None -> fail "no expected error path is listed for it"
              | true, Some path ->
                  if
                    code <> config_error
                    || not (reports ~file:relative ~path printed)
                  then fail ("bactrian reported no error at " ^ path))
            args)
        (Sys.readdir from))
    [ ("valid", false); ("invalid", true) ];
  List.iter
    (fun (name, _) ->
      if not (Sys.file_exists (Filename.concat fixtures ("invalid/" ^ name)))
      then (
        Printf.eprintf "%s is listed but is not a fixture\n" name;
        exit 1))
    expected_paths

let () =
  let dir, fixtures =
    match Sys.argv with
    | [| _; d; f |] -> (absolute d, absolute f)
    | _ ->
        prerr_endline "usage: gen_schema_samples DIR FIXTURES";
        exit 2
  in
  remove_tree dir;
  write (Filename.concat dir "scenarios/weather.yaml") scenario;
  write (Filename.concat dir "scenarios/checks/ok.py") checker;
  write (Filename.concat dir "prices.yaml") prices;
  write (Filename.concat dir "exp.yaml") experiment;
  with_upstream ~dir @@ fun up ->
  let env = (variable, upstream_url up) :: key_env in
  let cli' what ?(code = 0) args =
    let got, printed = cli ~dir ~env args in
    expect_code what code got;
    printed
  in
  let ran = cli' "run" [ "run"; "exp.yaml"; "--sandbox"; "off"; "--json" ] in
  save dir "run.json" ran;
  let runs =
    match Canon.list_member "runs" ran with
    | Some l -> l
    | None -> Alcotest.fail "run printed no runs"
  in
  let first = string_field "run_id" (List.hd runs) in
  let experiment_id = string_field "experiment_id" ran in
  (* A golden trace for the scenario: the first run's own trace. *)
  write
    (Filename.concat dir "scenarios/golden/ok.trace.jsonl")
    (read (Filename.concat dir (".bactrian/runs/" ^ first ^ "/trace.jsonl")));
  save dir "check.json"
    (cli' "check"
       [ "check"; "scenarios/weather.yaml"; "--json"; "--sandbox"; "off" ]);
  (* A replay whose request differs from the recording diverges. *)
  save dir "replay.json"
    (cli' "replay" ~code:1
       ([ "replay"; first; "--json"; "--sandbox"; "off"; "--" ]
       @ argv @ [ "Answer in French." ]));
  save dir "compare.json"
    (cli' "compare" [ "compare"; experiment_id; "--json" ]);
  save dir "plan.json"
    (cli' "plan"
       [
         "plan";
         "--baseline-rate";
         "0.8";
         "--threshold";
         "0.1";
         "--simulations";
         "50";
         "--max-repeats";
         "30";
         "--json";
       ]);
  check_fixtures_agree ~dir ~fixtures ~env
