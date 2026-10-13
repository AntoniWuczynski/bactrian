open Bactrian_core

let canon json =
  match Canon.of_string json with
  | Ok c -> c
  | Error _ -> Alcotest.fail "test document is not JSON"

let pp_errors = Fmt.(list ~sep:semi Config_error.pp)

let decoded json =
  match Experiment.decode (canon json) with
  | Ok t -> t
  | Error es -> Alcotest.failf "unexpected errors: %a" pp_errors es

let errors json =
  match Experiment.decode (canon json) with
  | Ok _ -> Alcotest.fail "expected errors"
  | Error es -> es

let paths json =
  List.sort String.compare
    (List.map (fun (e : Experiment.error) -> e.path) (errors json))

let strings = Alcotest.(list string)

let agent_cmd =
  {|"agent": {"protocol": "command", "command": ["node", "a.js"]}|}

(* A valid document with [extra] fields and [skip] left out. *)
let doc ?(agent = agent_cmd) ?(variants = {|"variants": {"a": {}}|}) extra =
  Printf.sprintf
    {|{"version": 1, "name": "e", %s, "scenarios": ["s/*.yaml"], %s%s}|} agent
    variants extra

let test_full () =
  let t =
    decoded
      {|{"version": 1, "name": "luna-vs-flash",
         "agent": {"protocol": "command", "command": ["node", "dist/adapter.js"],
                   "timeouts": {"turn": "120s", "total": "10m"},
                   "env_passthrough": ["LANG2"]},
         "scenarios": ["scenarios/*.yaml"],
         "variants": {"luna": {"env": {"AGENT_MODEL": "m1"}},
                      "flash": {"env": {"AGENT_MODEL": "m2"}}},
         "repeats": 20, "concurrency": 4,
         "intercept": ["model", "egress"],
         "egress": {"default": "replay", "hosts": {"www.googleapis.com": "record"}},
         "mcp": {"calendar": "passthrough"},
         "normalise": ["\\d{4}-\\d{2}-\\d{2}"], "redact": ["tok_[a-z]+"],
         "sandbox": "strict", "clock": {"faketime": "off"},
         "caps_usd": {"openai": 10, "gemini": 20.5}, "pricing": "./prices.yaml",
         "seed": 7,
         "compare": {"baseline": "luna", "confidence": 0.9,
                     "thresholds": {"pass_rate": 0.05, "cost": 0.2},
                     "gate": ["pass_rate", "cost"], "population": "scenarios"}}|}
  in
  Alcotest.(check string) "name" "luna-vs-flash" t.name;
  (match t.agent with
  | Command
      {
        argv = [ "node"; "dist/adapter.js" ];
        turn_timeout = 120.0;
        total_timeout = 600.0;
      } ->
      ()
  | Command _ | Http _ -> Alcotest.fail "agent");
  Alcotest.(check strings) "passthrough" [ "LANG2" ] t.env_passthrough;
  Alcotest.(check strings) "scenarios" [ "scenarios/*.yaml" ] t.scenarios;
  Alcotest.(check (list (pair string (list (pair string string)))))
    "variants"
    [
      ("flash", [ ("AGENT_MODEL", "m2") ]); ("luna", [ ("AGENT_MODEL", "m1") ]);
    ]
    (List.map (fun (v : Experiment.variant) -> (v.name, v.env)) t.variants);
  Alcotest.(check (pair int int))
    "repeats, concurrency" (20, 4) (t.repeats, t.concurrency);
  Alcotest.(check bool) "intercept" true (t.intercept = [ `Model; `Egress ]);
  Alcotest.(check bool)
    "egress default" true
    (t.egress_default = Experiment.Replay);
  Alcotest.(check bool)
    "hosts" true
    (t.egress_hosts = [ ("www.googleapis.com", Experiment.Record) ]);
  Alcotest.(check bool)
    "mcp" true
    (t.mcp = [ ("calendar", Experiment.Passthrough) ]);
  Alcotest.(check int) "normalise" 1 (List.length t.normalise);
  Alcotest.(check int) "redact" 1 (List.length t.redact);
  Alcotest.(check bool) "sandbox" true (t.sandbox = `Strict);
  Alcotest.(check bool) "faketime" false t.faketime;
  Alcotest.(check bool)
    "caps" true
    (t.caps_usd = [ ("gemini", 20.5); ("openai", 10.0) ]);
  Alcotest.(check (option string)) "pricing" (Some "./prices.yaml") t.pricing;
  Alcotest.(check int) "seed" 7 t.seed;
  match t.compare with
  | Some c ->
      Alcotest.(check string) "baseline" "luna" c.baseline;
      Alcotest.(check (float 0.0)) "confidence" 0.9 c.confidence;
      Alcotest.(check bool)
        "thresholds" true
        (c.thresholds = [ ("cost", 0.2); ("pass_rate", 0.05) ]);
      Alcotest.(check strings) "gate" [ "pass_rate"; "cost" ] c.gate;
      Alcotest.(check bool) "population" true (c.population = `Scenarios)
  | None -> Alcotest.fail "compare"

let test_defaults () =
  let t = decoded (doc "") in
  Alcotest.(check int) "repeats" 1 t.repeats;
  Alcotest.(check int) "concurrency" 1 t.concurrency;
  Alcotest.(check int) "seed" 0 t.seed;
  Alcotest.(check bool)
    "intercept" true
    (t.intercept = [ `Model; `Mcp; `Fixtures; `Egress ]);
  Alcotest.(check bool)
    "egress default is block" true
    (t.egress_default = Experiment.Block);
  Alcotest.(check bool) "no hosts" true (t.egress_hosts = []);
  Alcotest.(check bool) "no mcp" true (t.mcp = []);
  Alcotest.(check bool) "sandbox auto" true (t.sandbox = `Auto);
  Alcotest.(check bool) "faketime auto" true t.faketime;
  Alcotest.(check bool) "no caps" true (t.caps_usd = []);
  Alcotest.(check (option string)) "pricing" None t.pricing;
  Alcotest.(check strings) "passthrough" [] t.env_passthrough;
  Alcotest.(check bool) "no compare" true (t.compare = None);
  match t.agent with
  | Command { turn_timeout = 120.0; total_timeout = 600.0; _ } -> ()
  | Command _ | Http _ -> Alcotest.fail "default timeouts"

let test_compare_defaults () =
  match (decoded (doc {|, "compare": {"baseline": "a"}|})).compare with
  | Some c ->
      Alcotest.(check strings) "gate" [ "pass_rate" ] c.gate;
      Alcotest.(check bool) "population suite" true (c.population = `Suite);
      Alcotest.(check (float 0.0)) "confidence" 0.95 c.confidence;
      Alcotest.(check bool) "thresholds" true (c.thresholds = [])
  | None -> Alcotest.fail "compare"

let test_http () =
  let t =
    decoded
      (doc
         ~agent:
           {|"agent": {"protocol": "http", "url": "http://localhost:3000/chat",
                       "namespace_header": "x-ns", "timeouts": {"turn": "1.5s", "total": "2m"}}|}
         "")
  in
  match t.agent with
  | Http
      {
        url = "http://localhost:3000/chat";
        namespace_header = "x-ns";
        turn_timeout = 1.5;
        total_timeout = 120.0;
      } ->
      ()
  | Http _ | Command _ -> Alcotest.fail "agent"

let test_ms_duration () =
  let t =
    decoded
      (doc
         ~agent:
           {|"agent": {"protocol": "command", "command": ["x"], "timeouts": {"turn": "250ms", "total": "1.5s"}}|}
         "")
  in
  match t.agent with
  | Command { turn_timeout = 0.25; total_timeout = 1.5; _ } -> ()
  | Command _ | Http _ -> Alcotest.fail "ms duration"

let test_caseless_normalise () =
  let t = decoded (doc {|, "normalise": ["(?i)date: .*"]|}) in
  Alcotest.(check int) "normalise" 1 (List.length t.normalise)

let test_variant_list () =
  let t =
    decoded
      (doc
         ~variants:
           {|"variants": [{"name": "x", "env": {"K": "v"}}, {"name": "y"}]|}
         "")
  in
  Alcotest.(check strings)
    "names" [ "x"; "y" ]
    (List.map (fun (v : Experiment.variant) -> v.name) t.variants)

let test_paths name expected json () =
  Alcotest.(check strings) name expected (paths json)

let case name expected json =
  Alcotest.test_case name `Quick (test_paths name expected json)

let test_message () =
  match errors (doc {|, "colour": 1|}) with
  | [ { path = "colour"; message = "unknown field" } ] -> ()
  | es -> Alcotest.failf "got %a" pp_errors es

(* Review M7: settings this build parses but does not enforce are listed. *)
let test_unenforced () =
  let names t =
    List.map
      (fun w -> List.hd (String.split_on_char ':' w))
      (Experiment.unenforced t)
  in
  Alcotest.check strings "defaults"
    [ "sandbox"; "egress"; "clock" ]
    (names (decoded (doc "")));
  Alcotest.check strings "mcp servers listed"
    [ "sandbox"; "egress"; "mcp"; "clock" ]
    (names (decoded (doc {|, "mcp": {"calendar": "record"}|})));
  Alcotest.check strings "everything off" []
    (names
       (decoded
          (doc
             {|, "sandbox": "off", "intercept": ["model", "fixtures"], "clock": {"faketime": "off"}|})));
  Alcotest.check strings "strict is refused elsewhere, not listed here"
    [ "egress"; "clock" ]
    (names (decoded (doc {|, "sandbox": "strict"|})))

(* Review m16: a variant cannot set a variable bactrian sets itself. *)
let test_reserved_env () =
  let reserved = Experiment.reserved_env in
  Alcotest.(check bool) "TZ" true (List.mem "TZ" reserved);
  Alcotest.(check bool) "key" true (List.mem "OPENAI_API_KEY" reserved);
  Alcotest.check strings "mapping form"
    [ "variants.a.env.TZ"; "variants.b.env.BACTRIAN_RUN_ID" ]
    (paths
       (doc
          ~variants:
            {|"variants": {"a": {"env": {"TZ": "UTC", "OK": "1"}}, "b": {"env": {"BACTRIAN_RUN_ID": "x"}}}|}
          ""));
  Alcotest.check strings "list form"
    [ "variants[0].env.SIM_NOW" ]
    (paths
       (doc ~variants:{|"variants": [{"name": "a", "env": {"SIM_NOW": "x"}}]|}
          ""))

(* Review m1: a bad baseline is reported together with the other errors. *)
let test_baseline_with_other_errors () =
  Alcotest.check strings "both reported"
    [ "compare.baseline"; "repeats" ]
    (paths (doc {|, "repeats": 0, "compare": {"baseline": "zzz"}|}))

let five =
  {|{"version": 1, "name": 5, "bogus": 1,
     "agent": {"protocol": "command", "command": ["x"], "timeouts": {"turn": "soon"}},
     "scenarios": ["s"],
     "variants": [{"name": "a"}, {"name": "a"}],
     "repeats": 0}|}

let () =
  Alcotest.run "experiment"
    [
      ( "valid",
        [
          Alcotest.test_case "full document" `Quick test_full;
          Alcotest.test_case "defaults" `Quick test_defaults;
          Alcotest.test_case "compare defaults" `Quick test_compare_defaults;
          Alcotest.test_case "http agent and durations" `Quick test_http;
          Alcotest.test_case "millisecond durations" `Quick test_ms_duration;
          Alcotest.test_case "caseless normalise" `Quick test_caseless_normalise;
          Alcotest.test_case "variants as a list" `Quick test_variant_list;
          Alcotest.test_case "unenforced settings" `Quick test_unenforced;
        ] );
      ( "errors",
        [
          case "root not an object" [ "" ] "3";
          case "unknown field" [ "colour" ] (doc {|, "colour": 1|});
          Alcotest.test_case "unknown message" `Quick test_message;
          Alcotest.test_case "reserved variant variables" `Quick
            test_reserved_env;
          Alcotest.test_case "baseline with other errors" `Quick
            test_baseline_with_other_errors;
          case "unknown nested field" [ "agent.extra" ]
            (doc
               ~agent:
                 {|"agent": {"protocol": "command", "command": ["x"], "extra": 1}|}
               "");
          case "wrong type" [ "name" ]
            {|{"version":1,"name":3,"agent":{"protocol":"command","command":["x"]},"scenarios":["s"],"variants":{"a":{}}}|};
          case "missing name" [ "name" ]
            {|{"version":1,"agent":{"protocol":"command","command":["x"]},"scenarios":["s"],"variants":{"a":{}}}|};
          case "missing agent, scenarios, variants"
            [ "agent"; "scenarios"; "variants" ]
            {|{"version":1,"name":"e"}|};
          case "wrong version" [ "version" ]
            {|{"version":2,"name":"e","agent":{"protocol":"command","command":["x"]},"scenarios":["s"],"variants":{"a":{}}}|};
          case "empty scenarios" [ "scenarios" ]
            {|{"version":1,"name":"e","agent":{"protocol":"command","command":["x"]},"scenarios":[],"variants":{"a":{}}}|};
          case "empty variants" [ "variants" ]
            (doc ~variants:{|"variants": {}|} "");
          case "duplicate variant names" [ "variants[2].name" ]
            (doc
               ~variants:
                 {|"variants": [{"name":"a"},{"name":"b"},{"name":"a"}]|}
               "");
          case "bad duration" [ "agent.timeouts.total" ]
            (doc
               ~agent:
                 {|"agent": {"protocol":"command","command":["x"],"timeouts":{"total":"2 minutes"}}|}
               "");
          case "zero duration" [ "agent.timeouts.turn" ]
            (doc
               ~agent:
                 {|"agent": {"protocol":"command","command":["x"],"timeouts":{"turn":"0s"}}|}
               "");
          case "duration without unit" [ "agent.timeouts.turn" ]
            (doc
               ~agent:
                 {|"agent": {"protocol":"command","command":["x"],"timeouts":{"turn":30}}|}
               "");
          case "repeats below one" [ "repeats" ] (doc {|, "repeats": 0|});
          case "concurrency below one" [ "concurrency" ]
            (doc {|, "concurrency": 0|});
          case "unknown intercept value" [ "intercept[1]" ]
            (doc {|, "intercept": ["model", "radio"]|});
          case "unknown egress policy" [ "egress.default" ]
            (doc {|, "egress": {"default": "allow"}|});
          case "unknown host policy"
            [ "egress.hosts[\"a.example.com\"]" ]
            (doc {|, "egress": {"hosts": {"a.example.com": "maybe"}}|});
          case "unknown mcp policy" [ "mcp.calendar" ]
            (doc {|, "mcp": {"calendar": "maybe"}|});
          case "unknown protocol" [ "agent.protocol" ]
            (doc
               ~agent:
                 {|"agent": {"protocol": "carrier-pigeon", "command": ["x"]}|}
               "");
          case "command agent without command" [ "agent.command" ]
            (doc ~agent:{|"agent": {"protocol": "command"}|} "");
          case "command agent with url" [ "agent.url" ]
            (doc
               ~agent:
                 {|"agent": {"protocol": "command", "command": ["x"], "url": "http://x"}|}
               "");
          case "http agent missing fields"
            [ "agent.namespace_header"; "agent.url" ]
            (doc ~agent:{|"agent": {"protocol": "http"}|} "");
          case "invalid regex"
            [ "normalise[1]"; "redact[0]" ]
            (doc {|, "normalise": ["ok", "(unclosed"], "redact": ["[bad"]|});
          case "unknown sandbox mode" [ "sandbox" ]
            (doc {|, "sandbox": "maybe"|});
          case "unknown faketime mode" [ "clock.faketime" ]
            (doc {|, "clock": {"faketime": "sometimes"}|});
          case "negative cap" [ "caps_usd.openai" ]
            (doc {|, "caps_usd": {"openai": -1}|});
          case "unknown cap provider" [ "caps_usd.openia" ]
            (doc {|, "caps_usd": {"openai": 1, "openia": 2}|});
          case "bad confidence" [ "compare.confidence" ]
            (doc {|, "compare": {"baseline": "a", "confidence": 1.5}|});
          case "unknown baseline" [ "compare.baseline" ]
            (doc {|, "compare": {"baseline": "zzz"}|});
          case "five mistakes, five errors"
            [
              "agent.timeouts.turn";
              "bogus";
              "name";
              "repeats";
              "variants[1].name";
            ]
            five;
        ] );
    ]
