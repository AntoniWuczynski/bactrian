open Bactrian_runtime
open Executor_support

let () = Mirage_crypto_rng_unix.use_default ()

(* Plan *)

let names n prefix = List.init n (fun i -> Printf.sprintf "%s%d" prefix i)

let plan ?(seed = 1) ?(s = 3) ?(v = 2) ?(r = 4) () =
  E.plan ~seed ~scenarios:(names s "s") ~variants:(names v "v") ~repeats:r

let key (p : E.planned) = (p.scenario, p.variant, p.repeat)

let test_plan_same_seed () =
  Alcotest.(check bool) "same" true (plan () = plan ());
  Alcotest.(check bool) "other seed differs" true (plan () <> plan ~seed:2 ())

let test_plan_complete () =
  let p = plan () in
  Alcotest.(check (list int))
    "indices" (List.init 24 Fun.id)
    (List.map (fun (x : E.planned) -> x.index) p);
  let expected =
    List.concat_map
      (fun s ->
        List.concat_map
          (fun v -> List.init 4 (fun r -> (s, v, r + 1)))
          (names 2 "v"))
      (names 3 "s")
  in
  Alcotest.(check bool)
    "every combination once" true
    (List.sort compare (List.map key p) = List.sort compare expected)

let test_plan_interleaved () =
  let p = plan () in
  List.iteri
    (fun i (x : E.planned) ->
      Alcotest.(check string)
        "round robin"
        (Printf.sprintf "s%d" (i mod 3))
        x.scenario)
    p;
  (* Each scenario's own order is a shuffle, not variant by variant. *)
  let of_s0 = List.filter (fun (x : E.planned) -> x.scenario = "s0") p in
  Alcotest.(check bool)
    "variants mixed" true
    (List.map (fun (x : E.planned) -> x.variant) of_s0
    <> List.sort compare (List.map (fun (x : E.planned) -> x.variant) of_s0))

(* Manifests of old experiments are only resumable while the same
   seed gives the same plan, so a change of the stdlib generator or of the
   allocation must show up here. *)
let test_plan_pinned () =
  let p =
    E.plan ~seed:1 ~scenarios:[ "a"; "b" ] ~variants:[ "x"; "y" ] ~repeats:2
  in
  Alcotest.(check (list (triple string string int)))
    "seed 1"
    [
      ("a", "x", 2);
      ("b", "y", 1);
      ("a", "y", 1);
      ("b", "y", 2);
      ("a", "x", 1);
      ("b", "x", 1);
      ("a", "y", 2);
      ("b", "x", 2);
    ]
    (List.map key p)

let test_plan_uneven () =
  let p = E.plan ~seed:0 ~scenarios:[ "a" ] ~variants:[ "x" ] ~repeats:2 in
  Alcotest.(check int) "single scenario" 2 (List.length p);
  Alcotest.(check int)
    "no scenarios" 0
    (List.length (E.plan ~seed:0 ~scenarios:[] ~variants:[ "x" ] ~repeats:2))

let test_full_run =
  with_ctx "full" @@ fun ctx ->
  let s = ok (go ctx) in
  List.iter
    (fun (_, st, id) ->
      (match st with
      | E.Done o ->
          Printf.printf "DEBUG reason: %s\n%!"
            (Option.value (Run.outcome_reason o) ~default:"-")
      | Pending | Interrupted -> ());
      Option.iter
        (fun id ->
          Printf.printf "DEBUG stderr: %s\n%!"
            (read ctx (Printf.sprintf ".bactrian/runs/%s/agent.stderr" id)))
        id)
    s.runs;
  Alcotest.(check int) "runs" 8 (List.length s.runs);
  Alcotest.(check (list string))
    "all passed"
    (List.init 8 (fun _ -> "passed"))
    (kinds s);
  Alcotest.(check bool)
    "plan from the seed" true
    (List.map (fun (p, _, _) -> p) s.runs
    = E.plan ~seed:3
        ~scenarios:[ "scenarios/s1.json"; "scenarios/s2.json" ]
        ~variants:[ "a"; "b" ] ~repeats:2);
  let ids = List.filter_map (fun (_, _, id) -> id) s.runs in
  Alcotest.(check int)
    "distinct run ids" 8
    (List.length (List.sort_uniq compare ids));
  Alcotest.(check int) "model requests" 16 (List.length (F.requests ctx.up));
  (* The manifest lists every run in plan order. *)
  let m = manifest ctx s in
  Alcotest.(check string) "experiment" "demo" (str m "experiment");
  let runs = Yojson.Safe.Util.(to_list (member "runs" m)) in
  Alcotest.(check (list string))
    "manifest ids" ids
    (List.map (fun r -> str r "run_id") runs);
  Alcotest.(check (list string))
    "manifest status"
    (List.init 8 (fun _ -> "done"))
    (List.map (fun r -> str r "status") runs);
  Alcotest.(check (list int))
    "manifest indices" (List.init 8 Fun.id)
    (List.map (fun r -> Yojson.Safe.Util.(to_int (member "index" r))) runs);
  (* Verdicts and the variant's environment reached every run. *)
  List.iter
    (fun id ->
      let v =
        Yojson.Safe.from_string
          (read ctx (Printf.sprintf ".bactrian/runs/%s/verdicts.json" id))
      in
      Alcotest.(check bool)
        "verdicts passed" true
        Yojson.Safe.Util.(to_bool (member "passed" v));
      Alcotest.(check int)
        "two assertions" 2
        (List.length Yojson.Safe.Util.(to_list (member "items" v)));
      let env =
        Yojson.Safe.from_string
          (read ctx (Printf.sprintf ".bactrian/runs/%s/state/env.json" id))
      in
      Alcotest.(check string)
        "TZ from the scenario" "Europe/London" (str env "TZ");
      Alcotest.(check bool)
        "variant env" true
        (List.mem (str env "VARIANT_NAME") [ "a"; "b" ]))
    ids

(* The limit holds and is reached: no more than 2 agents between S and E. *)
let max_overlap log =
  let live = ref 0 and best = ref 0 in
  List.iter
    (fun l ->
      if String.starts_with ~prefix:"S " l then incr live else decr live;
      best := max !best !live)
    log;
  !best

let test_concurrency =
  with_ctx "concurrency" @@ fun ctx ->
  ignore (ok (go ctx));
  Alcotest.(check int) "at most 2, and 2 happen" 2 (max_overlap (agent_log ctx))

let test_concurrency_one =
  with_ctx "concurrency1" @@ fun ctx ->
  write ctx "exp.json" (experiment_json ctx ~concurrency:1 ());
  ignore (ok (go ctx));
  Alcotest.(check int) "sequential" 1 (max_overlap (agent_log ctx))

(* Interrupt and resume *)

let wait_for ctx pred =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock ctx.env) 60. (fun () ->
      let rec loop () =
        if pred () then ()
        else (
          Eio.Time.sleep (Eio.Stdenv.clock ctx.env) 0.05;
          loop ())
      in
      loop ())

let test_interrupt_and_resume =
  with_ctx "resume" @@ fun ctx ->
  write ctx "exp.json" (experiment_json ctx ~concurrency:1 ());
  write ctx "block" "3";
  let id = ref None in
  (* The 4th agent hangs: interrupt once it has started. *)
  Eio.Fiber.first
    (fun () ->
      ignore (go ~announce:(fun i -> id := Some i) ctx);
      Alcotest.fail "should not finish")
    (fun () -> wait_for ctx (fun () -> count_prefix "S " (agent_log ctx) = 4));
  let id = Option.get !id in
  let first_log = agent_log ctx in
  let m =
    Yojson.Safe.from_string
      (Eio.Path.load Eio.Path.(ctx.root / "experiments" / id / "manifest.json"))
  in
  let runs = Yojson.Safe.Util.(to_list (member "runs" m)) in
  let status = List.map (fun r -> str r "status") runs in
  Alcotest.(check (list string))
    "after the interrupt"
    ([ "done"; "done"; "done"; "interrupted" ]
    @ List.init 4 (fun _ -> "pending"))
    status;
  let finished =
    List.filteri (fun i _ -> i < 3) runs |> List.map (fun r -> str r "run_id")
  in
  Alcotest.(check bool)
    "the interrupted run has an id" true
    (Yojson.Safe.Util.(member "run_id" (List.nth runs 3)) <> `Null);
  (* Resume without the hang. *)
  Sys.remove (block_path ctx);
  let s = ok (go ~resume:(Some id) ctx) in
  Alcotest.(check (list string))
    "all passed"
    (List.init 8 (fun _ -> "passed"))
    (kinds s);
  Alcotest.(check (list string))
    "finished runs untouched" finished
    (List.filteri (fun i _ -> i < 3) s.runs
    |> List.map (fun (_, _, i) -> Option.get i));
  let log = agent_log ctx in
  (* 4 starts before, then the interrupted run again and the 4 unstarted. *)
  Alcotest.(check int) "agent starts" 9 (count_prefix "S " log);
  let started_after =
    List.filteri (fun i _ -> i >= List.length first_log) log
  in
  List.iter
    (fun id ->
      Alcotest.(check bool)
        "finished run not repeated" false
        (List.exists (fun l -> l = "S " ^ id) started_after))
    finished;
  Alcotest.(check string) "same experiment" id s.experiment_id;
  (* Resuming a finished experiment starts nothing. *)
  let again = ok (go ~resume:(Some id) ctx) in
  Alcotest.(check int) "no new starts" 9 (count_prefix "S " (agent_log ctx));
  Alcotest.(check (list string)) "unchanged" (kinds s) (kinds again)

let test_resume_errors =
  with_ctx "resume-errors" @@ fun ctx ->
  (match go ~resume:(Some "no-such") ctx with
  | Error (E.Usage m) ->
      Alcotest.(check string) "unknown" "no experiment \"no-such\"" m
  | Ok _ | Error (E.Configuration _ | E.Environment _) ->
      Alcotest.fail "expected Usage");
  (match go ~resume:(Some "../x") ctx with
  | Error (E.Usage _) -> ()
  | Ok _ | Error (E.Configuration _ | E.Environment _) ->
      Alcotest.fail "expected Usage");
  let s = ok (go ctx) in
  write ctx "exp.json" (experiment_json ctx ~repeats:3 ());
  match go ~resume:(Some s.experiment_id) ctx with
  | Error (E.Usage _) -> ()
  | Ok _ | Error (E.Configuration _ | E.Environment _) ->
      Alcotest.fail "a changed plan must be refused"

(* Caps *)

let test_caps =
  with_ctx "caps" @@ fun ctx ->
  let cap = 0.0004 in
  write ctx "exp.json"
    (experiment_json ctx ~concurrency:1
       ~caps:(Printf.sprintf {|{"openai": %g}|} cap)
       ());
  let s = ok (go ctx) in
  let ks = kinds s in
  Alcotest.(check bool) "some run passed" true (List.mem "passed" ks);
  Alcotest.(check bool) "a run was not run" true (List.mem "not_run" ks);
  (* After the first Not_run nothing else starts. *)
  let rec after_cap = function
    | [] -> true
    | "not_run" :: rest -> List.for_all (( = ) "pending") rest
    | _ :: rest -> after_cap rest
  in
  Alcotest.(check bool) "no run started after the cap" true (after_cap ks);
  Alcotest.(check int)
    "agents started"
    (List.length (List.filter (( <> ) "pending") ks))
    (count_prefix "S " (agent_log ctx));
  let ledger =
    Ledger.create
      ~path:
        Eio.Path.(ctx.root / "experiments" / s.experiment_id / "ledger.jsonl")
      ~caps:[ ("openai", cap) ]
  in
  Alcotest.(check bool)
    "spent within the cap" true
    (Ledger.spent ledger ~provider:"openai" <= cap)

let test_preflight =
  with_ctx "preflight" @@ fun ctx ->
  write ctx "exp.json" (experiment_json ctx ~caps:"{}" ());
  (match go ctx with
  | Error (E.Environment _) -> ()
  | Ok _ | Error (E.Configuration _ | E.Usage _) ->
      Alcotest.fail "expected Environment");
  write ctx "exp.json" (experiment_json ctx ());
  match
    E.run_experiment ~root:ctx.root ~keys:[] ~sandbox:`Off
      ~bactrian_exe:"/nonexistent/bactrian" ~env:ctx.env
      ~experiment_file:(ctx.dir ^ "/exp.json") ~resume:None ()
  with
  | Error (E.Environment m) ->
      Alcotest.(check string)
        "missing key" "openai has a spend cap but OPENAI_API_KEY is not set" m
  | Ok _ | Error (E.Configuration _ | E.Usage _) ->
      Alcotest.fail "expected Environment"

(* Assertions and checkers decide the outcome *)

let test_failing_assertion_and_checker =
  with_ctx "verdicts" ~scenarios:[] @@ fun ctx ->
  write ctx "exp.json" (experiment_json ctx ~repeats:1 ~concurrency:1 ());
  write ctx "scenarios/lax.json" (scenario_json "lax");
  write ctx "scenarios/strict.json"
    (scenario_json ~assertions:{|[{"max_steps": 1}]|} "strict");
  write ctx "scenarios/checked.json"
    (scenario_json
       ~checkers:
         {|[{"command": ["python3", "-I", "-c", "import json; print(json.dumps({'pass': False, 'message': 'nope'}))"]}]|}
       "checked");
  let s = ok (go ctx) in
  let by_scenario sc =
    List.filter_map
      (fun ((p : E.planned), st, _) ->
        if p.scenario = "scenarios/" ^ sc then Some st else None)
      s.runs
  in
  List.iter
    (fun st -> Alcotest.(check string) "lax passes" "passed" (status_kind st))
    (by_scenario "lax.json");
  let failed sc needle =
    List.iter
      (fun st ->
        Alcotest.(check string) (sc ^ " kind") "failed" (status_kind st);
        let r =
          match st with
          | E.Done o -> Option.value (Run.outcome_reason o) ~default:""
          | Pending | Interrupted -> ""
        in
        Alcotest.(check bool)
          (sc ^ " reason: " ^ r)
          true
          (Re.execp (Re.compile (Re.str needle)) r))
      (by_scenario sc)
  in
  failed "strict.json" "assertions[0] max_steps";
  failed "checked.json" "nope"

(* Scenario and experiment settings *)

let settings_ctx name f =
  with_ctx name ~scenarios:[ "s1" ] (fun ctx ->
      write ctx "scenarios/fx/sub/data.txt" "hello";
      write ctx "scenarios/s1.json"
        (scenario_json ~extra:{|, "fixtures": "./fx"|} "s1");
      f ctx)

let fixtures_prelude =
  {|
import os
d = os.environ["BACTRIAN_STATE_DIR"]
with open(os.path.join(d, "sub", "data.txt")) as f:
    seen = f.read()
with open(os.path.join(d, "seen.txt"), "w") as f:
    f.write("saw:" + seen)
|}

let test_fixtures_and_normalise =
  settings_ctx "settings" @@ fun ctx ->
  write ctx "exp.json"
    (experiment_json ctx ~prelude:fixtures_prelude
       ~extra:{|, "normalise": ["Paris"]|} ());
  let s = ok (go ctx) in
  Alcotest.(check (list string))
    "all passed"
    [ "passed"; "passed"; "passed"; "passed" ]
    (kinds s);
  List.iter
    (fun id ->
      Alcotest.(check string)
        "fixture seen by the agent" "saw:hello"
        (read ctx (Printf.sprintf ".bactrian/runs/%s/state/seen.txt" id));
      Alcotest.(check bool)
        "masks counted" true
        Yojson.Safe.Util.(
          to_int
            (member "normalise_masks" (member "findings" (run_meta ctx id)))
          > 0))
    (run_ids s)

let test_missing_fixtures =
  settings_ctx "nofixtures" @@ fun ctx ->
  write ctx "scenarios/s1.json"
    (scenario_json ~extra:{|, "fixtures": "./nowhere"|} "s1");
  match go ctx with
  | Error (E.Configuration [ e ]) ->
      Alcotest.(check (pair string string))
        "located at fixtures"
        ("fixtures", "scenarios/s1.json")
        (e.path, Filename.concat "scenarios" (Filename.basename e.file))
  | Ok _ | Error (E.Configuration _ | E.Environment _ | E.Usage _) ->
      Alcotest.fail "expected one configuration error"

(* MCP servers reach the run: the agent learns where they are, and they are
   started only when it uses them (these commands do not exist). *)

let mcp_extra =
  {|, "mcp": {"calendar": {"command": ["no-such-server"], "env": {"K": "v"}, "cwd": "tools"},
              "search": {"url": "https://search.example.com/mcp", "policy": "passthrough"}}|}

let agent_env ctx s =
  match run_ids s with
  | id :: _ ->
      Yojson.Safe.from_string
        (read ctx (Printf.sprintf ".bactrian/runs/%s/state/env.json" id))
  | [] -> Alcotest.fail "no run"

let test_mcp_environment =
  settings_ctx "mcp" @@ fun ctx ->
  write ctx "exp.json" (experiment_json ctx ~extra:mcp_extra ());
  let s = ok (go ctx) in
  Alcotest.(check (list string))
    "all passed"
    [ "passed"; "passed"; "passed"; "passed" ]
    (kinds s);
  let env = agent_env ctx s in
  let has_infix infix v =
    Re.execp (Re.compile (Re.str infix)) (Yojson.Safe.Util.to_string v)
  in
  Alcotest.(check bool)
    "shim target" true
    (has_infix "/mcp" (member "BACTRIAN_MCP_URL" env));
  Alcotest.(check bool)
    "HTTP server's proxied URL" true
    (has_infix "/mcp-http/search" (member "BACTRIAN_MCP_HTTP_SEARCH" env));
  Alcotest.(check bool)
    "no variable for the stdio server" true
    (member "BACTRIAN_MCP_HTTP_CALENDAR" env = `Null);
  let unenforced =
    Yojson.Safe.Util.(
      to_list (member "unenforced" (run_meta ctx (List.hd (run_ids s)))))
  in
  Alcotest.(check bool)
    "mcp is no longer listed as unenforced" false
    (List.exists
       (fun w ->
         String.starts_with ~prefix:"mcp:" (Yojson.Safe.Util.to_string w))
       unenforced)

let test_mcp_not_intercepted =
  settings_ctx "mcpoff" @@ fun ctx ->
  write ctx "exp.json"
    (experiment_json ctx
       ~extra:({|, "intercept": ["model", "fixtures"]|} ^ mcp_extra)
       ());
  let s = ok (go ctx) in
  let env = agent_env ctx s in
  Alcotest.(check bool)
    "no shim target" true
    (member "BACTRIAN_MCP_URL" env = `Null);
  let unenforced =
    Yojson.Safe.Util.(
      to_list (member "unenforced" (run_meta ctx (List.hd (run_ids s)))))
  in
  Alcotest.(check bool)
    "the unused servers are reported" true
    (List.exists
       (fun w ->
         String.starts_with ~prefix:"mcp:" (Yojson.Safe.Util.to_string w))
       unenforced)

let test_mcp_replay_refused =
  settings_ctx "mcpreplay" @@ fun ctx ->
  write ctx "exp.json"
    (experiment_json ctx
       ~extra:{|, "mcp": {"calendar": {"command": ["c"], "policy": "replay"}}|}
       ());
  match go ctx with
  | Error (E.Configuration [ e ]) ->
      Alcotest.(check string) "path" "mcp.calendar.policy" e.path;
      Alcotest.(check bool) "located in the file" true (e.line > 1)
  | Ok _ | Error (E.Configuration _ | E.Environment _ | E.Usage _) ->
      Alcotest.fail "expected one configuration error"

let prices =
  {|version: 1
prices:
  - model: gpt-6-luna
    provider: openai
    rates: { input: 1.0, cached_input: 0.1, cache_write: 1.25, output: 5.0 }
    max_output: 65536
    max_input: 272000
|}

let test_pricing_file =
  settings_ctx "pricing" @@ fun ctx ->
  let first_spend () =
    let s = ok (go ctx) in
    spent ctx (List.hd (run_ids s))
  in
  let base = first_spend () in
  write ctx "prices.yaml" prices;
  write ctx "exp.json"
    (experiment_json ctx ~extra:{|, "pricing": "prices.yaml"|} ());
  let dear = first_spend () in
  Alcotest.(check bool) "base is positive" true (base > 0.);
  Alcotest.(check bool)
    "the file's rates (ten times the built-in ones) set the spend" true
    (Float.abs (dear -. (10. *. base)) < 1e-9 *. dear);
  write ctx "prices.yaml"
    "version: 1\nprices:\n  - model: m\n    provider: acme\n";
  match go ctx with
  | Error (E.Configuration es) ->
      Alcotest.(check bool)
        "errors are in the price file" true
        (List.exists
           (fun (e : Config_loader.located_error) ->
             Filename.basename e.file = "prices.yaml"
             && e.path = "prices[0].provider")
           es)
  | Ok _ | Error (E.Environment _ | E.Usage _) ->
      Alcotest.fail "expected configuration errors"

let () =
  Alcotest.run "executor"
    [
      ( "plan",
        [
          Alcotest.test_case "same seed, same plan" `Quick test_plan_same_seed;
          Alcotest.test_case "every run once" `Quick test_plan_complete;
          Alcotest.test_case "interleaved and shuffled" `Quick
            test_plan_interleaved;
          Alcotest.test_case "pinned for a seed" `Quick test_plan_pinned;
          Alcotest.test_case "edge cases" `Quick test_plan_uneven;
        ] );
      ( "run",
        [
          Alcotest.test_case "2x2x2 experiment" `Slow test_full_run;
          Alcotest.test_case "concurrency limit" `Slow test_concurrency;
          Alcotest.test_case "concurrency 1" `Slow test_concurrency_one;
          Alcotest.test_case "assertions and checkers decide" `Slow
            test_failing_assertion_and_checker;
          Alcotest.test_case "preflight" `Quick test_preflight;
        ] );
      ( "resume",
        [
          Alcotest.test_case "interrupt then resume" `Slow
            test_interrupt_and_resume;
          Alcotest.test_case "resume errors" `Slow test_resume_errors;
        ] );
      ( "settings",
        [
          Alcotest.test_case "fixtures and normalise" `Slow
            test_fixtures_and_normalise;
          Alcotest.test_case "missing fixtures" `Slow test_missing_fixtures;
          Alcotest.test_case "pricing file" `Slow test_pricing_file;
          Alcotest.test_case "mcp servers reach the run" `Slow
            test_mcp_environment;
          Alcotest.test_case "mcp not intercepted" `Slow
            test_mcp_not_intercepted;
          Alcotest.test_case "replay policy refused" `Slow
            test_mcp_replay_refused;
        ] );
      ("caps", [ Alcotest.test_case "caps respected" `Slow test_caps ]);
    ]
