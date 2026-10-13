(* The loader against an experiment produced by the executor and the fake
   upstream (the setup of test_executor.ml). *)

open Bactrian_core
open Bactrian_runtime
module E = Executor
module P = Model_proxy
module F = Fake_upstream
module D = Report_data

let () = Mirage_crypto_rng_unix.use_default ()

let weather_turn =
  "What is the weather in Paris right now? Use the get_weather tool."

let scenario_json ?(checkers = "[]") ~max_steps id =
  Printf.sprintf
    {|{"version": 1, "id": %S, "turns": [{"user": %S}],
 "assertions": [{"tool_sequence": {"mode": "exact", "tools": ["get_weather"]}}, {"max_steps": %d}],
 "checkers": %s}|}
    id weather_turn max_steps checkers

let experiment_json ~compare =
  let variant name =
    Printf.sprintf {|%S: {"env": {"VARIANT_NAME": %S}}|} name name
  in
  Printf.sprintf
    {|{"version": 1, "name": "demo",
 "agent": {"protocol": "command", "command": ["python3", "-I", "-c", %s]},
 "scenarios": ["scenarios/*.json"],
 "variants": {%s, %s},
 "repeats": 2, "concurrency": 2, "caps_usd": {"openai": 100}, "seed": 3%s}|}
    (Yojson.Safe.to_string (`String Weather_agent.script))
    (variant "a") (variant "b") compare

let default_compare =
  {|, "compare": {"baseline": "a", "thresholds": {"pass_rate": 0.1, "cost": 0.2}, "gate": ["pass_rate"]}|}

type ctx = {
  env : Eio_unix.Stdenv.base;
  dir : string;
  root : Eio.Fs.dir_ty Eio.Path.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  id : string;
}

let write ctx name text =
  let path = Eio.Path.(Eio.Stdenv.cwd ctx.env / ctx.dir / name) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700
    (match Eio.Path.split path with Some (d, _) -> d | None -> assert false);
  Eio.Path.save ~create:(`Or_truncate 0o600) path text

let with_experiment name f () =
  Eio_main.run @@ fun env ->
  let dir = "tmp-results-" ^ name in
  let abs = Eio.Path.(Eio.Stdenv.cwd env / dir) in
  Eio.Path.rmtree ~missing_ok:true abs;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 abs;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true abs)
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let up =
    match
      F.start ~sw ~net:(Eio.Stdenv.net env) (F.Fixtures "../fixtures/protocols")
    with
    | Ok u -> u
    | Error e -> Alcotest.failf "fake upstream: %a" F.pp_error e
  in
  let ctx0 =
    {
      env;
      dir;
      root = Eio.Path.(abs / ".bactrian");
      fs = Eio.Stdenv.fs env;
      id = "";
    }
  in
  write ctx0 "scenarios/s1.json"
    (scenario_json ~max_steps:2
       ~checkers:
         {|[{"command": ["python3", "-I", "-c", "import json; print(json.dumps({'pass': True}))"]}]|}
       "s1");
  write ctx0 "scenarios/s2.json" (scenario_json ~max_steps:1 "s2");
  write ctx0 "exp.json" (experiment_json ~compare:default_compare);
  let summary =
    match
      E.run_experiment ~root:ctx0.root ~sandbox:`Off
        ~bactrian_exe:"/nonexistent/bactrian"
        ~keys:[ (P.Openai, "real-key") ]
        ~upstream:(fun _ -> Uri.of_string (F.base_url up))
        ~env ~experiment_file:(dir ^ "/exp.json") ~resume:None ()
    with
    | Ok s -> s
    | Error _ -> Alcotest.fail "experiment did not run"
  in
  f { ctx0 with id = summary.experiment_id }

let load ctx =
  match Results.load ~fs:ctx.fs ~root:ctx.root ~experiment_id:ctx.id with
  | Ok l -> l
  | Error e -> Alcotest.failf "load: %a" Results.pp_error e

let on scenario variant (l : Results.loaded) =
  List.filter
    (fun (r : D.run) ->
      String.equal r.scenario ("scenarios/" ^ scenario ^ ".json")
      && String.equal r.variant variant)
    l.runs

let test_load =
  with_experiment "load" @@ fun ctx ->
  let l = load ctx in
  Alcotest.(check int) "all runs, in plan order" 8 (List.length l.runs);
  Alcotest.(check (list int))
    "indices" (List.init 8 Fun.id)
    (List.map (fun (r : D.run) -> r.index) l.runs);
  Alcotest.(check string) "experiment name" "demo" l.manifest.name;
  Alcotest.(check bool) "experiment re-read" true (Option.is_some l.experiment);
  List.iter
    (fun (r : D.run) ->
      Alcotest.(check bool) "run id" true (Option.is_some r.run_id);
      Alcotest.(check bool) "trace" true (r.trace <> []);
      Alcotest.(check bool)
        "priced" true
        (match r.cost with Some c -> c > 0. | None -> false);
      Alcotest.(check bool) "latency" true (Option.is_some r.latency_ms);
      Alcotest.(check bool) "tokens" true (Option.is_some r.tokens);
      let s1 = String.equal r.scenario "scenarios/s1.json" in
      Alcotest.(check int)
        "two assertions, and a checker in s1"
        (if s1 then 3 else 2)
        (List.length r.checks);
      Alcotest.(check (option string))
        "unsandboxed, from the label" (Some "sandbox: off") r.unsandboxed;
      Alcotest.(check (option string))
        "checkers unsandboxed, from verdicts.json"
        (if s1 then Some "sandbox: off" else None)
        r.checkers_unsandboxed)
    l.runs;
  List.iter
    (fun v ->
      Alcotest.(check bool)
        "s1 passes" true
        (List.for_all (fun (r : D.run) -> r.status = Passed) (on "s1" v l));
      let s2 = on "s2" v l in
      Alcotest.(check int) "two repeats" 2 (List.length s2);
      List.iter
        (fun (r : D.run) ->
          Alcotest.(check bool) "s2 fails" true (r.status = Failed);
          Alcotest.(check bool)
            "max_steps message" true
            (List.exists
               (fun (c : D.check) ->
                 c.status = Check_fail
                 && String.length (Option.value c.reason ~default:"") > 0
                 && String.starts_with ~prefix:"assertions[1] max_steps" c.name)
               r.checks))
        s2)
    [ "a"; "b" ]

let test_settings =
  with_experiment "settings" @@ fun ctx ->
  let l = load ctx in
  let get ?baseline ?candidate ?(fail_on_inconclusive = false) l =
    Results.settings l ~baseline ~candidate ~fail_on_inconclusive
  in
  (match get l with
  | Error m -> Alcotest.fail m
  | Ok s ->
      Alcotest.(check string) "baseline from the experiment" "a" s.baseline;
      Alcotest.(check string) "the other variant" "b" s.candidate;
      Alcotest.(check bool) "gate" true (s.gate = [ (Pass_rate, 0.1) ]);
      Alcotest.(check bool)
        "non-gate threshold" true
        (s.report_metrics = [ (Cost, 0.2) ]);
      Alcotest.(check int) "seed" 3 s.seed;
      Alcotest.(check int) "runs" 8 (List.length s.runs));
  (match get ~baseline:"b" ~fail_on_inconclusive:true l with
  | Error m -> Alcotest.fail m
  | Ok s ->
      Alcotest.(check (pair string string))
        "flags win" ("b", "a") (s.baseline, s.candidate);
      Alcotest.(check bool) "flag" true s.fail_on_inconclusive);
  let is_error = function Error _ -> true | Ok _ -> false in
  Alcotest.(check bool) "same variant" true (is_error (get ~candidate:"a" l));
  Alcotest.(check bool)
    "unknown variant" true
    (is_error (get ~candidate:"zzz" l));
  (* Without the experiment file the flags are required, and so is a
     threshold, which only the file can give. *)
  Eio.Path.unlink Eio.Path.(Eio.Stdenv.cwd ctx.env / ctx.dir / "exp.json");
  let l = load ctx in
  Alcotest.(check bool) "experiment gone" true (Option.is_none l.experiment);
  Alcotest.(check bool) "baseline required" true (is_error (get l));
  Alcotest.(check bool)
    "candidate required" true
    (is_error (get ~baseline:"a" l));
  (* No threshold is ever assumed: the pass rate is named. *)
  match get ~baseline:"a" ~candidate:"b" l with
  | Ok _ -> Alcotest.fail "a gate without a threshold must be refused"
  | Error m ->
      Alcotest.(check bool)
        ("names the metric: " ^ m) true
        (Re.execp (Re.compile (Re.str "pass_rate has no threshold")) m)

let test_gate_without_threshold =
  with_experiment "nothreshold" @@ fun ctx ->
  write ctx "exp.json"
    (experiment_json
       ~compare:
         {|, "compare": {"baseline": "a", "gate": ["pass_rate", "bogus"], "thresholds": {"pass_rate": 0.1}}|});
  match
    Results.settings (load ctx) ~baseline:None ~candidate:None
      ~fail_on_inconclusive:false
  with
  | Error m ->
      Alcotest.(check bool)
        "names the metric" true
        (String.length m > 0 && String.contains m '"')
  | Ok _ -> Alcotest.fail "an unknown gate metric must be refused"

(* Cost, latency, tokens and divergences are read from what the run wrote,
   not recomputed. *)
let test_reads_what_the_run_wrote =
  with_experiment "reads" @@ fun ctx ->
  let l = load ctx in
  let first = List.hd l.runs in
  let id = Option.get first.run_id in
  let meta =
    Yojson.Safe.from_string
      (Eio.Path.load Eio.Path.(ctx.root / "runs" / id / "meta.json"))
  in
  Alcotest.(check (option (float 1e-12)))
    "cost is the ledger's"
    (Some Yojson.Safe.Util.(to_float (member "cost_usd" meta)))
    first.cost;
  Alcotest.(check (option int))
    "tokens"
    (Some Yojson.Safe.Util.(to_int (member "tokens" meta)))
    first.tokens;
  Alcotest.(check bool)
    "no divergences in a record run" true (first.divergences = []);
  (* Edit the files and the loader follows them. *)
  let path name = Printf.sprintf ".bactrian/runs/%s/%s" id name in
  write ctx (path "meta.json")
    {|{"cost_usd": 123.5, "latency_ms": 42.0, "tokens": 7}|};
  write ctx (path "divergences.json")
    {|{"version": 1, "divergences": [{"kind": "missing_request", "seq": 3, "similarity": 0.8, "changes": [{"path": "messages[0].content", "change": "changed"}]}]}|};
  let again = List.hd (load ctx).runs in
  Alcotest.(check (option string))
    "no violations line without a sandbox" None again.sandbox_violations;
  Alcotest.(check (option (float 0.))) "cost" (Some 123.5) again.cost;
  Alcotest.(check (option (float 0.))) "latency" (Some 42.) again.latency_ms;
  Alcotest.(check (option int)) "tokens" (Some 7) again.tokens;
  (match again.divergences with
  | [ d ] ->
      Alcotest.(check bool)
        "described, with the diff path" true
        (Re.execp (Re.compile (Re.str "nearest recorded #3")) d.description
        && List.exists
             (fun (c : Diff.change) ->
               String.equal c.path "messages[0].content")
             d.changes)
  | _ -> Alcotest.fail "one divergence expected");
  let violations json =
    write ctx (path "meta.json")
      ({|{"sandbox": {"mode": "strict", "applied": true}, "sandbox_violations": |}
     ^ json ^ "}");
    (List.hd (load ctx).runs).sandbox_violations
  in
  Alcotest.(check (option string))
    "macOS: a lower bound" (Some "at least 1 (the system log can drop denials)")
    (violations
       {|{"collected": true, "coverage": "seatbelt-log", "complete": false, "reason": "the system log can drop denials", "items": [{"kind": "network-outbound", "detail": "x"}]}|});
  Alcotest.(check (option string))
    "Linux: relay failures only"
    (Some "0 relay failures (Linux records no other kind of denial)")
    (violations
       {|{"collected": true, "coverage": "relay-failures-only", "complete": true, "items": []}|});
  Alcotest.(check (option string))
    "not collected" (Some "not collected: log show failed")
    (violations
       {|{"collected": false, "reason": "log show failed", "items": []}|});
  Alcotest.(check (option string))
    "recorded before the coverage" None
    (violations {|{"collected": true, "items": []}|});
  write ctx (path "divergences.json") "{";
  match Results.load ~fs:ctx.fs ~root:ctx.root ~experiment_id:ctx.id with
  | Error (Results.Bad_run { file; _ }) ->
      Alcotest.(check string) "which file" "divergences.json" file
  | Ok _ | Error (No_such_experiment _ | Bad_manifest _ | Bad_experiment _) ->
      Alcotest.fail "a malformed divergences.json is an error"

let test_end_to_end =
  with_experiment "e2e" @@ fun ctx ->
  let l = load ctx in
  let settings =
    match
      Results.settings l ~baseline:None ~candidate:None
        ~fail_on_inconclusive:false
    with
    | Ok s -> { s with Compare.resamples = 200 }
    | Error m -> Alcotest.fail m
  in
  match Compare.run settings with
  | Error m -> Alcotest.fail m
  | Ok result ->
      let data =
        D.build ~experiment_id:l.manifest.summary.experiment_id
          ~experiment:l.manifest.name ~settings ~result l.runs
      in
      Alcotest.(check int) "four failures" 4 (List.length data.failures);
      let html = Report_html.render data in
      let contains s sub = Re.execp (Re.compile (Re.str sub)) s in
      Alcotest.(check bool)
        "report names the failing assertion" true
        (contains html "assertions[1] max_steps");
      Alcotest.(check bool) "trace embedded" true (contains html "tool_call")

let test_errors =
  with_experiment "errors" @@ fun ctx ->
  let err id =
    match Results.load ~fs:ctx.fs ~root:ctx.root ~experiment_id:id with
    | Error e -> e
    | Ok _ -> Alcotest.fail "expected an error"
  in
  Alcotest.(check bool)
    "unknown" true
    (err "nope" = Results.No_such_experiment "nope");
  Alcotest.(check bool)
    "traversal" true
    (err "../x" = Results.No_such_experiment "../x");
  write ctx
    (Printf.sprintf ".bactrian/experiments/%s/manifest.json" "broken")
    "{";
  (match err "broken" with
  | Results.Bad_manifest _ -> ()
  | No_such_experiment _ | Bad_run _ | Bad_experiment _ ->
      Alcotest.fail "expected Bad_manifest");
  (* A malformed trace is an error naming the run. *)
  let l = load ctx in
  let id = Option.get (List.hd l.runs).run_id in
  write ctx (Printf.sprintf ".bactrian/runs/%s/trace.jsonl" id) "not json\n";
  match err ctx.id with
  | Results.Bad_run { run_id; file; _ } ->
      Alcotest.(check (pair string string))
        "which" (id, "trace.jsonl") (run_id, file)
  | No_such_experiment _ | Bad_manifest _ | Bad_experiment _ ->
      Alcotest.fail "expected Bad_run"

(* An experiment file with a mistake is an error at the mistake, not a
   missing setting. A file that is gone is no file. *)
let test_bad_experiment_file =
  with_experiment "badexp" @@ fun ctx ->
  write ctx "exp.json"
    (experiment_json
       ~compare:
         {|, "compare": {"baseline": "a", "confidence": 1.5, "thresholds": {"pass_rate": 0.1}}|});
  (match Results.load ~fs:ctx.fs ~root:ctx.root ~experiment_id:ctx.id with
  | Error (Results.Bad_experiment [ e ]) ->
      Alcotest.(check string) "located" "compare.confidence" e.path
  | Error (Bad_experiment es) ->
      Alcotest.failf "expected one error, got %d" (List.length es)
  | Ok _ | Error (No_such_experiment _ | Bad_manifest _ | Bad_run _) ->
      Alcotest.fail "expected Bad_experiment");
  Eio.Path.unlink Eio.Path.(Eio.Stdenv.cwd ctx.env / ctx.dir / "exp.json");
  let l = load ctx in
  Alcotest.(check bool) "gone" true (Option.is_none l.experiment);
  match
    Results.settings l ~baseline:None ~candidate:None
      ~fail_on_inconclusive:false
  with
  | Error m ->
      Alcotest.(check bool)
        ("says the file is gone: " ^ m)
        true
        (Re.execp (Re.compile (Re.str "is gone")) m)
  | Ok _ -> Alcotest.fail "no baseline without the file"

let () =
  Alcotest.run "results"
    [
      ( "load",
        [
          Alcotest.test_case "runs, checks and measures" `Slow test_load;
          Alcotest.test_case "errors" `Slow test_errors;
          Alcotest.test_case "bad or missing experiment file" `Slow
            test_bad_experiment_file;
          Alcotest.test_case "reads meta.json and divergences.json" `Slow
            test_reads_what_the_run_wrote;
        ] );
      ( "settings",
        [
          Alcotest.test_case "from the experiment and flags" `Slow test_settings;
          Alcotest.test_case "unknown gate metric" `Slow
            test_gate_without_threshold;
        ] );
      ( "end to end",
        [ Alcotest.test_case "compare and render" `Slow test_end_to_end ] );
    ]
