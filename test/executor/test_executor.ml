open Bactrian_runtime
module E = Executor
module P = Model_proxy
module F = Fake_upstream

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

let test_plan_uneven () =
  let p = E.plan ~seed:0 ~scenarios:[ "a" ] ~variants:[ "x" ] ~repeats:2 in
  Alcotest.(check int) "single scenario" 2 (List.length p);
  Alcotest.(check int)
    "no scenarios" 0
    (List.length (E.plan ~seed:0 ~scenarios:[] ~variants:[ "x" ] ~repeats:2))

(* Running *)

let fixtures_root = "../fixtures/protocols"

let weather_turn =
  "What is the weather in Paris right now? Use the get_weather tool."

let counter = ref 0

let random_bytes n =
  incr counter;
  String.init n (fun i -> Char.chr ((!counter + (i * 7)) land 255))

(* Appends "S <run id>" when the agent starts and "E <run id>" 0.2 s later. If
   the block file exists it holds N: the agent then hangs once N agents have
   started. *)
let agent_prefix =
  {|
import os, time
log = os.environ["AGENT_LOG"]
def w(tag):
    with open(log, "a") as f:
        f.write(tag + " " + os.environ["BACTRIAN_RUN_ID"] + "\n")
w("S")
block = os.environ["AGENT_BLOCK"]
if os.path.exists(block):
    started = sum(1 for l in open(log) if l.startswith("S "))
    if started > int(open(block).read()):
        time.sleep(600)
time.sleep(0.2)
w("E")
|}

type ctx = {
  env : Eio_unix.Stdenv.base;
  dir : string;  (** Relative to the working directory. *)
  root : Eio.Fs.dir_ty Eio.Path.t;
  up : F.t;
}

let write ctx name text =
  let path = Eio.Path.(Eio.Stdenv.cwd ctx.env / ctx.dir / name) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700
    (match Eio.Path.split path with Some (d, _) -> d | None -> assert false);
  Eio.Path.save ~create:(`Or_truncate 0o600) path text

let read ctx name =
  Eio.Path.load Eio.Path.(Eio.Stdenv.cwd ctx.env / ctx.dir / name)

let log_path ctx = Filename.concat (Sys.getcwd ()) (ctx.dir ^ "/agent.log")
let block_path ctx = Filename.concat (Sys.getcwd ()) (ctx.dir ^ "/block")

let scenario_json
    ?(assertions =
      {|[{"tool_sequence": {"mode": "exact", "tools": ["get_weather"]}}, {"max_steps": 2}]|})
    ?(checkers = "[]") id =
  Printf.sprintf
    {|{"version": 1, "id": %S, "timezone": "Europe/London",
 "turns": [{"user": %S}], "assertions": %s, "checkers": %s}|}
    id weather_turn assertions checkers

let experiment_json ctx ?(repeats = 2) ?(concurrency = 2)
    ?(caps = {|{"openai": 100}|}) ?(seed = 3) () =
  let variant name =
    Printf.sprintf
      {|%S: {"env": {"AGENT_LOG": %S, "AGENT_BLOCK": %S, "VARIANT_NAME": %S}}|}
      name (log_path ctx) (block_path ctx) name
  in
  Printf.sprintf
    {|{"version": 1, "name": "demo",
 "agent": {"protocol": "command", "command": ["python3", "-I", "-c", %s]},
 "scenarios": ["scenarios/*.json"],
 "variants": {%s, %s},
 "repeats": %d, "concurrency": %d, "caps_usd": %s, "seed": %d}|}
    (Yojson.Safe.to_string (`String (agent_prefix ^ Weather_agent.script)))
    (variant "a") (variant "b") repeats concurrency caps seed

let with_ctx name ?(scenarios = [ "s1"; "s2" ]) f () =
  Eio_main.run @@ fun env ->
  let dir = "tmp-exec-" ^ name in
  let abs = Eio.Path.(Eio.Stdenv.cwd env / dir) in
  Eio.Path.rmtree ~missing_ok:true abs;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 abs;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true abs)
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let up =
    match F.start ~sw ~net:(Eio.Stdenv.net env) (F.Fixtures fixtures_root) with
    | Ok u -> u
    | Error e -> Alcotest.failf "fake upstream: %a" F.pp_error e
  in
  let ctx = { env; dir; root = Eio.Path.(abs / ".bactrian"); up } in
  List.iter
    (fun id ->
      write ctx (Printf.sprintf "scenarios/%s.json" id) (scenario_json id))
    scenarios;
  write ctx "exp.json" (experiment_json ctx ());
  f ctx

let go ?(resume = None) ?announce ctx =
  E.run_experiment ~root:ctx.root ~random_bytes ?announce
    ~keys:[ (P.Openai, "real-key") ]
    ~upstream:(fun _ -> Uri.of_string (F.base_url ctx.up))
    ~env:ctx.env ~experiment_file:(ctx.dir ^ "/exp.json") ~resume ()

let ok = function
  | Ok v -> v
  | Error (E.Configuration _) -> Alcotest.fail "configuration error"
  | Error (E.Environment m) -> Alcotest.failf "environment: %s" m
  | Error (E.Usage m) -> Alcotest.failf "usage: %s" m

let lines ctx name =
  match read ctx name with
  | text -> String.split_on_char '\n' text |> List.filter (( <> ) "")
  | exception Eio.Io _ -> []

let agent_log ctx = lines ctx "agent.log"

let count_prefix p l =
  List.length (List.filter (String.starts_with ~prefix:p) l)

let manifest ctx (s : E.summary) =
  Yojson.Safe.from_string
    (Eio.Path.load
       Eio.Path.(ctx.root / "experiments" / s.experiment_id / "manifest.json"))

let member = Yojson.Safe.Util.member
let str j k = Yojson.Safe.Util.to_string (member k j)

let status_kind : E.status -> string = function
  | Pending -> "pending"
  | Interrupted -> "interrupted"
  | Done o -> Run.outcome_kind o

let kinds (s : E.summary) = List.map (fun (_, st, _) -> status_kind st) s.runs

let test_full_run =
  with_ctx "full" @@ fun ctx ->
  let s = ok (go ctx) in
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
    E.run_experiment ~root:ctx.root ~keys:[] ~env:ctx.env
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

let () =
  Alcotest.run "executor"
    [
      ( "plan",
        [
          Alcotest.test_case "same seed, same plan" `Quick test_plan_same_seed;
          Alcotest.test_case "every run once" `Quick test_plan_complete;
          Alcotest.test_case "interleaved and shuffled" `Quick
            test_plan_interleaved;
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
      ("caps", [ Alcotest.test_case "caps respected" `Slow test_caps ]);
    ]
