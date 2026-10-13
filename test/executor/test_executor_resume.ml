(* Spend attribution, resume rules, locking and the settings a build cannot
   enforce yet. *)

open Bactrian_runtime
open Executor_support
module J = Yojson.Safe.Util

(* Every reservation, settlement and gap belongs to the run that made it. *)

let ledger_lines ctx (s : E.summary) =
  read ctx
    (Printf.sprintf ".bactrian/experiments/%s/ledger.jsonl" s.experiment_id)
  |> String.split_on_char '\n'
  |> List.filter (( <> ) "")
  |> List.map (fun l -> Yojson.Safe.from_string l)

(* Per owner, from the file alone: reserved amounts by id, replaced by the
   settlement of the same id. *)
let owned_spend lines =
  let owner = Hashtbl.create 16 and reserved = Hashtbl.create 16 in
  List.iter
    (fun l ->
      let id = J.(to_int (member "id" l)) in
      match J.(to_string (member "event" l)) with
      | "reserve" ->
          Hashtbl.replace owner id J.(to_string (member "owner" l));
          Hashtbl.replace reserved id J.(to_float (member "reserved_usd" l))
      | _ -> Hashtbl.replace reserved id J.(to_float (member "usd" l)))
    lines;
  Hashtbl.fold
    (fun id usd acc ->
      let o = Hashtbl.find owner id in
      (o, usd +. Option.value ~default:0. (List.assoc_opt o acc))
      :: List.remove_assoc o acc)
    reserved []

let test_spend_per_run =
  with_ctx "spend-per-run" @@ fun ctx ->
  write ctx "exp.json" (experiment_json ctx ~concurrency:4 ());
  let s = ok (go ctx) in
  let ids = run_ids s in
  Alcotest.(check int) "eight runs" 8 (List.length ids);
  let lines = ledger_lines ctx s in
  let by_owner = owned_spend lines in
  Alcotest.(check int) "every run owns spend" 8 (List.length by_owner);
  List.iter
    (fun id ->
      Alcotest.(check (float 1e-12))
        ("run " ^ id ^ " records its own settlements")
        (List.assoc id by_owner) (spent ctx id))
    ids;
  let total = List.fold_left (fun acc id -> acc +. spent ctx id) 0. ids in
  let ledger =
    Ledger.create
      ~path:
        Eio.Path.(ctx.root / "experiments" / s.experiment_id / "ledger.jsonl")
      ~caps:[ ("openai", 100.) ]
  in
  Alcotest.(check bool) "spend is positive" true (total > 0.);
  Alcotest.(check (float 1e-12))
    "per-run spends sum to the ledger total"
    (Ledger.spent ledger ~provider:"openai")
    total

(* Resume *)

let refused = function
  | Error (E.Usage m) -> m
  | Ok _ | Error (E.Configuration _ | E.Environment _) ->
      Alcotest.fail "expected the resume to be refused"

let mentions needle m =
  Alcotest.(check bool)
    (Printf.sprintf "%S mentions %S" m needle)
    true
    (Re.execp (Re.compile (Re.str needle)) m)

let finished_then name change expect =
  Alcotest.test_case name `Slow
    ( with_ctx name @@ fun ctx ->
      let s = ok (go ctx) in
      change ctx;
      mentions expect (refused (go ~resume:(Some s.experiment_id) ctx)) )

let test_resume_allows_caps_and_concurrency =
  with_ctx "resume-caps" @@ fun ctx ->
  let s = ok (go ctx) in
  write ctx "exp.json"
    (experiment_json ctx ~concurrency:3 ~caps:{|{"openai": 200}|} ());
  let again = ok (go ~resume:(Some s.experiment_id) ctx) in
  Alcotest.(check (list string)) "unchanged" (kinds s) (kinds again)

let edit_manifest ctx (s : E.summary) f =
  let path =
    Eio.Path.(ctx.root / "experiments" / s.experiment_id / "manifest.json")
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) path
    (Yojson.Safe.pretty_to_string
       (f (Yojson.Safe.from_string (Eio.Path.load path))))

let test_resume_reruns_errored =
  with_ctx "resume-errored" @@ fun ctx ->
  let s = ok (go ctx) in
  let victim = List.nth (run_ids s) 2 in
  edit_manifest ctx s (function
    | `Assoc fields ->
        `Assoc
          (List.map
             (function
               | "runs", `List runs ->
                   ( "runs",
                     `List
                       (List.map
                          (fun r ->
                            if
                              String.equal
                                J.(to_string (member "run_id" r))
                                victim
                            then
                              match r with
                              | `Assoc f ->
                                  `Assoc
                                    (List.map
                                       (function
                                         | "outcome", _ ->
                                             ("outcome", `String "errored")
                                         | "reason", _ ->
                                             ("reason", `String "disk full")
                                         | kv -> kv)
                                       f)
                              | other -> other
                            else r)
                          runs) )
               | kv -> kv)
             fields)
    | other -> other);
  let log_before = count_prefix "S " (agent_log ctx) in
  let again = ok (go ~resume:(Some s.experiment_id) ctx) in
  Alcotest.(check (list string)) "all passed" (kinds s) (kinds again);
  Alcotest.(check int)
    "one agent started" (log_before + 1)
    (count_prefix "S " (agent_log ctx));
  let ids = run_ids again in
  Alcotest.(check bool)
    "the errored run has a new id" true
    (not (List.mem victim ids));
  Alcotest.(check int)
    "seven runs kept their ids" 7
    (List.length (List.filter (fun i -> List.mem i (run_ids s)) ids));
  let m = manifest ctx again in
  let rerun =
    List.find
      (fun r -> not (List.mem J.(to_string (member "run_id" r)) (run_ids s)))
      J.(to_list (member "runs" m))
  in
  Alcotest.(check (list string))
    "previous_run_ids" [ victim ]
    (List.map J.to_string J.(to_list (member "previous_run_ids" rerun)))

let test_resume_after_cap =
  with_ctx "resume-cap" @@ fun ctx ->
  write ctx "exp.json"
    (experiment_json ctx ~concurrency:1 ~caps:{|{"openai": 0.0004}|} ());
  let s = ok (go ctx) in
  Alcotest.(check bool) "capped" true (List.mem "not_run" (kinds s));
  let passed_before =
    List.filter_map
      (fun (_, st, id) -> if status_kind st = "passed" then id else None)
      s.runs
  in
  write ctx "exp.json"
    (experiment_json ctx ~concurrency:1 ~caps:{|{"openai": 100}|} ());
  let again = ok (go ~resume:(Some s.experiment_id) ctx) in
  Alcotest.(check (list string))
    "everything passed"
    (List.init 8 (fun _ -> "passed"))
    (kinds again);
  List.iter
    (fun id ->
      Alcotest.(check bool)
        "a finished run keeps its id" true
        (List.mem id (run_ids again)))
    passed_before

(* One process runs an experiment at a time. *)

let test_lock =
  with_ctx "lock" @@ fun ctx ->
  let s = ok (go ctx) in
  let lock =
    Filename.concat (Sys.getcwd ())
      (Printf.sprintf "%s/.bactrian/experiments/%s/run.lock" ctx.dir
         s.experiment_id)
  in
  let stdin_r, stdin_w = Unix.pipe ~cloexec:true () in
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process "python3"
      [|
        "python3";
        "-I";
        "-c";
        "import fcntl, sys\n\
         f = open(sys.argv[1], 'a')\n\
         fcntl.lockf(f, fcntl.LOCK_EX | fcntl.LOCK_NB)\n\
         print('locked', flush=True)\n\
         sys.stdin.read()";
        lock;
      |]
      stdin_r stdout_w Unix.stderr
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  Fun.protect ~finally:(fun () ->
      Unix.close stdin_w;
      Unix.close stdout_r;
      let rec reap () =
        try ignore (Unix.waitpid [] pid)
        with Unix.Unix_error (EINTR, _, _) -> reap ()
      in
      reap ())
  @@ fun () ->
  Alcotest.(check string)
    "child holds the lock" "locked"
    (input_line (Unix.in_channel_of_descr stdout_r));
  mentions "already running" (refused (go ~resume:(Some s.experiment_id) ctx))

(* Settings this build parses but cannot enforce: a strict sandbox stops
   the experiment, the others are listed. *)

let test_sandbox_strict =
  with_ctx "strict" @@ fun ctx ->
  write ctx "exp.json" (experiment_json ctx ~extra:{|, "sandbox": "strict"|} ());
  match go ctx with
  | Error (E.Environment m) -> mentions "no sandbox" m
  | Ok _ | Error (E.Configuration _ | E.Usage _) ->
      Alcotest.fail "sandbox: strict must stop with an environment error"

let test_unenforced_warned_and_recorded =
  with_ctx "unenforced" @@ fun ctx ->
  let warnings = ref [] in
  let s = ok (go ~warn:(fun w -> warnings := w :: !warnings) ctx) in
  let warnings = List.rev !warnings in
  Alcotest.(check bool) "warned" true (List.length warnings >= 3);
  List.iter
    (fun id ->
      Alcotest.(check (list string))
        "recorded in meta.json" warnings
        (List.map J.to_string
           J.(to_list (member "unenforced" (run_meta ctx id)))))
    (run_ids s)

(* Duplicate scenario IDs are configuration errors, reported with the
   others. *)

let configuration = function
  | Error (E.Configuration es) -> es
  | Ok _ | Error (E.Environment _ | E.Usage _) ->
      Alcotest.fail "expected configuration errors"

let test_duplicate_scenario_ids =
  with_ctx "dup-ids" ~scenarios:[] @@ fun ctx ->
  write ctx "scenarios/a.json" (scenario_json "same");
  write ctx "scenarios/b.json" (scenario_json "same");
  match configuration (go ctx) with
  | [ e ] ->
      Alcotest.(check string)
        "at the second file" "b.json" (Filename.basename e.file);
      Alcotest.(check string) "path" "id" e.path;
      mentions "same" e.message;
      mentions "a.json" e.message
  | es -> Alcotest.failf "expected one error, got %d" (List.length es)

let test_all_stages_at_once =
  with_ctx "stages" ~scenarios:[] @@ fun ctx ->
  write ctx "scenarios/bad.json"
    {|{"version": 1, "id": "bad", "colour": 1, "turns": [{"user": "hi"}]}|};
  write ctx "exp.json"
    (experiment_json ctx ~repeats:0 ~extra:{|, "pricing": "nope.yaml"|} ()
    |> Re.replace_string
         (Re.compile (Re.str {|"scenarios/*.json"|}))
         ~by:{|"scenarios/*.json", "scenaros/*.json"|});
  let es = configuration (go ctx) in
  let at file path =
    List.exists
      (fun (e : Config_loader.located_error) ->
        Filename.basename e.file = file && e.path = path)
      es
  in
  Alcotest.(check bool) "repeats" true (at "exp.json" "repeats");
  Alcotest.(check bool) "glob typo" true (at "exp.json" "scenarios[1]");
  Alcotest.(check bool) "missing price file" true (at "exp.json" "pricing");
  Alcotest.(check bool) "scenario" true (at "bad.json" "colour");
  let pricing =
    List.find (fun (e : Config_loader.located_error) -> e.path = "pricing") es
  in
  Alcotest.(check bool)
    "no raw exception" false
    (Re.execp (Re.compile (Re.str "Eio.Io")) pricing.message);
  let positions =
    List.filter_map
      (fun (e : Config_loader.located_error) ->
        if Filename.basename e.file = "exp.json" then Some (e.line, e.column)
        else None)
      es
  in
  Alcotest.(check bool)
    "sorted by position" true
    (positions = List.sort compare positions)

let () =
  Alcotest.run "executor_resume"
    [
      ( "spend",
        [
          Alcotest.test_case "per-run spend sums to the ledger" `Slow
            test_spend_per_run;
        ] );
      ( "resume",
        [
          finished_then "agent changed"
            (fun ctx ->
              write ctx "exp.json" (experiment_json ctx ~prelude:"x = 1\n" ()))
            "agent";
          finished_then "normalise changed"
            (fun ctx ->
              write ctx "exp.json"
                (experiment_json ctx ~extra:{|, "normalise": ["Paris"]|} ()))
            "experiment";
          finished_then "scenario changed"
            (fun ctx ->
              write ctx "scenarios/s1.json"
                (scenario_json ~assertions:{|[{"max_steps": 9}]|} "s1"))
            "scenarios/s1.json";
          finished_then "price file changed"
            (fun ctx ->
              write ctx "prices.yaml"
                "version: 1\n\
                 prices:\n\
                \  - model: gpt-6-luna\n\
                \    provider: openai\n\
                \    rates: { input: 1.0, cached_input: 0.1, cache_write: \
                 1.25, output: 5.0 }\n\
                \    max_output: 65536\n\
                \    max_input: 272000\n";
              write ctx "exp.json"
                (experiment_json ctx ~extra:{|, "pricing": "prices.yaml"|} ()))
            "price file";
          Alcotest.test_case "caps and concurrency may change" `Slow
            test_resume_allows_caps_and_concurrency;
          Alcotest.test_case "errored runs are re-run" `Slow
            test_resume_reruns_errored;
          Alcotest.test_case "not_run runs are re-run after a cap" `Slow
            test_resume_after_cap;
          Alcotest.test_case "one process at a time" `Slow test_lock;
        ] );
      ( "settings",
        [
          Alcotest.test_case "sandbox strict" `Quick test_sandbox_strict;
          Alcotest.test_case "unenforced settings" `Slow
            test_unenforced_warned_and_recorded;
        ] );
      ( "configuration",
        [
          Alcotest.test_case "duplicate scenario ids" `Quick
            test_duplicate_scenario_ids;
          Alcotest.test_case "every stage reports" `Quick
            test_all_stages_at_once;
        ] );
    ]
