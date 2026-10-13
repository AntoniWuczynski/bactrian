open Bactrian_core
module G = Bactrian_runtime.Golden
module Loader = Bactrian_runtime.Config_loader

let with_dir f =
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_dir "bactrian_golden" "" in
  Fun.protect
    ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / tmp))
    (fun () -> f env tmp)

let write dir name text =
  let path = Filename.concat dir name in
  (try Sys.mkdir (Filename.dirname path) 0o700 with Sys_error _ -> ());
  Out_channel.with_open_bin path (fun oc -> output_string oc text)

(* Traces *)

let canon s =
  match Canon.of_string s with
  | Ok v -> v
  | Error e -> Alcotest.failf "bad JSON %s: %a" s Canon.pp_error e

let trace_text names =
  let events =
    [
      Trace.Run_start
        {
          run_id = "golden-1";
          scenario = "sched";
          variant = "v";
          sim_now = "2026-10-14T09:00:00Z";
          clock = Contract_only;
        };
      Turn_start { turn = 1; content = "hi" };
    ]
    @ List.mapi
        (fun i name ->
          Trace.Tool_call
            { turn = 1; index = i; name; arguments = canon "{}"; mcp = Joined })
        names
    @ [ Reply { turn = 1; content = "done" }; Done ]
  in
  String.concat ""
    (List.mapi
       (fun i event -> Trace.to_line { seq = i; t_ms = i; event } ^ "\n")
       events)

let checker =
  {|import json, sys
t = json.load(sys.stdin)
n = sum(1 for e in t if e["type"] == "tool_call" and e["name"] == "calendar_create")
print(json.dumps({"pass": n < 2, "message": "double booked"}))|}

let scenario ?(assertions = {|{"max_steps": 2}|})
    ?(checkers = {|[{"command": ["python3", "-I", "checks/no_double.py"]}]|})
    ?(pass = {|["golden/ok.jsonl"]|}) ?(fail = {|["golden/bad.jsonl"]|}) () =
  Printf.sprintf
    {|{"version": 1, "id": "sched", "turns": [{"user": "hi"}],
 "assertions": [{"tool_sequence": {"mode": "contains", "tools": ["calendar_create"]}}, %s],
 "checkers": %s, "golden": {"pass": %s, "fail": %s}}|}
    assertions checkers pass fail

let setup dir text =
  write dir "s.json" text;
  write dir "checks/no_double.py" checker;
  write dir "golden/ok.jsonl" (trace_text [ "calendar_create" ]);
  write dir "golden/bad.jsonl"
    (trace_text [ "calendar_create"; "calendar_create" ])

let check env dir =
  match G.check ~env ~scenario_file:(Filename.concat dir "s.json") with
  | Ok r -> r
  | Error _ -> Alcotest.fail "unexpected configuration errors"

let offenders (r : G.report) =
  List.map
    (fun (o : G.offender) -> (Filename.basename o.trace, o.expected))
    r.offenders

let pp_offender ppf (t, e) =
  Format.fprintf ppf "%s/%s" t
    (match e with G.Pass -> "pass" | G.Fail -> "fail")

let offenders_t = Alcotest.(list (testable pp_offender ( = )))

let test_clean () =
  with_dir @@ fun env dir ->
  setup dir (scenario ());
  Alcotest.check offenders_t "none" [] (offenders (check env dir))

(* Without the checker nothing rejects the double booking, so the golden fail
   trace exposes the gap. *)
let test_lax_checker_is_caught () =
  with_dir @@ fun env dir ->
  setup dir (scenario ~checkers:"[]" ());
  let r = check env dir in
  Alcotest.check offenders_t "bad trace passes everything"
    [ ("bad.jsonl", G.Fail) ]
    (offenders r)

let test_strict_assertion_is_caught () =
  with_dir @@ fun env dir ->
  setup dir (scenario ());
  (* The pass trace never calls the tool the first assertion requires. *)
  write dir "golden/ok.jsonl" (trace_text [ "other" ]);
  let r = check env dir in
  Alcotest.check offenders_t "ok trace fails"
    [ ("ok.jsonl", G.Pass) ]
    (offenders r);
  match r.offenders with
  | [ o ] ->
      Alcotest.(check bool)
        "names the assertion" true
        (Re.execp (Re.compile (Re.str "assertions[0] tool_sequence")) o.reason)
  | _ -> Alcotest.fail "one offender"

let test_checker_without_verdict () =
  with_dir @@ fun env dir ->
  setup dir (scenario ());
  write dir "checks/no_double.py" "print('garbage')";
  let r = check env dir in
  (* The pass trace fails because of the checker error, and the fail trace is
     an offender too: a checker with no verdict proves nothing. *)
  Alcotest.check offenders_t "both"
    [ ("ok.jsonl", G.Pass); ("bad.jsonl", G.Fail) ]
    (offenders r)

let test_unreadable_and_invalid_traces () =
  with_dir @@ fun env dir ->
  setup dir (scenario ~pass:{|["golden/missing.jsonl"]|} ());
  write dir "golden/bad.jsonl" "not json\n";
  let r = check env dir in
  Alcotest.check offenders_t "both"
    [ ("missing.jsonl", G.Pass); ("bad.jsonl", G.Fail) ]
    (offenders r)

let test_invalid_scenario () =
  with_dir @@ fun env dir ->
  write dir "s.json"
    {|{"version": 1, "id": "x", "turns": [], "bogus": 1,
 "assertions": [{"max_steps": "many"}]}|};
  match G.check ~env ~scenario_file:(Filename.concat dir "s.json") with
  | Ok _ -> Alcotest.fail "expected errors"
  | Error errors ->
      Alcotest.(check bool) "all collected" true (List.length errors >= 3);
      List.iter
        (fun (e : Loader.located_error) ->
          Alcotest.(check bool) "located" true (e.line >= 1 && e.column >= 1))
        errors

let () =
  Alcotest.run "golden"
    [
      ( "check",
        [
          Alcotest.test_case "clean scenario" `Quick test_clean;
          Alcotest.test_case "lax checker caught" `Quick
            test_lax_checker_is_caught;
          Alcotest.test_case "strict assertion caught" `Quick
            test_strict_assertion_is_caught;
          Alcotest.test_case "checker without verdict" `Quick
            test_checker_without_verdict;
          Alcotest.test_case "unreadable and invalid traces" `Quick
            test_unreadable_and_invalid_traces;
          Alcotest.test_case "invalid scenario" `Quick test_invalid_scenario;
        ] );
    ]
