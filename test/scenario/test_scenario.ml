open Bactrian_core

let canon json =
  match Canon.of_string json with
  | Ok c -> c
  | Error _ -> Alcotest.fail "test document is not JSON"

let errors json =
  match Scenario.decode (canon json) with
  | Ok _ -> Alcotest.fail "expected errors"
  | Error es -> es

let paths json =
  List.sort String.compare
    (List.map (fun (e : Scenario.error) -> e.path) (errors json))

let decoded json =
  match Scenario.decode (canon json) with
  | Ok t -> t
  | Error es ->
      Alcotest.failf "unexpected errors: %a"
        Fmt.(list ~sep:semi Config_error.pp)
        es

let strings = Alcotest.(list string)
let base = {|"version": 1, "id": "s", "turns": [{"user": "hi"}]|}
let doc extra = "{" ^ base ^ extra ^ "}"

let is_sequence expected : Assertion.t -> bool = function
  | Tool_sequence { tools; _ } -> List.equal String.equal tools expected
  | Tool_args _ | Max_steps _ | Output _ | Max_cost_usd _ | Max_tokens _
  | Max_latency_ms _ | No_egress _ ->
      false

let is_max_steps n : Assertion.t -> bool = function
  | Max_steps m -> Int.equal m n
  | Tool_sequence _ | Tool_args _ | Output _ | Max_cost_usd _ | Max_tokens _
  | Max_latency_ms _ | No_egress _ ->
      false

let test_full () =
  let t =
    decoded
      {|{"version": 1, "id": "schedule-meeting",
         "now": "2026-10-14T09:00:00+01:00", "timezone": "Europe/London",
         "fixtures": "./fixtures/basic",
         "turns": [{"user": "one"}, {"user": "two"}],
         "assertions": [
           {"tool_sequence": {"mode": "contains", "tools": ["a", "b"]}},
           {"max_steps": 12}],
         "checkers": [{"command": ["python3", "checks/x.py"]}],
         "golden": {"pass": ["golden/ok.jsonl"], "fail": ["golden/bad.jsonl"]}}|}
  in
  Alcotest.(check string) "id" "schedule-meeting" t.id;
  Alcotest.(check (option string))
    "now" (Some "2026-10-14T09:00:00+01:00") t.now;
  Alcotest.(check (option string)) "tz" (Some "Europe/London") t.timezone;
  Alcotest.(check (option string))
    "fixtures" (Some "./fixtures/basic") t.fixtures;
  Alcotest.(check strings)
    "turns" [ "one"; "two" ]
    (List.map (fun (u : Scenario.turn) -> u.user) t.turns);
  (match t.assertions with
  | [ first; second ] ->
      Alcotest.(check bool)
        "tool_sequence" true
        (is_sequence [ "a"; "b" ] first);
      Alcotest.(check bool) "max_steps" true (is_max_steps 12 second)
  | _ -> Alcotest.fail "assertions");
  Alcotest.(check (list strings))
    "checkers"
    [ [ "python3"; "checks/x.py" ] ]
    t.checkers;
  Alcotest.(check strings) "pass" [ "golden/ok.jsonl" ] t.golden_pass;
  Alcotest.(check strings) "fail" [ "golden/bad.jsonl" ] t.golden_fail

let test_defaults () =
  let t = decoded (doc "") in
  Alcotest.(check (option string)) "now" None t.now;
  Alcotest.(check (option string)) "tz" None t.timezone;
  Alcotest.(check (option string)) "fixtures" None t.fixtures;
  Alcotest.(check int) "assertions" 0 (List.length t.assertions);
  Alcotest.(check (list strings)) "checkers" [] t.checkers;
  Alcotest.(check strings) "pass" [] t.golden_pass;
  Alcotest.(check strings) "fail" [] t.golden_fail

let test_paths name expected json () =
  Alcotest.(check strings) name expected (paths json)

let test_message () =
  match errors (doc {|, "colour": 1|}) with
  | [ { path = "colour"; message = "unknown field" } ] -> ()
  | es -> Alcotest.failf "got %a" Fmt.(list ~sep:semi Config_error.pp) es

let test_assertion_prefix () =
  match
    paths
      (doc
         {|, "assertions": [{"max_steps": 1}, {"max_steps": 2}, {"no_such_kind": 1}]|})
  with
  | [ p ] when String.starts_with ~prefix:"assertions[2]" p -> ()
  | ps -> Alcotest.failf "got %s" (String.concat ", " ps)

let five =
  {|{"version": 1, "bogus": true, "id": 3,
     "turns": [{"user": "ok"}, {}],
     "checkers": [{"command": ["x"]}, {"command": []}],
     "golden": {"pass": "not-a-list"}}|}

let () =
  Alcotest.run "scenario"
    [
      ( "valid",
        [
          Alcotest.test_case "full document" `Quick test_full;
          Alcotest.test_case "defaults" `Quick test_defaults;
        ] );
      ( "errors",
        [
          Alcotest.test_case "root not an object" `Quick
            (test_paths "root" [ "" ] "[]");
          Alcotest.test_case "unknown field" `Quick
            (test_paths "unknown" [ "colour" ] (doc {|, "colour": 1|}));
          Alcotest.test_case "unknown nested field" `Quick
            (test_paths "nested" [ "turns[1].extra" ]
               {|{"version":1,"id":"s","turns":[{"user":"a"},{"user":"b","extra":1}]}|});
          Alcotest.test_case "unknown message" `Quick test_message;
          Alcotest.test_case "wrong type" `Quick
            (test_paths "id" [ "id" ]
               {|{"version":1,"id":3,"turns":[{"user":"a"}]}|});
          Alcotest.test_case "missing id" `Quick
            (test_paths "id" [ "id" ] {|{"version":1,"turns":[{"user":"a"}]}|});
          Alcotest.test_case "missing turns" `Quick
            (test_paths "turns" [ "turns" ] {|{"version":1,"id":"s"}|});
          Alcotest.test_case "empty turns" `Quick
            (test_paths "turns" [ "turns" ]
               {|{"version":1,"id":"s","turns":[]}|});
          Alcotest.test_case "turn without user" `Quick
            (test_paths "user" [ "turns[0].user" ]
               {|{"version":1,"id":"s","turns":[{}]}|});
          Alcotest.test_case "missing version" `Quick
            (test_paths "version" [ "version" ]
               {|{"id":"s","turns":[{"user":"a"}]}|});
          Alcotest.test_case "wrong version" `Quick
            (test_paths "version" [ "version" ]
               {|{"version":2,"id":"s","turns":[{"user":"a"}]}|});
          Alcotest.test_case "empty checker command" `Quick
            (test_paths "command" [ "checkers[0].command" ]
               (doc {|, "checkers": [{"command": []}]|}));
          Alcotest.test_case "unknown golden field" `Quick
            (test_paths "golden" [ "golden.maybe" ]
               (doc {|, "golden": {"maybe": []}|}));
          Alcotest.test_case "assertion errors carry the document path" `Quick
            test_assertion_prefix;
          Alcotest.test_case "five mistakes, five errors" `Quick
            (test_paths "five"
               [
                 "bogus";
                 "checkers[1].command";
                 "golden.pass";
                 "id";
                 "turns[1].user";
               ]
               five);
        ] );
    ]
