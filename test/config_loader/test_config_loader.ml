open Bactrian_core
module Loader = Bactrian_runtime.Config_loader

let with_dir f =
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_dir "bactrian_loader" "" in
  let dir = Eio.Path.(env#fs / tmp) in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
    (fun () -> f dir)

let write dir name text =
  let path = Eio.Path.(dir / name) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700
    (match Eio.Path.split path with Some (d, _) -> d | None -> dir);
  Eio.Path.save ~create:(`Or_truncate 0o600) path text

let loaded = function
  | Ok v -> v
  | Error (e : Loader.located_error list) ->
      Alcotest.failf "unexpected errors: %s"
        (String.concat "; "
           (List.map
              (fun (e : Loader.located_error) ->
                Printf.sprintf "%d:%d %s %s" e.line e.column e.path e.message)
              e))

let errors = function
  | Ok _ -> Alcotest.fail "expected errors"
  | Error (e : Loader.located_error list) -> e

let pos = Alcotest.(option (pair int int))

let yaml_text =
  "version: 1\n\
   id: demo\n\
   turns:\n\
  \  - user: hello\n\
  \  - user: \"two\"\n\
   assertions:\n\
  \  - max_steps: 12\n\
  \  - tool_args: { tool: x, match: { a: 1.5 } }\n\
   ratio: 0.25\n\
   on: true\n\
   nothing: ~\n\
   when: 2026-10-14T09:00:00+01:00\n"

let test_values_and_positions () =
  with_dir @@ fun dir ->
  write dir "s.yaml" yaml_text;
  let canon, find = loaded (Loader.load Eio.Path.(dir / "s.yaml")) in
  Alcotest.(check string)
    "canon"
    {|{"assertions":[{"max_steps":12},{"tool_args":{"match":{"a":1.5},"tool":"x"}}],"id":"demo","nothing":null,"on":true,"ratio":0.25,"turns":[{"user":"hello"},{"user":"two"}],"version":1,"when":"2026-10-14T09:00:00+01:00"}|}
    (Canon.to_string canon);
  Alcotest.check pos "key" (Some (2, 1)) (find "id");
  Alcotest.check pos "nested key" (Some (4, 5)) (find "turns[0].user");
  Alcotest.check pos "list item" (Some (5, 5)) (find "turns[1]");
  Alcotest.check pos "deep"
    (Some (8, 36))
    (find "assertions[1].tool_args.match.a");
  Alcotest.check pos "ancestor fallback"
    (Some (7, 5))
    (find "assertions[0].max_steps.extra");
  Alcotest.check pos "missing key falls back to parent"
    (Some (3, 1))
    (find "turns[9]")

let test_json () =
  with_dir @@ fun dir ->
  write dir "s.json"
    "{\n  \"id\": \"a\",\n  \"turns\": [\n    {\"user\": \"x\"}\n  ]\n}\n";
  let canon, find = loaded (Loader.load Eio.Path.(dir / "s.json")) in
  Alcotest.(check string)
    "canon" {|{"id":"a","turns":[{"user":"x"}]}|} (Canon.to_string canon);
  Alcotest.check pos "line" (Some (4, 6)) (find "turns[0].user")

let test_syntax_error () =
  with_dir @@ fun dir ->
  write dir "bad.yaml" "id: a\nturns:\n  - user: x\n bad: [1, 2\nnext: 3\n";
  match errors (Loader.load Eio.Path.(dir / "bad.yaml")) with
  | [ e ] ->
      Alcotest.(check bool) "line within file" true (e.line >= 3 && e.line <= 5);
      Alcotest.(check bool)
        "message from parser" true
        (String.length e.message > 0)
  | _ -> Alcotest.fail "one error expected"

let test_syntax_error_line () =
  with_dir @@ fun dir ->
  write dir "bad.yaml" "a: 1\nb: 2\nc: [\nd: 4\n";
  match errors (Loader.load Eio.Path.(dir / "bad.yaml")) with
  | [ e ] -> Alcotest.(check (pair int int)) "position" (5, 1) (e.line, e.column)
  | _ -> Alcotest.fail "one error expected"

let test_duplicate_key_and_alias () =
  with_dir @@ fun dir ->
  write dir "d.yaml" "a: 1\nb: 2\na: 3\n";
  (match errors (Loader.load Eio.Path.(dir / "d.yaml")) with
  | [ e ] -> Alcotest.(check (pair int string)) "dup" (3, "a") (e.line, e.path)
  | _ -> Alcotest.fail "one error expected");
  write dir "al.yaml" "a: &x 1\nb: *x\n";
  match errors (Loader.load Eio.Path.(dir / "al.yaml")) with
  | [ e ] -> Alcotest.(check (pair int string)) "alias" (2, "b") (e.line, e.path)
  | _ -> Alcotest.fail "one error expected"

let test_missing_file () =
  with_dir @@ fun dir ->
  match errors (Loader.load Eio.Path.(dir / "nope.yaml")) with
  | [ e ] -> Alcotest.(check bool) "named" true (String.length e.file > 0)
  | _ -> Alcotest.fail "one error expected"

let touch dir name = write dir name "x: 1\n"

let test_globs () =
  with_dir @@ fun dir ->
  List.iter (touch dir)
    [
      "a.yaml";
      "b.yaml";
      "c.json";
      "sub/d.yaml";
      "sub/deep/e.yaml";
      "sub/deep/f.txt";
    ];
  let expand pats = loaded (Loader.expand_globs ~base:dir pats) in
  let strs = Alcotest.(list string) in
  Alcotest.check strs "star" [ "a.yaml"; "b.yaml" ] (expand [ "*.yaml" ]);
  Alcotest.check strs "question" [ "c.json" ] (expand [ "?.json" ]);
  Alcotest.check strs "double star nested"
    [ "a.yaml"; "b.yaml"; "sub/d.yaml"; "sub/deep/e.yaml" ]
    (expand [ "**/*.yaml" ]);
  Alcotest.check strs "dir then star" [ "sub/d.yaml" ] (expand [ "sub/*.yaml" ]);
  Alcotest.check strs "double star in middle" [ "sub/deep/e.yaml" ]
    (expand [ "sub/**/e.yaml" ]);
  Alcotest.check strs "literal" [ "c.json" ] (expand [ "c.json" ]);
  Alcotest.check strs "union without duplicates"
    [ "a.yaml"; "b.yaml"; "c.json" ]
    (expand [ "*.yaml"; "a.yaml"; "*.json" ]);
  Alcotest.check strs "star does not cross directories" [ "a.yaml"; "b.yaml" ]
    (expand [ "*.yaml" ])

let test_glob_nothing () =
  with_dir @@ fun dir ->
  touch dir "a.yaml";
  match
    errors (Loader.expand_globs ~base:dir [ "*.yaml"; "none/*.yaml"; "*.zip" ])
  with
  | [ e1; e2 ] ->
      Alcotest.(check (list string))
        "patterns" [ "none/*.yaml"; "*.zip" ] [ e1.path; e2.path ]
  | _ -> Alcotest.fail "two errors expected"

(* Three mistakes on known lines are all reported with their lines. *)
let broken_scenario =
  "version: 1\n\
   id: broken\n\
   colour: blue\n\
   turns:\n\
  \  - user: hello\n\
  \  - nobody: here\n\
   assertions:\n\
  \  - max_steps: 12\n\
  \  - max_steps: lots\n"

let test_scenario_three_mistakes () =
  with_dir @@ fun dir ->
  write dir "s.yaml" broken_scenario;
  let errs = errors (Loader.load_scenario Eio.Path.(dir / "s.yaml")) in
  let lines =
    List.map (fun (e : Loader.located_error) -> (e.line, e.path)) errs
  in
  List.iter
    (fun expected ->
      Alcotest.(check bool)
        (Printf.sprintf "line %d reported in [%s]" expected
           (String.concat "; "
              (List.map
                 (fun (e : Loader.located_error) ->
                   Printf.sprintf "%d:%d %s: %s" e.line e.column e.path
                     e.message)
                 errs)))
        true
        (List.exists (fun (l, _) -> l = expected) lines))
    [ 3; 6; 9 ];
  List.iter
    (fun (e : Loader.located_error) ->
      Alcotest.(check string) "file" (snd Eio.Path.(dir / "s.yaml")) e.file;
      Alcotest.(check bool) "column" true (e.column >= 1))
    errs

let valid_scenario =
  "version: 1\nid: ok\nturns:\n  - user: hi\nassertions:\n  - max_steps: 3\n"

let test_scenario_ok_and_json () =
  with_dir @@ fun dir ->
  write dir "s.yaml" valid_scenario;
  let s = loaded (Loader.load_scenario Eio.Path.(dir / "s.yaml")) in
  Alcotest.(check string) "id" "ok" s.Scenario.id;
  write dir "s.json"
    {|{"version":1,"id":"j","turns":[{"user":"hi"}],"assertions":[{"max_steps":3}]}|};
  let j = loaded (Loader.load_scenario Eio.Path.(dir / "s.json")) in
  Alcotest.(check string) "json id" "j" j.Scenario.id

let with_fixtures line = valid_scenario ^ Printf.sprintf "%s\n" line

let test_fixtures_directory () =
  with_dir @@ fun dir ->
  write dir "s/sc.yaml" (with_fixtures "fixtures: ./data");
  (match errors (Loader.load_scenario Eio.Path.(dir / "s" / "sc.yaml")) with
  | [ e ] ->
      Alcotest.(check (triple int string bool))
        "located at the fixtures line" (7, "fixtures", true)
        (e.line, e.path, String.length e.message > 0)
  | _ -> Alcotest.fail "one error expected");
  write dir "s/data/file.txt" "hello";
  let s = loaded (Loader.load_scenario Eio.Path.(dir / "s" / "sc.yaml")) in
  Alcotest.(check (option string)) "kept" (Some "./data") s.Scenario.fixtures;
  write dir "s/sc2.yaml" (with_fixtures "fixtures: data/file.txt");
  Alcotest.(check int)
    "a file is not a directory" 1
    (List.length
       (errors (Loader.load_scenario Eio.Path.(dir / "s" / "sc2.yaml"))))

let test_pricing_file () =
  with_dir @@ fun dir ->
  write dir "p.yaml"
    "version: 1\n\
     prices:\n\
    \  - model: gpt-6-luna\n\
    \    provider: openai\n\
    \    effective_from: 2026-11-01\n\
    \    rates: { input: 7, cached_input: 1, cache_write: 1, output: 2 }\n\
    \    max_output: 10\n\
    \    max_input: 20\n";
  let table = loaded (Loader.load_pricing Eio.Path.(dir / "p.yaml")) in
  let input date =
    Option.map
      (fun (e : Pricing.entry) -> e.rates.input)
      (Pricing.find_in table ~model:"gpt-6-luna" ~date)
  in
  Alcotest.(check (option (float 1e-9)))
    "before" (Some 0.10) (input "2026-10-01");
  Alcotest.(check (option (float 1e-9))) "after" (Some 7.) (input "2026-11-02");
  write dir "bad.yaml" "version: 1\nprices:\n  - model: m\n    provider: acme\n";
  let paths =
    List.map
      (fun (e : Loader.located_error) -> (e.line, e.path))
      (errors (Loader.load_pricing Eio.Path.(dir / "bad.yaml")))
  in
  Alcotest.(check bool)
    "provider located" true
    (List.mem (4, "prices[0].provider") paths)

let test_experiment_error_located () =
  with_dir @@ fun dir ->
  write dir "e.yaml" "version: 1\nname: x\nrepeats: 0\nwat: 1\n";
  let errs = errors (Loader.load_experiment Eio.Path.(dir / "e.yaml")) in
  Alcotest.(check bool)
    "unknown field on line 4" true
    (List.exists (fun (e : Loader.located_error) -> e.line = 4) errs);
  Alcotest.(check bool)
    "repeats on line 3" true
    (List.exists (fun (e : Loader.located_error) -> e.line = 3) errs)

let () =
  Alcotest.run "config_loader"
    [
      ( "load",
        [
          Alcotest.test_case "values and positions" `Quick
            test_values_and_positions;
          Alcotest.test_case "json" `Quick test_json;
          Alcotest.test_case "syntax error" `Quick test_syntax_error;
          Alcotest.test_case "syntax error line" `Quick test_syntax_error_line;
          Alcotest.test_case "duplicate key and alias" `Quick
            test_duplicate_key_and_alias;
          Alcotest.test_case "missing file" `Quick test_missing_file;
        ] );
      ( "globs",
        [
          Alcotest.test_case "patterns" `Quick test_globs;
          Alcotest.test_case "no match" `Quick test_glob_nothing;
        ] );
      ( "decoding",
        [
          Alcotest.test_case "three mistakes" `Quick
            test_scenario_three_mistakes;
          Alcotest.test_case "valid and json" `Quick test_scenario_ok_and_json;
          Alcotest.test_case "experiment" `Quick test_experiment_error_located;
          Alcotest.test_case "fixtures directory" `Quick test_fixtures_directory;
          Alcotest.test_case "pricing file" `Quick test_pricing_file;
        ] );
    ]
