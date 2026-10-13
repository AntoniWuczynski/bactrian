open Bactrian_core
module P = Checker_protocol

let verdict =
  Alcotest.testable
    (fun ppf (v : P.verdict) ->
      Format.fprintf ppf "{%b %s %s}" v.pass
        (Option.fold ~none:"-" ~some:string_of_float v.score)
        (Option.value ~default:"-" v.message))
    ( = )

let ok text expected () =
  Alcotest.(check (result verdict string))
    text (Ok expected) (P.decode_verdict text)

let bad text () =
  match P.decode_verdict text with
  | Error _ -> ()
  | Ok _ -> Alcotest.failf "accepted %S" text

let test_trace_json () =
  let entries : Trace.entry list =
    [ { seq = 0; t_ms = 1; event = Done }; { seq = 1; t_ms = 2; event = Done } ]
  in
  let json = P.trace_json entries in
  let expected =
    List.map
      (fun l -> Yojson.Safe.from_string l)
      (List.map Trace.to_line entries)
  in
  Alcotest.(check bool)
    "entries as an array" true
    (Yojson.Safe.equal (Yojson.Safe.from_string json) (`List expected))

let test_empty () = Alcotest.(check string) "empty" "[]" (P.trace_json [])

let () =
  Alcotest.run "checker_protocol"
    [
      ( "decode_verdict",
        [
          Alcotest.test_case "minimal" `Quick
            (ok {|{"pass": true}|}
               { pass = true; score = None; message = None });
          Alcotest.test_case "full" `Quick
            (ok {| {"pass": false, "score": 1, "message": "m"}
|}
               { pass = false; score = Some 1.; message = Some "m" });
          Alcotest.test_case "null optionals" `Quick
            (ok {|{"pass": true, "score": null, "message": null}|}
               { pass = true; score = None; message = None });
          Alcotest.test_case "missing pass" `Quick (bad {|{"score": 1}|});
          Alcotest.test_case "pass not bool" `Quick (bad {|{"pass": "yes"}|});
          Alcotest.test_case "score not number" `Quick
            (bad {|{"pass": true, "score": "1"}|});
          Alcotest.test_case "message not string" `Quick
            (bad {|{"pass": true, "message": 3}|});
          Alcotest.test_case "unknown field" `Quick
            (bad {|{"pass": true, "x": 1}|});
          Alcotest.test_case "not an object" `Quick (bad {|[true]|});
          Alcotest.test_case "not json" `Quick (bad "pass");
          Alcotest.test_case "trailing text" `Quick (bad {|{"pass": true} x|});
          Alcotest.test_case "empty" `Quick (bad "");
        ] );
      ( "trace_json",
        [
          Alcotest.test_case "array" `Quick test_trace_json;
          Alcotest.test_case "empty" `Quick test_empty;
        ] );
    ]
