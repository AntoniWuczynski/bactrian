open Bactrian_core

let parse s =
  match Canon.of_string s with
  | Ok v -> v
  | Error _ -> Alcotest.fail ("bad fixture: " ^ s)

let kind_str = function
  | Diff.Changed -> "changed"
  | Diff.Added -> "added"
  | Diff.Removed -> "removed"

let change_t =
  Alcotest.testable
    (fun ppf (c : Diff.change) ->
      Format.fprintf ppf "%s:%s" (kind_str c.kind) c.path)
    ( = )

let ch kind path : Diff.change = { path; kind }

let check_changes name a b expected =
  Alcotest.(check (list change_t))
    name expected
    (Diff.changes (parse a) (parse b))

let sim a b = Diff.similarity (parse a) (parse b)
let float_t = Alcotest.float 1e-9

let equal_docs () =
  let d = {|{"a":[1,2,{"b":"x"}],"c":null}|} in
  check_changes "no changes" d d [];
  Alcotest.check float_t "similarity" 1.0 (sim d d)

let nested_path () =
  check_changes "messages[3].content"
    {|{"messages":[{"content":"a"},{"content":"b"},{"content":"c"},{"content":"d"}]}|}
    {|{"messages":[{"content":"a"},{"content":"b"},{"content":"c"},{"content":"e"}]}|}
    [ ch Diff.Changed "messages[3].content" ]

let root_path () =
  check_changes "root scalar" "1" "2" [ ch Diff.Changed "" ];
  check_changes "root kind" "[]" "{}" [ ch Diff.Changed "" ]

let object_fields () =
  check_changes "added and removed" {|{"a":1,"b":2,"d":4}|}
    {|{"a":1,"c":3,"d":4}|}
    [ ch Diff.Removed "b"; ch Diff.Added "c" ];
  check_changes "nested added" {|{"x":{"a":1}}|} {|{"x":{"a":1,"b":2}}|}
    [ ch Diff.Added "x.b" ]

let list_tails () =
  check_changes "added tail" "[1,2]" "[1,2,3,4]"
    [ ch Diff.Added "[2]"; ch Diff.Added "[3]" ];
  check_changes "removed tail" {|{"l":[1,2,3]}|} {|{"l":[1]}|}
    [ ch Diff.Removed "l[1]"; ch Diff.Removed "l[2]" ]

let kind_mismatch () =
  check_changes "object to string" {|{"a":{"b":1}}|} {|{"a":"s"}|}
    [ ch Diff.Changed "a" ];
  check_changes "document order" {|{"a":1,"b":[1,2],"c":1}|}
    {|{"a":2,"b":[1,3],"c":2}|}
    [ ch Diff.Changed "a"; ch Diff.Changed "b[1]"; ch Diff.Changed "c" ]

let similarity_values () =
  Alcotest.check float_t "half" 0.5 (sim {|{"a":1,"b":2}|} {|{"a":1,"b":3}|});
  Alcotest.check float_t "nothing" 0.0 (sim {|{"a":1}|} {|{"b":1}|});
  Alcotest.check float_t "larger document" 0.5
    (sim {|{"a":1}|} {|{"a":1,"b":2}|});
  Alcotest.check float_t "empty containers" 1.0 (sim "[]" "[]");
  Alcotest.check float_t "empty containers differ" 0.0 (sim "[]" "{}");
  Alcotest.check float_t "same value, other path" 0.0 (sim "[1,2]" "[2,1]")

let leaf_counts () =
  let n s = Diff.leaf_count (parse s) in
  Alcotest.(check int) "scalar" 1 (n "1");
  Alcotest.(check int) "empty list" 1 (n "[]");
  Alcotest.(check int) "empty object" 1 (n "{}");
  Alcotest.(check int) "nested" 4 (n {|{"a":[1,2,{"b":"x"}],"c":[]}|});
  Alcotest.(check bool)
    "bounds similarity" true
    (let a = parse {|{"a":1,"b":2,"c":3}|} and b = parse {|{"a":1}|} in
     Diff.similarity a b
     <= float_of_int (Diff.leaf_count b) /. float_of_int (Diff.leaf_count a))

let performance () =
  let arr other =
    Canon.list
      (List.init 10_000 (fun i -> Canon.int (if i = 5_000 then other else i)))
  in
  let a = arr (-1) and b = arr (-2) in
  let t0 = Sys.time () in
  let cs = Diff.changes a b in
  let s = Diff.similarity a b in
  let dt = Sys.time () -. t0 in
  Alcotest.(check (list change_t)) "one change" [ ch Diff.Changed "[5000]" ] cs;
  Alcotest.(check bool) "similarity high" true (s > 0.999 && s < 1.0);
  Alcotest.(check bool) (Printf.sprintf "fast (%.3fs)" dt) true (dt < 0.5)

let gen_canon : Canon.t QCheck.Gen.t =
  let open QCheck.Gen in
  let key = oneof_list [ "a"; "b"; "c"; "d" ] in
  let leaf =
    oneof
      [
        return Canon.null;
        map Canon.bool bool;
        map Canon.int (int_range 0 3);
        map Canon.string (oneof_list [ "x"; "y" ]);
      ]
  in
  let obj fields =
    let fields =
      List.sort_uniq (fun (k1, _) (k2, _) -> String.compare k1 k2) fields
    in
    match Canon.obj fields with Ok v -> v | Error _ -> Canon.null
  in
  sized
  @@ fix (fun self n ->
      if n <= 0 then leaf
      else
        oneof_weighted
          [
            (2, leaf);
            (1, map Canon.list (list_size (int_bound 3) (self (n / 2))));
            (1, map obj (list_size (int_bound 3) (pair key (self (n / 2)))));
          ])

let arb = QCheck.make ~print:Canon.to_string gen_canon
let arb2 = QCheck.pair arb arb

let prop_empty_iff_equal =
  QCheck.Test.make ~name:"changes = [] iff equal" ~count:500 arb2 (fun (a, b) ->
      Bool.equal (Diff.changes a b = []) (Canon.equal a b))

let prop_symmetric =
  QCheck.Test.make ~name:"similarity symmetric and bounded" ~count:500 arb2
    (fun (a, b) ->
      let s = Diff.similarity a b in
      Float.equal s (Diff.similarity b a) && s >= 0.0 && s <= 1.0)

let prop_self =
  QCheck.Test.make ~name:"similarity of equal is 1" ~count:200 arb (fun a ->
      Float.equal (Diff.similarity a a) 1.0)

let () =
  Alcotest.run "diff"
    [
      ( "changes",
        [
          Alcotest.test_case "equal documents" `Quick equal_docs;
          Alcotest.test_case "nested path" `Quick nested_path;
          Alcotest.test_case "root path" `Quick root_path;
          Alcotest.test_case "object fields" `Quick object_fields;
          Alcotest.test_case "list tails" `Quick list_tails;
          Alcotest.test_case "kind mismatch and order" `Quick kind_mismatch;
        ] );
      ("similarity", [ Alcotest.test_case "values" `Quick similarity_values ]);
      ("leaves", [ Alcotest.test_case "leaf count" `Quick leaf_counts ]);
      ("performance", [ Alcotest.test_case "10k arrays" `Quick performance ]);
      ( "properties",
        List.map
          (fun t -> QCheck_alcotest.to_alcotest t)
          [ prop_empty_iff_equal; prop_symmetric; prop_self ] );
    ]
