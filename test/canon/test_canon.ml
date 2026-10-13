open Bactrian_core

let error_equal a b =
  match (a, b) with
  | Canon.Syntax x, Canon.Syntax y -> String.equal x y
  | Duplicate_key x, Duplicate_key y -> String.equal x y
  | Non_finite_float, Non_finite_float -> true
  | Syntax _, (Duplicate_key _ | Non_finite_float)
  | Duplicate_key _, (Syntax _ | Non_finite_float)
  | Non_finite_float, (Syntax _ | Duplicate_key _) ->
      false

let canon =
  Alcotest.testable
    (fun ppf v -> Fmt.string ppf (Canon.to_string v))
    Canon.equal

let error = Alcotest.testable Canon.pp_error error_equal
let parsed = Alcotest.result canon error
let get = function Ok v -> v | Error _ -> Alcotest.fail "unexpected Error"

let is_syntax = function
  | Error (Canon.Syntax _) -> true
  | Error (Duplicate_key _ | Non_finite_float) | Ok _ -> false

let test_order () =
  let a = Canon.of_string "{\"b\":1,\"a\":[true,null]}" in
  let b = Canon.of_string "{ \"a\" : [true, null], \"b\" : 1 }" in
  Alcotest.check parsed "equal" a b;
  Alcotest.(check string)
    "compact" "{\"a\":[true,null],\"b\":1}"
    (Canon.to_string (get a))

let test_numbers () =
  Alcotest.check parsed "1.0 = 1" (Canon.of_string "1") (Canon.of_string "1.0");
  Alcotest.check parsed "1.0 is Int 1"
    (Ok (Canon.int 1))
    (Canon.of_string "1.0");
  Alcotest.check parsed "-0.0 is Int 0"
    (Ok (Canon.int 0))
    (Canon.of_string "-0.0");
  Alcotest.check parsed "0.5" (Canon.float 0.5) (Canon.of_string "0.5");
  Alcotest.check parsed "1e2 is Int 100"
    (Ok (Canon.int 100))
    (Canon.of_string "1e2")

let test_duplicates () =
  Alcotest.check parsed "dup" (Error (Canon.Duplicate_key "a"))
    (Canon.of_string "{\"a\":1,\"a\":2}");
  Alcotest.check parsed "nested dup" (Error (Canon.Duplicate_key "k"))
    (Canon.of_string "{\"a\":[{\"k\":1,\"k\":1}]}");
  Alcotest.check parsed "obj dup" (Error (Canon.Duplicate_key "x"))
    (Canon.obj [ ("x", Canon.null); ("y", Canon.null); ("x", Canon.null) ])

let test_non_finite () =
  List.iter
    (fun s ->
      Alcotest.check parsed s (Error Canon.Non_finite_float) (Canon.of_string s))
    [ "NaN"; "Infinity"; "-Infinity" ];
  Alcotest.check parsed "float nan" (Error Canon.Non_finite_float)
    (Canon.float nan);
  Alcotest.check parsed "float inf" (Error Canon.Non_finite_float)
    (Canon.float infinity);
  Alcotest.(check bool) "syntax" true (is_syntax (Canon.of_string "{"))

let test_big_int () =
  let s = "123456789012345678901234567890" in
  (match Canon.of_string s with
  | Ok (Canon.Big_int d) -> Alcotest.(check string) "digits" s d
  | Ok (Null | Bool _ | Int _ | Float _ | String _ | List _ | Object _)
  | Error _ ->
      Alcotest.fail "expected Big_int");
  let v = get (Canon.of_string s) in
  Alcotest.(check string) "to_string" s (Canon.to_string v);
  Alcotest.check parsed "round trip" (Ok v)
    (Canon.of_string (Canon.to_string v))

let is_lower_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

let test_digest () =
  let a = get (Canon.of_string "{\"x\":1,\"y\":[2,3]}") in
  let b = get (Canon.of_string "{\"y\":[2,3],\"x\":1}") in
  let d = Canon.digest a in
  Alcotest.(check int) "length" 64 (String.length d);
  Alcotest.(check bool) "hex" true (String.for_all is_lower_hex d);
  Alcotest.(check string) "order independent" d (Canon.digest b)

let test_to_string_floats () =
  Alcotest.(check string)
    "0.1" "0.10000000000000001"
    (Canon.to_string (get (Canon.float 0.1)))

let json s = get (Canon.of_string s)

let check_json msg expected actual =
  Alcotest.check canon msg (json expected) actual

let segment =
  Alcotest.testable
    (fun ppf -> function
      | Canon.Key k -> Fmt.pf ppf "Key %S" k | Each -> Fmt.string ppf "Each")
    ( = )

let test_readers () =
  Alcotest.(check (option string))
    "string" (Some "a")
    (Canon.to_string_opt (Canon.string "a"));
  Alcotest.(check (option string))
    "not string" None
    (Canon.to_string_opt (Canon.int 1));
  Alcotest.(check (option int)) "int" (Some 3) (Canon.to_int_opt (Canon.int 3));
  Alcotest.(check (option int))
    "not int" None
    (Canon.to_int_opt (Canon.string "3"));
  Alcotest.(check (option bool))
    "bool" (Some true)
    (Canon.to_bool_opt (Canon.bool true));
  Alcotest.(check (option bool)) "not bool" None (Canon.to_bool_opt Canon.null);
  Alcotest.(check int)
    "list" 2
    (List.length (Option.get (Canon.to_list_opt (json "[1,2]"))));
  Alcotest.(check bool)
    "not list" true
    (Option.is_none (Canon.to_list_opt (json "{}")));
  Alcotest.(check int)
    "fields" 1
    (List.length (Option.get (Canon.to_fields_opt (json "{\"a\":1}"))));
  Alcotest.(check bool)
    "not fields" true
    (Option.is_none (Canon.to_fields_opt (json "[]")));
  Alcotest.(check bool)
    "member" true
    (Option.equal Canon.equal
       (Some (Canon.int 1))
       (Canon.member "a" (json "{\"a\":1}")));
  Alcotest.(check bool)
    "member missing" true
    (Option.is_none (Canon.member "b" (json "{\"a\":1}")));
  Alcotest.(check bool)
    "member non-object" true
    (Option.is_none (Canon.member "a" (json "[1]")))

let test_edit () =
  check_json "insert middle" "{\"a\":1,\"b\":2,\"c\":3}"
    (Canon.set_member "b" (Canon.int 2) (json "{\"a\":1,\"c\":3}"));
  check_json "insert first" "{\"a\":0,\"b\":1}"
    (Canon.set_member "a" (Canon.int 0) (json "{\"b\":1}"));
  check_json "insert last" "{\"a\":1,\"z\":0}"
    (Canon.set_member "z" (Canon.int 0) (json "{\"a\":1}"));
  check_json "replace" "{\"a\":9}"
    (Canon.set_member "a" (Canon.int 9) (json "{\"a\":1}"));
  check_json "set on non-object" "[1]"
    (Canon.set_member "a" (Canon.int 9) (json "[1]"));
  check_json "remove" "{\"b\":2}"
    (Canon.remove_member "a" (json "{\"a\":1,\"b\":2}"));
  check_json "remove missing" "{\"b\":2}"
    (Canon.remove_member "a" (json "{\"b\":2}"));
  check_json "remove non-object" "3" (Canon.remove_member "a" (json "3"));
  check_json "map_fields" "{\"a\":\"a\",\"b\":\"b\"}"
    (Canon.map_fields (fun k _ -> Canon.string k) (json "{\"a\":1,\"b\":2}"));
  check_json "map_fields non-object" "[1]"
    (Canon.map_fields (fun _ _ -> Canon.null) (json "[1]"));
  check_json "map_items" "[0,0]"
    (Canon.map_items (fun _ -> Canon.int 0) (json "[1,2]"));
  check_json "map_items non-list" "{}"
    (Canon.map_items (fun _ -> Canon.null) (json "{}"))

let test_paths () =
  Alcotest.(check (list segment))
    "nested"
    [ Canon.Key "messages"; Each; Key "content"; Each; Key "signature" ]
    (Canon.path_of_string "messages[].content[].signature");
  Alcotest.(check (list segment))
    "dotted" [ Canon.Key "a"; Key "b" ]
    (Canon.path_of_string "a.b");
  Alcotest.(check (list segment))
    "single" [ Canon.Key "a" ] (Canon.path_of_string "a")

let rm p s = Canon.remove_at (Canon.path_of_string p) (json s)

let test_remove_at () =
  check_json "prune emptied parent" "{}" (rm "a.b" "{\"a\":{\"b\":1}}");
  check_json "keep non-empty parent" "{\"a\":{\"c\":2}}"
    (rm "a.b" "{\"a\":{\"b\":1,\"c\":2}}");
  check_json "keep pre-existing empty" "{\"a\":{}}" (rm "a.b" "{\"a\":{}}");
  check_json "top-level key" "{\"b\":2}" (rm "a" "{\"a\":1,\"b\":2}");
  check_json "each" "{\"xs\":[{\"j\":2},{}]}"
    (rm "xs[].k" "{\"xs\":[{\"k\":1,\"j\":2},{\"k\":3}]}");
  check_json "nested each" "{\"m\":[{\"c\":[{\"t\":1},{\"t\":2}]}]}"
    (rm "m[].c[].s" "{\"m\":[{\"c\":[{\"s\":0,\"t\":1},{\"t\":2}]}]}");
  check_json "no match" "{\"a\":1}" (rm "b.c" "{\"a\":1}");
  check_json "each on non-list" "{\"xs\":1}" (rm "xs[].k" "{\"xs\":1}");
  check_json "descend into scalar" "{\"a\":1}" (rm "a.b" "{\"a\":1}")

(* generators *)

let finite_float =
  QCheck.Gen.(
    oneof
      [
        float_range (-1e6) 1e6;
        map (fun f -> if Float.is_finite f then f else 0.5) float;
        map float_of_int (int_range (-1000) 1000);
      ])

let key = QCheck.Gen.(string_size ~gen:printable (int_range 0 4))

let leaf =
  QCheck.Gen.(
    oneof
      [
        return `Null;
        map (fun b -> `Bool b) bool;
        map (fun i -> `Int i) int;
        map (fun f -> `Float f) finite_float;
        map (fun s -> `String s) (string_size ~gen:printable (int_range 0 8));
        map (fun s -> `Intlit ("1" ^ s)) (string_size ~gen:numeral (return 25));
      ])

let rec tree depth : Yojson.Safe.t QCheck.Gen.t =
  let open QCheck.Gen in
  if depth <= 0 then leaf
  else
    oneof_weighted
      [
        (3, leaf);
        ( 1,
          map (fun l -> `List l) (list_size (int_range 0 4) (tree (depth - 1)))
        );
        ( 1,
          list_size (int_range 0 4) key >>= fun keys ->
          let keys = List.sort_uniq String.compare keys in
          shuffle_list keys >>= fun keys ->
          flatten_list
            (List.map (fun k -> map (fun v -> (k, v)) (tree (depth - 1))) keys)
          >|= fun fields -> `Assoc fields );
      ]

let rec shuffle_tree (v : Yojson.Safe.t) : Yojson.Safe.t QCheck.Gen.t =
  let open QCheck.Gen in
  match v with
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ -> return v
  | `List l -> map (fun l -> `List l) (flatten_list (List.map shuffle_tree l))
  | `Assoc fields ->
      flatten_list
        (List.map (fun (k, x) -> map (fun x -> (k, x)) (shuffle_tree x)) fields)
      >>= fun fields -> map (fun f -> `Assoc f) (shuffle_list fields)

let arb_tree = QCheck.make ~print:(fun y -> Yojson.Safe.to_string y) (tree 4)

let arb_shuffled =
  QCheck.make
    ~print:(fun (a, b) ->
      Yojson.Safe.to_string a ^ " / " ^ Yojson.Safe.to_string b)
    QCheck.Gen.(tree 4 >>= fun a -> map (fun b -> (a, b)) (shuffle_tree a))

let arb_pair =
  QCheck.make
    ~print:(fun (a, b) ->
      Yojson.Safe.to_string a ^ " / " ^ Yojson.Safe.to_string b)
    QCheck.Gen.(
      oneof
        [
          pair (tree 2) (tree 2);
          (tree 3 >>= fun a -> map (fun b -> (a, b)) (shuffle_tree a));
        ])

let prop_round_trip =
  QCheck.Test.make ~name:"of_string (to_string v) = v" ~count:500 arb_tree
    (fun y ->
      match Canon.of_yojson y with
      | Error _ -> false
      | Ok v -> (
          match Canon.of_string (Canon.to_string v) with
          | Ok v' -> Canon.equal v v'
          | Error _ -> false))

let prop_yojson_round_trip =
  QCheck.Test.make ~name:"of_yojson (to_yojson v) = v" ~count:500 arb_tree
    (fun y ->
      match Canon.of_yojson y with
      | Error _ -> false
      | Ok v -> (
          match Canon.of_yojson (Canon.to_yojson v) with
          | Ok v' -> Canon.equal v v'
          | Error _ -> false))

let prop_shuffle =
  QCheck.Test.make ~name:"to_string invariant under field shuffling" ~count:500
    arb_shuffled (fun (a, b) ->
      match (Canon.of_yojson a, Canon.of_yojson b) with
      | Ok a, Ok b -> String.equal (Canon.to_string a) (Canon.to_string b)
      | Error _, _ | _, Error _ -> false)

let prop_digest =
  QCheck.Test.make ~name:"equal iff equal digest" ~count:500 arb_pair
    (fun (a, b) ->
      match (Canon.of_yojson a, Canon.of_yojson b) with
      | Ok a, Ok b ->
          Bool.equal (Canon.equal a b)
            (String.equal (Canon.digest a) (Canon.digest b))
          && Bool.equal (Canon.equal a b) (Int.equal (Canon.compare a b) 0)
      | Error _, _ | _, Error _ -> false)

let arb_key = QCheck.make ~print:Fun.id key

let valid v =
  match Canon.of_string (Canon.to_string v) with
  | Ok v' -> Canon.equal v v'
  | Error _ -> false

let with_tree f y =
  match Canon.of_yojson y with Error _ -> false | Ok v -> f v

let prop_set_member =
  QCheck.Test.make ~name:"set_member then member" ~count:500
    (QCheck.pair arb_tree arb_key) (fun (y, k) ->
      with_tree
        (fun v ->
          let x = Canon.string "x" in
          let v' = Canon.set_member k x v in
          valid v'
          &&
          match Canon.to_fields_opt v with
          | None -> Canon.equal v v'
          | Some _ -> Option.equal Canon.equal (Canon.member k v') (Some x))
        y)

let prop_remove_member =
  QCheck.Test.make ~name:"remove_member then member is None" ~count:500
    (QCheck.pair arb_tree arb_key) (fun (y, k) ->
      with_tree
        (fun v ->
          let v' = Canon.remove_member k v in
          valid v' && Option.is_none (Canon.member k v'))
        y)

let prop_remove_existing =
  QCheck.Test.make ~name:"remove_member of existing keys" ~count:500 arb_tree
    (fun y ->
      with_tree
        (fun v ->
          match Canon.to_fields_opt v with
          | None -> true
          | Some fs ->
              List.for_all
                (fun (k, _) ->
                  let v' = Canon.remove_member k v in
                  valid v' && Option.is_none (Canon.member k v'))
                fs)
        y)

let arb_path =
  QCheck.make ~print:(String.concat ".")
    QCheck.Gen.(list_size (int_range 1 3) key)

let prop_remove_at_valid =
  QCheck.Test.make ~name:"remove_at keeps the invariant" ~count:500
    (QCheck.pair arb_tree arb_path) (fun (y, p) ->
      with_tree
        (fun v ->
          valid
            (Canon.remove_at (Canon.path_of_string (String.concat "[]." p)) v))
        y)

let prop_remove_at_noop =
  QCheck.Test.make ~name:"remove_at of an absent path is identity" ~count:500
    arb_tree (fun y ->
      with_tree
        (fun v ->
          Canon.equal v (Canon.remove_at [ Canon.Key "\000absent"; Key "x" ] v)
          && Canon.equal v
               (Canon.remove_at [ Canon.Key "\000absent"; Each; Key "x" ] v))
        y)

let () =
  Alcotest.run "canon"
    [
      ( "unit",
        [
          Alcotest.test_case "key order" `Quick test_order;
          Alcotest.test_case "numbers" `Quick test_numbers;
          Alcotest.test_case "duplicate keys" `Quick test_duplicates;
          Alcotest.test_case "non-finite and syntax" `Quick test_non_finite;
          Alcotest.test_case "big int" `Quick test_big_int;
          Alcotest.test_case "digest" `Quick test_digest;
          Alcotest.test_case "float printing" `Quick test_to_string_floats;
          Alcotest.test_case "readers" `Quick test_readers;
          Alcotest.test_case "edits" `Quick test_edit;
          Alcotest.test_case "paths" `Quick test_paths;
          Alcotest.test_case "remove_at" `Quick test_remove_at;
        ] );
      ( "properties",
        List.map
          (fun t -> QCheck_alcotest.to_alcotest t)
          [
            prop_round_trip;
            prop_yojson_round_trip;
            prop_shuffle;
            prop_digest;
            prop_set_member;
            prop_remove_member;
            prop_remove_existing;
            prop_remove_at_valid;
            prop_remove_at_noop;
          ] );
    ]
