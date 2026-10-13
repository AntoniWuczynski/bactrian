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
        ] );
      ( "properties",
        List.map
          (fun t -> QCheck_alcotest.to_alcotest t)
          [ prop_round_trip; prop_yojson_round_trip; prop_shuffle; prop_digest ]
      );
    ]
