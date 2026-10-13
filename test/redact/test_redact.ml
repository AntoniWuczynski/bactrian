open Bactrian_core

let pat s =
  match Redact.pattern s with
  | Ok p -> p
  | Error _ -> Alcotest.failf "pattern %S should compile" s

let scrub ?(patterns = Redact.provider_key_patterns) ?(known = [])
    ?(st = Redact.empty) s =
  Redact.scrub ~patterns ~known st s

let ant = "sk-ant-" ^ "api03-" ^ String.make 16 'x'
let proj = "sk-proj-" ^ String.make 24 'y'
let key_a = "sk-" ^ String.make 24 'a'

let test_two_keys () =
  let st, out = scrub (Printf.sprintf "key %s and %s" ant proj) in
  Alcotest.(check string) "out" "key <secret:1> and <secret:2>" out;
  Alcotest.(check int) "count" 2 (Redact.count st)

let test_same_key_same_placeholder () =
  let st, out = scrub (Printf.sprintf "%s %s %s" ant key_a ant) in
  Alcotest.(check string) "out" "<secret:1> <secret:2> <secret:1>" out;
  let st, out = scrub ~st (Printf.sprintf "%s then %s" ant proj) in
  Alcotest.(check string) "reuse" "<secret:1> then <secret:3>" out;
  Alcotest.(check int) "count" 3 (Redact.count st)

let test_known () =
  let st, out =
    scrub ~patterns:[] ~known:[ "hunter2" ] "pw hunter2 ok hunter2"
  in
  Alcotest.(check string) "out" "pw <secret:1> ok <secret:1>" out;
  Alcotest.(check int) "count" 1 (Redact.count st);
  let st, out = scrub ~patterns:[] ~known:[ "" ] "nothing here" in
  Alcotest.(check string) "empty ignored" "nothing here" out;
  Alcotest.(check int) "count0" 0 (Redact.count st)

let test_known_longest_first () =
  let _, out = scrub ~patterns:[] ~known:[ "abc"; "abcdef" ] "abcdef abc" in
  Alcotest.(check string) "out" "<secret:1> <secret:2>" out

let test_invalid_patterns () =
  let is_invalid s =
    match Redact.pattern s with
    | Error (Redact.Invalid_pattern _) -> true
    | Ok _ -> false
  in
  Alcotest.(check bool) "paren" true (is_invalid "(");
  Alcotest.(check bool) "empty match" true (is_invalid "a*")

let test_existing_placeholder () =
  let _, out = scrub ~patterns:[ pat {|\d+|} ] "id 42 and <secret:1>" in
  Alcotest.(check string) "out" "id <secret:1> and <secret:1>" out

let test_placeholder_fn () =
  Alcotest.(check string) "ph" "<secret:3>" (Redact.placeholder 3)

let test_ordinary_words () =
  let text =
    "the disk-encryption-configuration task-management-system AIzaShort"
  in
  let _, out = scrub text in
  Alcotest.(check string) "unchanged" text out

let test_google_and_openrouter () =
  let g = "AIza" ^ String.make 35 'z' in
  let o = "sk-or-" ^ String.make 22 'q' in
  let st, out = scrub (Printf.sprintf "%s,%s" g o) in
  Alcotest.(check string) "out" "<secret:2>,<secret:1>" out;
  Alcotest.(check int) "count" 2 (Redact.count st)

let test_provider_patterns_nonempty () =
  Alcotest.(check bool)
    "has patterns" true
    (List.length Redact.provider_key_patterns >= 3)

(* Properties *)

let gen_str alphabet len =
  QCheck.Gen.(
    string_size
      ~gen:
        (oneof_list (List.init (String.length alphabet) (String.get alphabet)))
      len)

let key_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-"

let gen_secret : string QCheck.Gen.t =
  let open QCheck.Gen in
  oneof
    [
      map (fun s -> "sk-" ^ s) (gen_str key_chars (int_range 20 40));
      map (fun s -> "sk-proj-" ^ s) (gen_str key_chars (int_range 20 40));
      map (fun s -> "sk-ant-" ^ s) (gen_str key_chars (int_range 20 40));
      map (fun s -> "sk-or-" ^ s) (gen_str key_chars (int_range 20 40));
      map (fun s -> "AIza" ^ s) (gen_str key_chars (return 35));
    ]

let gen_token =
  gen_str "abcdefghijklmnopqrstuvwxyz0123456789" (QCheck.Gen.return 20)

let gen_filler = gen_str "ab 12.,:;<>!" (QCheck.Gen.int_range 0 12)

let contains hay needle =
  match Re.exec_opt (Re.compile (Re.str needle)) hay with
  | Some _ -> true
  | None -> false

let arb_embedded =
  let open QCheck.Gen in
  let gen =
    let* keys = list_size (int_range 0 4) gen_secret in
    let* toks = list_size (int_range 0 3) gen_token in
    let* fillers = list_size (return 8) gen_filler in
    return (keys, toks, fillers)
  in
  QCheck.make ~print:(fun (k, t, f) -> String.concat "|" (k @ t @ f)) gen

let build keys toks fillers =
  let secrets = keys @ toks in
  let rec go secrets fillers acc =
    match (secrets, fillers) with
    | s :: ss, f :: fs -> go ss fs (acc ^ " " ^ f ^ " " ^ s ^ " ")
    | _, _ -> acc ^ String.concat " " fillers
  in
  go secrets fillers ""

let prop_never_appears =
  QCheck.Test.make ~name:"secrets never appear" ~count:500 arb_embedded
    (fun (keys, toks, fillers) ->
      let text = build keys toks fillers in
      let _, out = scrub ~known:toks text in
      List.for_all (fun s -> not (contains out s)) (keys @ toks))

let arb_text =
  let open QCheck.Gen in
  let piece =
    oneof
      [
        gen_str "abc xyz" (int_range 0 6);
        gen_str "0123456789" (int_range 1 4);
        oneof_list [ "<secret:"; "<secret:1>"; "<secret:2>"; ">"; " "; "<" ];
        gen_secret;
        gen_token;
      ]
  in
  QCheck.make ~print:Fun.id
    (map (String.concat "") (list_size (int_range 0 10) piece))

let prop_idempotent =
  let patterns = Redact.provider_key_patterns @ [ pat {|\d+|}; pat "[a-z]+" ] in
  QCheck.Test.make ~name:"scrub is idempotent" ~count:500 arb_text (fun text ->
      let st1, out1 = Redact.scrub ~patterns ~known:[] Redact.empty text in
      let st2, out2 = Redact.scrub ~patterns ~known:[] st1 out1 in
      String.equal out1 out2 && Int.equal (Redact.count st1) (Redact.count st2))

let prop_idempotent_known =
  let patterns = [ pat {|\d+|} ] in
  QCheck.Test.make ~name:"scrub with known is idempotent" ~count:500
    QCheck.(pair arb_text (make gen_token))
    (fun (text, tok) ->
      let known = [ tok; "ab" ] in
      let st1, out1 = Redact.scrub ~patterns ~known Redact.empty (text ^ tok) in
      let st2, out2 = Redact.scrub ~patterns ~known st1 out1 in
      String.equal out1 out2 && Int.equal (Redact.count st1) (Redact.count st2))

let test_provider_patterns_compile () =
  Alcotest.(check int)
    "all four provider patterns compile" 4
    (List.length Bactrian_core.Redact.provider_key_patterns)

let () =
  Alcotest.run "redact"
    [
      ( "unit",
        [
          Alcotest.test_case "provider patterns compile" `Quick
            test_provider_patterns_compile;
          Alcotest.test_case "two keys" `Quick test_two_keys;
          Alcotest.test_case "same key" `Quick test_same_key_same_placeholder;
          Alcotest.test_case "known" `Quick test_known;
          Alcotest.test_case "known longest first" `Quick
            test_known_longest_first;
          Alcotest.test_case "invalid patterns" `Quick test_invalid_patterns;
          Alcotest.test_case "existing placeholder" `Quick
            test_existing_placeholder;
          Alcotest.test_case "placeholder" `Quick test_placeholder_fn;
          Alcotest.test_case "ordinary words" `Quick test_ordinary_words;
          Alcotest.test_case "google and openrouter" `Quick
            test_google_and_openrouter;
          Alcotest.test_case "provider patterns" `Quick
            test_provider_patterns_nonempty;
        ] );
      ( "properties",
        List.map
          (fun t -> QCheck_alcotest.to_alcotest t)
          [ prop_never_appears; prop_idempotent; prop_idempotent_known ] );
    ]
