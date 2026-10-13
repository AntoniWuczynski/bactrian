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
let key_a = "sk-" ^ String.make 32 'a'

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

(* Spec decision: placeholders are numbered in order of first replacement
   (literals, then each pattern in order), not by position in the text. Here
   the OpenRouter pattern runs before the Google one, so the key that appears
   later in the text gets number 1. *)
let test_numbering_by_replacement_order () =
  let g = "AIza" ^ String.make 35 'z' in
  let o = "sk-or-" ^ String.make 22 'q' in
  let st, out = scrub (Printf.sprintf "%s,%s" g o) in
  Alcotest.(check string) "out" "<secret:2>,<secret:1>" out;
  Alcotest.(check int) "count" 2 (Redact.count st)

let test_provider_patterns_nonempty () =
  Alcotest.(check bool)
    "has patterns" true
    (List.length Redact.provider_key_patterns >= 3)

let scrub_provider text = snd (scrub text)

let test_keys_after_escapes () =
  let proj = "sk-proj-" ^ String.make 24 'A' in
  let check name text expected =
    Alcotest.(check string) name expected (scrub_provider text)
  in
  check "json newline"
    (Printf.sprintf {|{"msg":"hi\n%s"}|} proj)
    {|{"msg":"hi\n<secret:1>"}|};
  check "url encoded equals"
    (Printf.sprintf "key%%3D%s" key_a)
    "key%3D<secret:1>";
  check "bare after json escape"
    (Printf.sprintf {|a\"%s|} key_a)
    {|a\"<secret:1>|};
  check "bare after equals" (Printf.sprintf "k=%s" key_a) "k=<secret:1>";
  check "bare at start" key_a "<secret:1>";
  let g = "AIza" ^ String.make 35 'B' in
  check "google glued to a letter" ("x" ^ g) "x<secret:1>";
  check "proj glued to a word" ("word" ^ proj) "word<secret:1>";
  let svc = "sk-svcacct-" ^ String.make 24 'c' in
  check "svcacct" ("t" ^ svc) "t<secret:1>"

let test_bare_sk_boundary () =
  let words = "disk-encryption-key-rotation-policy-v2-final-draft-notes" in
  Alcotest.(check string) "word" words (scrub_provider words);
  let glued = "x" ^ key_a in
  Alcotest.(check string) "glued to a letter" glued (scrub_provider glued);
  let short = "sk-" ^ String.make 31 'a' in
  Alcotest.(check string) "31 chars" short (scrub_provider short)

let test_fixpoint_context () =
  let text = "9" ^ key_a in
  let patterns = Redact.provider_key_patterns @ [ pat {|\d+|} ] in
  let st1, out1 = Redact.scrub ~patterns ~known:[] Redact.empty text in
  Alcotest.(check string) "out" "<secret:1><secret:2>" out1;
  let st2, out2 = Redact.scrub ~patterns ~known:[] st1 out1 in
  Alcotest.(check string) "again" out1 out2;
  Alcotest.(check int) "count" (Redact.count st1) (Redact.count st2)

let test_performance () =
  let n = 20_000 in
  let text = String.concat "," (List.init n string_of_int) in
  let t0 = Sys.time () in
  let st, out = scrub ~patterns:[ pat {|\d+|} ] text in
  let dt = Sys.time () -. t0 in
  Alcotest.(check int) "count" n (Redact.count st);
  Alcotest.(check bool) "output" true (String.length out > String.length text);
  if dt > 2.0 then Alcotest.failf "scrub took %.2f s" dt

(* Properties *)

let gen_str alphabet len =
  QCheck.Gen.(
    string_size
      ~gen:
        (oneof_list (List.init (String.length alphabet) (String.get alphabet)))
      len)

let key_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-"

(* A secret and whether it is a bare [sk-] key, which needs a left boundary. *)
let gen_secret_kind : (string * bool) QCheck.Gen.t =
  let open QCheck.Gen in
  let prefixed p n = map (fun s -> (p ^ s, false)) (gen_str key_chars n) in
  oneof
    [
      map (fun s -> ("sk-" ^ s, true)) (gen_str key_chars (int_range 32 40));
      prefixed "sk-proj-" (int_range 20 40);
      prefixed "sk-ant-" (int_range 20 40);
      prefixed "sk-or-" (int_range 20 40);
      prefixed "sk-svcacct-" (int_range 20 40);
      prefixed "AIza" (return 35);
    ]

let gen_secret = QCheck.Gen.map fst gen_secret_kind

let gen_token =
  gen_str "abcdefghijklmnopqrstuvwxyz0123456789" (QCheck.Gen.return 20)

let gen_filler = gen_str "ab 12.,:;<>!" (QCheck.Gen.int_range 0 12)
let escapes = [ {|\n|}; {|\t|}; {|\r|}; {|\"|}; {|\/|}; "%3D"; "%2F" ]
let boundary_prefixes = [ ""; " "; "="; ":"; "\""; "," ] @ escapes
let word_prefixes = [ "a"; "Z"; "7"; "_"; "word"; "123" ]
let suffixes = [ ""; "x"; "7"; "-"; "_" ] @ escapes

(* Glue a secret directly to neighbours on both sides. A bare [sk-] key only
   gets prefixes that are boundaries. *)
let gen_glued (secret, bare) =
  let open QCheck.Gen in
  let* pre =
    oneof_list
      (if bare then boundary_prefixes else boundary_prefixes @ word_prefixes)
  in
  let* suf = oneof_list suffixes in
  return (pre ^ secret ^ suf, secret)

let contains hay needle =
  match Re.exec_opt (Re.compile (Re.str needle)) hay with
  | Some _ -> true
  | None -> false

let arb_embedded =
  let open QCheck.Gen in
  let gen =
    let* keys = list_size (int_range 0 4) (gen_secret_kind >>= gen_glued) in
    let* toks =
      list_size (int_range 0 3)
        ( gen_token >>= fun t ->
          let* pre = oneof_list (boundary_prefixes @ word_prefixes) in
          let* suf = oneof_list suffixes in
          return (pre ^ t ^ suf, t) )
    in
    let* fillers = list_size (return 8) gen_filler in
    return (keys, toks, fillers)
  in
  QCheck.make
    ~print:(fun (k, t, f) -> String.concat "|" (List.map fst (k @ t) @ f))
    gen

let build glued fillers =
  let rec go glued fillers acc =
    match (glued, fillers) with
    | g :: gs, f :: fs -> go gs fs (acc ^ " " ^ f ^ " " ^ g)
    | _, _ -> acc ^ String.concat " " fillers
  in
  go glued fillers ""

let prop_never_appears =
  QCheck.Test.make ~name:"secrets never appear" ~count:500 arb_embedded
    (fun (keys, toks, fillers) ->
      let text = build (List.map fst (keys @ toks)) fillers in
      let _, out = scrub ~known:(List.map snd toks) text in
      List.for_all (fun (_, s) -> not (contains out s)) (keys @ toks))

let arb_text =
  let open QCheck.Gen in
  let piece =
    oneof
      [
        gen_str "abc xyz" (int_range 0 6);
        gen_str "0123456789" (int_range 1 4);
        oneof_list
          [ "<secret:"; "<secret:1>"; "<secret:2>"; ">"; " "; "<"; "x"; "%3D" ];
        oneof_list escapes;
        gen_secret;
        gen_token;
      ]
  in
  QCheck.make ~print:Fun.id
    (map (String.concat "") (list_size (int_range 0 10) piece))

let prop_idempotent_with name patterns =
  QCheck.Test.make ~name ~count:500 arb_text (fun text ->
      let st1, out1 = Redact.scrub ~patterns ~known:[] Redact.empty text in
      let st2, out2 = Redact.scrub ~patterns ~known:[] st1 out1 in
      String.equal out1 out2 && Int.equal (Redact.count st1) (Redact.count st2))

let prop_idempotent =
  prop_idempotent_with "scrub is idempotent"
    (Redact.provider_key_patterns @ [ pat {|\d+|}; pat "[a-z]+" ])

let prop_idempotent_digits =
  prop_idempotent_with "scrub is idempotent with digits but no letters"
    (Redact.provider_key_patterns @ [ pat {|\d+|} ])

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
    "all six provider patterns compile" 6
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
          Alcotest.test_case "numbered by replacement order, not position"
            `Quick test_numbering_by_replacement_order;
          Alcotest.test_case "keys after escapes and in words" `Quick
            test_keys_after_escapes;
          Alcotest.test_case "bare sk- needs a boundary" `Quick
            test_bare_sk_boundary;
          Alcotest.test_case "fixpoint closes the context gap" `Quick
            test_fixpoint_context;
          Alcotest.test_case "20k distinct secrets are linear" `Quick
            test_performance;
          Alcotest.test_case "provider patterns" `Quick
            test_provider_patterns_nonempty;
        ] );
      ( "properties",
        List.map
          (fun t -> QCheck_alcotest.to_alcotest t)
          [
            prop_never_appears;
            prop_idempotent;
            prop_idempotent_digits;
            prop_idempotent_known;
          ] );
    ]
