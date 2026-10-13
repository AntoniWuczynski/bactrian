open Bactrian_core

let item_testable =
  Alcotest.testable
    (fun fmt -> function
      | Sse.Event e ->
          Format.fprintf fmt "Event{event=%s;data=%S;id=%s;retry=%s}"
            (Option.fold ~none:"-" ~some:(Printf.sprintf "%S") e.event)
            e.data
            (Option.fold ~none:"-" ~some:(Printf.sprintf "%S") e.id)
            (Option.fold ~none:"-" ~some:string_of_int e.retry)
      | Sse.Comment c -> Format.fprintf fmt "Comment %S" c)
    Sse.equal_item

let ev ?event ?id ?retry data = Sse.Event { event; data; id; retry }
let parse s = snd (Sse.feed Sse.init s)

let check_items name expected s =
  Alcotest.(check (list item_testable)) name expected (parse s)

let finish_equal a b =
  match (a, b) with
  | `Clean, `Clean -> true
  | `Truncated x, `Truncated y -> Int.equal x y
  | `Clean, `Truncated _ | `Truncated _, `Clean -> false

let finish_testable =
  Alcotest.testable
    (fun fmt -> function
      | `Clean -> Format.pp_print_string fmt "Clean"
      | `Truncated n -> Format.fprintf fmt "Truncated %d" n)
    finish_equal

let finish_of s = Sse.finish (fst (Sse.feed Sse.init s))

let unit_tests =
  let open Alcotest in
  [
    test_case "single event" `Quick (fun () ->
        check_items "hello" [ ev "hello" ] "data: hello\n\n");
    test_case "multi data lines" `Quick (fun () ->
        check_items "join" [ ev "a\nb" ] "data: a\ndata: b\n\n");
    test_case "data without colon" `Quick (fun () ->
        check_items "empty" [ ev "" ] "data\n\n");
    test_case "data without space" `Quick (fun () ->
        check_items "x" [ ev "x" ] "data:x\n\n");
    test_case "only one space stripped" `Quick (fun () ->
        check_items "sp" [ ev " x" ] "data:  x\n\n");
    test_case "no data, no item" `Quick (fun () ->
        check_items "none" [] "event: ping\n\n");
    test_case "event field" `Quick (fun () ->
        check_items "ev"
          [ ev ~event:"ping" ~id:"7" ~retry:10 "x" ]
          "event: ping\nid: 7\nretry: 10\ndata: x\n\n");
    test_case "comment" `Quick (fun () ->
        check_items "c" [ Sse.Comment "keep-alive" ] ": keep-alive\n");
    test_case "CRLF" `Quick (fun () ->
        check_items "crlf" [ ev "a"; ev "b" ] "data: a\r\n\r\ndata: b\r\n\r\n");
    test_case "lone CR" `Quick (fun () ->
        check_items "cr" [ ev "a" ] "data: a\r\r");
    test_case "invalid retry" `Quick (fun () ->
        check_items "retry" [] "retry: 1x\n data: a\n\n");
    test_case "retry invalid ignored but data kept" `Quick (fun () ->
        check_items "retry" [ ev "a" ] "retry: 1x\ndata: a\n\n");
    test_case "id with NUL ignored" `Quick (fun () ->
        check_items "nul" [ ev "a" ] "id: a\000b\ndata: a\n\n");
    test_case "id is per block" `Quick (fun () ->
        check_items "id"
          [ ev ~id:"1" "a"; ev "b" ]
          "id: 1\ndata: a\n\ndata: b\n\n");
    test_case "BOM skipped at start" `Quick (fun () ->
        check_items "bom" [ ev "a" ] "\xEF\xBB\xBFdata: a\n\n");
    test_case "BOM skipped across chunks" `Quick (fun () ->
        let st, i1 = Sse.feed Sse.init "\xEF\xBB" in
        let st, i2 = Sse.feed st "\xBFdata: a\n\n" in
        Alcotest.(check (list item_testable)) "bom" [ ev "a" ] (i1 @ i2);
        Alcotest.(check finish_testable) "fin" `Clean (Sse.finish st));
    test_case "BOM not skipped later" `Quick (fun () ->
        check_items "bom" [ ev "\xEF\xBB\xBFx" ] "data: \xEF\xBB\xBFx\n\n";
        check_items "bom2" [] "\n\xEF\xBB\xBFdata: a\n\n");
    test_case "OpenAI shape" `Quick (fun () ->
        check_items "oa"
          [ ev "{\"a\":1}"; ev "[DONE]" ]
          "data: {\"a\":1}\n\ndata: [DONE]\n\n");
    test_case "finish truncated" `Quick (fun () ->
        Alcotest.(check finish_testable)
          "t" (`Truncated 8) (finish_of "data: x\n"));
    test_case "finish clean" `Quick (fun () ->
        Alcotest.(check finish_testable) "c" `Clean (finish_of "data: x\n\n"));
    test_case "finish partial line" `Quick (fun () ->
        Alcotest.(check finish_testable) "p" (`Truncated 6) (finish_of "data: "));
    test_case "serialise" `Quick (fun () ->
        Alcotest.(check string)
          "s" "event: e\ndata: a\ndata: b\nid: 1\nretry: 5\n\n"
          (Sse.serialise (ev ~event:"e" ~id:"1" ~retry:5 "a\nb"));
        Alcotest.(check string) "c" ": hi\n" (Sse.serialise (Sse.Comment "hi")));
    test_case "serialise rejects" `Quick (fun () ->
        Alcotest.check_raises "cr"
          (Invalid_argument "Sse.serialise: data contains a carriage return")
          (fun () -> ignore (Sse.serialise (ev "a\rb"))));
  ]

(* Generators *)
open QCheck

let token_gen =
  Gen.oneof
    [
      Gen.return "data:";
      Gen.return "event:";
      Gen.return "id:";
      Gen.return "retry:";
      Gen.return ":";
      Gen.return "\r";
      Gen.return "\n";
      Gen.return "\r\n";
      Gen.return " ";
      Gen.return "\xEF\xBB\xBF";
      Gen.map (String.make 1) Gen.printable;
      Gen.string_size ~gen:(Gen.char_range 'a' 'z') (Gen.int_range 1 3);
    ]

let input_gen =
  Gen.map (String.concat "") (Gen.list_size (Gen.int_range 0 25) token_gen)

let split_at cuts s =
  let n = String.length s in
  let cuts =
    List.sort_uniq Int.compare (List.map (fun c -> c mod (n + 1)) cuts)
  in
  let rec go prev = function
    | [] -> [ String.sub s prev (n - prev) ]
    | c :: rest -> String.sub s prev (c - prev) :: go c rest
  in
  go 0 cuts

let run_pieces pieces =
  let st, items =
    List.fold_left
      (fun (st, acc) p ->
        let st, is = Sse.feed st p in
        (st, acc @ is))
      (Sse.init, []) pieces
  in
  (items, Sse.finish st)

let split_gen =
  Gen.pair input_gen (Gen.list_size (Gen.int_range 0 8) (Gen.int_range 0 1000))

let print_split (s, cuts) =
  Printf.sprintf "%S cuts=[%s]" s
    (String.concat ";" (List.map string_of_int cuts))

let split_invariance =
  Test.make ~name:"split invariance" ~count:1000
    (make ~print:print_split split_gen) (fun (s, cuts) ->
      let whole_items, whole_fin = run_pieces [ s ] in
      let items, fin = run_pieces (split_at cuts s) in
      List.equal Sse.equal_item whole_items items && finish_equal fin whole_fin)

(* Every byte-position split of inputs rich in CRLF, so the CR|LF cut is always hit. *)
let every_cut =
  Test.make ~name:"split invariance, every single cut" ~count:300
    (make ~print:Print.string input_gen) (fun s ->
      let whole_items, whole_fin = run_pieces [ s ] in
      let ok = ref true in
      for i = 0 to String.length s do
        let items, fin =
          run_pieces [ String.sub s 0 i; String.sub s i (String.length s - i) ]
        in
        if
          not
            (List.equal Sse.equal_item whole_items items
            && finish_equal fin whole_fin)
        then ok := false
      done;
      !ok)

let cr_lf_cut_case =
  Alcotest.test_case "CR|LF cut" `Quick (fun () ->
      let items, fin =
        run_pieces [ "data: a\r"; "\n\r"; "\ndata: b\r\n\r"; "\n" ]
      in
      Alcotest.(check (list item_testable)) "items" [ ev "a"; ev "b" ] items;
      Alcotest.(check finish_testable) "fin" `Clean fin)

let line_gen =
  Gen.string_size
    ~gen:
      (Gen.oneof
         [
           Gen.char_range 'a' 'z';
           Gen.return ' ';
           Gen.return ':';
           Gen.return '\t';
         ])
    (Gen.int_range 0 8)

let item_gen =
  let open Gen in
  oneof
    [
      map (fun c -> Sse.Comment c) line_gen;
      (let* event = option line_gen in
       let* id = option line_gen in
       let* retry = option (int_range 0 100000) in
       let* lines = list_size (int_range 1 3) line_gen in
       return (Sse.Event { event; data = String.concat "\n" lines; id; retry }));
    ]

let round_trip =
  Test.make ~name:"serialise round trip" ~count:500
    (make
       ~print:(fun l -> String.concat "|" (List.map Sse.serialise l))
       (Gen.list_size (Gen.int_range 0 6) item_gen))
    (fun items ->
      let _, out =
        Sse.feed Sse.init (String.concat "" (List.map Sse.serialise items))
      in
      List.equal Sse.equal_item items out)

let () =
  Alcotest.run "sse"
    [
      ("unit", unit_tests @ [ cr_lf_cut_case ]);
      ( "properties",
        List.map
          (fun t -> QCheck_alcotest.to_alcotest t)
          [ split_invariance; every_cut; round_trip ] );
    ]
