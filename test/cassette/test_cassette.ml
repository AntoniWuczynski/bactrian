open Bactrian_core

let get_canon = function
  | Ok v -> v
  | Error _ -> Alcotest.fail "unexpected Canon error"

let doc s = get_canon (Canon.of_string s)

let header =
  {
    Cassette.run_id = "run-1";
    scenario = "refund";
    variant = "baseline";
    bactrian_version = "0.1.0";
    recorded_at = "2026-10-13T10:00:00Z";
    sim_now = "2026-01-02T03:04:05Z";
    clock = Trace.Enforced;
  }

let exchange ?(seq = 0) ?(channel = Cassette.Model)
    ?(request = Cassette.Inline "{}")
    ?(outcome =
      Cassette.Response
        {
          status = 200;
          headers = [ ("content-type", "application/json") ];
          body = Whole (Inline "ok");
        }) () =
  {
    Cassette.seq;
    t_ms = 10 * seq;
    channel;
    protocol = "openai_chat";
    meth = "POST";
    path = "/v1/chat/completions";
    query = [ ("a", "1"); ("a", "2") ];
    key = "k" ^ string_of_int seq;
    occurrence = 0;
    canonical =
      doc {|{"model":"m","messages":[{"role":"user","content":"hi"}]}|};
    request;
    outcome;
  }

let error_equal a b =
  match (a, b) with
  | Cassette.Invalid_json x, Cassette.Invalid_json y -> Int.equal x.line y.line
  | Unsupported_version x, Unsupported_version y -> Int.equal x y
  | Bad_line x, Bad_line y -> Int.equal x.line y.line
  | Missing_header, Missing_header -> true
  | Out_of_order x, Out_of_order y -> Int.equal x.line y.line
  | ( Invalid_json _,
      (Unsupported_version _ | Bad_line _ | Missing_header | Out_of_order _) )
  | ( Unsupported_version _,
      (Invalid_json _ | Bad_line _ | Missing_header | Out_of_order _) )
  | ( Bad_line _,
      (Invalid_json _ | Unsupported_version _ | Missing_header | Out_of_order _)
    )
  | ( Missing_header,
      (Invalid_json _ | Unsupported_version _ | Bad_line _ | Out_of_order _) )
  | ( Out_of_order _,
      (Invalid_json _ | Unsupported_version _ | Bad_line _ | Missing_header) )
    ->
      false

let error_t = Alcotest.testable Cassette.pp_error error_equal

let exchange_t =
  Alcotest.testable
    (fun ppf e -> Fmt.string ppf (Cassette.exchange_line e))
    Cassette.equal_exchange

let expect_error name expected lines =
  match Cassette.of_lines lines with
  | Error e -> Alcotest.check error_t name expected e
  | Ok _ -> Alcotest.fail (name ^ ": expected an error")

let decode lines =
  match Cassette.of_lines lines with
  | Ok t -> t
  | Error e ->
      Alcotest.fail (Format.asprintf "decode failed: %a" Cassette.pp_error e)

let hl = Cassette.header_line header

(* Rewrites a line's JSON object with [f] applied to its fields. *)
let edit f line =
  match Yojson.Safe.from_string line with
  | `Assoc fields -> Yojson.Safe.to_string (`Assoc (f fields))
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
      Alcotest.fail "not an object"

let add k v = edit (fun fs -> (k, v) :: fs)
let drop k = edit (List.filter (fun (k', _) -> not (String.equal k k')))

let set k v =
  edit
    (List.map (fun (k', v') -> if String.equal k k' then (k', v) else (k', v')))

let round_trip name e =
  Alcotest.test_case name `Quick (fun () ->
      let t = decode [ hl; Cassette.exchange_line e; Cassette.end_line ] in
      Alcotest.check (Alcotest.list exchange_t) name [ e ] t.exchanges)

let whole p = Cassette.Response { status = 200; headers = []; body = Whole p }

let round_trips =
  [
    round_trip "model channel" (exchange ~channel:Model ());
    round_trip "mcp channel" (exchange ~channel:Mcp ());
    round_trip "http channel" (exchange ~channel:Http ());
    round_trip "inline request" (exchange ~request:(Inline "a\nb\"c") ());
    round_trip "blob request" (exchange ~request:(Blob (String.make 64 'a')) ());
    round_trip "whole inline response"
      (exchange ~outcome:(whole (Inline "x\ny")) ());
    round_trip "whole blob response" (exchange ~outcome:(whole (Blob "ff")) ());
    round_trip "stream response"
      (exchange
         ~outcome:
           (Response
              {
                status = 200;
                headers = [ ("content-type", "text/event-stream") ];
                body = Stream [ (0, "data: a\n\n"); (15, "data: \xff\n\n") ];
              })
         ());
    round_trip "empty stream"
      (exchange
         ~outcome:(Response { status = 204; headers = []; body = Stream [] })
         ());
    round_trip "transport error"
      (exchange ~outcome:(Transport_error "refused") ());
  ]

let test_header () =
  let t = decode [ hl ] in
  Alcotest.(check bool) "incomplete" false t.complete;
  Alcotest.(check string) "run id" "run-1" t.header.run_id;
  Alcotest.(check string)
    "recorded_at" "2026-10-13T10:00:00Z" t.header.recorded_at;
  let c =
    decode [ Cassette.header_line { header with clock = Contract_only } ]
  in
  Alcotest.(check bool)
    "clock" true
    (match c.header.clock with
    | Trace.Contract_only -> true
    | Trace.Enforced -> false)

let test_complete () =
  let e0 = exchange ~seq:0 () and e1 = exchange ~seq:1 () in
  let lines = [ hl; Cassette.exchange_line e0; Cassette.exchange_line e1 ] in
  let t = decode (lines @ [ Cassette.end_line ]) in
  Alcotest.(check bool) "complete" true t.complete;
  Alcotest.(check int) "exchanges" 2 (List.length t.exchanges)

let test_partial () =
  let e0 = exchange ~seq:0 () in
  let t = decode [ hl; Cassette.exchange_line e0 ] in
  Alcotest.(check bool) "partial" false t.complete;
  Alcotest.check (Alcotest.list exchange_t) "kept" [ e0 ] t.exchanges

let test_trailing_empty () =
  let t = decode [ hl; Cassette.end_line; ""; "" ] in
  Alcotest.(check bool) "complete" true t.complete;
  let t = decode [ hl; "" ] in
  Alcotest.(check bool) "partial" false t.complete

let test_missing_header () =
  expect_error "empty" Missing_header [];
  expect_error "only blanks" Missing_header [ ""; "" ];
  expect_error "exchange first" Missing_header
    [ Cassette.exchange_line (exchange ()) ];
  expect_error "end first" Missing_header [ Cassette.end_line ]

let test_out_of_order () =
  let l n = Cassette.exchange_line (exchange ~seq:n ()) in
  expect_error "equal" (Out_of_order { line = 3 }) [ hl; l 1; l 1 ];
  expect_error "decreasing" (Out_of_order { line = 4 }) [ hl; l 1; l 2; l 0 ]

let test_version () =
  expect_error "header" (Unsupported_version 2) [ set "v" (`Int 2) hl ];
  expect_error "exchange" (Unsupported_version 9)
    [ hl; set "v" (`Int 9) (Cassette.exchange_line (exchange ())) ];
  expect_error "end" (Unsupported_version 2)
    [ hl; set "v" (`Int 2) Cassette.end_line ]

let test_unknown_fields () =
  let el = Cassette.exchange_line (exchange ()) in
  expect_error "exchange"
    (Bad_line { line = 2; reason = "" })
    [ hl; add "extra" (`Int 1) el ];
  expect_error "header"
    (Bad_line { line = 1; reason = "" })
    [ add "extra" (`Int 1) hl ];
  expect_error "end"
    (Bad_line { line = 2; reason = "" })
    [ hl; add "extra" (`Int 1) Cassette.end_line ];
  let nested =
    set "body"
      (`Assoc
         [ ("whole", `Assoc [ ("inline", `String "x"); ("extra", `Null) ]) ])
      (Cassette.exchange_line (exchange ()))
  in
  expect_error "nested body" (Bad_line { line = 2; reason = "" }) [ hl; nested ]

let test_bad_lines () =
  let el = Cassette.exchange_line (exchange ()) in
  expect_error "unknown type"
    (Bad_line { line = 2; reason = "" })
    [ hl; set "type" (`String "mystery") el ];
  expect_error "missing field"
    (Bad_line { line = 2; reason = "" })
    [ hl; drop "key" el ];
  expect_error "wrong field type"
    (Bad_line { line = 2; reason = "" })
    [ hl; set "seq" (`String "0") el ];
  expect_error "bad channel"
    (Bad_line { line = 2; reason = "" })
    [ hl; set "channel" (`String "carrier-pigeon") el ];
  expect_error "bad payload"
    (Bad_line { line = 2; reason = "" })
    [
      hl;
      set "request"
        (`Assoc [ ("inline", `String "x"); ("blob", `String "y") ])
        el;
    ];
  expect_error "missing v"
    (Bad_line { line = 2; reason = "" })
    [ hl; drop "v" el ];
  expect_error "not an object"
    (Bad_line { line = 2; reason = "" })
    [ hl; "[1]" ];
  expect_error "second header" (Bad_line { line = 2; reason = "" }) [ hl; hl ];
  expect_error "line after end"
    (Bad_line { line = 3; reason = "" })
    [ hl; Cassette.end_line; el ];
  expect_error "response without status"
    (Bad_line { line = 2; reason = "" })
    [ hl; drop "status" el ];
  expect_error "error with status"
    (Bad_line { line = 2; reason = "" })
    [ hl; add "error" (`String "refused") el ]

let test_invalid_json () =
  expect_error "header" (Invalid_json { line = 1; reason = "" }) [ "{nope" ];
  expect_error "followed by another line"
    (Invalid_json { line = 2; reason = "" })
    [ hl; "{\"v\":"; Cassette.exchange_line (exchange ~seq:1 ()) ];
  expect_error "blank in the middle"
    (Invalid_json { line = 2; reason = "" })
    [ hl; ""; Cassette.end_line ]

let test_truncated_last_line () =
  let e0 = exchange ~seq:0 () and e1 = exchange ~seq:1 () in
  let l1 = Cassette.exchange_line e1 in
  let cut = String.sub l1 0 (String.length l1 / 2) in
  let t = decode [ hl; Cassette.exchange_line e0; cut ] in
  Alcotest.(check bool) "incomplete" false t.complete;
  Alcotest.check (Alcotest.list exchange_t) "kept" [ e0 ] t.exchanges;
  let t = decode [ hl; Cassette.exchange_line e0; cut; ""; "" ] in
  Alcotest.(check int) "blank lines after it" 1 (List.length t.exchanges);
  let t = decode [ hl; "{\"v\":1,\"ty" ] in
  Alcotest.(check int) "nothing else" 0 (List.length t.exchanges);
  Alcotest.(check bool) "still incomplete" false t.complete;
  (* only a line that is not JSON at all is forgiven, and only the last one *)
  expect_error "earlier line"
    (Invalid_json { line = 2; reason = "" })
    [ hl; cut; l1 ];
  expect_error "before the end line"
    (Invalid_json { line = 2; reason = "" })
    [ hl; cut; Cassette.end_line ];
  expect_error "the header"
    (Invalid_json { line = 1; reason = "" })
    [ "{\"v\":1,\"ty" ];
  expect_error "valid JSON, bad line"
    (Bad_line { line = 2; reason = "" })
    [ hl; drop "key" l1 ]

let b64_cases =
  [
    ("\xff", "/w==");
    ("\xff\xfe", "//4=");
    ("\xff\xfe\xfd", "//79");
    ("\xe2\x82", "4oI=");
  ]

let contains line sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length line
    && (String.equal (String.sub line i n) sub || go (i + 1))
  in
  go 0

let test_binary_encoding () =
  List.iter
    (fun (raw, b64) ->
      let line = Cassette.exchange_line (exchange ~request:(Inline raw) ()) in
      Alcotest.(check bool)
        ("payload " ^ b64) true
        (contains line (Printf.sprintf {|"inline_b64":"%s"|} b64));
      Alcotest.(check bool) "valid UTF-8" true (String.is_valid_utf_8 line);
      let line =
        Cassette.exchange_line
          (exchange
             ~outcome:
               (Response
                  { status = 200; headers = []; body = Stream [ (3, raw) ] })
             ())
      in
      Alcotest.(check bool)
        ("chunk " ^ b64) true
        (contains line (Printf.sprintf {|[3,{"b64":"%s"}]|} b64)))
    b64_cases;
  let line =
    Cassette.exchange_line (exchange ~request:(Inline "h\xc3\xa9") ())
  in
  Alcotest.(check bool)
    "UTF-8 stays inline" true
    (contains line {|"inline":"h|});
  let line =
    Cassette.exchange_line
      (exchange
         ~outcome:
           (Response { status = 200; headers = []; body = Stream [ (3, "ok") ] })
         ())
  in
  Alcotest.(check bool) "text chunk" true (contains line {|[3,"ok"]|});
  Alcotest.(check bool)
    "empty payload stays inline" true
    (contains
       (Cassette.exchange_line (exchange ~request:(Inline "") ()))
       {|"inline":""|})

let test_split_multibyte () =
  let e =
    exchange
      ~outcome:
        (Response
           {
             status = 200;
             headers = [];
             body = Stream [ (0, "data: \xe2\x82"); (1, "\xacx\n\n") ];
           })
      ()
  in
  let line = Cassette.exchange_line e in
  Alcotest.(check bool) "valid UTF-8 line" true (String.is_valid_utf_8 line);
  let t = decode [ hl; line; Cassette.end_line ] in
  Alcotest.check (Alcotest.list exchange_t) "round trip" [ e ] t.exchanges

let test_bad_base64 () =
  let el = Cassette.exchange_line (exchange ()) in
  let with_request v =
    set "request" (`Assoc [ ("inline_b64", `String v) ]) el
  in
  List.iter
    (fun v ->
      expect_error ("rejects " ^ v)
        (Bad_line { line = 2; reason = "" })
        [ hl; with_request v ])
    [ "!!!!"; "abc"; "ab=c"; "a==="; "=abc" ];
  let t = decode [ hl; with_request "//79" ] in
  Alcotest.check (Alcotest.list exchange_t) "accepts good input"
    [ exchange ~request:(Inline "\xff\xfe\xfd") () ]
    t.exchanges

let test_equal_chunk_timing () =
  let stream ms =
    exchange
      ~outcome:
        (Response { status = 200; headers = []; body = Stream [ (ms, "a") ] })
      ()
  in
  Alcotest.(check bool)
    "same" true
    (Cassette.equal_exchange (stream 1) (stream 1));
  Alcotest.(check bool)
    "different timing" false
    (Cassette.equal_exchange (stream 1) (stream 2))

let test_encoding () =
  let line = Cassette.exchange_line (exchange ~request:(Inline "a\nb") ()) in
  Alcotest.(check bool) "single line" false (String.contains line '\n');
  Alcotest.(check string)
    "canonical key order"
    (Canon.to_string (doc line))
    line;
  Alcotest.(check string) "header canonical" (Canon.to_string (doc hl)) hl;
  Alcotest.(check string) "end line" {|{"type":"end","v":1}|} Cassette.end_line;
  Alcotest.(check int) "version" 1 Cassette.schema_version;
  Alcotest.(check bool)
    "differs on seq" false
    (Cassette.equal_exchange (exchange ~seq:0 ()) (exchange ~seq:1 ()));
  Alcotest.(check bool)
    "differs on payload kind" false
    (Cassette.equal_exchange
       (exchange ~request:(Inline "a") ())
       (exchange ~request:(Blob "a") ()))

(* Generators *)
let gen_small_string = QCheck.Gen.(string_size ~gen:char (int_range 0 8))
let gen_nat = QCheck.Gen.int_range 0 100000

let gen_canon : Canon.t QCheck.Gen.t =
  let open QCheck.Gen in
  let leaf =
    oneof
      [
        return Canon.null;
        map Canon.bool bool;
        map Canon.int (int_range (-1000) 1000);
        map Canon.string gen_small_string;
      ]
  in
  let obj_of kvs =
    match Canon.obj kvs with Ok v -> v | Error _ -> Canon.null
  in
  oneof_weighted
    [
      (3, leaf);
      (1, map Canon.list (list_size (int_range 0 3) leaf));
      (1, map obj_of (list_size (int_range 0 3) (pair gen_small_string leaf)));
    ]

let gen_pairs =
  QCheck.Gen.(
    list_size (int_range 0 3) (pair gen_small_string gen_small_string))

let gen_payload =
  QCheck.Gen.(
    oneof
      [
        map (fun s -> Cassette.Inline s) gen_small_string;
        map (fun s -> Cassette.Blob s) gen_small_string;
      ])

let gen_outcome =
  let open QCheck.Gen in
  oneof
    [
      map (fun s -> Cassette.Transport_error s) gen_small_string;
      (let* status = int_range 100 599 in
       let* headers = gen_pairs in
       let* body =
         oneof
           [
             map (fun p -> Cassette.Whole p) gen_payload;
             map
               (fun l -> Cassette.Stream l)
               (list_size (int_range 0 3) (pair gen_nat gen_small_string));
           ]
       in
       return (Cassette.Response { status; headers; body }));
    ]

let gen_channel = QCheck.Gen.oneof_list Cassette.[ Model; Mcp; Http ]

let gen_exchange seq =
  let open QCheck.Gen in
  let* channel = gen_channel in
  let* protocol = gen_small_string in
  let* meth = gen_small_string in
  let* path = gen_small_string in
  let* query = gen_pairs in
  let* key = gen_small_string in
  let* occurrence = gen_nat in
  let* t_ms = gen_nat in
  let* canonical = gen_canon in
  let* request = gen_payload in
  let* outcome = gen_outcome in
  return
    {
      Cassette.seq;
      t_ms;
      channel;
      protocol;
      meth;
      path;
      query;
      key;
      occurrence;
      canonical;
      request;
      outcome;
    }

let gen_exchanges =
  let open QCheck.Gen in
  let* n = int_range 0 5 in
  let* gaps = list_size (return n) (int_range 1 5) in
  let seqs =
    List.rev
      (snd
         (List.fold_left
            (fun (s, acc) g -> (s + g, (s + g) :: acc))
            (0, []) gaps))
  in
  flatten_list (List.map gen_exchange seqs)

let gen_header =
  let open QCheck.Gen in
  let* run_id = gen_small_string in
  let* scenario = gen_small_string in
  let* variant = gen_small_string in
  let* bactrian_version = gen_small_string in
  let* recorded_at = gen_small_string in
  let* sim_now = gen_small_string in
  let* clock = oneof_list Trace.[ Enforced; Contract_only ] in
  return
    {
      Cassette.run_id;
      scenario;
      variant;
      bactrian_version;
      recorded_at;
      sim_now;
      clock;
    }

let prop_round_trip =
  QCheck.Test.make ~name:"cassettes round-trip line by line" ~count:300
    (QCheck.make
       QCheck.Gen.(triple gen_header gen_exchanges bool)
       ~print:(fun (h, es, _) ->
         String.concat "\n"
           (Cassette.header_line h :: List.map Cassette.exchange_line es)))
    (fun (h, es, complete) ->
      let lines =
        (Cassette.header_line h :: List.map Cassette.exchange_line es)
        @ if complete then [ Cassette.end_line ] else []
      in
      match Cassette.of_lines lines with
      | Error _ -> false
      | Ok t ->
          Bool.equal t.complete complete
          && String.equal
               (Cassette.header_line t.header)
               (Cassette.header_line h)
          && List.equal Cassette.equal_exchange t.exchanges es
          && List.equal String.equal
               (List.map Cassette.exchange_line t.exchanges)
               (List.map Cassette.exchange_line es))

let gen_bytes = QCheck.Gen.(string_size ~gen:char (int_range 0 12))

let prop_binary =
  QCheck.Test.make ~name:"arbitrary bytes round-trip as valid UTF-8 lines"
    ~count:500
    (QCheck.make
       QCheck.Gen.(
         triple gen_bytes gen_bytes
           (list_size (int_range 0 3) (pair gen_nat gen_bytes)))
       ~print:(fun (a, b, _) -> String.escaped a ^ " / " ^ String.escaped b))
    (fun (request, whole_body, chunks) ->
      let mk outcome = exchange ~request:(Inline request) ~outcome () in
      List.for_all
        (fun e ->
          let line = Cassette.exchange_line e in
          String.is_valid_utf_8 line
          &&
          match Cassette.of_lines [ hl; line ] with
          | Ok { exchanges = [ e' ]; _ } -> Cassette.equal_exchange e e'
          | Ok _ | Error _ -> false)
        [
          mk (whole (Inline whole_body));
          mk (Response { status = 200; headers = []; body = Stream chunks });
        ])

let () =
  Alcotest.run "cassette"
    [
      ("round trips", round_trips);
      ( "decoding",
        [
          Alcotest.test_case "header" `Quick test_header;
          Alcotest.test_case "complete" `Quick test_complete;
          Alcotest.test_case "partial without end line" `Quick test_partial;
          Alcotest.test_case "trailing empty lines" `Quick test_trailing_empty;
          Alcotest.test_case "missing header" `Quick test_missing_header;
          Alcotest.test_case "out of order" `Quick test_out_of_order;
          Alcotest.test_case "version" `Quick test_version;
          Alcotest.test_case "unknown fields" `Quick test_unknown_fields;
          Alcotest.test_case "bad lines" `Quick test_bad_lines;
          Alcotest.test_case "invalid json" `Quick test_invalid_json;
          Alcotest.test_case "truncated last line" `Quick
            test_truncated_last_line;
          Alcotest.test_case "bad base64" `Quick test_bad_base64;
        ] );
      ( "encoding",
        [
          Alcotest.test_case "encoding" `Quick test_encoding;
          Alcotest.test_case "binary encoding" `Quick test_binary_encoding;
          Alcotest.test_case "split multibyte" `Quick test_split_multibyte;
          Alcotest.test_case "chunk timing equality" `Quick
            test_equal_chunk_timing;
        ] );
      ( "props",
        [
          QCheck_alcotest.to_alcotest prop_round_trip;
          QCheck_alcotest.to_alcotest prop_binary;
        ] );
    ]
