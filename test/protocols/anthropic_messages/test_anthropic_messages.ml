module A = Bactrian_core.Anthropic_messages
module C = Bactrian_core.Canon
module P = Bactrian_core.Protocol
module S = Bactrian_core.Sse
open Protocol_test_support

let read name file = fixture ~protocol:"anthropic_messages" name file

let fixtures =
  [ "haiku_stream"; "haiku_nonstream"; "haiku_thinking_stream"; "haiku_turn2" ]

let streamed = [ "haiku_stream"; "haiku_thinking_stream" ]
let request name = json (read name "request.json")
let meta name = json (read name "meta.json")

let chunks name =
  recorded_pieces ~raw:(read name "response.raw")
    ~chunks:(read name "chunks.json")

let body name =
  if List.mem name streamed then P.Sse (sse_items (chunks name))
  else P.Json (json (read name "response.raw"))

let render (s : P.summary) =
  let opt f = function None -> "-" | Some x -> f x in
  let u (u : Bactrian_core.Trace.usage) =
    Printf.sprintf "%d/%d/%s/%s/%s" u.input_tokens u.output_tokens
      (opt string_of_int u.cached_input_tokens)
      (opt string_of_int u.cache_write_tokens)
      (opt string_of_int u.reasoning_tokens)
  in
  String.concat "|"
    [
      opt Fun.id s.model;
      s.text;
      String.concat ";"
        (List.map
           (fun (c : P.tool_call) -> c.name ^ C.to_string c.arguments)
           s.tool_calls);
      opt u s.usage;
      opt Fun.id s.stop_reason;
      opt Fun.id s.error;
    ]

let summary name = ok_exn (A.summarise (body name))
let check_str = Alcotest.(check string)

let test_matches () =
  List.iter
    (fun n ->
      let m = meta n in
      Alcotest.(check bool)
        n true
        (A.matches ~meth:(str "method" m) ~path:(str "path" m)))
    fixtures;
  Alcotest.(check bool) "get" false (A.matches ~meth:"GET" ~path:"/v1/messages");
  Alcotest.(check bool)
    "lower-case method" false
    (A.matches ~meth:"post" ~path:"/v1/messages");
  Alcotest.(check bool)
    "other" false
    (A.matches ~meth:"POST" ~path:"/v1/chat/completions")

let test_streaming () =
  List.iter
    (fun n ->
      let m = meta n in
      let ct = str "content-type" (get "response_headers" m) in
      let is_sse = String.starts_with ~prefix:"text/event-stream" ct in
      Alcotest.(check bool)
        n is_sse
        (A.streaming ~path:"/v1/messages" ~query:"" (request n)))
    fixtures

let test_summaries () =
  let tool_ok n =
    let s = summary n in
    check_str (n ^ " model") "claude-haiku-5-5" (Option.get s.model);
    (match s.tool_calls with
    | [ c ] ->
        check_str "tool" "get_weather" c.name;
        check_str "args" {|{"city":"Paris"}|} (C.to_string c.arguments)
    | _ -> Alcotest.fail "one tool call expected");
    Alcotest.(check (option string)) "stop" (Some "tool_use") s.stop_reason;
    let u = Option.get s.usage in
    Alcotest.(check (pair int int))
      "usage" (395, 50)
      (u.input_tokens, u.output_tokens);
    Alcotest.(check (option int)) "reasoning" (Some 0) u.reasoning_tokens;
    Alcotest.(check (option string)) "error" None s.error
  in
  tool_ok "haiku_stream";
  tool_ok "haiku_nonstream";
  let t = summary "haiku_thinking_stream" in
  check_str "thinking text" "The ball costs **$0.05**." t.text;
  Alcotest.(check (option string)) "stop" (Some "end_turn") t.stop_reason;
  let tu = Option.get t.usage in
  (* message_start says 16-ish output tokens, message_delta the final 41. *)
  Alcotest.(check (pair int int))
    "usage" (426, 41)
    (tu.input_tokens, tu.output_tokens);
  Alcotest.(check (option int)) "thinking tokens" (Some 27) tu.reasoning_tokens;
  Alcotest.(check (list string))
    "no tools" []
    (List.map (fun (c : P.tool_call) -> c.name) t.tool_calls);
  let t2 = summary "haiku_turn2" in
  check_str "turn2 text"
    "The weather in Paris right now is 18\xc2\xb0C and cloudy." t2.text;
  Alcotest.(check (option string)) "stop" (Some "end_turn") t2.stop_reason;
  Alcotest.(check (pair int int))
    "usage" (464, 22)
    ((Option.get t2.usage).input_tokens, (Option.get t2.usage).output_tokens)

let test_usage_definition () =
  let u =
    Option.get
      (ok_exn
         (A.summarise
            (P.Json
               (json
                  {|{"model":"m","content":[],"stop_reason":"end_turn","usage":{"input_tokens":10,"cache_read_input_tokens":3,"cache_creation_input_tokens":5,"output_tokens":7}}|}))))
        .usage
  in
  Alcotest.(check (list int))
    "input is the total prompt" [ 18; 7 ]
    [ u.input_tokens; u.output_tokens ];
  Alcotest.(check (list (option int)))
    "subsets" [ Some 3; Some 5 ]
    [ u.cached_input_tokens; u.cache_write_tokens ]

let test_usage_relations () =
  List.iter (fun n -> check_usage_relations n (summary n)) fixtures

let test_truncation () =
  List.iter (fun n -> check_truncation n A.summarise (body n)) streamed

let test_split_invariance () =
  List.iter
    (fun n ->
      let r c = render (ok_exn (A.summarise (P.Sse (sse_items c)))) in
      let recorded = r (chunks n) in
      check_str (n ^ " bytes") recorded
        (r (single_bytes (read n "response.raw")));
      check_str (n ^ " whole") recorded (r [ read n "response.raw" ]))
    streamed

let test_comments_and_pings () =
  let items = sse_items (chunks "haiku_stream") in
  let noisy = List.concat_map (fun i -> [ S.Comment " keep-alive"; i ]) items in
  check_str "same"
    (render (summary "haiku_stream"))
    (render (ok_exn (A.summarise (P.Sse noisy))))

let ev ?event data = S.Event { event; data; id = None; retry = None }

let test_cumulative_usage () =
  let items =
    [
      ev
        {|{"type":"message_start","message":{"model":"m","usage":{"input_tokens":10,"output_tokens":5,"cache_read_input_tokens":3}}}|};
      ev
        {|{"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":20}}|};
      ev
        {|{"type":"message_delta","delta":{"stop_reason":null},"usage":{"output_tokens":25}}|};
    ]
  in
  let s = ok_exn (A.summarise (P.Sse items)) in
  let u = Option.get s.usage in
  Alcotest.(check (list int))
    "input includes cache reads; last output wins" [ 13; 25; 3 ]
    [ u.input_tokens; u.output_tokens; Option.get u.cached_input_tokens ];
  Alcotest.(check (option string)) "stop kept" (Some "end_turn") s.stop_reason

let test_error_frame () =
  let items =
    [
      ev ~event:"message_start"
        {|{"type":"message_start","message":{"model":"m","usage":{"input_tokens":1,"output_tokens":1}}}|};
      ev ~event:"content_block_start"
        {|{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}|};
      ev
        {|{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hel"}}|};
      ev ~event:"error"
        {|{"type":"error","error":{"type":"overloaded_error","message":"Overloaded"}}|};
    ]
  in
  let s = ok_exn (A.summarise (P.Sse items)) in
  Alcotest.(check (option string))
    "error" (Some "overloaded_error: Overloaded") s.error;
  check_str "partial text" "Hel" s.text;
  let j =
    ok_exn
      (A.summarise
         (P.Json
            (json
               {|{"type":"error","error":{"type":"invalid_request_error","message":"bad"},"request_id":"r"}|})))
  in
  Alcotest.(check (option string))
    "json error" (Some "invalid_request_error: bad") j.error

let test_interleaved_tools () =
  let d i j =
    ev
      (Printf.sprintf
         {|{"type":"content_block_delta","index":%d,"delta":{"type":"input_json_delta","partial_json":%s}}|}
         i
         (C.to_string (C.string j)))
  in
  let start i n =
    ev
      (Printf.sprintf
         {|{"type":"content_block_start","index":%d,"content_block":{"type":"tool_use","id":"x","name":"%s","input":{}}}|}
         i n)
  in
  let items =
    [ start 0 "a"; start 1 "b"; d 0 "{\"k\":"; d 1 "{\"z\":1}"; d 0 "2}" ]
  in
  let s = ok_exn (A.summarise (P.Sse items)) in
  Alcotest.(check (list string))
    "calls"
    [ {|a{"k":2}|}; {|b{"z":1}|} ]
    (List.map
       (fun (c : P.tool_call) -> c.name ^ C.to_string c.arguments)
       s.tool_calls)

let test_bad_inputs () =
  Alcotest.(check bool)
    "json stream" true
    (Result.is_error (A.summarise (P.Json_stream [])));
  Alcotest.(check bool)
    "bad data" true
    (Result.is_error (A.summarise (P.Sse [ ev "{nope" ])));
  Alcotest.(check bool)
    "bad args" true
    (Result.is_error
       (A.summarise
          (P.Sse
             [
               ev
                 {|{"type":"content_block_start","index":0,"content_block":{"type":"tool_use","name":"a"}}|};
               ev
                 {|{"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"{"}}|};
             ])));
  Alcotest.(check bool)
    "not object" true
    (Result.is_error (A.canonical_request ~path:"/v1/messages" (json "[]")))

let canonical s =
  C.to_string (ok_exn (A.canonical_request ~path:"/v1/messages" (json s)))

let base =
  {|{"model":"m","max_tokens":10,"messages":[{"role":"user","content":"hi"},{"role":"assistant","content":[{"type":"thinking","thinking":"t","signature":"SIGA"},{"type":"text","text":"x"}]}],"tools":[{"name":"a","input_schema":{"type":"object"}}]}|}

let mutations =
  [
    ("metadata", {|{"metadata":{"user_id":"u"},|});
    ("service_tier", {|{"service_tier":"auto",|});
    ("inference_geo", {|{"inference_geo":"us",|});
    ("container", {|{"container":"c1",|});
  ]

(* Every block kind that can carry the removed fields. *)
let rich =
  json
    {|{"model":"m","max_tokens":10,
       "system":[{"type":"text","text":"s"}],
       "tools":[{"name":"a","input_schema":{"type":"object"}}],
       "messages":[
         {"role":"user","content":"hi"},
         {"role":"assistant","content":[
           {"type":"thinking","thinking":"t","signature":"SIGA"},
           {"type":"redacted_thinking","data":"OPAQUE1"},
           {"type":"text","text":"x"},
           {"type":"tool_use","id":"t1","name":"a","input":{}}]},
         {"role":"user","content":[
           {"type":"tool_result","tool_use_id":"t1","content":[{"type":"text","text":"r"}]}]}]}|}

let typed_paths =
  [ "messages[].content[].signature"; "messages[].content[].data" ]

let canonical_json v =
  C.to_string (ok_exn (A.canonical_request ~path:"/v1/messages" v))

let test_ignored () =
  let with_prefix p = p ^ String.sub base 1 (String.length base - 1) in
  List.iter
    (fun (k, p) -> check_str k (canonical base) (canonical (with_prefix p)))
    mutations;
  Alcotest.(check (list string))
    "table lists exactly what the code removes"
    (List.sort String.compare
       ([
          "cache_control";
          "system[].cache_control";
          "tools[].cache_control";
          "messages[].content[].cache_control";
          "messages[].content[].content[].cache_control";
        ]
       @ typed_paths @ List.map fst mutations))
    (List.sort String.compare (List.map fst A.ignored_fields));
  List.iter
    (fun (p, _) ->
      if not (List.mem p typed_paths) then
        check_str ("removed " ^ p) (canonical_json rich)
          (canonical_json (set_path p (C.string "volatile") rich)))
    (List.filter
       (fun (p, _) -> not (List.mem_assoc p mutations))
       A.ignored_fields);
  let sg = replace_all ~sub:"SIGA" ~by:"SIGB" base in
  check_str "signature" (canonical base) (canonical sg);
  let kept =
    canonical
      (replace_all ~sub:{|"model":"m"|} ~by:{|"model":"m","brand_new":1|} base)
  in
  Alcotest.(check bool)
    "unknown kept" true
    (contains ~sub:{|"brand_new":1|} kept);
  let verbatim =
    {|{"messages":[{"role":"assistant","content":[{"type":"tool_use","id":"t","name":"n","input":{"cache_control":1,"id":"keep"}}]}]}|}
  in
  Alcotest.(check bool)
    "tool input untouched" true
    (contains ~sub:{|"input":{"cache_control":1,"id":"keep"}|}
       (canonical verbatim))

let test_opaque_blobs () =
  (* Opaque per-run blobs of earlier thinking turns, in history only. *)
  let req data signature =
    json
      (Printf.sprintf
         {|{"messages":[{"role":"assistant","content":[
            {"type":"thinking","thinking":"t","signature":%S},
            {"type":"redacted_thinking","data":%S},
            {"type":"text","text":"x","data":"keep","signature":"keep"},
            {"type":"tool_use","id":"t","name":"n","input":{"data":"keep","signature":"keep"}}]}]}|}
         signature data)
  in
  check_str "blobs do not matter"
    (canonical_json (req "A" "S1"))
    (canonical_json (req "B" "S2"));
  let c = canonical_json (req "A" "S1") in
  Alcotest.(check bool) "redacted data gone" false (contains ~sub:"OPAQUE" c);
  Alcotest.(check bool)
    "other blocks keep data and signature" true
    (contains ~sub:{|"data":"keep","signature":"keep","text":"x"|} c);
  Alcotest.(check bool)
    "tool input untouched" true
    (contains ~sub:{|"input":{"data":"keep","signature":"keep"}|} c)

let test_fixture_ignored () =
  let r = read "haiku_stream" "request.json" in
  let r' =
    "{\"metadata\":{\"user_id\":\"u\"},\"service_tier\":\"auto\","
    ^ String.sub r 1 (String.length r - 1)
  in
  check_str "fixture" (canonical r) (canonical r')

let test_tool_results () =
  match A.tool_results (request "haiku_turn2") with
  | [ r ] ->
      Alcotest.(check int) "index" 0 r.call_index;
      check_str "content" "18 degrees Celsius, cloudy" r.content;
      Alcotest.(check bool) "error" false r.is_error
  | _ -> Alcotest.fail "one result expected"

let two_calls a b =
  Printf.sprintf
    {|{"messages":[{"role":"user","content":"q"},{"role":"assistant","content":[{"type":"tool_use","id":"%s","name":"f","input":{"n":1}},{"type":"tool_use","id":"%s","name":"f","input":{"n":2}}]},{"role":"user","content":[{"type":"tool_result","tool_use_id":"%s","content":[{"type":"text","text":"two"},{"type":"text","text":"!"}],"is_error":true},{"type":"tool_result","tool_use_id":"%s","content":"one"}]}]}|}
    a b b a

let test_tool_result_matching () =
  let req = json (two_calls "id_a" "id_b") in
  (match A.tool_results req with
  | [ r2; r1 ] ->
      Alcotest.(check (list int))
        "idx" [ 1; 0 ]
        [ r2.call_index; r1.call_index ];
      check_str "joined" "two!" r2.content;
      Alcotest.(check bool) "err" true r2.is_error;
      check_str "one" "one" r1.content
  | _ -> Alcotest.fail "two results expected");
  Alcotest.(check bool)
    "ids replaced by positions" true
    (contains ~sub:{|"tool_use_id":1|} (canonical (two_calls "id_a" "id_b")));
  Alcotest.(check bool)
    "swapped pairing differs" false
    (String.equal
       (canonical (two_calls "id_a" "id_b"))
       (canonical
          (replace_all ~sub:{|"tool_use_id":"id_b"|}
             ~by:{|"tool_use_id":"id_a"|}
             (replace_all ~sub:{|"tool_use_id":"id_a"|}
                ~by:{|"tool_use_id":"id_c"|} (two_calls "id_a" "id_b")))))

let tool_use id n =
  Printf.sprintf {|{"type":"tool_use","id":%S,"name":"f","input":{"n":%d}}|} id
    n

let tool_result id text =
  Printf.sprintf {|{"type":"tool_result","tool_use_id":%S,"content":%S}|} id
    text

let turns blocks =
  json
    (Printf.sprintf {|{"messages":[%s]}|}
       (String.concat ","
          (List.map
             (fun (role, bs) ->
               Printf.sprintf {|{"role":%S,"content":[%s]}|} role
                 (String.concat "," bs))
             blocks)))

let indices req =
  List.map (fun (r : P.tool_result) -> r.call_index) (A.tool_results req)

let canonical_result_ids req =
  let c = ok_exn (A.canonical_request ~path:"/v1/messages" req) in
  List.concat_map
    (fun m ->
      List.filter_map
        (fun b ->
          if
            String.equal
              (Option.value ~default:"" (C.str_member "type" b))
              "tool_result"
          then C.member "tool_use_id" b
          else None)
        (Option.value ~default:[] (C.list_member "content" m)))
    (list_exn (get "messages" c))

let test_linking () =
  let reuse =
    turns
      [
        ("assistant", [ tool_use "x" 1 ]);
        ("user", [ tool_result "x" "r1" ]);
        ("assistant", [ tool_use "x" 2 ]);
        ("user", [ tool_result "x" "r2" ]);
      ]
  in
  Alcotest.(check (list int)) "reuse across turns" [ 0; 1 ] (indices reuse);
  Alcotest.(check (list canon_t))
    "canonical agrees"
    [ C.int 0; C.int 1 ]
    (canonical_result_ids reuse);
  let parallel =
    turns
      [
        ("assistant", [ tool_use "" 1; tool_use "" 2 ]);
        ("user", [ tool_result "" "a"; tool_result "" "b" ]);
      ]
  in
  Alcotest.(check (list int)) "equal ids in order" [ 0; 1 ] (indices parallel);
  let ghost = turns [ ("user", [ tool_result "ghost" "r" ]) ] in
  Alcotest.(check (list int)) "unlinked is left out" [] (indices ghost);
  Alcotest.(check (list canon_t))
    "unlinked is null in the key" [ C.null ]
    (canonical_result_ids ghost)

let id_gen =
  QCheck.Gen.(
    string_size
      ~gen:(oneof [ char_range 'a' 'z'; char_range '0' '9' ])
      (int_range 4 30))

let prop_id_rename =
  let original = read "haiku_turn2" "request.json" in
  let expected = canonical original in
  QCheck.Test.make ~name:"renaming tool ids keeps the canonical request"
    ~count:200
    (QCheck.make QCheck.Gen.(pair id_gen id_gen))
    (fun (a, _) ->
      let r =
        replace_all ~sub:"toolu_01ReCiyPQ49xxQfSYauscpbK" ~by:("toolu_" ^ a)
          original
      in
      String.equal expected (canonical r)
      && String.equal
           (canonical (two_calls "x1" "x2"))
           (canonical (two_calls ("p" ^ a) ("q" ^ a))))

let () =
  Alcotest.run "anthropic_messages"
    [
      ( "fixtures",
        [
          Alcotest.test_case "matches" `Quick test_matches;
          Alcotest.test_case "streaming" `Quick test_streaming;
          Alcotest.test_case "summaries" `Quick test_summaries;
          Alcotest.test_case "split invariance" `Quick test_split_invariance;
          Alcotest.test_case "comments and pings" `Quick test_comments_and_pings;
          Alcotest.test_case "tool results" `Quick test_tool_results;
          Alcotest.test_case "usage definition" `Quick test_usage_definition;
          Alcotest.test_case "usage relations" `Quick test_usage_relations;
          Alcotest.test_case "truncated streams" `Quick test_truncation;
          Alcotest.test_case "tool result linking" `Quick test_linking;
        ] );
      ( "edge cases",
        [
          Alcotest.test_case "cumulative usage" `Quick test_cumulative_usage;
          Alcotest.test_case "error frame" `Quick test_error_frame;
          Alcotest.test_case "interleaved tool blocks" `Quick
            test_interleaved_tools;
          Alcotest.test_case "bad inputs" `Quick test_bad_inputs;
        ] );
      ( "canonical",
        [
          Alcotest.test_case "ignored fields" `Quick test_ignored;
          Alcotest.test_case "opaque blobs" `Quick test_opaque_blobs;
          Alcotest.test_case "fixture ignored" `Quick test_fixture_ignored;
          Alcotest.test_case "tool result matching" `Quick
            test_tool_result_matching;
          QCheck_alcotest.to_alcotest prop_id_rename;
        ] );
    ]
