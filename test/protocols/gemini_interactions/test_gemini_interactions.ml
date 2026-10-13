open Bactrian_core
open Protocol_test_support
module G = Gemini_interactions

let read = fixture ~protocol:"gemini_interactions" "flash_stream"
let fixture_request = json (read "request.json")
let raw = read "response.raw"
let recorded_chunks () = recorded_pieces ~raw ~chunks:(read "chunks.json")
let sse_summary s = ok_exn (G.summarise (Protocol.Sse (sse_items [ s ])))

(* Tests *)

let test_matches () =
  Alcotest.(check bool)
    "fixture" true
    (G.matches ~meth:"POST" ~path:"/v1beta/interactions");
  Alcotest.(check bool)
    "get" false
    (G.matches ~meth:"GET" ~path:"/v1beta/interactions");
  Alcotest.(check bool)
    "other" false
    (G.matches ~meth:"POST" ~path:"/v1beta/interactions/abc");
  Alcotest.(check bool)
    "generate" false
    (G.matches ~meth:"POST"
       ~path:"/v1beta/models/gemini-3.8-flash:generateContent")

let test_streaming () =
  Alcotest.(check bool)
    "fixture" true
    (G.streaming ~path:"/v1beta/interactions" ~query:"alt=sse" fixture_request);
  Alcotest.(check bool)
    "no stream" false
    (G.streaming ~path:"/v1beta/interactions" ~query:""
       (json {|{"model":"m","input":"hi"}|}));
  Alcotest.(check bool)
    "stream false" false
    (G.streaming ~path:"/v1beta/interactions" ~query:""
       (json {|{"model":"m","input":"hi","stream":false}|}))

let test_fixture_summary () =
  let s = sse_summary raw in
  Alcotest.(check (option string)) "model" (Some "gemini-3.8-flash") s.model;
  Alcotest.(check (option string)) "stop" (Some "requires_action") s.stop_reason;
  Alcotest.(check (option string)) "error" None s.error;
  (match s.tool_calls with
  | [ c ] ->
      Alcotest.(check string) "name" "get_weather" c.name;
      Alcotest.check canon_t "args" (json {|{"city":"Paris"}|}) c.arguments
  | _ -> Alcotest.fail "expected one tool call");
  match s.usage with
  | Some u ->
      Alcotest.(check int) "input" 69 u.input_tokens;
      Alcotest.(check int) "output includes thoughts" 58 u.output_tokens;
      Alcotest.(check (option int)) "cached" (Some 0) u.cached_input_tokens;
      Alcotest.(check (option int)) "reasoning" (Some 42) u.reasoning_tokens
  | None -> Alcotest.fail "no usage"

let test_split_invariance () =
  let whole = sse_summary raw in
  let by_chunks =
    ok_exn (G.summarise (Protocol.Sse (sse_items (recorded_chunks ()))))
  in
  let by_bytes =
    ok_exn (G.summarise (Protocol.Sse (sse_items (single_bytes raw))))
  in
  Alcotest.check summary_t "recorded chunks" whole by_chunks;
  Alcotest.check summary_t "single bytes" whole by_bytes

let canonical req =
  ok_exn (G.canonical_request ~path:"/v1beta/interactions" req)

let with_field = Canon.set_member

let test_ignored_fields () =
  let base = canonical fixture_request in
  List.iter
    (fun (k, _) ->
      let noisy = with_field k (Canon.string "volatile") fixture_request in
      Alcotest.check canon_t ("ignored " ^ k) base (canonical noisy);
      Alcotest.(check bool)
        ("absent " ^ k) false
        (Option.is_some (Canon.member k (canonical noisy))))
    G.ignored_fields

let test_unknown_kept () =
  let extended = with_field "brand_new_field" (Canon.int 1) fixture_request in
  Alcotest.(check bool)
    "kept" true
    (Option.is_some (Canon.member "brand_new_field" (canonical extended)));
  Alcotest.(check bool)
    "differs" false
    (Canon.equal (canonical extended) (canonical fixture_request))

let test_model_and_stream_kept () =
  let c = canonical fixture_request in
  Alcotest.(check bool) "model" true (Option.is_some (Canon.member "model" c));
  Alcotest.(check bool) "stream" true (Option.is_some (Canon.member "stream" c))

let test_not_object () =
  match G.canonical_request ~path:"/v1beta/interactions" (Canon.list []) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Malformed"

(* Hand-written second turn with two calls, for matching by call id. The
   captured single-call turn 2 is the [flash_turn2] fixture below. *)
let turn2 ~id1 ~id2 ~sig1 ~sig2 ~swap =
  let r1, r2 = if swap then (id2, id1) else (id1, id2) in
  Printf.sprintf
    {|{"model":"gemini-3.8-flash","stream":true,
       "input":[
        {"type":"user_input","content":"Weather in Paris and Rome?"},
        {"type":"thought","signature":"%s"},
        {"type":"function_call","id":"%s","signature":"%s","name":"get_weather","arguments":{"city":"Paris"}},
        {"type":"function_call","id":"%s","name":"get_weather","arguments":{"city":"Rome"}},
        {"type":"function_result","call_id":"%s","name":"get_weather","result":"18C cloudy"},
        {"type":"function_result","call_id":"%s","name":"get_weather","result":"25C sunny"}]}|}
    sig1 id1 sig2 id2 r1 r2

let t2 ?(swap = false) () =
  json (turn2 ~id1:"call_1" ~id2:"call_2" ~sig1:"AAA" ~sig2:"BBB" ~swap)

let test_tool_results () =
  match G.tool_results (t2 ()) with
  | [ a; b ] ->
      Alcotest.(check int) "first index" 0 a.call_index;
      Alcotest.(check string) "first content" "18C cloudy" a.content;
      Alcotest.(check bool) "not error" false a.is_error;
      Alcotest.(check int) "second index" 1 b.call_index;
      Alcotest.(check string) "second content" "25C sunny" b.content
  | _ -> Alcotest.fail "expected two results"

let test_tool_results_swapped () =
  match G.tool_results (t2 ~swap:true ()) with
  | [ a; b ] ->
      Alcotest.(check int) "first answers call 1" 1 a.call_index;
      Alcotest.(check int) "second answers call 0" 0 b.call_index
  | _ -> Alcotest.fail "expected two results"

let test_tool_results_none () =
  Alcotest.(check int) "none" 0 (List.length (G.tool_results fixture_request))

let test_matching_survives_in_key () =
  Alcotest.(check bool)
    "swapped results differ" false
    (Canon.equal (canonical (t2 ())) (canonical (t2 ~swap:true ())))

let test_ids_absent_from_key () =
  let s = Canon.to_string (canonical (t2 ())) in
  Alcotest.(check bool) "call id" false (contains ~sub:"call_1" s);
  Alcotest.(check bool) "signature" false (contains ~sub:"AAA" s)

let id_gen =
  QCheck.(string_size_of (Gen.int_range 1 12) Gen.(char_range 'a' 'z'))

let prop_ids =
  QCheck.Test.make ~name:"renaming ids and signatures keeps the key" ~count:200
    QCheck.(quad id_gen id_gen id_gen id_gen)
    (fun (a, b, s1, s2) ->
      QCheck.assume (not (String.equal a b));
      Canon.equal
        (canonical (t2 ()))
        (canonical
           (json
              (turn2 ~id1:("call_" ^ a) ~id2:("call_" ^ b) ~sig1:s1 ~sig2:s2
                 ~swap:false))))

(* Captured fixtures: a turn-2 request and a text-only stream *)

let turn2_request =
  json (fixture ~protocol:"gemini_interactions" "flash_turn2" "request.json")

let test_captured_tool_results () =
  match G.tool_results turn2_request with
  | [ r ] ->
      Alcotest.(check int) "call index" 0 r.call_index;
      Alcotest.(check string) "content" "18 degrees Celsius, cloudy" r.content;
      Alcotest.(check bool) "not error" false r.is_error
  | [] | _ :: _ :: _ -> Alcotest.fail "expected one result"

let test_text_parts_concatenated () =
  let req =
    json
      {|{"input":[
          {"type":"function_call","id":"c","name":"f","arguments":{}},
          {"type":"function_result","call_id":"c","name":"f","result":[
            {"type":"text","text":"a, "},{"type":"image","data":"x"},{"type":"text","text":"b"}]}]}|}
  in
  match G.tool_results req with
  | [ r ] -> Alcotest.(check string) "content" "a, b" r.content
  | [] | _ :: _ :: _ -> Alcotest.fail "expected one result"

let text_stream_raw =
  fixture ~protocol:"gemini_interactions" "flash_text_stream" "response.raw"

let test_text_stream_summary () =
  let s = sse_summary text_stream_raw in
  Alcotest.(check bool) "text non-empty" true (String.length s.text > 0);
  (match s.usage with
  | Some u ->
      Alcotest.(check int) "output 7 + thoughts 266" 273 u.output_tokens;
      Alcotest.(check (option int)) "reasoning" (Some 266) u.reasoning_tokens
  | None -> Alcotest.fail "no usage");
  Alcotest.(check (option string)) "stop" (Some "completed") s.stop_reason;
  Alcotest.(check (option string)) "error" None s.error;
  Alcotest.(check int) "no calls" 0 (List.length s.tool_calls);
  let by_bytes =
    ok_exn
      (G.summarise (Protocol.Sse (sse_items (single_bytes text_stream_raw))))
  in
  Alcotest.check summary_t "split invariance" s by_bytes

(* Rename the call id and both signatures of the captured turn 2. *)
let prop_captured_ids =
  let raw =
    fixture ~protocol:"gemini_interactions" "flash_turn2" "request.json"
  in
  let thought_sig =
    str "signature" (List.nth (list_exn (get "input" turn2_request)) 1)
  in
  let call_sig =
    str "signature" (List.nth (list_exn (get "input" turn2_request)) 2)
  in
  QCheck.Test.make ~name:"captured turn 2: renaming ids keeps the key"
    ~count:100
    QCheck.(triple id_gen id_gen id_gen)
    (fun (a, s1, s2) ->
      let renamed =
        raw
        |> replace_all ~sub:"call_343217" ~by:("call_" ^ a)
        |> replace_all ~sub:thought_sig ~by:s1
        |> replace_all ~sub:call_sig ~by:s2
      in
      Canon.equal (canonical turn2_request) (canonical (json renamed)))

(* Hand-written streams *)

let ev name data = Printf.sprintf "event: %s\ndata: %s\n\n" name data

let completed =
  ev "interaction.completed"
    {|{"interaction":{"id":"x","status":"completed","usage":{"total_input_tokens":3,"total_output_tokens":4},"model":"m"},"event_type":"interaction.completed"}|}

let text_delta t =
  ev "step.delta"
    (Printf.sprintf
       {|{"index":0,"delta":{"type":"text","text":%S},"event_type":"step.delta"}|}
       t)

let test_usage_definition () =
  let s =
    sse_summary
      (ev "interaction.completed"
         {|{"interaction":{"id":"x","status":"completed","model":"m","usage":{"total_input_tokens":10,"total_tool_use_tokens":5,"total_cached_tokens":4,"total_output_tokens":2,"total_thought_tokens":3}},"event_type":"interaction.completed"}|})
  in
  match s.usage with
  | Some u ->
      Alcotest.(check (list int))
        "input includes tool use, output includes thoughts" [ 15; 5 ]
        [ u.input_tokens; u.output_tokens ];
      check_usage_relations "hand" s
  | None -> Alcotest.fail "no usage"

let test_fixture_relations_and_truncation () =
  List.iter
    (fun (n, r) ->
      let body = Protocol.Sse (sse_items [ r ]) in
      check_usage_relations n (ok_exn (G.summarise body));
      check_truncation n G.summarise body)
    [ ("flash_stream", raw); ("flash_text_stream", text_stream_raw) ]

let test_hand_truncation () =
  let s = sse_summary (text_delta "Hi") in
  Alcotest.(check bool) "no terminal event" true (Option.is_some s.error);
  Alcotest.(check (option string))
    "done event completes it" None
    (sse_summary (text_delta "Hi" ^ ev "done" "[DONE]")).error

let call_item id =
  Printf.sprintf {|{"type":"function_call","id":%S,"name":"f","arguments":{}}|}
    id

let result_item id text =
  Printf.sprintf
    {|{"type":"function_result","call_id":%S,"name":"f","result":%S}|} id text

let input_of items =
  json (Printf.sprintf {|{"model":"m","input":[%s]}|} (String.concat "," items))

let indices req =
  List.map (fun (r : Protocol.tool_result) -> r.call_index) (G.tool_results req)

let canonical_result_ids req =
  list_exn (get "input" (canonical req))
  |> List.filter_map (fun it ->
      if String.equal (str "type" it) "function_result" then
        Some (get "call_id" it)
      else None)

let test_linking () =
  let reuse =
    input_of
      [
        call_item "x"; result_item "x" "r1"; call_item "x"; result_item "x" "r2";
      ]
  in
  Alcotest.(check (list int)) "reuse across turns" [ 0; 1 ] (indices reuse);
  Alcotest.(check (list canon_t))
    "canonical agrees"
    [ Canon.int 0; Canon.int 1 ]
    (canonical_result_ids reuse);
  let parallel =
    input_of
      [ call_item ""; call_item ""; result_item "" "a"; result_item "" "b" ]
  in
  Alcotest.(check (list int)) "equal ids in order" [ 0; 1 ] (indices parallel);
  let ghost = input_of [ result_item "ghost" "r" ] in
  Alcotest.(check (list int)) "unlinked left out" [] (indices ghost);
  Alcotest.(check (list canon_t))
    "unlinked is null in the key" [ Canon.null ]
    (canonical_result_ids ghost)

let test_store_kept () =
  Alcotest.(check bool)
    "not ignored" false
    (List.mem_assoc "store" G.ignored_fields);
  Alcotest.(check bool)
    "differs" false
    (Canon.equal
       (canonical fixture_request)
       (canonical (with_field "store" (Canon.bool true) fixture_request)))

let test_text () =
  let s =
    sse_summary
      (text_delta "Hel" ^ text_delta "lo" ^ completed ^ ev "done" "[DONE]")
  in
  Alcotest.(check string) "text" "Hello" s.text;
  Alcotest.(check (option string)) "stop" (Some "completed") s.stop_reason;
  match s.usage with
  | Some u ->
      Alcotest.(check (option int)) "cached absent" None u.cached_input_tokens;
      Alcotest.(check int) "out" 4 u.output_tokens
  | None -> Alcotest.fail "no usage"

let test_keepalive_comment () =
  let base = text_delta "Hi" ^ completed in
  Alcotest.check summary_t "comments ignored" (sse_summary base)
    (sse_summary
       (": keep-alive\n\n" ^ text_delta "Hi" ^ ": ping\n\n" ^ completed))

let test_error_frame () =
  let s =
    sse_summary
      (text_delta "partial"
      ^ ev "error"
          {|{"error":{"code":500,"message":"backend exploded"},"event_type":"error"}|}
      )
  in
  Alcotest.(check (option string)) "error" (Some "backend exploded") s.error;
  Alcotest.(check string) "partial text kept" "partial" s.text

let test_failed_status () =
  let s =
    sse_summary
      (ev "interaction.status_update"
         {|{"interaction_id":"x","status":"failed","event_type":"interaction.status_update"}|})
  in
  Alcotest.(check (option string)) "error" (Some "interaction failed") s.error

let test_two_calls_split_args () =
  let start i name =
    ev "step.start"
      (Printf.sprintf
         {|{"index":%d,"step":{"id":"call_%d","type":"function_call","name":%S,"arguments":{}},"event_type":"step.start"}|}
         i i name)
  in
  let delta i part =
    ev "step.delta"
      (Printf.sprintf
         {|{"index":%d,"delta":{"type":"arguments_delta","arguments":%S},"event_type":"step.delta"}|}
         i part)
  in
  let s =
    sse_summary
      (start 1 "a" ^ start 2 "b" ^ delta 1 {|{"x":|} ^ delta 2 {|{"y":2}|}
     ^ delta 1 {|1}|} ^ completed)
  in
  match s.tool_calls with
  | [ a; b ] ->
      Alcotest.(check string) "a" "a" a.name;
      Alcotest.check canon_t "a args" (json {|{"x":1}|}) a.arguments;
      Alcotest.check canon_t "b args" (json {|{"y":2}|}) b.arguments
  | _ -> Alcotest.fail "expected two calls"

let test_start_arguments_kept () =
  let s =
    sse_summary
      (ev "step.start"
         {|{"index":0,"step":{"type":"function_call","name":"a","arguments":{"k":"v"}},"event_type":"step.start"}|}
      ^ completed)
  in
  match s.tool_calls with
  | [ a ] -> Alcotest.check canon_t "args" (json {|{"k":"v"}|}) a.arguments
  | _ -> Alcotest.fail "expected one call"

let test_no_args_is_empty_object () =
  let s =
    sse_summary
      (ev "step.start"
         {|{"index":0,"step":{"type":"function_call","name":"a","arguments":{}},"event_type":"step.start"}|}
      ^ completed)
  in
  match s.tool_calls with
  | [ a ] -> Alcotest.check canon_t "args" (json "{}") a.arguments
  | _ -> Alcotest.fail "expected one call"

let test_malformed () =
  let bad items =
    match G.summarise items with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "expected Malformed"
  in
  bad (Protocol.Sse (sse_items [ "event: step.delta\ndata: not json\n\n" ]));
  bad
    (Protocol.Sse
       (sse_items
          [
            ev "step.delta"
              {|{"index":3,"delta":{"type":"arguments_delta","arguments":"{}"}}|};
          ]));
  bad (Protocol.Json_stream []);
  bad (Protocol.Json (Canon.list []))

let test_json () =
  let s =
    ok_exn
      (G.summarise
         (Protocol.Json
            (json
               {|{"status":"completed","model":"m","usage":{"total_input_tokens":1,"total_output_tokens":2}}|})))
  in
  Alcotest.(check (option string)) "model" (Some "m") s.model;
  Alcotest.(check (option string)) "stop" (Some "completed") s.stop_reason

let () =
  Alcotest.run "gemini_interactions"
    [
      ( "fixture",
        [
          Alcotest.test_case "matches" `Quick test_matches;
          Alcotest.test_case "streaming" `Quick test_streaming;
          Alcotest.test_case "summary" `Quick test_fixture_summary;
          Alcotest.test_case "split invariance" `Quick test_split_invariance;
        ] );
      ( "canonical_request",
        [
          Alcotest.test_case "ignored fields" `Quick test_ignored_fields;
          Alcotest.test_case "unknown kept" `Quick test_unknown_kept;
          Alcotest.test_case "model and stream kept" `Quick
            test_model_and_stream_kept;
          Alcotest.test_case "not an object" `Quick test_not_object;
          Alcotest.test_case "store kept" `Quick test_store_kept;
          Alcotest.test_case "ids absent" `Quick test_ids_absent_from_key;
          Alcotest.test_case "result matching kept" `Quick
            test_matching_survives_in_key;
          QCheck_alcotest.to_alcotest prop_ids;
          QCheck_alcotest.to_alcotest prop_captured_ids;
        ] );
      ( "tool_results",
        [
          Alcotest.test_case "turn 2" `Quick test_tool_results;
          Alcotest.test_case "swapped" `Quick test_tool_results_swapped;
          Alcotest.test_case "none" `Quick test_tool_results_none;
          Alcotest.test_case "linking" `Quick test_linking;
          Alcotest.test_case "captured turn 2" `Quick test_captured_tool_results;
          Alcotest.test_case "text parts concatenated" `Quick
            test_text_parts_concatenated;
        ] );
      ( "streams",
        [
          Alcotest.test_case "captured text stream" `Quick
            test_text_stream_summary;
          Alcotest.test_case "text" `Quick test_text;
          Alcotest.test_case "usage definition" `Quick test_usage_definition;
          Alcotest.test_case "fixture relations and truncation" `Quick
            test_fixture_relations_and_truncation;
          Alcotest.test_case "hand truncation" `Quick test_hand_truncation;
          Alcotest.test_case "keep-alive comments" `Quick test_keepalive_comment;
          Alcotest.test_case "error frame" `Quick test_error_frame;
          Alcotest.test_case "failed status" `Quick test_failed_status;
          Alcotest.test_case "two calls, split args" `Quick
            test_two_calls_split_args;
          Alcotest.test_case "start arguments" `Quick test_start_arguments_kept;
          Alcotest.test_case "empty arguments" `Quick
            test_no_args_is_empty_object;
          Alcotest.test_case "malformed" `Quick test_malformed;
          Alcotest.test_case "whole json" `Quick test_json;
        ] );
    ]
