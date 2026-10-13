open Bactrian_core
open Protocol_test_support
module A = Openai_responses

let fixtures = [ "luna_stream"; "luna_nonstream"; "luna_turn2" ]
let read name file = fixture ~protocol:"openai_responses" name file
let request name = json (read name "request.json")
let meta name = json (read name "meta.json")

let recorded name raw =
  sse_items (recorded_pieces ~raw ~chunks:(read name "chunks.json"))

let singles raw = sse_items (single_bytes raw)
let feed_all = sse_items

let tool_call_t =
  Alcotest.testable
    (fun ppf (c : Protocol.tool_call) ->
      Fmt.pf ppf "%s %s" c.name (Canon.to_string c.arguments))
    (fun (a : Protocol.tool_call) b ->
      String.equal a.name b.name && Canon.equal a.arguments b.arguments)

let summarise_ok body = ok_exn (A.summarise body)

let is_sse name =
  let ct = str "content-type" (get "response_headers" (meta name)) in
  String.starts_with ~prefix:"text/event-stream" ct

let body_of name =
  let raw = read name "response.raw" in
  if is_sse name then Protocol.Sse (recorded name raw)
  else Protocol.Json (json raw)

let canonical req = ok_exn (A.canonical_request ~path:"/v1/responses" req)

(* Fixture tests *)

let test_fixture name () =
  let m = meta name in
  let req = request name in
  Alcotest.(check bool)
    "matches" true
    (A.matches ~meth:(str "method" m) ~path:(str "path" m));
  Alcotest.(check bool)
    "streaming" (is_sse name)
    (A.streaming ~path:(str "path" m) ~query:(str "query" m) req);
  let s = summarise_ok (body_of name) in
  Alcotest.(check (option string)) "model" (Some "gpt-6-luna") s.model;
  Alcotest.(check (option string)) "stop" (Some "completed") s.stop_reason;
  Alcotest.(check (option string)) "error" None s.error;
  (match s.usage with
  | Some u ->
      Alcotest.(check bool)
        "usage" true
        (u.input_tokens > 0 && u.output_tokens > 0)
  | None -> Alcotest.fail "no usage");
  if String.equal name "luna_turn2" then
    Alcotest.(check bool)
      "text" true
      (String.length s.text > 0 && s.tool_calls = [])
  else
    Alcotest.(check (list tool_call_t))
      "calls"
      [ { name = "get_weather"; arguments = json {|{"city":"Paris"}|} } ]
      s.tool_calls

let test_split_invariance () =
  let raw = read "luna_stream" "response.raw" in
  let a = summarise_ok (Protocol.Sse (recorded "luna_stream" raw)) in
  let b = summarise_ok (Protocol.Sse (singles raw)) in
  let c = summarise_ok (Protocol.Sse (feed_all [ raw ])) in
  Alcotest.check summary_t "singles" a b;
  Alcotest.check summary_t "whole" a c

let test_stream_matches_nonstream () =
  let s = summarise_ok (body_of "luna_stream") in
  let n = summarise_ok (body_of "luna_nonstream") in
  Alcotest.(check (list tool_call_t)) "same call" n.tool_calls s.tool_calls

(* canonical_request *)

let with_path = set_path

let test_ignored_removed () =
  let req = request "luna_stream" in
  List.iter
    (fun (p, _) ->
      let a = canonical (with_path p (Canon.bool true) req) in
      let b = canonical (with_path p (Canon.string "other") req) in
      Alcotest.check canon_t (p ^ " removed") (canonical req) a;
      Alcotest.check canon_t (p ^ " value irrelevant") a b)
    A.ignored_fields

let test_kept_fields () =
  let req = request "luna_stream" in
  List.iter
    (fun k ->
      let changed = canonical (with_path k (Canon.string "x") req) in
      Alcotest.(check bool)
        (k ^ " kept") false
        (Canon.equal changed (canonical req)))
    [
      "x_unknown_field";
      "previous_response_id";
      "conversation";
      "stream_options.include_usage";
      "model";
      "store";
      "background";
      "include";
    ];
  List.iter
    (fun k ->
      Alcotest.(check bool)
        (k ^ " not ignored") false
        (List.mem_assoc k A.ignored_fields))
    [ "store"; "background"; "include" ];
  Alcotest.(check bool)
    "model present" true
    (Option.is_some (Canon.member "model" (canonical req)))

let id_gen =
  QCheck.Gen.(
    string_size
      ~gen:(oneof_list (List.init 26 (fun i -> Char.chr (97 + i))))
      (int_range 1 30))

let turn2_ids =
  ( "fc_059fe00ca2779f6f006acd848180c487d28a85c62b0983bf75",
    "call_mXpgkYFrcbPPkMAnOhXUvPoW" )

let prop_ids =
  QCheck.Test.make ~name:"renaming ids keeps canonical form" ~count:200
    (QCheck.make QCheck.Gen.(pair id_gen id_gen))
    (fun (a, b) ->
      let raw = read "luna_turn2" "request.json" in
      let fc, call = turn2_ids in
      let renamed =
        raw
        |> replace_all ~sub:fc ~by:("fc_" ^ a)
        |> replace_all ~sub:call ~by:("call_" ^ b)
      in
      Canon.equal (canonical (json raw)) (canonical (json renamed)))

let two_calls ~a ~b ~out_a ~out_b =
  json
    (Printf.sprintf
       {|{"model":"m","input":[{"id":"fc_1","type":"function_call","call_id":"%s","name":"f","arguments":"{}"},{"id":"fc_2","type":"function_call","call_id":"%s","name":"f","arguments":"{}"},{"type":"function_call_output","call_id":"%s","output":"x"},{"type":"function_call_output","call_id":"%s","output":"y"}]}|}
       a b out_a out_b)

let test_pairing () =
  let base = canonical (two_calls ~a:"A" ~b:"B" ~out_a:"A" ~out_b:"B") in
  Alcotest.check canon_t "renamed" base
    (canonical (two_calls ~a:"Q" ~b:"Z" ~out_a:"Q" ~out_b:"Z"));
  Alcotest.(check bool)
    "swapped pairing differs" false
    (Canon.equal base
       (canonical (two_calls ~a:"A" ~b:"B" ~out_a:"B" ~out_b:"A")))

let test_item_reference_kept () =
  let mk id =
    json
      (Printf.sprintf
         {|{"model":"m","input":[{"type":"item_reference","id":"%s"}]}|} id)
  in
  Alcotest.(check bool)
    "differs" false
    (Canon.equal (canonical (mk "msg_1")) (canonical (mk "msg_2")))

let test_stream_options_pruned () =
  let a =
    json {|{"model":"m","stream_options":{"include_obfuscation":false}}|}
  in
  Alcotest.check canon_t "pruned"
    (canonical (json {|{"model":"m"}|}))
    (canonical a)

let test_non_object () =
  match A.canonical_request ~path:"/v1/responses" (json "[1]") with
  | Error (Protocol.Malformed _) -> ()
  | Ok _ -> Alcotest.fail "expected error"

(* tool_results *)

let test_tool_results () =
  match A.tool_results (request "luna_turn2") with
  | [ r ] ->
      Alcotest.(check int) "index" 0 r.call_index;
      Alcotest.(check string) "content" "18 degrees Celsius, cloudy" r.content;
      Alcotest.(check bool) "is_error" false r.is_error
  | _ -> Alcotest.fail "expected one result"

let test_tool_results_order () =
  match A.tool_results (two_calls ~a:"A" ~b:"B" ~out_a:"B" ~out_b:"A") with
  | [ r1; r2 ] ->
      Alcotest.(check (pair int string))
        "first" (1, "x")
        (r1.call_index, r1.content);
      Alcotest.(check (pair int string))
        "second" (0, "y")
        (r2.call_index, r2.content)
  | _ -> Alcotest.fail "expected two results"

let test_tool_results_none () =
  Alcotest.(check int)
    "none" 0
    (List.length (A.tool_results (request "luna_stream")))

let call id =
  Printf.sprintf
    {|{"type":"function_call","call_id":%S,"name":"f","arguments":"{}"}|} id

let output id text =
  Printf.sprintf {|{"type":"function_call_output","call_id":%S,"output":%S}|} id
    text

let input_of items =
  json (Printf.sprintf {|{"model":"m","input":[%s]}|} (String.concat "," items))

let indices req =
  List.map (fun (r : Protocol.tool_result) -> r.call_index) (A.tool_results req)

let canonical_output_ids req =
  list_exn (get "input" (canonical req))
  |> List.filter_map (fun it ->
      if String.equal (str "type" it) "function_call_output" then
        Some (get "call_id" it)
      else None)

let test_linking () =
  let reuse =
    input_of [ call "x"; output "x" "r1"; call "x"; output "x" "r2" ]
  in
  Alcotest.(check (list int)) "reuse across turns" [ 0; 1 ] (indices reuse);
  Alcotest.(check (list canon_t))
    "canonical agrees"
    [ Canon.int 0; Canon.int 1 ]
    (canonical_output_ids reuse);
  let parallel = input_of [ call ""; call ""; output "" "a"; output "" "b" ] in
  Alcotest.(check (list int)) "equal ids in order" [ 0; 1 ] (indices parallel);
  let ghost = input_of [ output "ghost" "r" ] in
  Alcotest.(check (list int)) "unlinked left out" [] (indices ghost);
  Alcotest.(check (list canon_t))
    "unlinked is null in the key" [ Canon.null ]
    (canonical_output_ids ghost)

let test_encrypted_content () =
  let req blob =
    json
      (Printf.sprintf
         {|{"model":"m","input":[
            {"type":"reasoning","id":"rs_1","summary":[],"encrypted_content":%S},
            {"type":"message","role":"user","content":"hi","encrypted_content":"keep"}]}|}
         blob)
  in
  Alcotest.check canon_t "blob of a reasoning item does not matter"
    (canonical (req "A"))
    (canonical (req "B"));
  Alcotest.(check bool)
    "other items keep the field" true
    (contains ~sub:{|"encrypted_content":"keep"|}
       (Canon.to_string (canonical (req "A"))))

let test_usage_and_truncation () =
  List.iter
    (fun n -> check_usage_relations n (summarise_ok (body_of n)))
    fixtures;
  check_truncation "luna_stream" A.summarise (body_of "luna_stream")

(* Hand-written stream edge cases *)

let sse_of text = feed_all [ text ]

let ev ty extra =
  Printf.sprintf "event: %s\ndata: {\"type\":\"%s\"%s}\n\n" ty ty extra

let resp ?(status = "completed") ?(extra = "") () =
  Printf.sprintf
    {|,"response":{"id":"resp_1","model":"gpt-6-luna","status":"%s","output":[{"id":"msg_1","type":"message","content":[{"type":"output_text","text":"Hello"},{"type":"output_text","text":" world"}]}],"usage":{"input_tokens":10,"input_tokens_details":{"cached_tokens":4},"output_tokens":5,"output_tokens_details":{"reasoning_tokens":2}}%s}|}
    status extra

let test_text_and_usage () =
  let s =
    summarise_ok
      (Protocol.Sse
         (sse_of
            (ev "response.created" (resp ~status:"in_progress" ())
            ^ ev "response.completed" (resp ()))))
  in
  Alcotest.(check string) "text" "Hello world" s.text;
  match s.usage with
  | Some u ->
      Alcotest.(check (list (option int)))
        "details" [ Some 4; None; Some 2 ]
        [ u.cached_input_tokens; u.cache_write_tokens; u.reasoning_tokens ]
  | None -> Alcotest.fail "usage"

let test_keepalive_and_done () =
  let plain =
    ev "response.created" (resp ~status:"in_progress" ())
    ^ ev "response.completed" (resp ())
  in
  let noisy =
    ": keep-alive\n\n"
    ^ ev "response.created" (resp ~status:"in_progress" ())
    ^ ": ping\n\n"
    ^ ev "response.completed" (resp ())
    ^ "data: [DONE]\n\n"
  in
  Alcotest.check summary_t "same"
    (summarise_ok (Protocol.Sse (sse_of plain)))
    (summarise_ok (Protocol.Sse (sse_of noisy)))

let test_error_frame () =
  let s =
    summarise_ok
      (Protocol.Sse
         (sse_of
            (ev "response.created" (resp ~status:"in_progress" ())
            ^ ev "error"
                {|,"code":"server_error","message":"boom","sequence_number":3|}
            )))
  in
  Alcotest.(check (option string)) "error" (Some "boom") s.error;
  Alcotest.(check (option string)) "stop" None s.stop_reason

let test_failed () =
  let s =
    summarise_ok
      (Protocol.Sse
         (sse_of
            (ev "response.failed"
               (resp ~status:"failed"
                  ~extra:{|,"error":{"code":"x","message":"overloaded"}|} ()))))
  in
  Alcotest.(check (option string)) "error" (Some "overloaded") s.error;
  Alcotest.(check (option string)) "stop" (Some "failed") s.stop_reason

let test_truncated () =
  let s =
    summarise_ok
      (Protocol.Sse
         (sse_of (ev "response.created" (resp ~status:"in_progress" ()))))
  in
  Alcotest.(check bool) "flagged" true (Option.is_some s.error)

let test_empty_stream () =
  Alcotest.(check bool)
    "flagged" true
    (Option.is_some (summarise_ok (Protocol.Sse [])).error)

let test_unknown_events () =
  let s =
    summarise_ok
      (Protocol.Sse
         (sse_of
            (ev "response.future_thing" {|,"x":1|}
            ^ ev "response.completed" (resp ()))))
  in
  Alcotest.(check string) "text" "Hello world" s.text

let test_json_error_body () =
  let s =
    summarise_ok
      (Protocol.Json
         (json
            {|{"error":{"message":"bad key","type":"invalid_request_error"}}|}))
  in
  Alcotest.(check (option string)) "error" (Some "bad key") s.error

let test_bad_arguments () =
  match
    A.summarise
      (Protocol.Json
         (json
            {|{"output":[{"type":"function_call","name":"f","arguments":"{oops"}]}|}))
  with
  | Error (Protocol.Malformed _) -> ()
  | Ok _ -> Alcotest.fail "expected error"

let test_bad_sse_json () =
  match A.summarise (Protocol.Sse (sse_of "data: {nope\n\n")) with
  | Error (Protocol.Malformed _) -> ()
  | Ok _ -> Alcotest.fail "expected error"

let test_json_stream_rejected () =
  match A.summarise (Protocol.Json_stream []) with
  | Error (Protocol.Malformed _) -> ()
  | Ok _ -> Alcotest.fail "expected error"

let test_matches () =
  Alcotest.(check bool)
    "get" false
    (A.matches ~meth:"GET" ~path:"/v1/responses");
  Alcotest.(check bool)
    "lower-case method" false
    (A.matches ~meth:"post" ~path:"/v1/responses");
  Alcotest.(check bool)
    "other" false
    (A.matches ~meth:"POST" ~path:"/v1/chat/completions")

let test_custom_call_ids () =
  let req id =
    json
      (Printf.sprintf
         {|{"model":"m","input":[{"type":"custom_tool_call","call_id":"%s","name":"t","input":"x"},{"type":"custom_tool_call_output","call_id":"%s","output":"ok"}]}|}
         id id)
  in
  let canon r = ok_exn (A.canonical_request ~path:"/v1/responses" r) in
  Alcotest.(check bool)
    "renaming a custom tool call id keeps the key" true
    (Canon.equal (canon (req "call_aaa")) (canon (req "call_bbb")));
  Alcotest.(check int)
    "its output links to the call" 1
    (List.length (A.tool_results (req "call_aaa")))

let () =
  let c n f = Alcotest.test_case n `Quick f in
  Alcotest.run "openai_responses"
    [
      ( "fixtures",
        List.map (fun n -> c n (test_fixture n)) fixtures
        @ [
            c "split invariance" test_split_invariance;
            c "stream vs nonstream" test_stream_matches_nonstream;
            c "matches" test_matches;
          ] );
      ( "canonical_request",
        [
          c "ignored removed" test_ignored_removed;
          c "kept fields" test_kept_fields;
          QCheck_alcotest.to_alcotest prop_ids;
          c "pairing" test_pairing;
          c "custom tool call ids" test_custom_call_ids;
          c "item_reference kept" test_item_reference_kept;
          c "stream_options pruned" test_stream_options_pruned;
          c "non-object" test_non_object;
          c "encrypted_content" test_encrypted_content;
        ] );
      ( "tool_results",
        [
          c "turn2" test_tool_results;
          c "order" test_tool_results_order;
          c "none" test_tool_results_none;
          c "linking" test_linking;
          c "usage and truncation" test_usage_and_truncation;
        ] );
      ( "edge cases",
        [
          c "text and usage" test_text_and_usage;
          c "keep-alive and DONE" test_keepalive_and_done;
          c "error frame" test_error_frame;
          c "response.failed" test_failed;
          c "truncated" test_truncated;
          c "empty stream" test_empty_stream;
          c "unknown events" test_unknown_events;
          c "json error body" test_json_error_body;
          c "bad arguments" test_bad_arguments;
          c "bad sse json" test_bad_sse_json;
          c "json stream rejected" test_json_stream_rejected;
        ] );
    ]
