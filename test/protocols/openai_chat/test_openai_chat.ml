open Bactrian_core
open Protocol_test_support
module P = Openai_chat

let fixtures =
  [
    "luna_stream";
    "luna_nonstream";
    "luna_turn2";
    "nano_stream";
    "openrouter_nemotron_stream";
  ]

let file = fixture ~protocol:"openai_chat"
let request name = json (file name "request.json")
let raw name = file name "response.raw"
let meta name = json (file name "meta.json")
let str_of = str

let pieces_recorded name =
  recorded_pieces ~raw:(raw name) ~chunks:(file name "chunks.json")

let pieces_bytes name = single_bytes (raw name)

let body_of name =
  if P.streaming ~path:"" ~query:"" (request name) then
    Protocol.Sse (sse_items (pieces_recorded name))
  else Protocol.Json (json (raw name))

let summary_exn b = ok_exn (P.summarise b)

let show (s : Protocol.summary) =
  let opt f = function None -> "-" | Some x -> f x in
  let u (u : Trace.usage) =
    Printf.sprintf "%d/%d/%s/%s/%s" u.input_tokens u.output_tokens
      (opt string_of_int u.cached_input_tokens)
      (opt string_of_int u.cache_write_tokens)
      (opt string_of_int u.reasoning_tokens)
  in
  Printf.sprintf "model=%s text=%S calls=[%s] usage=%s stop=%s err=%s"
    (opt Fun.id s.model) s.text
    (String.concat ";"
       (List.map
          (fun (c : Protocol.tool_call) -> c.name ^ Canon.to_string c.arguments)
          s.tool_calls))
    (opt u s.usage) (opt Fun.id s.stop_reason) (opt Fun.id s.error)

let canon_exn ?(path = "/v1/chat/completions") r =
  match P.canonical_request ~path r with
  | Ok c -> c
  | Error e -> Alcotest.failf "%a" Protocol.pp_error e

(* Fixture tests *)

let test_fixture name () =
  let m = meta name in
  Alcotest.(check bool)
    "matches" true
    (P.matches ~meth:(str_of "method" m) ~path:(str_of "path" m));
  let req = request name in
  let is_stream =
    String.equal
      (str_of "content-type" (get "response_headers" m))
      "text/event-stream; charset=utf-8"
    || String.equal "text/event-stream"
         (str_of "content-type" (get "response_headers" m))
  in
  Alcotest.(check bool)
    "streaming agrees with capture" is_stream
    (P.streaming ~path:(str_of "path" m) ~query:(str_of "query" m) req);
  let s = summary_exn (body_of name) in
  Alcotest.(check bool) "model" true (Option.is_some s.model);
  Alcotest.(check (option string)) "no error" None s.error;
  Alcotest.(check bool) "stop reason" true (Option.is_some s.stop_reason);
  Alcotest.(check bool)
    "text or tool calls" true
    (s.text <> "" || s.tool_calls <> []);
  (match s.usage with
  | Some u ->
      Alcotest.(check bool)
        "usage" true
        (u.input_tokens > 0 && u.output_tokens > 0)
  | None -> Alcotest.fail "no usage");
  check_usage_relations name s;
  if not (String.equal name "luna_turn2") then
    match s.tool_calls with
    | [ c ] ->
        Alcotest.(check string) "tool name" "get_weather" c.name;
        Alcotest.check canon_t "arguments"
          (obj [ ("city", Canon.string "Paris") ])
          c.arguments
    | _ -> Alcotest.fail "expected one tool call"

let test_expected () =
  let s = summary_exn (body_of "luna_stream") in
  Alcotest.(check (option string)) "stop" (Some "tool_calls") s.stop_reason;
  Alcotest.(check (option string)) "model" (Some "gpt-6-luna") s.model;
  (match s.usage with
  | Some u ->
      Alcotest.(check int) "in" 145 u.input_tokens;
      Alcotest.(check int) "out" 17 u.output_tokens;
      Alcotest.(check (option int)) "cached" (Some 0) u.cached_input_tokens
  | None -> Alcotest.fail "usage");
  let t = summary_exn (body_of "luna_turn2") in
  Alcotest.(check string)
    "text" "It\xe2\x80\x99s 18\xc2\xb0C and cloudy in Paris right now." t.text;
  let n = summary_exn (body_of "openrouter_nemotron_stream") in
  (match n.usage with
  | Some u ->
      Alcotest.(check (option int)) "reasoning" (Some 12) u.reasoning_tokens
  | None -> Alcotest.fail "usage");
  Alcotest.(check string) "reasoning not in text" "" n.text

let test_split_invariance name () =
  let a =
    show (summary_exn (Protocol.Sse (sse_items (pieces_recorded name))))
  in
  let b = show (summary_exn (Protocol.Sse (sse_items (pieces_bytes name)))) in
  let c = show (summary_exn (Protocol.Sse (sse_items [ raw name ]))) in
  Alcotest.(check string) "bytes" a b;
  Alcotest.(check string) "whole" a c

(* canonical_request *)

let test_ignored_removed () =
  let base = request "luna_stream" in
  let expected = canon_exn base in
  List.iter
    (fun (p, _) ->
      let with_field = set_path p (Canon.string "x") base in
      Alcotest.check canon_t ("ignored " ^ p) expected (canon_exn with_field))
    P.ignored_fields

let test_ignored_all_at_once () =
  let base = request "luna_stream" in
  let all =
    List.fold_left
      (fun v (p, _) -> set_path p (Canon.int 1) v)
      base P.ignored_fields
  in
  Alcotest.check canon_t "all ignored" (canon_exn base) (canon_exn all)

let test_unknown_kept () =
  let base = request "luna_stream" in
  let with_unknown = set_path "frobnicate" (Canon.int 1) base in
  Alcotest.(check bool)
    "differs" false
    (Canon.equal (canon_exn base) (canon_exn with_unknown));
  let nested = set_path "stream_options.future_flag" (Canon.bool true) base in
  Alcotest.(check bool)
    "nested unknown differs" false
    (Canon.equal (canon_exn base) (canon_exn nested));
  let model = set_path "model" (Canon.string "other") base in
  Alcotest.(check bool)
    "model in key" false
    (Canon.equal (canon_exn base) (canon_exn model))

let test_routing_and_store_kept () =
  (* These change which model answers, what it sees or how it is billed, so
     they stay in the key. *)
  let base = request "openrouter_nemotron_stream" in
  List.iter
    (fun p ->
      Alcotest.(check bool)
        (p ^ " kept") false
        (Canon.equal (canon_exn base)
           (canon_exn (set_path p (Canon.string "x") base)));
      Alcotest.(check bool)
        (p ^ " not ignored") false
        (List.mem_assoc p P.ignored_fields))
    [ "provider"; "models"; "route"; "plugins"; "debug"; "store" ]

let test_ids_replaced () =
  let c = canon_exn (request "luna_turn2") in
  let s = Canon.to_string c in
  Alcotest.(check bool) "no call id" false (contains ~sub:"call_akD6" s);
  Alcotest.(check bool)
    "positions" true
    (contains ~sub:"\"tool_call_id\":0" s && contains ~sub:"\"id\":0" s)

let two_call_request a b =
  json
    (Printf.sprintf
       {|{"model":"m","messages":[
          {"role":"user","content":"hi"},
          {"role":"assistant","content":null,"tool_calls":[
            {"id":%S,"type":"function","function":{"name":"f","arguments":"{}"}},
            {"id":%S,"type":"function","function":{"name":"g","arguments":"{}"}}]},
          {"role":"tool","tool_call_id":%S,"content":"second"},
          {"role":"tool","tool_call_id":%S,"content":"first"}]}|}
       a b b a)

let rec rename tbl v =
  Canon.map_items (rename tbl)
    (Canon.map_fields
       (fun k x ->
         match (k, Canon.to_string_opt x) with
         | ("id" | "tool_call_id"), Some s ->
             Canon.string (Option.value (List.assoc_opt s tbl) ~default:s)
         | _ -> rename tbl x)
       v)

let prop_ids =
  QCheck.Test.make ~name:"renaming IDs leaves the key unchanged" ~count:200
    QCheck.(pair string_printable string_printable)
    (fun (x, y) ->
      let x = "call_" ^ x and y = "call_" ^ y ^ "!" in
      let two = two_call_request "call_A" "call_B" in
      let one = request "luna_turn2" in
      let tbl2 = [ ("call_A", x); ("call_B", y) ] in
      let tbl1 = [ ("call_akD6AqG6oUnDOsFQf92S7F9Z", x) ] in
      Canon.equal (canon_exn two) (canon_exn (rename tbl2 two))
      && Canon.equal (canon_exn one) (canon_exn (rename tbl1 one))
      && P.tool_results two = P.tool_results (rename tbl2 two)
      && P.tool_results one = P.tool_results (rename tbl1 one))

let test_ids_distinct_calls () =
  (* Swapping which call a result answers must change the key. *)
  let a = two_call_request "call_A" "call_B" in
  let swapped =
    json
      {|{"model":"m","messages":[
          {"role":"user","content":"hi"},
          {"role":"assistant","content":null,"tool_calls":[
            {"id":"call_A","type":"function","function":{"name":"f","arguments":"{}"}},
            {"id":"call_B","type":"function","function":{"name":"g","arguments":"{}"}}]},
          {"role":"tool","tool_call_id":"call_A","content":"second"},
          {"role":"tool","tool_call_id":"call_B","content":"first"}]}|}
  in
  Alcotest.(check bool)
    "differs" false
    (Canon.equal (canon_exn a) (canon_exn swapped))

let test_tool_results () =
  (match P.tool_results (request "luna_turn2") with
  | [ r ] ->
      Alcotest.(check int) "index" 0 r.call_index;
      Alcotest.(check string) "content" "18 degrees Celsius, cloudy" r.content;
      Alcotest.(check bool) "not error" false r.is_error
  | _ -> Alcotest.fail "expected one result");
  let rs = P.tool_results (two_call_request "call_A" "call_B") in
  Alcotest.(check (list int))
    "indices" [ 1; 0 ]
    (List.map (fun (r : Protocol.tool_result) -> r.call_index) rs);
  Alcotest.(check int)
    "none in first turn" 0
    (List.length (P.tool_results (request "luna_stream")));
  let parts =
    json
      {|{"messages":[{"role":"assistant","tool_calls":[{"id":"a"}]},{"role":"tool","tool_call_id":"a","content":[{"type":"text","text":"x"},{"type":"text","text":"y"}]}]}|}
  in
  Alcotest.(check (list string))
    "text parts" [ "xy" ]
    (List.map
       (fun (r : Protocol.tool_result) -> r.content)
       (P.tool_results parts))

let history calls_and_results =
  let call id =
    Printf.sprintf
      {|{"role":"assistant","content":null,"tool_calls":[{"id":%S,"type":"function","function":{"name":"f","arguments":"{}"}}]}|}
      id
  in
  let result id text =
    Printf.sprintf {|{"role":"tool","tool_call_id":%S,"content":%S}|} id text
  in
  json
    (Printf.sprintf
       {|{"model":"m","messages":[{"role":"user","content":"q"},%s]}|}
       (String.concat ","
          (List.map
             (function `Call id -> call id | `Result (id, t) -> result id t)
             calls_and_results)))

let result_indices req =
  List.map (fun (r : Protocol.tool_result) -> r.call_index) (P.tool_results req)

(* The canonical form of every tool message points at the call that
   [tool_results] reports. *)
let canonical_result_ids req =
  list_exn (get "messages" (canon_exn req))
  |> List.filter_map (fun m ->
      if String.equal (str "role" m) "tool" then Some (get "tool_call_id" m)
      else None)

let test_equal_ids_link_in_order () =
  let two_in_one =
    json
      {|{"model":"m","messages":[{"role":"user","content":"q"},
         {"role":"assistant","content":null,"tool_calls":[
           {"id":"x","type":"function","function":{"name":"f","arguments":"{}"}},
           {"id":"x","type":"function","function":{"name":"g","arguments":"{}"}}]},
         {"role":"tool","tool_call_id":"x","content":"ra"},
         {"role":"tool","tool_call_id":"x","content":"rb"}]}|}
  in
  Alcotest.(check (list int))
    "same id, one message" [ 0; 1 ]
    (result_indices two_in_one);
  Alcotest.(check (list canon_t))
    "canonical agrees"
    [ Canon.int 0; Canon.int 1 ]
    (canonical_result_ids two_in_one);
  let empty_ids =
    json
      {|{"model":"m","messages":[
         {"role":"assistant","content":null,"tool_calls":[
           {"id":"","type":"function","function":{"name":"f","arguments":"{}"}},
           {"id":"","type":"function","function":{"name":"g","arguments":"{}"}}]},
         {"role":"tool","tool_call_id":"","content":"ra"},
         {"role":"tool","tool_call_id":"","content":"rb"}]}|}
  in
  Alcotest.(check (list int)) "empty ids" [ 0; 1 ] (result_indices empty_ids)

let test_reused_ids_across_turns () =
  let req =
    history [ `Call "x"; `Result ("x", "r1"); `Call "x"; `Result ("x", "r2") ]
  in
  Alcotest.(check (list int))
    "latest unanswered call" [ 0; 1 ] (result_indices req);
  Alcotest.(check (list canon_t))
    "canonical agrees"
    [ Canon.int 0; Canon.int 1 ]
    (canonical_result_ids req)

let test_unlinked_result () =
  let req = history [ `Result ("ghost", "r"); `Call "x"; `Result ("x", "r") ] in
  Alcotest.(check (list int)) "left out" [ 0 ] (result_indices req);
  Alcotest.(check (list canon_t))
    "null in the key, raw id not leaked"
    [ Canon.null; Canon.int 0 ]
    (canonical_result_ids req)

let test_matches () =
  Alcotest.(check bool)
    "openai" true
    (P.matches ~meth:"POST" ~path:"/v1/chat/completions");
  Alcotest.(check bool)
    "openrouter" true
    (P.matches ~meth:"POST" ~path:"/api/v1/chat/completions");
  Alcotest.(check bool)
    "get" false
    (P.matches ~meth:"GET" ~path:"/v1/chat/completions");
  Alcotest.(check bool)
    "responses" false
    (P.matches ~meth:"POST" ~path:"/v1/responses")

(* Edge cases *)

let ev data = Sse.Event { event = None; data; id = None; retry = None }

let chunk_text t =
  ev
    (Printf.sprintf
       {|{"model":"m","choices":[{"index":0,"delta":{"content":%S},"finish_reason":null}]}|}
       t)

let test_comments_ignored () =
  let plain = [ chunk_text "a"; chunk_text "b"; ev "[DONE]" ] in
  let noisy =
    [
      Sse.Comment " OPENROUTER PROCESSING";
      chunk_text "a";
      Sse.Comment " OPENROUTER PROCESSING";
      chunk_text "b";
      ev "[DONE]";
    ]
  in
  Alcotest.(check string)
    "same"
    (show (summary_exn (Protocol.Sse plain)))
    (show (summary_exn (Protocol.Sse noisy)));
  Alcotest.(check string) "text" "ab" (summary_exn (Protocol.Sse plain)).text

let test_error_frames () =
  let top =
    [
      chunk_text "partial";
      ev
        {|{"error":{"code":502,"message":"upstream died"},"choices":[{"index":0,"delta":{"content":""},"finish_reason":"error"}]}|};
      ev "[DONE]";
    ]
  in
  let s = summary_exn (Protocol.Sse top) in
  Alcotest.(check (option string)) "error" (Some "upstream died") s.error;
  Alcotest.(check string) "keeps partial text" "partial" s.text;
  let fr =
    [ ev {|{"choices":[{"index":0,"delta":{},"finish_reason":"error"}]}|} ]
  in
  let s = summary_exn (Protocol.Sse fr) in
  Alcotest.(check bool) "finish_reason error" true (Option.is_some s.error);
  let only = [ ev {|{"error":{"message":"rate limited","type":"x"}}|} ] in
  Alcotest.(check (option string))
    "error only" (Some "rate limited") (summary_exn (Protocol.Sse only)).error;
  let named =
    [
      Sse.Event { event = Some "error"; data = "boom"; id = None; retry = None };
    ]
  in
  Alcotest.(check (option string))
    "event: error" (Some "boom") (summary_exn (Protocol.Sse named)).error;
  let whole =
    summary_exn
      (Protocol.Json (json {|{"error":{"message":"bad key","code":401}}|}))
  in
  Alcotest.(check (option string)) "json error" (Some "bad key") whole.error

let test_done_stops () =
  let s =
    summary_exn
      (Protocol.Sse [ chunk_text "a"; ev "[DONE]"; chunk_text "ignored" ])
  in
  Alcotest.(check string) "text" "a" s.text

let test_truncated_stream () =
  let s = summary_exn (Protocol.Sse [ chunk_text "a" ]) in
  Alcotest.(check bool) "flagged" true (Option.is_some s.error);
  Alcotest.(check string) "partial text kept" "a" s.text;
  Alcotest.(check bool)
    "empty stream flagged" true
    (Option.is_some (summary_exn (Protocol.Sse [])).error);
  let finished =
    summary_exn
      (Protocol.Sse
         [
           chunk_text "a";
           ev {|{"choices":[{"index":0,"delta":{},"finish_reason":"stop"}]}|};
         ])
  in
  Alcotest.(check (option string))
    "a finish_reason completes it without [DONE]" None finished.error

let test_fixture_truncation name () =
  check_truncation name P.summarise (body_of name)

let test_extra_choices_ignored () =
  let s =
    summary_exn
      (Protocol.Json
         (json
            {|{"model":"m","choices":[
               {"index":0,"message":{"content":"zero"},"finish_reason":"stop"},
               {"index":1,"message":{"content":"one"},"finish_reason":"stop"}]}|}))
  in
  Alcotest.(check string) "only choice 0 is summarised" "zero" s.text

let test_malformed () =
  (match P.summarise (Protocol.Sse [ ev "not json" ]) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error");
  (match P.summarise (Protocol.Json (json {|{"hello":1}|})) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error");
  (match P.summarise (Protocol.Json_stream []) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error");
  (match P.canonical_request ~path:"" (Canon.list []) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error");
  match
    P.summarise
      (Protocol.Sse
         [
           ev
             {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"x","function":{"name":"f","arguments":"{\"a"}}]}}]}|};
         ])
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error on truncated arguments"

let test_parallel_calls () =
  let s =
    summary_exn
      (Protocol.Sse
         [
           ev
             {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"a","function":{"name":"f","arguments":""}},{"index":1,"id":"b","function":{"name":"g","arguments":""}}]}}]}|};
           ev
             {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":1,"function":{"arguments":"{\"y\":2}"}}]}}]}|};
           ev
             {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"{\"x\":1}"}}]}}]}|};
         ])
  in
  Alcotest.(check (list string))
    "order and args"
    [ "f{\"x\":1}"; "g{\"y\":2}" ]
    (List.map
       (fun (c : Protocol.tool_call) -> c.name ^ Canon.to_string c.arguments)
       s.tool_calls)

let test_obfuscation_irrelevant () =
  let a =
    ev
      {|{"model":"m","choices":[{"index":0,"delta":{"content":"a"}}],"obfuscation":"zzz"}|}
  in
  let b =
    ev {|{"model":"m","choices":[{"index":0,"delta":{"content":"a"}}]}|}
  in
  Alcotest.(check string)
    "same"
    (show (summary_exn (Protocol.Sse [ a ])))
    (show (summary_exn (Protocol.Sse [ b ])))

let test_openrouter_repeated_finish () =
  let s = summary_exn (body_of "openrouter_nemotron_stream") in
  Alcotest.(check (option string)) "stop" (Some "tool_calls") s.stop_reason;
  Alcotest.(check int) "one call" 1 (List.length s.tool_calls)

let () =
  Alcotest.run "openai_chat"
    [
      ( "fixtures",
        List.map
          (fun n -> Alcotest.test_case n `Quick (test_fixture n))
          fixtures
        @ [ Alcotest.test_case "expected values" `Quick test_expected ] );
      ( "split invariance",
        List.map
          (fun n -> Alcotest.test_case n `Quick (test_split_invariance n))
          [ "luna_stream"; "nano_stream"; "openrouter_nemotron_stream" ] );
      ( "truncation",
        List.map
          (fun n -> Alcotest.test_case n `Quick (test_fixture_truncation n))
          [ "luna_stream"; "nano_stream"; "openrouter_nemotron_stream" ] );
      ( "canonical_request",
        [
          Alcotest.test_case "matches" `Quick test_matches;
          Alcotest.test_case "ignored fields removed" `Quick
            test_ignored_removed;
          Alcotest.test_case "all ignored at once" `Quick
            test_ignored_all_at_once;
          Alcotest.test_case "unknown fields kept" `Quick test_unknown_kept;
          Alcotest.test_case "routing fields and store kept" `Quick
            test_routing_and_store_kept;
          Alcotest.test_case "ids replaced by positions" `Quick
            test_ids_replaced;
          Alcotest.test_case "results stay matched to calls" `Quick
            test_ids_distinct_calls;
          QCheck_alcotest.to_alcotest prop_ids;
        ] );
      ( "tool_results",
        [
          Alcotest.test_case "history" `Quick test_tool_results;
          Alcotest.test_case "equal ids link in order" `Quick
            test_equal_ids_link_in_order;
          Alcotest.test_case "ids reused across turns" `Quick
            test_reused_ids_across_turns;
          Alcotest.test_case "unlinked result" `Quick test_unlinked_result;
        ] );
      ( "edge cases",
        [
          Alcotest.test_case "keep-alive comments" `Quick test_comments_ignored;
          Alcotest.test_case "error frames" `Quick test_error_frames;
          Alcotest.test_case "[DONE] stops" `Quick test_done_stops;
          Alcotest.test_case "truncated stream" `Quick test_truncated_stream;
          Alcotest.test_case "choices after 0 are ignored" `Quick
            test_extra_choices_ignored;
          Alcotest.test_case "malformed input" `Quick test_malformed;
          Alcotest.test_case "parallel tool calls" `Quick test_parallel_calls;
          Alcotest.test_case "obfuscation irrelevant" `Quick
            test_obfuscation_irrelevant;
          Alcotest.test_case "openrouter repeated finish" `Quick
            test_openrouter_repeated_finish;
        ] );
    ]
