open Bactrian_core
open Protocol_test_support
module G = Gemini_generate

let fixture_file = fixture ~protocol:"gemini_generate"
let obj fs = Result.get_ok (Canon.obj fs)

let contains sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let fixtures =
  [
    "flash_generate";
    "flash_stream_sse";
    "flash_stream_json";
    "flash_turn2";
    "pro_generate";
  ]

type fixture = {
  meth : string;
  path : string;
  query : string;
  model : string;
  request : Canon.t;
  raw : string;
  chunks : string;
}

let load name =
  let meta = json (fixture_file name "meta.json") in
  {
    meth = str "method" meta;
    path = str "path" meta;
    query = str "query" meta;
    model = str "model" meta;
    request = json (fixture_file name "request.json");
    raw = fixture_file name "response.raw";
    chunks = fixture_file name "chunks.json";
  }

let is_sse f = String.equal f.query "alt=sse"
let is_stream f = String.ends_with ~suffix:":streamGenerateContent" f.path
let pieces f = recorded_pieces ~raw:f.raw ~chunks:f.chunks

let body_of f =
  if is_sse f then Protocol.Sse (sse_items (pieces f))
  else if is_stream f then Protocol.Json_stream (list_exn (json f.raw))
  else Protocol.Json (json f.raw)

let summary_of body =
  match G.summarise body with
  | Ok s -> s
  | Error e -> Alcotest.failf "summarise: %a" Protocol.pp_error e

let canonical ~path req =
  match G.canonical_request ~path req with
  | Ok c -> c
  | Error e -> Alcotest.failf "canonical_request: %a" Protocol.pp_error e

let gen_path = "/v1beta/models/gemini-3.8-flash:generateContent"
let stream_path = "/v1beta/models/gemini-3.8-flash:streamGenerateContent"

let summary_testable =
  Alcotest.testable
    (fun fmt (s : Protocol.summary) ->
      Format.fprintf fmt "{model=%s text=%S calls=%d err=%s}"
        (Option.value ~default:"-" s.model)
        s.text (List.length s.tool_calls)
        (Option.value ~default:"-" s.error))
    (fun (a : Protocol.summary) (b : Protocol.summary) ->
      Option.equal String.equal a.model b.model
      && String.equal a.text b.text
      && List.equal
           (fun (x : Protocol.tool_call) (y : Protocol.tool_call) ->
             String.equal x.name y.name && Canon.equal x.arguments y.arguments)
           a.tool_calls b.tool_calls
      && Option.equal ( = ) a.usage b.usage
      && Option.equal String.equal a.stop_reason b.stop_reason
      && Option.equal String.equal a.error b.error)

(* Fixture tests *)

let fixture_tests =
  List.concat_map
    (fun name ->
      let f = load name in
      let asks_for_tool = not (String.equal name "flash_turn2") in
      [
        Alcotest.test_case (name ^ ": matches") `Quick (fun () ->
            Alcotest.(check bool)
              "matches" true
              (G.matches ~meth:f.meth ~path:f.path));
        Alcotest.test_case (name ^ ": streaming") `Quick (fun () ->
            Alcotest.(check bool)
              "streaming" (is_stream f)
              (G.streaming ~path:f.path ~query:f.query f.request));
        Alcotest.test_case (name ^ ": summary") `Quick (fun () ->
            let s = summary_of (body_of f) in
            Alcotest.(check (option string)) "model" (Some f.model) s.model;
            Alcotest.(check (option string)) "no error" None s.error;
            Alcotest.(check (option string)) "stop" (Some "STOP") s.stop_reason;
            Alcotest.(check bool)
              "text or calls" true
              (s.text <> "" || s.tool_calls <> []);
            (match s.usage with
            | Some u ->
                Alcotest.(check bool) "input" true (u.input_tokens > 0);
                Alcotest.(check bool) "output" true (u.output_tokens > 0)
            | None -> Alcotest.fail "no usage");
            if asks_for_tool then
              match s.tool_calls with
              | [ c ] ->
                  Alcotest.(check string) "name" "get_weather" c.name;
                  Alcotest.check canon_t "args"
                    (obj [ ("city", Canon.string "Paris") ])
                    c.arguments
              | _ -> Alcotest.fail "expected one tool call"
            else Alcotest.(check bool) "text" true (s.text <> ""));
      ])
    fixtures

let usage_tests =
  [
    Alcotest.test_case "output tokens include thoughts" `Quick (fun () ->
        let s = summary_of (body_of (load "flash_generate")) in
        match s.usage with
        | Some u ->
            Alcotest.(check int) "input" 69 u.input_tokens;
            Alcotest.(check int) "output" 56 u.output_tokens;
            Alcotest.(check (option int))
              "reasoning" (Some 40) u.reasoning_tokens;
            Alcotest.(check (option int)) "cached" None u.cached_input_tokens
        | None -> Alcotest.fail "no usage");
    Alcotest.test_case "sse usage is the last chunk's, not summed" `Quick
      (fun () ->
        let s = summary_of (body_of (load "flash_stream_sse")) in
        match s.usage with
        | Some u ->
            Alcotest.(check int) "input" 69 u.input_tokens;
            Alcotest.(check int) "output" 67 u.output_tokens
        | None -> Alcotest.fail "no usage");
  ]

(* Split invariance and the three streaming shapes *)

let split_tests =
  [
    Alcotest.test_case "sse: recorded chunks, single bytes and whole agree"
      `Quick (fun () ->
        let f = load "flash_stream_sse" in
        let recorded = summary_of (Protocol.Sse (sse_items (pieces f))) in
        let bytes =
          summary_of (Protocol.Sse (sse_items (single_bytes f.raw)))
        in
        let whole = summary_of (Protocol.Sse (sse_items [ f.raw ])) in
        Alcotest.check summary_testable "bytes" recorded bytes;
        Alcotest.check summary_testable "whole" recorded whole);
    Alcotest.test_case "json array stream is read as Json_stream" `Quick
      (fun () ->
        let f = load "flash_stream_json" in
        Alcotest.(check int) "elements" 2 (List.length (list_exn (json f.raw)));
        let s = summary_of (body_of f) in
        Alcotest.(check int) "calls" 1 (List.length s.tool_calls);
        Alcotest.(check (option string)) "stop" (Some "STOP") s.stop_reason);
    Alcotest.test_case "json array fixture chunks concatenate to the array"
      `Quick (fun () ->
        let f = load "flash_stream_json" in
        let joined = String.concat "" (pieces f) in
        Alcotest.(check string) "raw" f.raw joined);
  ]

(* Hand-written response edge cases *)

let chunk ?(text = "") ?finish ?usage () =
  let part = Printf.sprintf {|{"text":%S}|} text in
  let finish =
    match finish with
    | None -> ""
    | Some f -> Printf.sprintf {|,"finishReason":%S|} f
  in
  let usage =
    match usage with
    | None -> ""
    | Some (p, c) ->
        Printf.sprintf
          {|,"usageMetadata":{"promptTokenCount":%d,"candidatesTokenCount":%d}|}
          p c
  in
  json
    (Printf.sprintf
       {|{"candidates":[{"content":{"role":"model","parts":[%s]},"index":0%s}]%s,"modelVersion":"m"}|}
       part finish usage)

let sse_of_data ds =
  Protocol.Sse
    (List.map
       (fun d -> Sse.Event { event = None; data = d; id = None; retry = None })
       ds)

let edge_tests =
  [
    Alcotest.test_case "text is concatenated and usage is the last" `Quick
      (fun () ->
        let s =
          summary_of
            (Protocol.Json_stream
               [
                 chunk ~text:"Hel" ~usage:(5, 1) ();
                 chunk ~text:"lo" ~finish:"STOP" ~usage:(5, 2) ();
               ])
        in
        Alcotest.(check string) "text" "Hello" s.text;
        match s.usage with
        | Some u -> Alcotest.(check int) "out" 2 u.output_tokens
        | None -> Alcotest.fail "usage");
    Alcotest.test_case "comments between events do not change the summary"
      `Quick (fun () ->
        let ds =
          [
            Canon.to_string (chunk ~text:"a" ());
            Canon.to_string (chunk ~text:"b" ~finish:"STOP" ~usage:(3, 4) ());
          ]
        in
        let plain = sse_of_data ds in
        let noisy =
          match plain with
          | Protocol.Sse its ->
              Protocol.Sse
                (Sse.Comment " keep-alive"
                :: List.concat_map (fun i -> [ i; Sse.Comment "" ]) its)
          | Protocol.Json _ | Protocol.Json_stream _ -> plain
        in
        Alcotest.check summary_testable "same" (summary_of plain)
          (summary_of noisy));
    Alcotest.test_case "error frame after 200 sets error" `Quick (fun () ->
        let s =
          summary_of
            (sse_of_data
               [
                 Canon.to_string (chunk ~text:"a" ());
                 {|{"error":{"code":503,"message":"The model is overloaded.","status":"UNAVAILABLE"}}|};
               ])
        in
        Alcotest.(check bool) "error" true (Option.is_some s.error);
        Alcotest.(check string) "text kept" "a" s.text);
    Alcotest.test_case "whole-body error sets error" `Quick (fun () ->
        let s =
          summary_of
            (Protocol.Json
               (json
                  {|{"error":{"code":400,"message":"bad key","status":"INVALID_ARGUMENT"}}|}))
        in
        match s.error with
        | Some e ->
            Alcotest.(check bool) "mentions message" true (contains "bad key" e)
        | None -> Alcotest.fail "no error");
    Alcotest.test_case "blocked prompt sets error" `Quick (fun () ->
        let s =
          summary_of
            (Protocol.Json
               (json {|{"promptFeedback":{"blockReason":"SAFETY"}}|}))
        in
        Alcotest.(check bool) "error" true (Option.is_some s.error));
    Alcotest.test_case "thought parts are not assistant text" `Quick (fun () ->
        let s =
          summary_of
            (Protocol.Json
               (json
                  {|{"candidates":[{"content":{"parts":[{"text":"thinking","thought":true},{"text":"answer"}]}}]}|}))
        in
        Alcotest.(check string) "text" "answer" s.text);
    Alcotest.test_case "cached tokens are reported" `Quick (fun () ->
        let s =
          summary_of
            (Protocol.Json
               (json
                  {|{"candidates":[],"usageMetadata":{"promptTokenCount":10,"cachedContentTokenCount":4,"candidatesTokenCount":2}}|}))
        in
        match s.usage with
        | Some u ->
            Alcotest.(check (option int))
              "cached" (Some 4) u.cached_input_tokens
        | None -> Alcotest.fail "usage");
    Alcotest.test_case "functionCall without args gets empty arguments" `Quick
      (fun () ->
        let s =
          summary_of
            (Protocol.Json
               (json
                  {|{"candidates":[{"content":{"parts":[{"functionCall":{"name":"ping"}}]}}]}|}))
        in
        match s.tool_calls with
        | [ c ] -> Alcotest.check canon_t "args" (obj []) c.arguments
        | _ -> Alcotest.fail "one call");
    Alcotest.test_case "non-object chunk is malformed" `Quick (fun () ->
        match G.summarise (Protocol.Json_stream [ Canon.int 1 ]) with
        | Error (Protocol.Malformed _) -> ()
        | Ok _ -> Alcotest.fail "accepted");
    Alcotest.test_case "non-JSON event data is malformed" `Quick (fun () ->
        match G.summarise (sse_of_data [ "not json" ]) with
        | Error (Protocol.Malformed _) -> ()
        | Ok _ -> Alcotest.fail "accepted");
  ]

(* matches and streaming *)

let routing_tests =
  [
    Alcotest.test_case "matches accepts both verbs, rejects the rest" `Quick
      (fun () ->
        let m meth path = G.matches ~meth ~path in
        Alcotest.(check bool) "generate" true (m "POST" gen_path);
        Alcotest.(check bool) "stream" true (m "POST" stream_path);
        Alcotest.(check bool) "get" false (m "GET" gen_path);
        Alcotest.(check bool)
          "count" false
          (m "POST" "/v1beta/models/gemini-3.8-flash:countTokens");
        Alcotest.(check bool)
          "no model" false
          (m "POST" "/v1beta/models/:generateContent");
        Alcotest.(check bool)
          "no verb" false
          (m "POST" "/v1beta/models/gemini-3.8-flash");
        Alcotest.(check bool) "openai" false (m "POST" "/v1/chat/completions");
        Alcotest.(check bool)
          "interactions" false
          (m "POST" "/v1beta/interactions"));
    Alcotest.test_case "streaming follows the verb, with or without alt=sse"
      `Quick (fun () ->
        let s path query = G.streaming ~path ~query Canon.null in
        Alcotest.(check bool) "gen" false (s gen_path "");
        Alcotest.(check bool) "stream json" true (s stream_path "");
        Alcotest.(check bool) "stream sse" true (s stream_path "alt=sse"));
  ]

(* canonical_request *)

let rec map_strings f v =
  match Canon.to_string_opt v with
  | Some s -> Canon.string (f s)
  | None ->
      Canon.map_items (map_strings f)
        (Canon.map_fields (fun _ x -> map_strings f x) v)

let rename_ids pairs =
  map_strings (fun s ->
      match List.assoc_opt s pairs with Some n -> n | None -> s)

(* Insert [value] at a dotted path with [] segments, creating the leaf. *)
let rec insert_at segs value v =
  match segs with
  | [] -> v
  | seg :: rest ->
      let v = if Option.is_some (Canon.to_fields_opt v) then v else obj [] in
      let each = String.ends_with ~suffix:"[]" seg in
      let key =
        if each then String.sub seg 0 (String.length seg - 2) else seg
      in
      let child =
        match (Canon.member key v, rest) with
        | _, [] -> value
        | Some c, _ when each -> Canon.map_items (insert_at rest value) c
        | Some c, _ -> insert_at rest value c
        | None, _ -> Canon.null
      in
      Canon.set_member key child v

let two_call_request a b =
  json
    (Printf.sprintf
       {|{"contents":[
  {"role":"user","parts":[{"text":"weather in two cities"}]},
  {"role":"model","parts":[
    {"functionCall":{"name":"get_weather","args":{"city":"Paris"},"id":%S},"thoughtSignature":"sig1"},
    {"functionCall":{"name":"get_weather","args":{"city":"Rome"},"id":%S}}]},
  {"role":"user","parts":[
    {"functionResponse":{"name":"get_weather","response":{"result":"rome-result"},"id":%S}},
    {"functionResponse":{"name":"get_weather","response":{"result":"paris-result"},"id":%S}}]}
]}|}
       a b b a)

let id_gen =
  QCheck.make ~print:Fun.id
    QCheck.Gen.(
      map
        (fun s -> "call_" ^ s)
        (string_size ~gen:(char_range 'a' 'z') (int_range 1 12)))

let turn2 = lazy (load "flash_turn2")

let canonical_tests =
  [
    QCheck_alcotest.to_alcotest
      (QCheck.Test.make ~name:"renaming ids leaves the key unchanged (fixture)"
         ~count:200 id_gen (fun n ->
           let f = Lazy.force turn2 in
           Canon.equal
             (canonical ~path:f.path f.request)
             (canonical ~path:f.path
                (rename_ids [ ("call_30065", n) ] f.request))));
    QCheck_alcotest.to_alcotest
      (QCheck.Test.make
         ~name:"renaming ids leaves the key unchanged (two calls)" ~count:200
         QCheck.(pair id_gen id_gen)
         (fun (x, y) ->
           QCheck.assume (not (String.equal x y));
           Canon.equal
             (canonical ~path:gen_path (two_call_request "call_1" "call_2"))
             (canonical ~path:gen_path (two_call_request x y))));
    Alcotest.test_case "results stay matched to their calls" `Quick (fun () ->
        let c = canonical ~path:gen_path (two_call_request "call_1" "call_2") in
        let ids k =
          list_exn (get "parts" (List.nth (list_exn (get "contents" c)) k))
          |> List.map (fun p ->
              let inner =
                match Canon.member "functionCall" p with
                | Some x -> x
                | None -> get "functionResponse" p
              in
              int_exn (get "id" inner))
        in
        Alcotest.(check (list int)) "calls" [ 0; 1 ] (ids 1);
        Alcotest.(check (list int)) "responses" [ 1; 0 ] (ids 2));
    Alcotest.test_case "answering a different call changes the key" `Quick
      (fun () ->
        let swapped =
          json
            {|{"contents":[
  {"role":"model","parts":[
    {"functionCall":{"name":"f","args":{},"id":"a"}},
    {"functionCall":{"name":"f","args":{},"id":"b"}}]},
  {"role":"user","parts":[{"functionResponse":{"name":"f","response":{},"id":"a"}}]}]}|}
        in
        let other =
          json
            {|{"contents":[
  {"role":"model","parts":[
    {"functionCall":{"name":"f","args":{},"id":"a"}},
    {"functionCall":{"name":"f","args":{},"id":"b"}}]},
  {"role":"user","parts":[{"functionResponse":{"name":"f","response":{},"id":"b"}}]}]}|}
        in
        Alcotest.(check bool)
          "differ" false
          (Canon.equal
             (canonical ~path:gen_path swapped)
             (canonical ~path:gen_path other)));
    Alcotest.test_case "thoughtSignature is gone from the key" `Quick (fun () ->
        let f = Lazy.force turn2 in
        let c = Canon.to_string (canonical ~path:f.path f.request) in
        Alcotest.(check bool) "absent" false (contains "thoughtSignature" c);
        Alcotest.(check bool) "id absent" false (contains "call_30065" c));
    Alcotest.test_case "model from the path is in the key" `Quick (fun () ->
        let f = Lazy.force turn2 in
        let c = canonical ~path:f.path f.request in
        Alcotest.check canon_t "model" (Canon.string f.model) (get "model" c);
        let c2 =
          canonical
            ~path:"/v1beta/models/gemini-3.1-pro-preview:generateContent"
            f.request
        in
        Alcotest.(check bool) "differs" false (Canon.equal c c2));
    Alcotest.test_case "streaming and non-streaming requests have distinct keys"
      `Quick (fun () ->
        let f = Lazy.force turn2 in
        Alcotest.(check bool)
          "differs" false
          (Canon.equal
             (canonical ~path:gen_path f.request)
             (canonical ~path:stream_path f.request)));
    Alcotest.test_case "every ignored field is removed" `Quick (fun () ->
        let f = Lazy.force turn2 in
        let base = canonical ~path:f.path f.request in
        List.iter
          (fun (p, reason) ->
            Alcotest.(check bool) ("has reason: " ^ p) true (reason <> "");
            let modified =
              insert_at
                (String.split_on_char '.' p)
                (Canon.string "volatile") f.request
            in
            Alcotest.(check bool)
              ("request changed: " ^ p) false
              (Canon.equal modified f.request);
            Alcotest.check canon_t p base (canonical ~path:f.path modified))
          G.ignored_fields);
    Alcotest.test_case "ignored_fields has the planned top-level names" `Quick
      (fun () ->
        List.iter
          (fun n ->
            Alcotest.(check bool)
              n true
              (List.exists (fun (p, _) -> String.equal p n) G.ignored_fields))
          [ "labels"; "store"; "serviceTier"; "continuationToken" ]);
    Alcotest.test_case "an unknown top-level field stays in the key" `Quick
      (fun () ->
        let f = Lazy.force turn2 in
        let base = canonical ~path:f.path f.request in
        let extra =
          canonical ~path:f.path
            (insert_at [ "futureField" ] (Canon.int 1) f.request)
        in
        Alcotest.(check bool) "differs" false (Canon.equal base extra);
        Alcotest.check canon_t "kept" (Canon.int 1) (get "futureField" extra));
    Alcotest.test_case "sampling parameters stay in the key" `Quick (fun () ->
        let f = Lazy.force turn2 in
        let base = canonical ~path:f.path f.request in
        let other =
          canonical ~path:f.path
            (insert_at
               [ "generationConfig"; "temperature" ]
               (Canon.int 1) f.request)
        in
        Alcotest.(check bool) "differs" false (Canon.equal base other));
    Alcotest.test_case "id-less calls and responses are left id-less" `Quick
      (fun () ->
        let req =
          json
            {|{"contents":[
  {"role":"model","parts":[{"functionCall":{"name":"f","args":{}}}]},
  {"role":"user","parts":[{"functionResponse":{"name":"f","response":{"result":"x"}}}]}]}|}
        in
        let c = canonical ~path:gen_path req in
        Alcotest.(check bool)
          "no id" false
          (contains {|"id"|} (Canon.to_string c)));
    Alcotest.test_case "pro request differs from flash only by model and config"
      `Quick (fun () ->
        let a = load "flash_generate" and b = load "pro_generate" in
        let ca = canonical ~path:a.path a.request in
        let cb = canonical ~path:b.path b.request in
        Alcotest.(check bool) "differ" false (Canon.equal ca cb);
        Alcotest.check canon_t "contents" (get "contents" ca)
          (get "contents" cb));
    Alcotest.test_case "same prompt, same key whether captured as sse or json"
      `Quick (fun () ->
        let a = load "flash_stream_sse" and b = load "flash_stream_json" in
        Alcotest.check canon_t "equal"
          (canonical ~path:a.path a.request)
          (canonical ~path:b.path b.request));
    Alcotest.test_case "bad path and bad body are malformed" `Quick (fun () ->
        (match G.canonical_request ~path:"/v1/chat/completions" (obj []) with
        | Error (Protocol.Malformed _) -> ()
        | Ok _ -> Alcotest.fail "path accepted");
        match G.canonical_request ~path:gen_path (Canon.int 1) with
        | Error (Protocol.Malformed _) -> ()
        | Ok _ -> Alcotest.fail "body accepted");
  ]

(* tool_results *)

let tool_result_tests =
  [
    Alcotest.test_case "turn 2 fixture carries one result at call 0" `Quick
      (fun () ->
        let f = Lazy.force turn2 in
        match G.tool_results f.request with
        | [ r ] ->
            Alcotest.(check int) "index" 0 r.call_index;
            Alcotest.(check string)
              "content" "18 degrees Celsius, cloudy" r.content;
            Alcotest.(check bool) "error" false r.is_error
        | _ -> Alcotest.fail "expected one result");
    Alcotest.test_case "results are linked by id, not by order" `Quick
      (fun () ->
        match G.tool_results (two_call_request "x" "y") with
        | [ r1; r2 ] ->
            Alcotest.(check (pair int string))
              "first" (1, "rome-result")
              (r1.call_index, r1.content);
            Alcotest.(check (pair int string))
              "second" (0, "paris-result")
              (r2.call_index, r2.content)
        | _ -> Alcotest.fail "expected two results");
    Alcotest.test_case "error responses are flagged, structured ones kept"
      `Quick (fun () ->
        let req =
          json
            {|{"contents":[
  {"role":"model","parts":[{"functionCall":{"name":"f","args":{}}}]},
  {"role":"user","parts":[{"functionResponse":{"name":"f","response":{"error":"boom"}}}]}]}|}
        in
        match G.tool_results req with
        | [ r ] ->
            Alcotest.(check bool) "is_error" true r.is_error;
            Alcotest.(check int) "index" 0 r.call_index;
            Alcotest.(check string) "content" {|{"error":"boom"}|} r.content
        | _ -> Alcotest.fail "expected one result");
    Alcotest.test_case "a first-turn request has none" `Quick (fun () ->
        let f = load "flash_generate" in
        Alcotest.(check int) "none" 0 (List.length (G.tool_results f.request)));
  ]

let () =
  Alcotest.run "gemini_generate"
    [
      ("fixtures", fixture_tests);
      ("usage", usage_tests);
      ("split", split_tests);
      ("edge", edge_tests);
      ("routing", routing_tests);
      ("canonical", canonical_tests);
      ("tool_results", tool_result_tests);
    ]
