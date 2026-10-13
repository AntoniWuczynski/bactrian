open Bactrian_core
module M = Mcp_msg

let c s =
  match Canon.of_string s with
  | Ok v -> v
  | Error _ -> Alcotest.fail ("bad canon fixture: " ^ s)

let opt_equal = Option.equal Canon.equal

let id_equal a b =
  match (a, b) with
  | M.Int_id x, M.Int_id y -> Int.equal x y
  | M.String_id x, M.String_id y -> String.equal x y
  | M.Int_id _, M.String_id _ | M.String_id _, M.Int_id _ -> false

let message_equal (a : M.message) (b : M.message) =
  match (a, b) with
  | Request x, Request y ->
      id_equal x.id y.id
      && String.equal x.method_ y.method_
      && opt_equal x.params y.params
  | Notification x, Notification y ->
      String.equal x.method_ y.method_ && opt_equal x.params y.params
  | Response x, Response y ->
      id_equal x.id y.id && Canon.equal x.result y.result
  | Error_response x, Error_response y ->
      Option.equal id_equal x.id y.id
      && Int.equal x.code y.code
      && String.equal x.message y.message
      && opt_equal x.data y.data
  | Request _, (Notification _ | Response _ | Error_response _)
  | Notification _, (Request _ | Response _ | Error_response _)
  | Response _, (Request _ | Notification _ | Error_response _)
  | Error_response _, (Request _ | Notification _ | Response _) ->
      false

let pp_message ppf m = Fmt.string ppf (M.encode m)
let message = Alcotest.testable pp_message message_equal

let canon =
  Alcotest.testable
    (fun ppf v -> Fmt.string ppf (Canon.to_string v))
    Canon.equal

let decoded s =
  match M.decode s with
  | Ok m -> m
  | Error _ -> Alcotest.fail ("decode failed: " ^ s)

let request ?(id = M.Int_id 1) ?params method_ : M.message =
  Request { id; method_; params = Option.map c params }

let key m =
  match M.canonical_request m with
  | Some k -> Canon.to_string k
  | None -> Alcotest.fail "no canonical request"

let samples : M.message list =
  [
    request "tools/list";
    request ~id:(String_id "abc") ~params:{|{"a":[1,2,{"b":null}]}|}
      "tools/call";
    Notification { method_ = "notifications/initialized"; params = None };
    Notification
      { method_ = "notifications/progress"; params = Some (c {|{"p":0.5}|}) };
    Response { id = Int_id 7; result = c {|{"tools":[]}|} };
    Response { id = String_id "x"; result = c "null" };
    Error_response
      { id = Some (Int_id 2); code = -32601; message = "no"; data = None };
    Error_response
      {
        id = None;
        code = -32700;
        message = "parse";
        data = Some (c {|{"at":3}|});
      };
  ]

let test_round_trip () =
  List.iter
    (fun m ->
      let line = M.encode m in
      Alcotest.(check bool) "one line" false (String.contains line '\n');
      Alcotest.check message line m (decoded line))
    samples

let gen_message =
  let open QCheck.Gen in
  let id =
    oneof
      [
        map (fun i -> M.Int_id i) int;
        map (fun s -> M.String_id s) string_printable;
      ]
  in
  let params = option (oneof_list [ c "{}"; c {|{"k":"v"}|}; c "[1,2]" ]) in
  oneof
    [
      map3
        (fun id method_ params -> M.Request { id; method_; params })
        id string_printable params;
      map2
        (fun method_ params -> M.Notification { method_; params })
        string_printable params;
      map2
        (fun id result -> M.Response { id; result })
        id
        (oneof_list [ c "{}"; c "[]"; c "3" ]);
      map3
        (fun id code message ->
          M.Error_response { id; code; message; data = None })
        (option id) int string_printable;
    ]

let prop_round_trip =
  QCheck.Test.make ~name:"decode (encode m) = m" ~count:300
    (QCheck.make ~print:(fun m -> M.encode m) gen_message)
    (fun m ->
      match M.decode (M.encode m) with
      | Ok m' -> message_equal m m'
      | Error _ -> false)

let test_decode_kinds () =
  Alcotest.check message "request"
    (request ~params:{|{"x":1}|} "ping")
    (decoded {|{"jsonrpc":"2.0","id":1,"method":"ping","params":{"x":1}}|});
  Alcotest.check message "notification"
    (Notification { method_ = "n"; params = None })
    (decoded {|{"jsonrpc":"2.0","method":"n"}|});
  Alcotest.check message "string id error"
    (Error_response
       { id = Some (String_id "a"); code = 1; message = "m"; data = None })
    (decoded {|{"jsonrpc":"2.0","id":"a","error":{"code":1,"message":"m"}}|});
  Alcotest.check message "null id error"
    (Error_response { id = None; code = 1; message = "m"; data = None })
    (decoded {|{"jsonrpc":"2.0","id":null,"error":{"code":1,"message":"m"}}|});
  Alcotest.check message "error without id member"
    (Error_response { id = None; code = 1; message = "m"; data = None })
    (decoded {|{"jsonrpc":"2.0","error":{"code":1,"message":"m"}}|})

let is_invalid = function
  | Error (M.Invalid_json _) -> true
  | Error (Not_jsonrpc _) | Ok _ -> false

let not_jsonrpc = function
  | Error (M.Not_jsonrpc _) -> true
  | Error (Invalid_json _) | Ok _ -> false

let test_invalid_json () =
  List.iter
    (fun s -> Alcotest.(check bool) s true (is_invalid (M.decode s)))
    [
      "";
      "{";
      "nope";
      {|{"jsonrpc":"2.0","jsonrpc":"2.0","method":"a"}|};
      {|{"jsonrpc":"2.0","method":"a","params":{"n":1e400}}|};
    ]

let test_batch_rejected () =
  let batch = {|[{"jsonrpc":"2.0","id":1,"method":"ping"}]|} in
  (match M.decode batch with
  | Error (Not_jsonrpc reason) ->
      Alcotest.(check bool)
        "mentions batch" true
        (Re.execp (Re.compile (Re.str "batch")) (String.lowercase_ascii reason))
  | Error (Invalid_json _) | Ok _ -> Alcotest.fail "batch must be Not_jsonrpc");
  Alcotest.(check bool) "empty batch" true (not_jsonrpc (M.decode "[]"))

let test_not_jsonrpc () =
  List.iter
    (fun s -> Alcotest.(check bool) s true (not_jsonrpc (M.decode s)))
    [
      "3";
      {|"x"|};
      "null";
      {|{"id":1,"method":"a"}|};
      {|{"jsonrpc":"1.0","id":1,"method":"a"}|};
      {|{"jsonrpc":"2.0"}|};
      {|{"jsonrpc":"2.0","id":1,"method":3}|};
      {|{"jsonrpc":"2.0","id":null,"method":"a"}|};
      {|{"jsonrpc":"2.0","id":1.5,"method":"a"}|};
      {|{"jsonrpc":"2.0","id":true,"method":"a"}|};
      {|{"jsonrpc":"2.0","id":100000000000000000000,"method":"a"}|};
      {|{"jsonrpc":"2.0","id":1,"method":"a","params":3}|};
      {|{"jsonrpc":"2.0","id":1,"method":"a","params":null}|};
      {|{"jsonrpc":"2.0","id":1,"method":"a","extra":1}|};
      {|{"jsonrpc":"2.0","result":{}}|};
      {|{"jsonrpc":"2.0","id":1,"result":{},"error":{"code":1,"message":"m"}}|};
      {|{"jsonrpc":"2.0","id":1,"error":{"code":"x","message":"m"}}|};
      {|{"jsonrpc":"2.0","id":1,"error":{"code":1}}|};
      {|{"jsonrpc":"2.0","id":1,"error":3}|};
      {|{"jsonrpc":"2.0","id":1}|};
      {|{"jsonrpc":"2.0","id":1,"method":"a","result":{}}|};
    ]

let modern_params =
  {|{"name":"get_weather","arguments":{"city":"Oslo"},"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{},"io.modelcontextprotocol/clientInfo":{"name":"claude-code","version":"2.1.292"},"traceparent":"00-aa-bb-01","tracestate":"x=y","baggage":"k=v","progressToken":"p1"}}|}

let test_era () =
  let era =
    Alcotest.option
      (Alcotest.testable
         (fun ppf e ->
           Fmt.string ppf
             (match e with M.Modern -> "Modern" | Legacy -> "Legacy"))
         ( = ))
  in
  Alcotest.check era "initialize" (Some Legacy)
    (M.era_of_request
       (request ~params:{|{"protocolVersion":"2025-11-25"}|} "initialize"));
  Alcotest.check era "initialize no params" (Some Legacy)
    (M.era_of_request (request "initialize"));
  Alcotest.check era "meta" (Some Modern)
    (M.era_of_request (request ~params:modern_params "tools/call"));
  Alcotest.check era "server/discover with meta" (Some Modern)
    (M.era_of_request
       (request
          ~params:
            {|{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}|}
          "server/discover"));
  Alcotest.check era "tools/list bare" None
    (M.era_of_request (request "tools/list"));
  Alcotest.check era "meta without version" None
    (M.era_of_request
       (request ~params:{|{"_meta":{"traceparent":"t"}}|} "tools/list"));
  Alcotest.check era "notification" None
    (M.era_of_request (Notification { method_ = "initialize"; params = None }));
  Alcotest.check era "response" None
    (M.era_of_request (Response { id = Int_id 1; result = c "{}" }))

let test_canonical_shape () =
  Alcotest.check canon "drops varying meta"
    (c
       {|{"method":"tools/call","params":{"name":"get_weather","arguments":{"city":"Oslo"},"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}}}|})
    (Option.get
       (M.canonical_request (request ~params:modern_params "tools/call")));
  Alcotest.check canon "no params"
    (c {|{"method":"tools/list"}|})
    (Option.get (M.canonical_request (request "tools/list")));
  Alcotest.check canon "id is not part of it"
    (Option.get (M.canonical_request (request ~id:(Int_id 1) "tools/list")))
    (Option.get
       (M.canonical_request (request ~id:(String_id "z") "tools/list")))

let test_canonical_non_requests () =
  List.iter
    (fun (m : M.message) ->
      match m with
      | Request _ -> ()
      | Notification _ | Response _ | Error_response _ ->
          Alcotest.(check bool)
            "none" true
            (Option.is_none (M.canonical_request m)))
    samples

let test_key_ignores_client () =
  let with_meta meta =
    request
      ~params:
        (Printf.sprintf {|{"name":"t","arguments":{"a":1},"_meta":{%s}}|} meta)
      "tools/call"
  in
  let v = {|"io.modelcontextprotocol/protocolVersion":"2026-07-28"|} in
  let a =
    with_meta
      (v
     ^ {|,"io.modelcontextprotocol/clientInfo":{"name":"a","version":"1"},"traceparent":"00-1-2-01","progressToken":1|}
      )
  in
  let b =
    with_meta
      (v
     ^ {|,"io.modelcontextprotocol/clientInfo":{"name":"b","version":"9"},"traceparent":"00-3-4-01","tracestate":"s=1","baggage":"x=y","progressToken":"q"|}
      )
  in
  let bare = with_meta v in
  Alcotest.(check string) "a=b" (key a) (key b);
  Alcotest.(check string) "a=bare" (key a) (key bare)

let test_key_empty_meta_and_params () =
  let a =
    request ~params:{|{"_meta":{"traceparent":"t","progressToken":3}}|}
      "tools/list"
  in
  Alcotest.(check string) "meta only" (key (request "tools/list")) (key a);
  Alcotest.(check string)
    "empty params"
    (key (request "tools/list"))
    (key (request ~params:"{}" "tools/list"))

let test_key_initialize () =
  let init name =
    request
      ~params:
        (Printf.sprintf
           {|{"protocolVersion":"2025-11-25","capabilities":{"roots":{}},"clientInfo":{"name":"%s","version":"1"}}|}
           name)
      "initialize"
  in
  Alcotest.(check string) "clientInfo ignored" (key (init "a")) (key (init "b"));
  let other =
    request
      ~params:
        {|{"protocolVersion":"2025-06-18","capabilities":{"roots":{}},"clientInfo":{"name":"a","version":"1"}}|}
      "initialize"
  in
  Alcotest.(check bool)
    "version kept" false
    (String.equal (key (init "a")) (key other))

let test_key_differs () =
  let call args ver =
    request
      ~params:
        (Printf.sprintf
           {|{"name":"t","arguments":%s,"_meta":{"io.modelcontextprotocol/protocolVersion":"%s","io.modelcontextprotocol/clientCapabilities":{}}}|}
           args ver)
      "tools/call"
  in
  let base = key (call {|{"city":"Oslo"}|} "2026-07-28") in
  Alcotest.(check bool)
    "arguments" false
    (String.equal base (key (call {|{"city":"Rome"}|} "2026-07-28")));
  Alcotest.(check bool)
    "protocolVersion" false
    (String.equal base (key (call {|{"city":"Oslo"}|} "2027-01-01")));
  Alcotest.(check bool)
    "method" false
    (String.equal (key (request "tools/list")) (key (request "prompts/list")));
  let caps caps =
    request
      ~params:
        (Printf.sprintf
           {|{"_meta":{"io.modelcontextprotocol/clientCapabilities":%s}}|} caps)
      "tools/list"
  in
  Alcotest.(check bool)
    "capabilities" false
    (String.equal
       (key (caps "{}"))
       (key (caps {|{"extensions":{"io.modelcontextprotocol/tasks":{}}}|})));
  Alcotest.(check bool)
    "argument key order irrelevant" true
    (String.equal
       (key
          (request ~params:{|{"name":"t","arguments":{"a":1,"b":2}}|}
             "tools/call"))
       (key
          (request ~params:{|{"arguments":{"b":2,"a":1},"name":"t"}|}
             "tools/call")))

let test_tool_call () =
  let tc = Alcotest.option (Alcotest.pair Alcotest.string canon) in
  Alcotest.check tc "with arguments"
    (Some ("get_weather", c {|{"city":"Oslo"}|}))
    (M.tool_call (request ~params:modern_params "tools/call"));
  Alcotest.check tc "no arguments"
    (Some ("t", Canon.empty_object))
    (M.tool_call (request ~params:{|{"name":"t"}|} "tools/call"));
  Alcotest.check tc "no name" None
    (M.tool_call (request ~params:{|{"arguments":{}}|} "tools/call"));
  Alcotest.check tc "name not string" None
    (M.tool_call (request ~params:{|{"name":3}|} "tools/call"));
  Alcotest.check tc "no params" None (M.tool_call (request "tools/call"));
  Alcotest.check tc "other method" None
    (M.tool_call (request ~params:{|{"name":"t"}|} "tools/list"));
  Alcotest.check tc "notification" None
    (M.tool_call
       (Notification
          { method_ = "tools/call"; params = Some (c {|{"name":"t"}|}) }))

let test_with_id () =
  let id = M.String_id "live" in
  Alcotest.check message "response"
    (Response { id; result = c "{}" })
    (M.with_id id (Response { id = Int_id 5; result = c "{}" }));
  Alcotest.check message "error"
    (Error_response { id = Some id; code = 1; message = "m"; data = None })
    (M.with_id id
       (Error_response
          { id = Some (Int_id 5); code = 1; message = "m"; data = None }));
  Alcotest.check message "error without id"
    (Error_response { id = Some id; code = 1; message = "m"; data = None })
    (M.with_id id
       (Error_response { id = None; code = 1; message = "m"; data = None }));
  Alcotest.check message "request" (request ~id "ping")
    (M.with_id id (request "ping"));
  let n : M.message = Notification { method_ = "n"; params = None } in
  Alcotest.check message "notification unchanged" n (M.with_id id n)

let () =
  Alcotest.run "mcp_msg"
    [
      ( "codec",
        [
          Alcotest.test_case "round trip" `Quick test_round_trip;
          QCheck_alcotest.to_alcotest prop_round_trip;
          Alcotest.test_case "kinds" `Quick test_decode_kinds;
          Alcotest.test_case "invalid json" `Quick test_invalid_json;
          Alcotest.test_case "batch rejected" `Quick test_batch_rejected;
          Alcotest.test_case "not jsonrpc" `Quick test_not_jsonrpc;
        ] );
      ("era", [ Alcotest.test_case "era" `Quick test_era ]);
      ( "canonical",
        [
          Alcotest.test_case "shape" `Quick test_canonical_shape;
          Alcotest.test_case "non requests" `Quick test_canonical_non_requests;
          Alcotest.test_case "ignores client" `Quick test_key_ignores_client;
          Alcotest.test_case "empty meta" `Quick test_key_empty_meta_and_params;
          Alcotest.test_case "initialize" `Quick test_key_initialize;
          Alcotest.test_case "differs" `Quick test_key_differs;
        ] );
      ("tool_call", [ Alcotest.test_case "tool_call" `Quick test_tool_call ]);
      ("with_id", [ Alcotest.test_case "with_id" `Quick test_with_id ]);
    ]
