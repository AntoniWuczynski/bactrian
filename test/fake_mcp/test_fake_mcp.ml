module F = Fake_mcp
module Canon = Bactrian_core.Canon

let exe = "../support/fake_mcp/fake_mcp_server.exe"

let parse s =
  match Canon.of_string s with
  | Ok v -> v
  | Error e -> Alcotest.failf "bad json %S: %a" s Canon.pp_error e

let rec at keys j =
  match keys with
  | [] -> Some j
  | k :: r -> Option.bind (Canon.member k j) (at r)

let str keys j = Option.bind (at keys j) Canon.to_string_opt
let int keys j = Option.bind (at keys j) Canon.to_int_opt
let has keys j = Option.is_some (at keys j)

let check_str name keys expected j =
  Alcotest.(check (option string)) name (Some expected) (str keys j)

let check_int name keys expected j =
  Alcotest.(check (option int)) name (Some expected) (int keys j)

let modern_meta =
  {|"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}|}

let modern_req ?(id = "1") meth params =
  Printf.sprintf {|{"jsonrpc":"2.0","id":%s,"method":%S,"params":{%s%s}}|} id
    meth params modern_meta

let call_params city =
  Printf.sprintf {|"name":"get_weather","arguments":{"city":%S},|} city

let legacy_req ?(id = "1") meth params =
  Printf.sprintf {|{"jsonrpc":"2.0","id":%s,"method":%S,"params":%s}|} id meth
    params

(* Stdio: drive the executable over pipes. *)

type child = { send : string -> unit; recv : unit -> Canon.t }

let with_stdio ?(env = [||]) ~args f =
  Eio_main.run @@ fun e ->
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock e) 20. @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let in_r, in_w = Eio_unix.pipe sw in
  let out_r, out_w = Eio_unix.pipe sw in
  let proc =
    Eio.Process.spawn ~sw (Eio.Stdenv.process_mgr e) ~stdin:in_r ~stdout:out_w
      ~env (exe :: args)
  in
  Eio.Resource.close in_r;
  Eio.Resource.close out_w;
  let reader = Eio.Buf_read.of_flow ~max_size:max_int out_r in
  f
    {
      send = (fun line -> Eio.Flow.copy_string (line ^ "\n") in_w);
      recv = (fun () -> parse (Eio.Buf_read.line reader));
    };
  Eio.Resource.close in_w;
  match Eio.Process.await proc with
  | `Exited 0 -> ()
  | `Exited n -> Alcotest.failf "server exited with %d" n
  | `Signaled n -> Alcotest.failf "server killed by signal %d" n

let with_log_file f =
  let path = Filename.temp_file "fake_mcp" ".log" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)

let read_lines path =
  In_channel.with_open_text path In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun l -> not (String.equal l ""))

let first_text j =
  match Option.bind (at [ "result"; "content" ] j) Canon.to_list_opt with
  | Some (item :: _) -> Canon.str_member "text" item
  | Some [] | None -> None

let tool_names j =
  match Option.bind (at [ "result"; "tools" ] j) Canon.to_list_opt with
  | Some tools -> List.filter_map (Canon.str_member "name") tools
  | None -> []

let test_stdio_modern () =
  with_log_file @@ fun log ->
  let discover = modern_req ~id:{|"a"|} "server/discover" "" in
  let cancelled =
    {|{"jsonrpc":"2.0","method":"notifications/cancelled","params":{}}|}
  in
  let list = modern_req ~id:"2" "tools/list" "" in
  let call = modern_req ~id:"3" "tools/call" (call_params "Paris") in
  let init = modern_req ~id:"4" "initialize" "" in
  let old_version =
    {|{"jsonrpc":"2.0","id":5,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2025-11-25"}}}|}
  in
  let batch = {|[{"jsonrpc":"2.0","id":6,"method":"tools/list"}]|} in
  let garbage = "not json" in
  with_stdio
    ~env:[| "FAKE_MCP_LOG=" ^ log |]
    ~args:[ "--era"; "modern" ]
    (fun c ->
      let ask line =
        c.send line;
        c.recv ()
      in
      let r = ask discover in
      check_str "discover id" [ "id" ] "a" r;
      check_str "discover resultType" [ "result"; "resultType" ] "complete" r;
      Alcotest.(check bool)
        "discover versions" true
        (has [ "result"; "supportedVersions" ] r);
      Alcotest.(check bool)
        "discover serverInfo" true
        (has [ "result"; "_meta"; "io.modelcontextprotocol/serverInfo" ] r);
      (* The notification gets no reply: the next line read answers id 2. *)
      c.send cancelled;
      let r = ask list in
      check_int "list id" [ "id" ] 2 r;
      check_str "list resultType" [ "result"; "resultType" ] "complete" r;
      check_int "list ttlMs" [ "result"; "ttlMs" ] 60000 r;
      check_str "list cacheScope" [ "result"; "cacheScope" ] "public" r;
      Alcotest.(check (list string)) "tools" [ "get_weather" ] (tool_names r);
      let r = ask call in
      check_int "call id" [ "id" ] 3 r;
      check_str "call resultType" [ "result"; "resultType" ] "complete" r;
      Alcotest.(check (option string))
        "call text" (Some "Sunny, 21C in Paris") (first_text r);
      check_int "initialize is unknown" [ "error"; "code" ] (-32601) (ask init);
      let r = ask old_version in
      check_int "version error" [ "error"; "code" ] (-32022) r;
      Alcotest.(check bool)
        "supported list" true
        (has [ "error"; "data"; "supported" ] r);
      check_int "batch rejected" [ "error"; "code" ] (-32600) (ask batch);
      check_int "parse error" [ "error"; "code" ] (-32700) (ask garbage));
  Alcotest.(check (list string))
    "log"
    [ discover; cancelled; list; call; init; old_version; batch; garbage ]
    (read_lines log)

let test_stdio_legacy () =
  with_log_file @@ fun log ->
  let init =
    legacy_req ~id:"1" "initialize"
      {|{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"0"}}|}
  in
  let initialized =
    {|{"jsonrpc":"2.0","method":"notifications/initialized"}|}
  in
  let list = legacy_req ~id:"2" "tools/list" "{}" in
  let call =
    legacy_req ~id:"3" "tools/call"
      {|{"name":"get_weather","arguments":{"city":"Oslo"}}|}
  in
  let discover = legacy_req ~id:"4" "server/discover" "{}" in
  let early = legacy_req ~id:"9" "tools/list" "{}" in
  with_stdio ~args:[ "--era"; "legacy"; "--log"; log ] (fun c ->
      let ask line =
        c.send line;
        c.recv ()
      in
      let r = ask init in
      check_int "init id" [ "id" ] 1 r;
      check_str "protocolVersion" [ "result"; "protocolVersion" ] "2025-11-25" r;
      Alcotest.(check bool) "serverInfo" true (has [ "result"; "serverInfo" ] r);
      (* Strict, as the SDK servers are: no request before the handshake ends. *)
      check_int "request before initialized" [ "error"; "code" ] (-32600)
        (ask early);
      c.send initialized;
      let r = ask list in
      check_int "list id" [ "id" ] 2 r;
      Alcotest.(check (list string)) "tools" [ "get_weather" ] (tool_names r);
      Alcotest.(check bool)
        "no modern fields" false
        (has [ "result"; "ttlMs" ] r || has [ "result"; "resultType" ] r);
      let r = ask call in
      Alcotest.(check (option string))
        "call text" (Some "Sunny, 21C in Oslo") (first_text r);
      Alcotest.(check bool)
        "no resultType" false
        (has [ "result"; "resultType" ] r);
      check_int "discover is unknown" [ "error"; "code" ] (-32601)
        (ask discover));
  Alcotest.(check (list string))
    "log"
    [ init; early; initialized; list; call; discover ]
    (read_lines log)

(* HTTP: a raw cohttp-eio client. *)

type reply = { status : int; headers : Http.Header.t; text : string }

let http ~sw e ?(headers = []) ?body meth url =
  let client = Cohttp_eio.Client.make ~https:None (Eio.Stdenv.net e) in
  let body = Option.map Cohttp_eio.Body.of_string body in
  let resp, source =
    Cohttp_eio.Client.call ~sw
      ~headers:(Http.Header.of_list headers)
      ?body client meth (Uri.of_string url)
  in
  {
    status = Http.Status.to_int resp.Http.Response.status;
    headers = resp.Http.Response.headers;
    text = Eio.Buf_read.(parse_exn take_all) source ~max_size:max_int;
  }

let with_http era f =
  Eio_main.run @@ fun e ->
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock e) 20. @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let server = F.start ~sw ~net:(Eio.Stdenv.net e) era in
  f ~sw e server;
  Eio.Switch.fail sw Exit

let run_http era f = try with_http era f with Exit -> ()
let accept = ("accept", "application/json, text/event-stream")

let modern_headers ?(version = "2026-07-28") ~mcp_method ?name () =
  [
    accept;
    ("content-type", "application/json");
    ("mcp-protocol-version", version);
    ("mcp-method", mcp_method);
  ]
  @ Option.fold ~none:[] ~some:(fun n -> [ ("mcp-name", n) ]) name

let body_json r = parse r.text
let check_status name expected r = Alcotest.(check int) name expected r.status

let test_http_modern () =
  run_http F.Modern @@ fun ~sw e server ->
  let url = F.base_url server ^ "/mcp" in
  let post ?(headers = modern_headers ~mcp_method:"tools/list" ()) body =
    http ~sw e ~headers ~body `POST url
  in
  let r = post (modern_req "tools/list" "") in
  check_status "list status" 200 r;
  Alcotest.(check (option string))
    "content-type" (Some "application/json")
    (Http.Header.get r.headers "content-type");
  Alcotest.(check (option string))
    "no session header" None
    (Http.Header.get r.headers "mcp-session-id");
  check_str "resultType" [ "result"; "resultType" ] "complete" (body_json r);
  let r =
    post
      ~headers:(modern_headers ~mcp_method:"tools/call" ~name:"get_weather" ())
      (modern_req ~id:"7" "tools/call" (call_params "Rome"))
  in
  check_status "call status" 200 r;
  check_int "call id" [ "id" ] 7 (body_json r);
  Alcotest.(check (option string))
    "call text" (Some "Sunny, 21C in Rome")
    (first_text (body_json r));
  let r =
    post
      ~headers:(modern_headers ~mcp_method:"server/discover" ())
      (modern_req "server/discover" "")
  in
  check_status "discover status" 200 r;
  (* Header validation: 400 and -32020. *)
  let mismatch name r =
    check_status (name ^ " status") 400 r;
    check_int (name ^ " code") [ "error"; "code" ] (-32020) (body_json r)
  in
  mismatch "missing method"
    (post
       ~headers:
         [
           accept;
           ("content-type", "application/json");
           ("mcp-protocol-version", "2026-07-28");
         ]
       (modern_req "tools/list" ""));
  mismatch "missing version"
    (post
       ~headers:
         [
           accept;
           ("content-type", "application/json");
           ("mcp-method", "tools/list");
         ]
       (modern_req "tools/list" ""));
  mismatch "method differs"
    (post
       ~headers:(modern_headers ~mcp_method:"server/discover" ())
       (modern_req "tools/list" ""));
  mismatch "name missing"
    (post
       ~headers:(modern_headers ~mcp_method:"tools/call" ())
       (modern_req "tools/call" (call_params "Rome")));
  mismatch "name differs"
    (post
       ~headers:(modern_headers ~mcp_method:"tools/call" ~name:"other" ())
       (modern_req "tools/call" (call_params "Rome")));
  mismatch "version differs from body"
    (post
       ~headers:
         (modern_headers ~version:"2025-11-25" ~mcp_method:"tools/list" ())
       (modern_req "tools/list" ""));
  (* Unsupported version, in header and body alike. *)
  let r =
    post
      ~headers:
        (modern_headers ~version:"2025-11-25" ~mcp_method:"tools/list" ())
      {|{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2025-11-25"}}}|}
  in
  check_status "unsupported version status" 400 r;
  check_int "unsupported version code" [ "error"; "code" ] (-32022)
    (body_json r);
  (* Unknown method, including the legacy handshake: 404 and -32601. *)
  let r =
    post
      ~headers:(modern_headers ~mcp_method:"initialize" ())
      (modern_req "initialize" "")
  in
  check_status "initialize status" 404 r;
  check_int "initialize code" [ "error"; "code" ] (-32601) (body_json r);
  (* A notification is accepted without a body. *)
  let r =
    post
      ~headers:(modern_headers ~mcp_method:"notifications/cancelled" ())
      {|{"jsonrpc":"2.0","method":"notifications/cancelled","params":{}}|}
  in
  check_status "notification status" 202 r;
  Alcotest.(check string) "notification body" "" r.text;
  (* GET and DELETE are refused. *)
  let r = http ~sw e ~headers:[ accept ] `GET url in
  check_status "GET" 405 r;
  Alcotest.(check (option string))
    "allow" (Some "POST")
    (Http.Header.get r.headers "allow");
  check_status "DELETE" 405 (http ~sw e ~headers:[ accept ] `DELETE url);
  let log = F.requests server in
  Alcotest.(check (list string))
    "logged methods"
    [
      "POST";
      "POST";
      "POST";
      "POST";
      "POST";
      "POST";
      "POST";
      "POST";
      "POST";
      "POST";
      "POST";
      "POST";
      "GET";
      "DELETE";
    ]
    (List.map (fun (r : F.request) -> r.meth) log);
  match log with
  | first :: _ ->
      Alcotest.(check string) "logged path" "/mcp" first.path;
      Alcotest.(check (option string))
        "logged header" (Some "tools/list")
        (List.assoc_opt "mcp-method" first.headers);
      Alcotest.(check string)
        "logged body"
        (modern_req "tools/list" "")
        first.body
  | [] -> Alcotest.fail "nothing logged"

let test_http_legacy () =
  run_http F.Legacy @@ fun ~sw e server ->
  let url = F.base_url server ^ "/mcp" in
  let base = [ accept; ("content-type", "application/json") ] in
  let post ?(headers = base) body = http ~sw e ~headers ~body `POST url in
  let r =
    post
      (legacy_req "initialize"
         {|{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"0"}}|})
  in
  check_status "initialize status" 200 r;
  check_str "protocolVersion"
    [ "result"; "protocolVersion" ]
    "2025-11-25" (body_json r);
  let session = Http.Header.get r.headers "mcp-session-id" in
  Alcotest.(check (option string)) "session id" (Some F.session_id) session;
  let with_session = base @ [ ("mcp-session-id", F.session_id) ] in
  check_int "request before initialized" [ "error"; "code" ] (-32600)
    (body_json
       (post ~headers:with_session (legacy_req ~id:"9" "tools/list" "{}")));
  let r =
    post ~headers:with_session
      {|{"jsonrpc":"2.0","method":"notifications/initialized"}|}
  in
  check_status "initialized status" 202 r;
  Alcotest.(check string) "initialized body" "" r.text;
  let r = post ~headers:with_session (legacy_req ~id:"2" "tools/list" "{}") in
  check_status "list status" 200 r;
  Alcotest.(check (list string))
    "tools" [ "get_weather" ]
    (tool_names (body_json r));
  let r =
    post ~headers:with_session
      (legacy_req ~id:"3" "tools/call"
         {|{"name":"get_weather","arguments":{"city":"Bern"}}|})
  in
  Alcotest.(check (option string))
    "call text" (Some "Sunny, 21C in Bern")
    (first_text (body_json r));
  check_status "no session" 400 (post (legacy_req ~id:"4" "tools/list" "{}"));
  check_status "wrong session" 404
    (post
       ~headers:(base @ [ ("mcp-session-id", "other") ])
       (legacy_req ~id:"5" "tools/list" "{}"));
  check_int "discover is unknown" [ "error"; "code" ] (-32601)
    (body_json
       (post ~headers:with_session (legacy_req ~id:"6" "server/discover" "{}")));
  check_status "GET" 405 (http ~sw e ~headers:[ accept ] `GET url);
  Alcotest.(check int) "logged" 9 (List.length (F.requests server))

let () =
  Alcotest.run "fake_mcp"
    [
      ( "stdio",
        [
          Alcotest.test_case "modern" `Quick test_stdio_modern;
          Alcotest.test_case "legacy" `Quick test_stdio_legacy;
        ] );
      ( "http",
        [
          Alcotest.test_case "modern" `Quick test_http_modern;
          Alcotest.test_case "legacy" `Quick test_http_legacy;
        ] );
    ]
