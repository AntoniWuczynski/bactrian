open Mcp_support

let start_fake ctx era =
  Fake_mcp.start ~sw:ctx.sw ~net:(Eio.Stdenv.net ctx.env) era

let header name (r : reply) = List.assoc_opt name r.headers

let modern_round_trip =
  Alcotest.test_case "modern: record, then strict replay without the server"
    `Quick
  @@ with_ctx "mcp-http-modern"
  @@ fun ctx ->
  let fake = start_fake ctx Modern in
  let run =
    record_run ctx ~token:token_a [ http "search" (Fake_mcp.base_url fake) ]
  in
  let keys =
    [
      ("authorization", "Bearer bactrian-" ^ token_a);
      ("x-api-key", "bactrian-" ^ token_a);
    ]
  in
  let send ~token ?name m body =
    post_http ctx ~token ~name:"search"
      ~headers:(keys @ modern_headers ?name m)
      body
  in
  let r1 = send ~token:token_a "server/discover" (discover (Int_id 1)) in
  check_status "discover" 200 r1;
  let _, discovered = result_of r1.body in
  let r2 =
    send ~token:token_a ~name:"get_weather" "tools/call"
      (call (Int_id 2) "Paris")
  in
  check_status "call" 200 r2;
  let _, called = result_of r2.body in
  (* A modern server refuses initialize with 404 and -32601. *)
  let r3 = send ~token:token_a "initialize" (initialize (Int_id 3)) in
  check_status "initialize" 404 r3;
  let f = P.unregister ctx.proxy ~token:token_a in
  no_divergence f;
  let seen = Fake_mcp.requests fake in
  Alcotest.(check int) "forwarded" 3 (List.length seen);
  List.iter
    (fun (r : Fake_mcp.request) ->
      List.iter
        (fun (k, v) ->
          if contains v token_a then
            Alcotest.failf "header %s carries the token" k)
        r.headers;
      Alcotest.(check bool)
        "protocol header" true
        (List.mem_assoc "mcp-protocol-version" r.headers))
    seen;
  let recording = recording ctx run in
  Alcotest.(check int) "recorded" 3 (List.length (mcp_exchanges recording));
  (* Replay against an address where nothing listens. *)
  let _ =
    replay_mcp ctx ~token:token_b recording [ http "search" (refused_url ctx) ]
  in
  let r =
    send ~token:token_b "server/discover" (discover ~client:"b" (Int_id 51))
  in
  let id, v = result_of r.body in
  Alcotest.check id_testable "id" (Int_id 51) id;
  Alcotest.check canon_testable "discover" discovered v;
  let r =
    send ~token:token_b ~name:"get_weather" "tools/call"
      (call ~client:"b" (Int_id 52) "Paris")
  in
  Alcotest.(check (option string))
    "content type" (Some "application/json") (header "content-type" r);
  let id, v = result_of r.body in
  Alcotest.check id_testable "id" (Int_id 52) id;
  Alcotest.check canon_testable "call" called v;
  let r = send ~token:token_b "initialize" (initialize (Int_id 53)) in
  check_status "replayed refusal" 404 r;
  let id, _, _, _ = error_of r.body in
  Alcotest.check id_testable "refusal id" (Int_id 53) id;
  no_divergence (P.unregister ctx.proxy ~token:token_b);
  Alcotest.(check int)
    "nothing new forwarded" 3
    (List.length (Fake_mcp.requests fake))

let legacy_session =
  Alcotest.test_case "legacy: the session header passes through and replays"
    `Quick
  @@ with_ctx "mcp-http-legacy"
  @@ fun ctx ->
  let fake = start_fake ctx Legacy in
  let run =
    record_run ctx ~token:token_a [ http "search" (Fake_mcp.base_url fake) ]
  in
  let send ~token ?session body =
    post_http ctx ~token ~name:"search"
      ~headers:
        (("content-type", "application/json")
        :: Option.fold ~none:[]
             ~some:(fun s -> [ ("mcp-session-id", s) ])
             session)
      body
  in
  let r = send ~token:token_a (initialize (Int_id 1)) in
  check_status "initialize" 200 r;
  let session = header "mcp-session-id" r in
  Alcotest.(check (option string)) "session" (Some Fake_mcp.session_id) session;
  check_status "initialized" 202 (send ~token:token_a ?session initialized);
  let _, listed =
    result_of
      (send ~token:token_a ?session (tools_list ~era:Legacy (Int_id 2))).body
  in
  no_divergence (P.unregister ctx.proxy ~token:token_a);
  let recording = recording ctx run in
  Alcotest.(check int) "recorded" 2 (List.length (mcp_exchanges recording));
  let before = List.length (Fake_mcp.requests fake) in
  let _ =
    replay_mcp ctx ~token:token_b recording
      [ http "search" (Fake_mcp.base_url fake) ]
  in
  let r = send ~token:token_b (initialize ~client:"b" (Int_id 9)) in
  Alcotest.(check (option string))
    "replayed session" session
    (header "mcp-session-id" r);
  check_status "dropped" 202
    (send ~token:token_b ~session:"another" initialized);
  let id, v =
    result_of
      (send ~token:token_b ~session:"another"
         (tools_list ~era:Legacy (Int_id 10)))
        .body
  in
  Alcotest.check id_testable "id" (Int_id 10) id;
  Alcotest.check canon_testable "list" listed v;
  no_divergence (P.unregister ctx.proxy ~token:token_b);
  Alcotest.(check int)
    "replay forwarded nothing" before
    (List.length (Fake_mcp.requests fake))

(* The live id goes into every data line of a recorded event stream. *)
let event_stream =
  Alcotest.test_case "an event-stream reply replays with the live id" `Quick
  @@ with_ctx "mcp-http-sse"
  @@ fun ctx ->
  let body =
    "event: message\r\n\
     data: {\"id\":7,\"jsonrpc\":\"2.0\",\"result\":{\"ok\":true}}\r\n\
     \r\n"
  in
  let url =
    raw_upstream ctx
      ~reply:
        (Printf.sprintf
           "HTTP/1.1 200 OK\r\n\
            content-type: text/event-stream\r\n\
            content-length: %d\r\n\
            \r\n\
            %s"
           (String.length body) body)
  in
  let run = record_run ctx ~token:token_a [ http "s" url ] in
  let r = post_http ctx ~token:token_a ~name:"s" (tools_list (Int_id 7)) in
  Alcotest.(check string) "live body" body r.body;
  ignore (P.unregister ctx.proxy ~token:token_a);
  let _ = replay_mcp ctx ~token:token_b (recording ctx run) [ http "s" url ] in
  let r = post_http ctx ~token:token_b ~name:"s" (tools_list (Int_id 99)) in
  Alcotest.(check string)
    "replayed body"
    "event: message\r\n\
     data: {\"id\":99,\"jsonrpc\":\"2.0\",\"result\":{\"ok\":true}}\r\n\
     \r\n"
    r.body;
  no_divergence (P.unregister ctx.proxy ~token:token_b)

(* A legacy server may send its own requests and notifications on the stream
   that answers a POST, and an event's JSON may span several data lines. Only
   the answer to the recorded request takes the live id. *)
let event_stream_events =
  Alcotest.test_case "only the answer in a replayed stream takes the live id"
    `Quick
  @@ with_ctx "mcp-http-sse-events"
  @@ fun ctx ->
  let server_request =
    "event: message\r\n\
     data: {\"id\":7,\"jsonrpc\":\"2.0\",\"method\":\"roots/list\"}\r\n\
     \r\n"
  in
  let body =
    server_request
    ^ "event: message\r\n\
       data: {\"id\":7,\"jsonrpc\":\"2.0\",\r\n\
       data: \"result\":{\"ok\":true}}\r\n\
       \r\n"
  in
  let url =
    raw_upstream ctx
      ~reply:
        (Printf.sprintf
           "HTTP/1.1 200 OK\r\n\
            content-type: text/event-stream\r\n\
            content-length: %d\r\n\
            \r\n\
            %s"
           (String.length body) body)
  in
  let run = record_run ctx ~token:token_a [ http "s" url ] in
  ignore (post_http ctx ~token:token_a ~name:"s" (tools_list (Int_id 7)));
  ignore (P.unregister ctx.proxy ~token:token_a);
  let _ = replay_mcp ctx ~token:token_b (recording ctx run) [ http "s" url ] in
  let r = post_http ctx ~token:token_b ~name:"s" (tools_list (Int_id 99)) in
  Alcotest.(check string)
    "replayed body"
    (server_request
   ^ "event: message\r\n\
      data: {\"id\":99,\"jsonrpc\":\"2.0\",\"result\":{\"ok\":true}}\r\n\
      \r\n")
    r.body;
  no_divergence (P.unregister ctx.proxy ~token:token_b)

let target =
  Alcotest.test_case "the declared query stays and the path cannot climb" `Quick
  @@ with_ctx "mcp-http-target"
  @@ fun ctx ->
  let fake = start_fake ctx Modern in
  let _ =
    record_run ctx ~token:token_a
      [
        http ~policy:Passthrough "search"
          (Fake_mcp.base_url fake ^ "/mcp?key=k1");
      ]
  in
  let send ?(query = "") path =
    send ctx ~seg:"mcp-http" ~token:token_a ~provider:P.Openai ~query
      ~meth:"POST"
      ~headers:(modern_headers "server/discover")
      path (discover (Int_id 5))
  in
  check_status "forwarded" 200 (send ~query:"x=1" "/search/sub");
  (match Fake_mcp.requests fake with
  | [ seen ] ->
      Alcotest.(check string) "path" "/mcp/sub" seen.path;
      Alcotest.(check (option string)) "query" (Some "key=k1&x=1") seen.query
  | seen -> Alcotest.failf "the server saw %d requests" (List.length seen));
  List.iter
    (fun path ->
      let r = send path in
      check_status path 400 r;
      let id, _, _, _ = error_of r.body in
      Alcotest.check id_testable "id" (Int_id 5) id)
    [ "/search/../admin"; "/search/%2e%2E/admin"; "/search/./x" ];
  Alcotest.(check int)
    "nothing more forwarded" 1
    (List.length (Fake_mcp.requests fake));
  ignore (P.unregister ctx.proxy ~token:token_a)

let refusals =
  Alcotest.test_case "block is 403, GET is 405, an unreachable server is 502"
    `Quick
  @@ with_ctx "mcp-http-refusals"
  @@ fun ctx ->
  let fake = start_fake ctx Modern in
  let run =
    record_run ctx ~token:token_a
      [
        http ~policy:Block "blocked" (Fake_mcp.base_url fake);
        http "down" (refused_url ctx);
      ]
  in
  check_status "blocked" 403
    (post_http ctx ~token:token_a ~name:"blocked" (discover (Int_id 1)));
  check_status "blocked notification dropped" 202
    (post_http ctx ~token:token_a ~name:"blocked" initialized);
  check_status "get" 405
    (send ctx ~seg:"mcp-http" ~token:token_a ~provider:P.Openai ~meth:"GET"
       "/down" "");
  let r = post_http ctx ~token:token_a ~name:"nobody" (discover (Int_id 3)) in
  check_status "unknown" 404 r;
  let id, _, _, _ = error_of r.body in
  Alcotest.check id_testable "unknown server's id" (Int_id 3) id;
  let r = post_http ctx ~token:token_a ~name:"down" (discover (Int_id 2)) in
  check_status "down" 502 r;
  let id, _, _, data = error_of r.body in
  Alcotest.check id_testable "id" (Int_id 2) id;
  Alcotest.(check string) "type" "bactrian_mcp_server_error" (error_type data);
  let f = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "transport errors" 1 f.transport_errors;
  Alcotest.(check int)
    "blocked server untouched" 0
    (List.length (Fake_mcp.requests fake));
  match (List.hd (mcp_exchanges (recording ctx run))).outcome with
  | Transport_error _ -> ()
  | Response _ -> Alcotest.fail "expected a recorded transport error"

let () =
  Alcotest.run "mcp_http"
    [
      ( "record and replay",
        [ modern_round_trip; legacy_session; event_stream; event_stream_events ]
      );
      ("refusals", [ target; refusals ]);
    ]
