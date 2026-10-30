(* The MCP server over Streamable HTTP: the checks on each request, as pure
   functions, and the listener in this process with tools that answer at once
   or wait to be cancelled. The client runs in a system thread, so the server's
   event loop keeps going while it waits. *)

open Bactrian_core
open Bactrian_server
open Server_support
module Interrupt = Bactrian_runtime.Interrupt
module H = Mcp_client.Http

let port = 4000
let token = "a-token-that-is-long-enough-0123456789"

let config : Mcp_http.config =
  { port; token; allowed_origins = [ "http://localhost:5173" ] }

let good =
  [
    ("host", "127.0.0.1:4000");
    ("authorization", "Bearer " ^ token);
    ("content-type", "application/json");
  ]

let replace name value headers =
  (name, value) :: List.filter (fun (k, _) -> not (String.equal k name)) headers

let without name headers =
  List.filter (fun (k, _) -> not (String.equal k name)) headers

let admitted ?(meth = `POST) ?(resource = "/mcp") headers =
  match
    Mcp_http.admit config
      (Http.Request.make ~meth ~headers:(Http.Header.of_list headers) resource)
  with
  | Ok () -> 200
  | Error r -> r.status

let test_admit () =
  let check what expected ?meth ?resource headers =
    Alcotest.(check int) what expected (admitted ?meth ?resource headers)
  in
  check "a good request" 200 good;
  check "localhost names it too" 200 (replace "host" "localhost:4000" good);
  check "a JSON media type with parameters" 200
    (replace "content-type" "application/json; charset=utf-8" good);
  check "another host: DNS rebinding" 403
    (replace "host" "evil.example:4000" good);
  check "another port" 403 (replace "host" "127.0.0.1:4001" good);
  check "no host" 403 (without "host" good);
  check "an origin of this server" 200
    (("origin", "http://127.0.0.1:4000") :: good);
  check "an allowed origin" 200 (("origin", "http://localhost:5173") :: good);
  check "another origin" 403 (("origin", "http://evil.example") :: good);
  check "the null origin" 403 (("origin", "null") :: good);
  check "another path" 404 ~resource:"/other" good;
  check "a query on the endpoint" 200 ~resource:"/mcp?x=1" good;
  check "GET" 405 ~meth:`GET good;
  check "DELETE" 405 ~meth:`DELETE good;
  check "no token" 401 (without "authorization" good);
  check "a wrong token" 401
    (replace "authorization" ("Bearer " ^ String.uppercase_ascii token) good);
  check "the scheme in any case" 200
    (replace "authorization" ("bearer " ^ token) good);
  check "not bearer" 401 (replace "authorization" ("Basic " ^ token) good);
  check "not JSON" 415 (replace "content-type" "text/plain" good);
  check "no content type" 415 (without "content-type" good);
  check "an allowed origin in another case" 200
    (("origin", "HTTP://LOCALHOST:5173") :: good);
  let preflight =
    [ ("host", "127.0.0.1:4000"); ("origin", "http://localhost:5173") ]
  in
  check "a preflight from an allowed origin" 204 ~meth:`OPTIONS preflight;
  check "a preflight from another origin" 403 ~meth:`OPTIONS
    (replace "origin" "http://evil.example" preflight);
  check "OPTIONS without an origin" 405 ~meth:`OPTIONS (without "origin" good);
  (* Origin before the token: a page learns nothing about the token. *)
  check "a bad origin without the token" 403
    (("origin", "http://evil.example") :: without "authorization" good)

let test_origin () =
  let check what expected v =
    Alcotest.(check (option string))
      what expected
      (Result.to_option (Mcp_http.origin v))
  in
  check "a host and port" (Some "http://localhost:5173") "http://localhost:5173";
  check "in lower case" (Some "https://example.com") "HTTPS://Example.COM";
  check "IPv6" (Some "http://[::1]:8080") "http://[::1]:8080";
  List.iter
    (fun v -> check v None v)
    [
      "http://localhost:5173/";
      "http://localhost:5173/app";
      "*";
      "null";
      "localhost:5173";
      "ftp://example.com";
      "http://";
      "http://a:0";
      "http://a:65536";
      "http://a:";
      "http://a:b";
      "http://user@a";
      "http://[]";
    ]

let test_header_value () =
  let check what expected v =
    Alcotest.(check (option string)) what expected (Mcp_http.header_value v)
  in
  check "plain" (Some "run_start") "run_start";
  check "Base64" (Some "Hello, \u{4e16}\u{754c}")
    "=?base64?SGVsbG8sIOS4lueVjA==?=";
  check "a sentinel, encoded" (Some "=?base64?literal?=")
    "=?base64?PT9iYXNlNjQ/bGl0ZXJhbD89?=";
  check "broken Base64" None "=?base64?***?=";
  check "a control character" None "a\001b";
  check "a byte outside ASCII" None "caf\xc3\xa9"

let rpc ?(meta = true) ~id method_ params =
  let params =
    if meta then H.with_meta ~tasks:false (canon params) else canon params
  in
  match Mcp_msg.decode (H.body ~id method_ params) with
  | Ok m -> m
  | Error _ -> Alcotest.fail "the test's request does not decode"

let modern_headers method_ params =
  Http.Header.of_list
    (H.rpc_headers ~token method_ (H.with_meta ~tasks:false (canon params)))

let era_name : Mcp_dispatch.era -> string = function
  | Undecided -> "undecided"
  | Legacy v -> "legacy " ^ v
  | Modern -> "modern"

let verdict headers msg =
  match Mcp_http.era_of_request headers msg with
  | Ok era -> era_name era
  | Error (Error_response { code; _ }) -> string_of_int code
  | Error (Response _ | Request _ | Notification _) -> "not an error"

let test_era_of_request () =
  let check what expected headers msg =
    Alcotest.(check string) what expected (verdict headers msg)
  in
  let call = {|{"name":"run_start","arguments":{}}|} in
  let edit f method_ params =
    Http.Header.of_list
      (f (Http.Header.to_list (modern_headers method_ params)))
  in
  let drop name =
    List.filter (fun (k, _) ->
        not
          (String.equal (String.lowercase_ascii k)
             (String.lowercase_ascii name)))
  in
  let set name v h = (name, v) :: drop name h in
  check "a modern call" "modern"
    (modern_headers "tools/call" call)
    (rpc ~id:1 "tools/call" call);
  check "no MCP-Protocol-Version" "-32020"
    (edit (drop "MCP-Protocol-Version") "tools/call" call)
    (rpc ~id:1 "tools/call" call);
  check "another MCP-Protocol-Version" "-32020"
    (edit (set "MCP-Protocol-Version" "2025-11-25") "tools/call" call)
    (rpc ~id:1 "tools/call" call);
  check "no Mcp-Method" "-32020"
    (edit (drop "Mcp-Method") "tools/call" call)
    (rpc ~id:1 "tools/call" call);
  check "another Mcp-Method" "-32020"
    (edit (set "Mcp-Method" "tools/list") "tools/call" call)
    (rpc ~id:1 "tools/call" call);
  check "no Mcp-Name on a call" "-32020"
    (edit (drop "Mcp-Name") "tools/call" call)
    (rpc ~id:1 "tools/call" call);
  check "another Mcp-Name" "-32020"
    (edit (set "Mcp-Name" "record_start") "tools/call" call)
    (rpc ~id:1 "tools/call" call);
  check "Mcp-Name in Base64" "modern"
    (edit (set "Mcp-Name" "=?base64?cnVuX3N0YXJ0?=") "tools/call" call)
    (rpc ~id:1 "tools/call" call);
  check "MCP-Protocol-Version in Base64" "-32020"
    (edit
       (set "MCP-Protocol-Version" "=?base64?MjAyNi0wNy0yOA==?=")
       "tools/call" call)
    (rpc ~id:1 "tools/call" call);
  check "Mcp-Method in Base64" "-32020"
    (edit (set "Mcp-Method" "=?base64?dG9vbHMvY2FsbA==?=") "tools/call" call)
    (rpc ~id:1 "tools/call" call);
  check "an Mcp-Name outside ASCII" "-32020"
    (edit (set "Mcp-Name" "run_start\xc3\xa9") "tools/call" call)
    (rpc ~id:1 "tools/call" call);
  let task = {|{"taskId":"job-1"}|} in
  check "a task method names its task" "modern"
    (modern_headers "tasks/get" task)
    (rpc ~id:2 "tasks/get" task);
  check "a task method naming another" "-32020"
    (edit (set "Mcp-Name" "job-2") "tasks/get" task)
    (rpc ~id:2 "tasks/get" task);
  check "the version header, unsupported, as the body" "modern"
    (Http.Header.of_list
       [ ("MCP-Protocol-Version", "2099-01-01"); ("Mcp-Method", "tools/list") ])
    (rpc ~meta:false ~id:3 "tools/list"
       {|{"_meta":{"io.modelcontextprotocol/protocolVersion":"2099-01-01","io.modelcontextprotocol/clientCapabilities":{}}}|});
  let init = {|{"protocolVersion":"2025-11-25","capabilities":{}}|} in
  check "initialize" "undecided" (Http.Header.init ())
    (rpc ~meta:false ~id:4 "initialize" init);
  check "no _meta, no header" "legacy 2025-03-26" (Http.Header.init ())
    (rpc ~meta:false ~id:5 "tools/list" "{}");
  check "no _meta, a legacy header" "legacy 2025-06-18"
    (Http.Header.of_list [ ("mcp-protocol-version", "2025-06-18") ])
    (rpc ~meta:false ~id:5 "tools/list" "{}");
  check "no _meta, the modern header" "modern"
    (Http.Header.of_list [ ("mcp-protocol-version", "2026-07-28") ])
    (rpc ~meta:false ~id:5 "tools/list" "{}");
  check "no _meta, an unknown header" "-32600"
    (Http.Header.of_list [ ("mcp-protocol-version", "1999-01-01") ])
    (rpc ~meta:false ~id:5 "tools/list" "{}")

let test_status () =
  let err code : Mcp_msg.message =
    Error_response { id = None; code; message = ""; data = None }
  in
  let ok : Mcp_msg.message = Response { id = Int_id 1; result = canon "{}" } in
  let check what expected era m =
    Alcotest.(check int) what expected (Mcp_http.status era m)
  in
  check "a result" 200 Modern ok;
  check "an unknown method" 404 Modern (err (-32601));
  check "an internal error" 500 Modern (err (-32603));
  check "a header mismatch" 400 Modern (err (-32020));
  check "missing _meta" 400 Modern (err (-32602));
  check "an unsupported version" 400 Modern (err (-32022));
  check "a legacy unknown method" 200 (Legacy "2025-11-25") (err (-32601));
  check "a legacy malformed request" 400 (Legacy "2025-11-25") (err (-32600))

(* The listener *)

(* A request from a system thread, which leaves the event loop to the
   server. *)
let client ~port g = Eio_unix.run_in_systhread (fun () -> g ~port)

let with_listener ?head_timeout f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let started, set_started = Eio.Promise.create () in
  let cancelled, set_cancelled = Eio.Promise.create () in
  let released, set_released = Eio.Promise.create () in
  let handlers : Mcp_dispatch.handlers =
    {
      call =
        (fun ~tasks:_ name _ ->
          match name with
          | "slow" ->
              Eio.Promise.resolve set_started ();
              Fun.protect
                ~finally:(fun () ->
                  if not (Eio.Promise.is_resolved released) then
                    ignore (Eio.Promise.try_resolve set_cancelled () : bool))
                (fun () -> Eio.Promise.await released);
              Complete (canon {|{"isError":false}|})
          | _ -> Complete (canon {|{"isError":false}|}));
      task = (fun _ _ -> None);
    }
  in
  match Mcp_http.listen ~sw ~net:(Eio.Stdenv.net env) 0 with
  | Error m -> Alcotest.fail m
  | Ok (socket, port) ->
      let stop = Interrupt.trigger () in
      let config = { config with port } in
      let ended =
        Eio.Fiber.pair
          (fun () ->
            Mcp_http.serve
              ~server:
                {
                  server with
                  tools = [ ("echo", canon "{}"); ("slow", canon "{}") ];
                }
              ~handlers ~stop ~log:ignore ~clock:(Eio.Stdenv.clock env)
              ?head_timeout config socket)
          (fun () ->
            let within p () =
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                  Eio.Promise.await p)
            in
            f ~port ~started:(within started) ~cancelled:(within cancelled)
              ~release:(Eio.Promise.resolve set_released);
            Interrupt.fire stop Sys.sigterm)
        |> fst
      in
      Alcotest.(check bool)
        "stopped by the trigger" true
        (match ended with
        | Stopped s -> s = Sys.sigterm
        | Input_closed -> false)

let call_params name =
  H.with_meta ~tasks:false
    (canon (Printf.sprintf {|{"name":"%s","arguments":{}}|} name))

let call_headers ?accept name =
  H.rpc_headers ?accept ~token "tools/call" (call_params name)

let test_listener () =
  with_listener @@ fun ~port ~started:_ ~cancelled:_ ~release:_ ->
  let sse =
    client ~port (fun ~port ->
        H.send ~port (call_headers "echo")
          (H.body ~id:1 "tools/call" (call_params "echo")))
  in
  Alcotest.(check int) "200" 200 sse.status;
  Alcotest.(check (option string))
    "an SSE stream" (Some "text/event-stream")
    (H.header sse "content-type");
  Alcotest.(check (option string))
    "unbuffered by proxies" (Some "no")
    (H.header sse "x-accel-buffering");
  Alcotest.(check int) "one event" 1 (List.length (H.events sse.body));
  Alcotest.(check (option string))
    "the result" (Some "complete")
    (Result.to_option (H.outcome sse)
    |> Fun.flip Option.bind (Canon.str_member "resultType"));
  let json =
    client ~port (fun ~port ->
        H.send ~port
          (call_headers ~accept:"application/json" "echo")
          (H.body ~id:2 "tools/call" (call_params "echo")))
  in
  Alcotest.(check (option string))
    "JSON when SSE is not accepted" (Some "application/json")
    (H.header json "content-type");
  Alcotest.(check bool) "a response" true (Result.is_ok (H.outcome json));
  let note =
    client ~port (fun ~port ->
        H.send ~port
          (List.filter
             (fun (k, _) ->
               String.equal k "Authorization" || String.equal k "Content-Type")
             (call_headers "echo"))
          {|{"jsonrpc":"2.0","method":"notifications/initialized"}|})
  in
  Alcotest.(check int) "a notification is accepted" 202 note.status;
  let answer =
    client ~port (fun ~port ->
        H.send ~port (call_headers "echo")
          {|{"jsonrpc":"2.0","id":3,"result":{}}|})
  in
  Alcotest.(check int) "a response from the client" 400 answer.status;
  let garbage =
    client ~port (fun ~port -> H.send ~port (call_headers "echo") "{")
  in
  Alcotest.(check int) "not JSON" 400 garbage.status;
  Alcotest.(check (option int))
    "a parse error" (Some (-32700))
    (match H.outcome garbage with Error e -> Some e.code | Ok _ -> None);
  let refused = client ~port (fun ~port -> H.send ~port [] "{}") in
  Alcotest.(check int) "a request without the token" 401 refused.status;
  Alcotest.(check (option string))
    "says how to authenticate" (Some "Bearer")
    (H.header refused "www-authenticate");
  let forbidden =
    client ~port (fun ~port ->
        H.send ~port
          (("Origin", "http://evil.example") :: call_headers "echo")
          "{}")
  in
  Alcotest.(check int) "a foreign origin" 403 forbidden.status;
  (match H.message forbidden with
  | Error_response { id = None; _ } -> ()
  | Error_response { id = Some _; _ } | Response _ | Request _ | Notification _
    ->
      Alcotest.fail "an error without an id");
  let get =
    client ~port (fun ~port ->
        H.send ~meth:"GET" ~port (call_headers "echo") "")
  in
  Alcotest.(check int) "GET" 405 get.status;
  Alcotest.(check (option string))
    "names POST" (Some "POST") (H.header get "allow")

(* Hanging up cancels the call, and the server goes on. *)
let test_hang_up () =
  with_listener @@ fun ~port ~started ~cancelled ~release:_ ->
  client ~port (fun ~port ->
      H.hang_up ~port (call_headers "slow")
        (H.body ~id:1 "tools/call" (call_params "slow")));
  started ();
  cancelled ();
  let after =
    client ~port (fun ~port ->
        H.send ~port (call_headers "echo")
          (H.body ~id:2 "tools/call" (call_params "echo")))
  in
  Alcotest.(check int) "the next request is answered" 200 after.status

(* What a connection got before the server closed it, or reset it while the
   client was still writing. *)
let until_closed ~port text =
  client ~port (fun ~port ->
      match H.exchange ~timeout:10. ~port text with
      | got -> got
      | exception Unix.Unix_error ((ECONNRESET | EPIPE), _, _) -> "")

let statuses got = List.map (fun (r : H.response) -> r.status) (H.answers got)
let status_list = List.map (fun (r : H.response) -> r.status)

(* A connection gets bounded time until its request is admitted, and
   an admitted one gets what it needs. *)
let test_bounds () =
  with_listener ~head_timeout:0.3
  @@ fun ~port ~started:_ ~cancelled:_ ~release:_ ->
  Alcotest.(check string)
    "an idle connection is closed" "" (until_closed ~port "");
  Alcotest.(check string)
    "so is one that stops in its head" ""
    (until_closed ~port "POST /mcp HTTP/1.1\r\nHost: 127.0.0.1\r\n");
  Alcotest.(check (list int))
    "a kept-alive connection idles out after its answer" [ 200 ]
    (statuses
       (until_closed ~port
          (H.request ~close:false ~port (call_headers "echo")
             (H.body ~id:1 "tools/call" (call_params "echo")))));
  let padded =
    H.with_meta ~tasks:false
      (canon
         (Printf.sprintf {|{"name":"echo","arguments":{"pad":"%s"}}|}
            (String.make 300_000 'x')))
  in
  Alcotest.(check int)
    "an admitted request may be long" 200
    (client ~port (fun ~port ->
         H.send ~port (call_headers "echo") (H.body ~id:2 "tools/call" padded)))
      .status

let answer_id (r : H.response) =
  let int : Mcp_msg.id -> int option = function
    | Int_id i -> Some i
    | String_id _ -> None
  in
  match H.message r with
  | Response { id; _ } -> int id
  | Error_response { id; _ } -> Option.bind id int
  | Request _ | Notification _ -> None

(* The body of a refused request is dropped, never parsed as the next request,
   which no check of the refused head saw. *)
let test_refused_body () =
  with_listener @@ fun ~port ~started:_ ~cancelled:_ ~release:_ ->
  let echo id =
    H.request ~close:false ~port (call_headers "echo")
      (H.body ~id "tools/call" (call_params "echo"))
  in
  let smuggling =
    H.request ~close:false ~port
      [
        ("Host", "evil.example");
        ("Origin", "http://evil.example");
        ("Content-Type", "text/plain");
      ]
      (echo 7)
  in
  let last =
    H.request ~port (call_headers "echo")
      (H.body ~id:8 "tools/call" (call_params "echo"))
  in
  let got = H.answers (until_closed ~port (smuggling ^ last)) in
  Alcotest.(check (list int))
    "the refusal, then the next request" [ 403; 200 ] (status_list got);
  Alcotest.(check (list (option int)))
    "the smuggled request is not run" [ None; Some 8 ] (List.map answer_id got);
  let long =
    H.request ~close:false ~port
      (List.filter
         (fun (k, _) -> not (String.equal k "Authorization"))
         (call_headers "echo"))
      (String.make 100_000 'x')
  in
  let got = H.answers (until_closed ~port (long ^ last)) in
  Alcotest.(check (list int))
    "a long refused body ends the connection" [ 401 ] (status_list got);
  Alcotest.(check (list (option string)))
    "and says so" [ Some "close" ]
    (List.map (fun r -> H.header r "connection") got)

let test_too_long () =
  with_listener @@ fun ~port ~started:_ ~cancelled:_ ~release:_ ->
  let got =
    H.answers
      (until_closed ~port
         (H.request ~close:false ~port (call_headers "echo")
            (String.make ((16 * 1024 * 1024) + 1) ' ')
         ^ H.request ~port (call_headers "echo")
             (H.body ~id:1 "tools/call" (call_params "echo"))))
  in
  Alcotest.(check (list int))
    "413, and the connection ends" [ 413 ] (status_list got);
  Alcotest.(check (list (option string)))
    "as it says" [ Some "close" ]
    (List.map (fun r -> H.header r "connection") got)

(* A page from an allowed origin gets through its preflight and can read the
   answers. *)
let test_cors () =
  with_listener @@ fun ~port ~started:_ ~cancelled:_ ~release:_ ->
  let page = ("Origin", "http://localhost:5173") in
  let preflight =
    client ~port (fun ~port ->
        H.send ~meth:"OPTIONS" ~port
          [
            page;
            ("Access-Control-Request-Method", "POST");
            ( "Access-Control-Request-Headers",
              "authorization, content-type, mcp-method, mcp-name" );
          ]
          "")
  in
  Alcotest.(check int) "the preflight" 204 preflight.status;
  let header what expected r name =
    Alcotest.(check (option string)) what expected (H.header r name)
  in
  header "the origin may read" (Some "http://localhost:5173") preflight
    "access-control-allow-origin";
  header "and POST" (Some "POST") preflight "access-control-allow-methods";
  header "with the headers it asked for"
    (Some "authorization, content-type, mcp-method, mcp-name") preflight
    "access-control-allow-headers";
  let post headers =
    client ~port (fun ~port ->
        H.send ~port headers (H.body ~id:1 "tools/call" (call_params "echo")))
  in
  let from_page = post (page :: call_headers "echo") in
  Alcotest.(check int) "the POST" 200 from_page.status;
  header "its answer is readable" (Some "http://localhost:5173") from_page
    "access-control-allow-origin";
  header "and varies by origin" (Some "Origin") from_page "vary";
  let refused =
    post
      (page
      :: List.filter
           (fun (k, _) -> not (String.equal k "Authorization"))
           (call_headers "echo"))
  in
  Alcotest.(check int) "no token" 401 refused.status;
  header "a refusal is readable too" (Some "http://localhost:5173") refused
    "access-control-allow-origin";
  header "nothing for a client without an origin" None
    (post (call_headers "echo"))
    "access-control-allow-origin";
  header "nor for a foreign origin" None
    (post (("Origin", "http://evil.example") :: call_headers "echo"))
    "access-control-allow-origin"

(* The next request on the connection, sent while a call runs, is no hang-up:
   the call goes on, and the request is answered after it. *)
let test_pipelined () =
  with_listener @@ fun ~port ~started ~cancelled:_ ~release ->
  let calls =
    H.request ~close:false ~port (call_headers "slow")
      (H.body ~id:1 "tools/call" (call_params "slow"))
    ^ H.request ~port (call_headers "echo")
        (H.body ~id:2 "tools/call" (call_params "echo"))
  in
  let got, () =
    Eio.Fiber.pair
      (fun () -> H.answers (until_closed ~port calls))
      (fun () ->
        started ();
        Eio_unix.sleep 0.3;
        release ())
  in
  Alcotest.(check (list int)) "both answered" [ 200; 200 ] (status_list got);
  Alcotest.(check (list (option int)))
    "in order" [ Some 1; Some 2 ] (List.map answer_id got)

let test_endless_head () =
  with_listener @@ fun ~port ~started:_ ~cancelled:_ ~release:_ ->
  let pad =
    String.concat "" (List.init 3000 (Printf.sprintf "X-Pad-%d: %064d\r\n" 0))
  in
  Alcotest.(check string)
    "a head past the bound is cut" ""
    (until_closed ~port ("POST /mcp HTTP/1.1\r\nHost: 127.0.0.1\r\n" ^ pad))

(* Closing the connection after a pipelined request still cancels the call. *)
let test_pipelined_hang_up () =
  with_listener @@ fun ~port ~started ~cancelled ~release:_ ->
  client ~port (fun ~port ->
      H.hang_up ~port
        ~pipelined:
          (H.request ~port (call_headers "echo")
             (H.body ~id:2 "tools/call" (call_params "echo")))
        (call_headers "slow")
        (H.body ~id:1 "tools/call" (call_params "slow")));
  started ();
  cancelled ()

(* A POST with neither Content-Length nor Transfer-Encoding has no body, so it
   is answered at once and the next request follows its head. *)
let test_no_length () =
  with_listener @@ fun ~port ~started:_ ~cancelled:_ ~release:_ ->
  let head headers =
    Printf.sprintf "POST /mcp HTTP/1.1\r\nHost: 127.0.0.1:%d\r\n%s\r\n" port
      (String.concat ""
         (List.map (fun (k, v) -> k ^ ": " ^ v ^ "\r\n") headers))
  in
  let last =
    H.request ~port (call_headers "echo")
      (H.body ~id:8 "tools/call" (call_params "echo"))
  in
  Alcotest.(check (list int))
    "refused, then the next request" [ 401; 200 ]
    (statuses
       (until_closed ~port
          (head [ ("Content-Type", "application/json") ] ^ last)));
  Alcotest.(check (list int))
    "admitted with an empty body, then the next request" [ 400; 200 ]
    (statuses (until_closed ~port (head (call_headers "echo") ^ last)))

(* A body whose length cannot be told is not read, and a request with both
   length headers ends the connection, so a request inside the body, or one
   after it, is not answered. A Content-Length that is not plain digits, or
   that repeats with another value, has no usable length either. *)
let test_bad_framing () =
  with_listener @@ fun ~port ~started:_ ~cancelled:_ ~release:_ ->
  let inner =
    H.request ~port (call_headers "echo")
      (H.body ~id:9 "tools/call" (call_params "echo"))
  in
  let outer headers body =
    let head =
      String.concat ""
        (List.map
           (fun (k, v) -> k ^ ": " ^ v ^ "\r\n")
           (("Host", Printf.sprintf "127.0.0.1:%d" port) :: headers))
    in
    until_closed ~port ("POST /mcp HTTP/1.1\r\n" ^ head ^ "\r\n" ^ body)
  in
  let inner_length = string_of_int (String.length inner) in
  let framings =
    [
      ("Transfer-Encoding gzip", [ ("Transfer-Encoding", "gzip") ]);
      ("Content-Length abc", [ ("Content-Length", "abc") ]);
      ("Content-Length -5", [ ("Content-Length", "-5") ]);
      ("Content-Length -1", [ ("Content-Length", "-1") ]);
      ("Content-Length +5", [ ("Content-Length", "+5") ]);
      ("Content-Length 0x10", [ ("Content-Length", "0x10") ]);
      ("Content-Length 1_0", [ ("Content-Length", "1_0") ]);
      ("Content-Length 5, 5", [ ("Content-Length", "5, 5") ]);
      ("Content-Length empty", [ ("Content-Length", "") ]);
      ( "Content-Length twice, then 0",
        [ ("Content-Length", inner_length); ("Content-Length", "0") ] );
      ( "Content-Length 0, then twice",
        [ ("Content-Length", "0"); ("Content-Length", inner_length) ] );
    ]
  in
  List.iter
    (fun (name, framing) ->
      Alcotest.(check (list int))
        (name ^ ", refused") [ 403 ]
        (statuses
           (outer
              (framing
              @ [
                  ("Origin", "https://evil.example");
                  ("Content-Type", "text/plain");
                ])
              inner));
      Alcotest.(check (list int))
        (name ^ ", admitted") [ 400 ]
        (statuses (outer (framing @ call_headers "echo") inner)))
    framings;
  Alcotest.(check (list int))
    "both length headers, then a request" [ 400 ]
    (statuses
       (outer
          ([ ("Transfer-Encoding", "chunked"); ("Content-Length", "5") ]
          @ call_headers "echo")
          ("0\r\n\r\n" ^ inner)));
  let call = H.body ~id:10 "tools/call" (call_params "echo") in
  let length = string_of_int (String.length call) in
  Alcotest.(check (list int))
    "the same Content-Length twice is one length" [ 200 ]
    (statuses
       (outer
          (call_headers "echo"
          @ [
              ("Content-Length", length);
              ("Content-Length", length);
              ("Connection", "close");
            ])
          call))

(* A GET, HEAD or TRACE cohttp reads no body for may still carry one by its
   headers. It is never parsed as the next request, so exactly one answer
   arrives (the hidden request would add a second) and the connection ends. *)
let test_bodyless_method_with_body () =
  with_listener @@ fun ~port ~started:_ ~cancelled:_ ~release:_ ->
  let inner =
    H.request ~port (call_headers "echo")
      (H.body ~id:9 "tools/call" (call_params "echo"))
  in
  let framings =
    [
      ( "Content-Length",
        [ ("Content-Length", string_of_int (String.length inner)) ],
        inner );
      ( "chunked",
        [ ("Transfer-Encoding", "chunked") ],
        Printf.sprintf "%x\r\n%s\r\n0\r\n\r\n" (String.length inner) inner );
    ]
  in
  let outer meth headers body =
    let head =
      String.concat ""
        (List.map
           (fun (k, v) -> k ^ ": " ^ v ^ "\r\n")
           (("Host", Printf.sprintf "127.0.0.1:%d" port) :: headers))
    in
    until_closed ~port (meth ^ " /mcp HTTP/1.1\r\n" ^ head ^ "\r\n" ^ body)
  in
  List.iter
    (fun meth ->
      List.iter
        (fun (name, framing, body) ->
          let check what extra expected =
            let got = H.answers (outer meth (framing @ extra) body) in
            let label = String.concat ", " [ meth; name; what ] in
            Alcotest.(check (list int)) label expected (status_list got);
            Alcotest.(check (list (option string)))
              (label ^ ": and the connection ends")
              [ Some "close" ]
              (List.map (fun r -> H.header r "connection") got)
          in
          check "refused" [ ("Origin", "https://evil.example") ] [ 403 ];
          check "admitted" (call_headers "echo") [ 405 ])
        framings)
    [ "GET"; "HEAD"; "TRACE" ]

(* A connection that sends refused requests and never reads the answers, until
   the server stops reading them: its writes stall, or it has closed the
   connection. *)
let stall ~port =
  let fd = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  Unix.setsockopt_int fd SO_RCVBUF 4096;
  Unix.setsockopt_float fd SO_SNDTIMEO 0.2;
  Unix.connect fd (ADDR_INET (Unix.inet_addr_loopback, port));
  let batch =
    String.concat ""
      (List.init 1000 (fun _ ->
           "POST /mcp HTTP/1.1\r\nContent-Length: 0\r\n\r\n"))
  in
  let rec fill off =
    match Unix.write_substring fd batch off (String.length batch - off) with
    | k -> fill ((off + k) mod String.length batch)
    | exception
        Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EPIPE | ECONNRESET), _, _) ->
        ()
  in
  fill 0;
  fd

(* Clients that never read fill all 64 connections, and are closed at the
   deadline all the same, so the next client is answered once one ends. *)
let test_stalled () =
  with_listener ~head_timeout:3.
  @@ fun ~port ~started:_ ~cancelled:_ ~release:_ ->
  let stalled =
    Eio.Fiber.List.map
      (fun () -> client ~port (fun ~port -> stall ~port))
      (List.init 64 (fun _ -> ()))
  in
  Fun.protect ~finally:(fun () -> List.iter Unix.close stalled) @@ fun () ->
  let start = Unix.gettimeofday () in
  let answer =
    client ~port (fun ~port ->
        H.send ~timeout:10. ~port (call_headers "echo")
          (H.body ~id:1 "tools/call" (call_params "echo")))
  in
  Alcotest.(check int) "the next client is answered" 200 answer.status;
  Alcotest.(check bool)
    "once a connection is free" true
    (Unix.gettimeofday () -. start > 1.)

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Alcotest.run "mcp_http"
    [
      ( "checks",
        [
          Alcotest.test_case "admission" `Quick test_admit;
          Alcotest.test_case "origins" `Quick test_origin;
          Alcotest.test_case "header values" `Quick test_header_value;
          Alcotest.test_case "headers and eras" `Quick test_era_of_request;
          Alcotest.test_case "statuses" `Quick test_status;
        ] );
      ( "listener",
        [
          Alcotest.test_case "answers" `Quick test_listener;
          Alcotest.test_case "hanging up cancels" `Quick test_hang_up;
          Alcotest.test_case "bounds before admission" `Quick test_bounds;
          Alcotest.test_case "an endless head" `Quick test_endless_head;
          Alcotest.test_case "a refused body" `Quick test_refused_body;
          Alcotest.test_case "a body too long" `Quick test_too_long;
          Alcotest.test_case "CORS" `Quick test_cors;
          Alcotest.test_case "a request during a call" `Quick test_pipelined;
          Alcotest.test_case "hanging up after a request during a call" `Quick
            test_pipelined_hang_up;
          Alcotest.test_case "a POST without a length" `Quick test_no_length;
          Alcotest.test_case "clients that never read" `Quick test_stalled;
          Alcotest.test_case "a body of unusable length" `Quick test_bad_framing;
          Alcotest.test_case "a body on GET, HEAD and TRACE" `Quick
            test_bodyless_method_with_body;
        ] );
    ]
