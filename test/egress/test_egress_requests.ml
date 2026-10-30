(* Requests on an egress tunnel: which request targets reach the tunnel's host
   and which are refused. *)

open Bactrian_runtime
open Proxy_support
open Egress_world

let () = Mirage_crypto_rng_unix.use_default ()

let test_target () =
  let target = E.target ~host:"api.example.com" ~port:443 in
  let ok = Alcotest.(result (pair string (option string)) string) in
  let accepted text expected =
    Alcotest.check ok text (Ok expected) (target text)
  in
  accepted "/v1/x?a=1&b" ("/v1/x", Some "a=1&b");
  accepted "//other.example/x" ("//other.example/x", None);
  accepted "/a:@127.0.0.1/x" ("/a:@127.0.0.1/x", None);
  accepted "https://API.example.com/p?q=1" ("/p", Some "q=1");
  accepted "https://api.example.com:443" ("/", None);
  accepted "https://api.example.com.:443/p" ("/p", None);
  accepted "https://api.example.com?q" ("/", Some "q");
  List.iter
    (fun text ->
      Alcotest.(check bool) text true (Result.is_error (target text)))
    [
      "a:@127.0.0.1%3A9/stolen";
      "@127.0.0.1:9/x";
      "https://other.example/p";
      "https://api.example.com:8443/p";
      "https://x@api.example.com/p";
      "https://api.example.com@other.example/p";
      "http://api.example.com/p";
      "*";
      "/p#fragment";
      "api.example.com:443";
    ];
  Alcotest.(check bool)
    "an IPv6 tunnel" true
    (Result.is_ok (E.target ~host:"::1" ~port:9 "https://[::1]:9/p"))

(* With localhost passed through and 127.0.0.1 blocked, a request on the
   localhost tunnel must not reach 127.0.0.1, whatever its target says. The
   fake server answers on both names, so the backend counts what got through. *)
let test_targets_stay_on_the_tunnel =
  with_world "targets" @@ fun w ->
  let _ =
    register w ~token:token_a (Some (policy [ ("localhost", Passthrough) ]))
  in
  let on_localhost ?host_header path =
    through ~target_host:"localhost" ?host_header ~creds:(creds token_a) w
      ~meth:"GET" ~path ()
  in
  List.iter
    (fun path ->
      let a = on_localhost path in
      Alcotest.(check int) path 400 a.status;
      Alcotest.(check int)
        (path ^ ": nothing reached the server")
        0
        (List.length (F.requests w.up)))
    [
      Printf.sprintf "a:@127.0.0.1%%3A%d/stolen?secret=1" w.https;
      Printf.sprintf "https://127.0.0.1:%d/stolen" w.https;
      Printf.sprintf "https://localhost:%d@127.0.0.1:%d/stolen" w.https w.https;
      Printf.sprintf "@127.0.0.1:%d/stolen" w.https;
    ];
  let absolute =
    on_localhost (Printf.sprintf "https://localhost:%d/abs?x=1" w.https)
  in
  Alcotest.(check (pair int string))
    "the absolute form of the tunnel's own host"
    (200, {|{"path":"/abs","query":"x=1","body":""}|})
    (absolute.status, absolute.body);
  Alcotest.(check bool) "a whole body ends" true absolute.ended;
  let fronted = on_localhost ~host_header:"127.0.0.1" "/fronted" in
  Alcotest.(check int) "another Host header is answered" 200 fronted.status;
  let host_seen =
    match List.rev (F.requests w.up) with
    | last :: _ -> List.assoc_opt "host" last.headers
    | [] -> None
  in
  Alcotest.(check (option string))
    "but sent to the tunnel's host"
    (Some (Printf.sprintf "localhost:%d" w.https))
    host_seen;
  let f = Model_proxy.unregister w.ctx.proxy ~token:token_a in
  Alcotest.(check (list (pair string int)))
    "the trace names the tunnel's host"
    [
      ("localhost", 400);
      ("localhost", 400);
      ("localhost", 400);
      ("localhost", 400);
      ("localhost", 200);
      ("localhost", 200);
    ]
    (List.map
       (fun (c : Bactrian_core.Trace_builder.http_call) -> (c.host, c.status))
       f.http_calls)

(* A secret in the path is redacted wherever it is kept, and sent as it is. *)
let test_path_redacted =
  with_world "path-redacted" @@ fun w ->
  let run =
    register w ~redact:[ "tok-[0-9]+" ] ~token:token_a
      (Some (policy [ ("127.0.0.1", Record) ]))
  in
  let a =
    through ~creds:(creds token_a) w ~meth:"GET"
      ~path:"/bot/tok-12345/send?x=tok-999" ()
  in
  Alcotest.(check int) "answered" 200 a.status;
  Alcotest.(check (list string))
    "the host got the real path" [ "/bot/tok-12345/send" ]
    (List.map (fun (r : F.request) -> r.path) (F.requests w.up));
  let f = Model_proxy.unregister w.ctx.proxy ~token:token_a in
  let secret = Re.compile (Re.str "tok-") in
  List.iter
    (fun (c : Bactrian_core.Trace_builder.http_call) ->
      Alcotest.(check bool) ("trace: " ^ c.path) false (Re.execp secret c.path))
    f.http_calls;
  List.iter
    (fun (e : Bactrian_core.Cassette.exchange) ->
      Alcotest.(check bool)
        ("cassette: " ^ e.path) false (Re.execp secret e.path);
      Alcotest.(check bool)
        "canonical" false
        (Re.execp secret (Bactrian_core.Canon.to_string e.canonical)))
    (recording w.ctx run).exchanges

(* The answer to a HEAD carries the length of a body it does not have, and a
   204 has none: both reach the agent whole and without a body. *)
let bodiless_answers line =
  if String.starts_with ~prefix:"HEAD " line then
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 5\r\n\r\n"
  else "HTTP/1.1 204 No Content\r\n\r\n"

let test_bodiless =
  with_world ~raw:bodiless_answers "bodiless" @@ fun w ->
  let run =
    register w ~token:token_a (Some (policy [ ("127.0.0.1", Record) ]))
  in
  List.iter
    (fun (meth, status) ->
      let a = through ~creds:(creds token_a) w ~meth ~path:"/thing" () in
      Alcotest.(check (pair int string)) meth (status, "") (a.status, a.body);
      Alcotest.(check bool)
        (meth ^ ": no body framing")
        false
        (Re.execp (Re.compile (Re.str "transfer-encoding")) a.head))
    [ ("HEAD", 200); ("DELETE", 204) ];
  let f = Model_proxy.unregister w.ctx.proxy ~token:token_a in
  Alcotest.(check (list string)) "no failure" [] f.errors;
  Alcotest.(check (list bool))
    "recorded whole" [ true; true ]
    (List.map
       (fun (e : Bactrian_core.Cassette.exchange) ->
         match e.outcome with
         | Response { body = Whole { complete; _ } | Stream { complete; _ }; _ }
           ->
             complete
         | Transport_error _ -> false)
       (recording w.ctx run).exchanges)

(* An agent whose clock libfaketime set to the run's SIM_NOW, a month back,
   still accepts the tunnel's certificate. *)
let test_past_sim_now =
  with_world "past-sim-now" @@ fun w ->
  let then_ =
    Option.get (Ptime.sub_span (now ()) (Ptime.Span.of_int_s (30 * 86_400)))
  in
  let _ =
    register w
      ~sim_now:(Ptime.to_rfc3339 ~tz_offset_s:0 then_)
      ~token:token_a
      (Some (policy [ ("127.0.0.1", Passthrough) ]))
  in
  let a =
    through
      ~at:(fun () -> then_)
      ~creds:(creds token_a) w ~meth:"GET" ~path:"/x" ()
  in
  Alcotest.(check int) "answered" 200 a.status;
  ignore (Model_proxy.unregister w.ctx.proxy ~token:token_a)

let has text = Re.execp (Re.compile (Re.str text))
let noted (f : Model_proxy.findings) text = List.exists (has text) f.errors

(* Refused inside the tunnel, before the host is called. *)
let test_refusals =
  with_world ~max_body:64 "refusals" @@ fun w ->
  let _ =
    register w ~token:token_a (Some (policy [ ("127.0.0.1", Passthrough) ]))
  in
  let big =
    through ~creds:(creds token_a) w ~meth:"POST" ~path:"/big"
      ~body:(String.make 100 'x') ()
  in
  Alcotest.(check int) "too large: 413" 413 big.status;
  Alcotest.(check bool)
    "bactrian_payload_too_large" true
    (has "bactrian_payload_too_large" big.body);
  let ws =
    through ~creds:(creds token_a) w ~meth:"GET" ~path:"/ws"
      ~headers:[ ("Upgrade", "websocket") ]
      ()
  in
  Alcotest.(check int) "an upgrade: 501" 501 ws.status;
  Alcotest.(check bool)
    "bactrian_unsupported" true
    (has "bactrian_unsupported" ws.body);
  Alcotest.(check int)
    "nothing reached the server" 0
    (List.length (F.requests w.up));
  let f = Model_proxy.unregister w.ctx.proxy ~token:token_a in
  Alcotest.(check bool) "413 noted" true (noted f "request body too large");
  Alcotest.(check bool) "501 noted" true (noted f "WebSocket");
  Alcotest.check call "both are calls"
    [
      (("POST", "127.0.0.1"), ("/big", 413));
      (("GET", "127.0.0.1"), ("/ws", 501));
    ]
    (flat (calls f))

let cut_short (r : F.request) : F.reply =
  if String.equal r.path "/cut" then
    {
      status = 200;
      headers = [ ("content-type", "text/plain") ];
      chunks = [ "partial" ];
      ending = Drop;
    }
  else backend r

(* A host that drops its answer, and one that cannot be reached. *)
let test_upstream_failures =
  with_world ~handler:cut_short "upstream-failures" @@ fun w ->
  let p = policy [ ("127.0.0.1", Record) ] in
  let run = register w ~token:token_a (Some p) in
  let cut = through ~creds:(creds token_a) w ~meth:"GET" ~path:"/cut" () in
  Alcotest.(check int) "the head is passed on" 200 cut.status;
  Alcotest.(check (pair string bool))
    "and the body, unfinished" ("partial", false) (cut.body, cut.ended);
  let f = Model_proxy.unregister w.ctx.proxy ~token:token_a in
  Alcotest.(check bool) "noted" true (noted f "egress to 127.0.0.1");
  Alcotest.(check (list bool))
    "recorded as incomplete" [ false ]
    (List.map
       (fun (e : Bactrian_core.Cassette.exchange) ->
         match e.outcome with
         | Response { body = Whole { complete; _ } | Stream { complete; _ }; _ }
           ->
             complete
         | Transport_error _ -> true)
       (recording w.ctx run).exchanges);
  (* The plain connector refuses https: nothing is sent. *)
  let unreachable =
    register w ~connector:Upstream.plain ~token:token_b (Some p)
  in
  let a = through ~creds:(creds token_b) w ~meth:"GET" ~path:"/x" () in
  Alcotest.(check int) "unreachable: 502" 502 a.status;
  Alcotest.(check bool)
    "bactrian_upstream_error" true
    (has "bactrian_upstream_error" a.body);
  ignore (Model_proxy.unregister w.ctx.proxy ~token:token_b);
  Alcotest.(check (list bool))
    "recorded as a transport error" [ true ]
    (List.map
       (fun (e : Bactrian_core.Cassette.exchange) ->
         match e.outcome with Transport_error _ -> true | Response _ -> false)
       (recording w.ctx unreachable).exchanges)

(* TLS that never starts: an agent that does not trust the project CA, and a
   CONNECT host that no certificate can name. *)
let test_tls_refused =
  with_world "tls-refused" @@ fun w ->
  let _ =
    register w ~token:token_a (Some { default = Passthrough; hosts = [] })
  in
  let other =
    load (Eio.Path.native_exn Eio.Path.(w.ctx.dir / "other-ca"))
    |> fst |> ca_cert
  in
  (match
     through ~anchors:[ other ] ~creds:(creds token_a) w ~meth:"GET" ~path:"/x"
       ()
   with
  | exception Tls_eio.Tls_failure _ -> ()
  | _ -> Alcotest.fail "an agent that does not trust the CA fails");
  let status, _ = connect_status ~creds:(creds token_a) w ~host:"not_a_host" in
  Alcotest.(check int) "the CONNECT itself is answered" 200 status;
  let f = Model_proxy.unregister w.ctx.proxy ~token:token_a in
  Alcotest.(check bool) "the agent's alert is noted" true (noted f "TLS");
  Alcotest.(check bool)
    "the bad name is noted" true
    (noted f "is not a host name");
  Alcotest.(check int)
    "nothing reached the server" 0
    (List.length (F.requests w.up))

let () =
  Alcotest.run "egress requests"
    [
      ( "targets",
        [
          Alcotest.test_case "request target forms" `Quick test_target;
          Alcotest.test_case "requests stay on the tunnel's host" `Quick
            test_targets_stay_on_the_tunnel;
        ] );
      ( "framing",
        [ Alcotest.test_case "HEAD and 204 have no body" `Quick test_bodiless ]
      );
      ( "failures",
        [
          Alcotest.test_case "413 and 501" `Quick test_refusals;
          Alcotest.test_case "a dropped and an unreachable host" `Quick
            test_upstream_failures;
          Alcotest.test_case "TLS that never starts" `Quick test_tls_refused;
        ] );
      ( "clock",
        [ Alcotest.test_case "a SIM_NOW a month back" `Quick test_past_sim_now ]
      );
      ( "redaction",
        [ Alcotest.test_case "a secret in the path" `Quick test_path_redacted ]
      );
    ]
