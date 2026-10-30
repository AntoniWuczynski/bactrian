(* The TLS connector against a local fake HTTPS server with a CA minted for the
   test: trust, host names, cut-off bodies and streams split across records. *)

open Bactrian_runtime
module H = Fake_https
module F = Fake_upstream

let connector = H.connector

let json_reply ?(ending = F.Complete) chunks : F.reply =
  {
    status = 200;
    headers = [ ("content-type", "application/json") ];
    chunks;
    ending;
  }

type outcome = {
  result : (unit, Upstream.error) result;
  status : int;
  body : string;
  reads : int;
}

let call env connector url =
  let status = ref 0 and body = Buffer.create 64 and reads = ref 0 in
  let result =
    Eio.Switch.run @@ fun sw ->
    Upstream.call ~sw ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
      ~timeout:10. connector
      { meth = "POST"; url = Uri.of_string url; headers = []; body = "{}" }
      ~on_head:(fun h -> status := h.status)
      ~on_chunk:(fun ~dt_ms:_ s ->
        incr reads;
        Buffer.add_string body s)
  in
  { result; status = !status; body = Buffer.contents body; reads = !reads }

(* A fake upstream behind a TLS front, for the length of [f]. *)
let with_server ?record_size ?ending ?relay
    ?(names = [ H.Dns "localhost"; H.Ip "127.0.0.1" ]) ?ca replies f =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  Eio.Switch.run @@ fun sw ->
  let ca = match ca with Some ca -> ca | None -> H.ca () in
  let up =
    match F.start ~sw ~net:(Eio.Stdenv.net env) (F.Script replies) with
    | Ok up -> up
    | Error e -> Alcotest.failf "fake upstream: %a" F.pp_error e
  in
  let front =
    H.start ~sw ~net:(Eio.Stdenv.net env) ?record_size ?ending ?relay
      ~chain:(H.leaf ca names) ~backend:(F.port up) ()
  in
  let r = f env ca (H.port front) in
  Eio.Switch.fail sw Exit;
  r

let run_server ?record_size ?ending ?relay ?names ?ca replies f () =
  match with_server ?record_size ?ending ?relay ?names ?ca replies f with
  | () -> ()
  | exception Exit -> ()

let ok o =
  match o.result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "call failed: %s" (Upstream.describe_error e)

let connect_failed what o =
  match o.result with
  | Error (Connect_failed reason) ->
      if not (String.length reason > 0) then Alcotest.failf "%s: no reason" what;
      reason
  | Ok () -> Alcotest.failf "%s: the call succeeded" what
  | Error ((Failed_after_send _ | Timed_out | Truncated _) as e) ->
      Alcotest.failf "%s: %s" what (Upstream.describe_error e)

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let url port host = Printf.sprintf "https://%s:%d/v1/chat/completions" host port

let trusted =
  run_server [ json_reply [ {|{"ok":|}; {|true}|} ]; json_reply [ "{}" ] ]
  @@ fun env ca port ->
  let o = call env (connector ca) (url port "localhost") in
  ok o;
  Alcotest.(check int) "status" 200 o.status;
  Alcotest.(check string) "body" {|{"ok":true}|} o.body;
  let o = call env (connector ca) (url port "127.0.0.1") in
  ok o;
  Alcotest.(check string) "an address host" "{}" o.body

let unknown_ca =
  run_server [ json_reply [ "{}" ] ] @@ fun env _ port ->
  let reason =
    connect_failed "another CA"
      (call env (connector (H.ca ())) (url port "localhost"))
  in
  Alcotest.(check bool) ("names TLS: " ^ reason) true (contains reason "TLS")

let wrong_host =
  run_server ~names:[ H.Dns "other.test" ] [ json_reply [ "{}" ] ]
  @@ fun env ca port ->
  ignore
    (connect_failed "a certificate for another name"
       (call env (connector ca) (url port "localhost")));
  ignore
    (connect_failed "an address not in the certificate"
       (call env (connector ca) (url port "127.0.0.1")))

let no_tls_without_connector =
  run_server [ json_reply [ "{}" ] ] @@ fun env _ port ->
  ignore
    (connect_failed "plain" (call env Upstream.plain (url port "localhost")));
  Alcotest.(check bool)
    "plain is not ready" true
    (Result.is_error (Upstream.tls_ready Upstream.plain))

let cut_off ending =
  run_server ~ending [ json_reply ~ending:F.Drop [ {|{"a":|}; {|1|} ] ]
  @@ fun env ca port ->
  match (call env (connector ca) (url port "localhost")).result with
  | Error (Truncated { bytes; _ }) ->
      Alcotest.(check int) "bytes passed on" 6 bytes
  | Ok () -> Alcotest.fail "a cut-off chunked body was taken for a whole one"
  | Error ((Connect_failed _ | Failed_after_send _ | Timed_out) as e) ->
      Alcotest.failf "expected Truncated, got %s" (Upstream.describe_error e)

(* A record that does not decrypt, inside the body or before the head, ends
   the call with the TLS failure as its reason, not an exception. *)
let tls_reason what reason =
  Alcotest.(check bool)
    (what ^ " names the TLS failure: " ^ reason)
    true
    (contains reason "TLS failure")

let bad_record_in_body =
  run_server ~ending:H.Bad_record
    [ json_reply ~ending:F.Drop [ {|{"a":|}; {|1|} ] ]
  @@ fun env ca port ->
  match (call env (connector ca) (url port "localhost")).result with
  | Error (Truncated { bytes; reason }) ->
      Alcotest.(check int) "bytes passed on" 6 bytes;
      tls_reason "Truncated" reason
  | Ok () -> Alcotest.fail "a body ended by a bad record was taken as whole"
  | Error ((Connect_failed _ | Failed_after_send _ | Timed_out) as e) ->
      Alcotest.failf "expected Truncated, got %s" (Upstream.describe_error e)

let bad_record_before_head =
  run_server ~ending:H.Bad_record ~relay:false [ json_reply [ "{}" ] ]
  @@ fun env ca port ->
  match (call env (connector ca) (url port "localhost")).result with
  | Error (Failed_after_send reason) -> tls_reason "Failed_after_send" reason
  | Ok () -> Alcotest.fail "a call with no response succeeded"
  | Error ((Connect_failed _ | Truncated _ | Timed_out) as e) ->
      Alcotest.failf "expected Failed_after_send, got %s"
        (Upstream.describe_error e)

let split_records =
  let events = List.init 4 (fun i -> Printf.sprintf "data: {\"n\":%d}\n\n" i) in
  run_server ~record_size:5
    [
      {
        status = 200;
        headers = [ ("content-type", "text/event-stream") ];
        chunks = events;
        ending = F.Complete;
      };
    ]
  @@ fun env ca port ->
  let o = call env (connector ca) (url port "localhost") in
  ok o;
  Alcotest.(check string) "every event, whole" (String.concat "" events) o.body;
  Alcotest.(check bool)
    "read record by record" true
    (o.reads > List.length events)

(* An http URL is not wrapped in TLS, even with a connector. *)
let http_stays_plain () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  Eio.Switch.run @@ fun sw ->
  let up =
    match
      F.start ~sw ~net:(Eio.Stdenv.net env) (F.Script [ json_reply [ "{}" ] ])
    with
    | Ok up -> up
    | Error e -> Alcotest.failf "fake upstream: %a" F.pp_error e
  in
  let o = call env (connector (H.ca ())) (F.base_url up ^ "/x") in
  ok o;
  Alcotest.(check string) "body" "{}" o.body;
  Eio.Switch.fail sw Exit

let http_stays_plain () = try http_stays_plain () with Exit -> ()

(* The system connector: roots from this host plus BACTRIAN_EXTRA_CA_CERTS. *)
let with_extra value f =
  let before =
    Option.value (Sys.getenv_opt Bactrian_tls.Trust.extra_variable) ~default:""
  in
  Unix.putenv Bactrian_tls.Trust.extra_variable value;
  Fun.protect
    ~finally:(fun () -> Unix.putenv Bactrian_tls.Trust.extra_variable before)
    f

let system_roots () =
  Eio_main.run @@ fun env ->
  with_extra "" @@ fun () ->
  match Bactrian_tls.Trust.load ~process_mgr:(Eio.Stdenv.process_mgr env) with
  | Ok { system; extra } ->
      Alcotest.(check bool)
        "this host has root certificates" true
        (List.length system > 10);
      Alcotest.(check int) "no extra certificates" 0 (List.length extra)
  | Error e -> Alcotest.fail e

let bad_extra () =
  Eio_main.run @@ fun env ->
  with_extra "/nonexistent/ca.pem" @@ fun () ->
  let c =
    Bactrian_tls.Tls_connector.system ~process_mgr:(Eio.Stdenv.process_mgr env)
  in
  match Upstream.tls_ready c with
  | Ok (_ : Upstream.trust) -> Alcotest.fail "a missing extra file was accepted"
  | Error e ->
      Alcotest.(check bool)
        ("names the variable: " ^ e)
        true
        (contains e Bactrian_tls.Trust.extra_variable)

let system_with_extra () =
  let ca =
    Mirage_crypto_rng_unix.use_default ();
    H.ca ()
  in
  let file = Filename.temp_file "bactrian-test-ca" ".pem" in
  Out_channel.with_open_bin file (fun oc -> output_string oc (H.ca_pem ca));
  Fun.protect ~finally:(fun () -> Sys.remove file) @@ fun () ->
  with_extra file @@ fun () ->
  run_server ~ca
    [ json_reply [ "{}" ] ]
    (fun env _ port ->
      let c =
        Bactrian_tls.Tls_connector.system
          ~process_mgr:(Eio.Stdenv.process_mgr env)
      in
      (match Upstream.tls_ready c with
      | Ok { extra = Some { variable; count }; roots } ->
          Alcotest.(check string)
            "the variable" Bactrian_tls.Trust.extra_variable variable;
          Alcotest.(check int) "one extra certificate" 1 count;
          Alcotest.(check bool) "the system's too" true (roots > count)
      | Ok { extra = None; _ } -> Alcotest.fail "the extra CA is not counted"
      | Error e -> Alcotest.fail e);
      ok (call env c (url port "localhost")))
    ()

(* The variables other tools read for their trust bundle do not reach
   bactrian's: a CA named only there is not trusted. *)
let with_variables vars f =
  let before = List.map (fun (v, _) -> (v, Sys.getenv_opt v)) vars in
  List.iter (fun (v, value) -> Unix.putenv v value) vars;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (v, value) -> Unix.putenv v (Option.value value ~default:""))
        before)
    f

let foreign_variables_ignored () =
  let ca =
    Mirage_crypto_rng_unix.use_default ();
    H.ca ()
  in
  let file = Filename.temp_file "bactrian-test-ca" ".pem" in
  Out_channel.with_open_bin file (fun oc -> output_string oc (H.ca_pem ca));
  Fun.protect ~finally:(fun () -> Sys.remove file) @@ fun () ->
  with_variables
    [
      ("SSL_CERT_FILE", file);
      ("NIX_SSL_CERT_FILE", file);
      ("OCAML_EXTRA_CA_CERTS", file);
      (Bactrian_tls.Trust.extra_variable, "");
    ]
  @@ fun () ->
  run_server ~ca
    [ json_reply [ "{}" ] ]
    (fun env _ port ->
      let c =
        Bactrian_tls.Tls_connector.system
          ~process_mgr:(Eio.Stdenv.process_mgr env)
      in
      (match Upstream.tls_ready c with
      | Ok { extra = None; _ } -> ()
      | Ok { extra = Some _; _ } -> Alcotest.fail "extra certificates counted"
      | Error e -> Alcotest.fail e);
      let reason =
        connect_failed "a CA named only by SSL_CERT_FILE and the others"
          (call env c (url port "localhost"))
      in
      Alcotest.(check bool)
        ("names TLS: " ^ reason) true (contains reason "TLS"))
    ()

(* A stream through the model proxy to an https upstream reaches the agent
   event by event, with events split across TLS records on the way. *)
let proxy_stream =
  let open Proxy_support in
  with_ctx "tls-stream" @@ fun ctx ->
  Mirage_crypto_rng_unix.use_default ();
  let f = fixture "openai_chat" "luna_stream" in
  let events =
    List.init 5 (fun i -> Printf.sprintf "data: event-%d\n\n" (i + 1))
  in
  let base = slow_upstream ctx ~chunks:events ~gap:0.1 in
  let backend = Option.get (Uri.port (Uri.of_string base)) in
  let ca = H.ca () in
  let front =
    H.start ~sw:ctx.sw ~net:(Eio.Stdenv.net ctx.env) ~record_size:7
      ~chain:(H.leaf ca [ H.Dns "localhost" ])
      ~backend ()
  in
  let run =
    make_run ~connector:(connector ca) ctx ~token:token_a
      ~upstream:(Printf.sprintf "https://localhost:%d" (H.port front))
  in
  P.register ctx.proxy run;
  let r = send_fixture ctx ~token:token_a f in
  check_ok r;
  Alcotest.(check string) "bytes" (String.concat "" events) r.body;
  List.iter
    (fun g ->
      if g < 0.06 then Alcotest.failf "events arrived %.0f ms apart" (g *. 1000.))
    (event_gaps r events);
  ignore (P.unregister ctx.proxy ~token:token_a);
  match (response_of (only (recording ctx run).exchanges)).body with
  | Stream { chunks; complete } ->
      Alcotest.(check bool) "complete" true complete;
      Alcotest.(check string)
        "recorded bytes" (String.concat "" events)
        (String.concat "" (List.map snd chunks))
  | Whole _ -> Alcotest.fail "expected a recorded stream"

let () =
  Alcotest.run "tls"
    [
      ( "connector",
        [
          Alcotest.test_case "a trusted server, by name and by address" `Quick
            trusted;
          Alcotest.test_case "a certificate from another CA" `Quick unknown_ca;
          Alcotest.test_case "a certificate for another host" `Quick wrong_host;
          Alcotest.test_case "no connector refuses https" `Quick
            no_tls_without_connector;
          Alcotest.test_case "a cut-off chunked body, close_notify" `Quick
            (cut_off H.Close_notify);
          Alcotest.test_case "a cut-off chunked body, TCP close" `Quick
            (cut_off H.Tcp_close);
          Alcotest.test_case "a bad record inside the body" `Quick
            bad_record_in_body;
          Alcotest.test_case "a bad record before the head" `Quick
            bad_record_before_head;
          Alcotest.test_case "a stream split across records" `Quick
            split_records;
          Alcotest.test_case "an http URL stays plain" `Quick http_stays_plain;
          Alcotest.test_case "SSE through the model proxy" `Quick proxy_stream;
        ] );
      ( "trust",
        [
          Alcotest.test_case "the system roots load" `Quick system_roots;
          Alcotest.test_case "a missing extra file" `Quick bad_extra;
          Alcotest.test_case "the system connector with an extra CA" `Quick
            system_with_extra;
          Alcotest.test_case "SSL_CERT_FILE and the others are ignored" `Quick
            foreign_variables_ignored;
        ] );
    ]
