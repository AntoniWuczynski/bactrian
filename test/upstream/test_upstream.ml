open Bactrian_runtime

let listen ~sw net =
  let sock =
    Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  match Eio.Net.listening_addr sock with
  | `Tcp (_, p) -> (sock, p)
  | `Unix _ -> Alcotest.fail "unexpected unix address"

(* cohttp-eio server answering every request through [callback]. *)
let serve ~sw net ~callback =
  let sock, port = listen ~sw net in
  let server = Cohttp_eio.Server.make ~callback () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Cohttp_eio.Server.run sock server ~on_error:(fun _ -> ()));
  port

(* Raw TCP server: reads the request head, writes [reply], then closes. *)
let serve_raw ~sw net ~reply =
  let sock, port = listen ~sw net in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server sock
        ~on_error:(fun _ -> ())
        (fun flow _ ->
          let r = Eio.Buf_read.of_flow ~max_size:65536 flow in
          let rec skip () =
            if not (String.equal (Eio.Buf_read.line r) "") then skip ()
          in
          skip ();
          Eio.Flow.copy_string reply flow));
  port

let url port path =
  Uri.of_string (Printf.sprintf "http://127.0.0.1:%d%s" port path)

let get u : Upstream.request =
  { meth = "GET"; url = u; headers = []; body = "" }

type outcome = {
  result : (unit, Upstream.error) result;
  head : Upstream.head option;
  chunks : (int * string) list; (* dt_ms, data *)
  arrivals : float list;
}

let run_call ?(timeout = 5.0) ~env ~sw request =
  let clock = Eio.Stdenv.clock env in
  let head = ref None and chunks = ref [] and arrivals = ref [] in
  let result =
    Upstream.call ~sw ~net:(Eio.Stdenv.net env) ~clock ~timeout Upstream.plain
      request
      ~on_head:(fun h -> head := Some h)
      ~on_chunk:(fun ~dt_ms s ->
        arrivals := Eio.Time.now clock :: !arrivals;
        chunks := (dt_ms, s) :: !chunks)
  in
  {
    result;
    head = !head;
    chunks = List.rev !chunks;
    arrivals = List.rev !arrivals;
  }

let body o = String.concat "" (List.map snd o.chunks)

let with_env f () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> f env sw

let error_testable =
  let pp ppf = function
    | Upstream.Connect_failed s -> Format.fprintf ppf "Connect_failed %S" s
    | Timed_out -> Format.pp_print_string ppf "Timed_out"
    | Truncated { bytes; reason } ->
        Format.fprintf ppf "Truncated{%d,%S}" bytes reason
  in
  Alcotest.testable pp ( = )

let some_head o =
  match o.head with Some h -> h | None -> Alcotest.fail "no head delivered"

let expect_connect_failed o =
  match o.result with
  | Error (Connect_failed _) -> ()
  | Ok () | Error Timed_out | Error (Truncated _) ->
      Alcotest.fail "expected Connect_failed"

let truncated_bytes o =
  match o.result with
  | Error (Truncated { bytes; _ }) -> bytes
  | Ok () -> Alcotest.fail "clean end after a dropped body"
  | Error (Connect_failed _ | Timed_out) -> Alcotest.fail "expected Truncated"

(* --- tests --- *)

let test_whole_body env sw =
  let seen = ref [] and seen_body = ref "" and seen_meth = ref "" in
  let port =
    serve ~sw (Eio.Stdenv.net env) ~callback:(fun _ req body ->
        seen := Http.Header.to_list (Cohttp.Request.headers req);
        seen_meth := Http.Method.to_string (Cohttp.Request.meth req);
        (seen_body := Eio.Buf_read.(take_all (of_flow ~max_size:max_int body)));
        Cohttp_eio.Server.respond_string ~status:`Created
          ~headers:
            (Http.Header.of_list [ ("content-type", "application/json") ])
          ~body:"{\"ok\":true}" ())
  in
  let req : Upstream.request =
    {
      meth = "POST";
      url = url port "/v1/x";
      body = "{\"a\":1}";
      headers = [ ("x-api-key", "k"); ("content-type", "application/json") ];
    }
  in
  let o = run_call ~env ~sw req in
  Alcotest.(check (result unit error_testable)) "ok" (Ok ()) o.result;
  Alcotest.(check int) "status" 201 (some_head o).status;
  Alcotest.(check string) "body" "{\"ok\":true}" (body o);
  Alcotest.(check string) "method" "POST" !seen_meth;
  Alcotest.(check string) "request body" "{\"a\":1}" !seen_body;
  Alcotest.(check (option string))
    "x-api-key forwarded" (Some "k")
    (List.assoc_opt "x-api-key" !seen)

let test_accept_encoding env sw =
  let seen = ref [] in
  let port =
    serve ~sw (Eio.Stdenv.net env) ~callback:(fun _ req _ ->
        seen := Http.Header.to_list (Cohttp.Request.headers req);
        Cohttp_eio.Server.respond_string ~status:`OK ~body:"x" ())
  in
  let o =
    run_call ~env ~sw
      {
        (get (url port "/")) with
        headers = [ ("Accept-Encoding", "gzip, br") ];
      }
  in
  Alcotest.(check (result unit error_testable)) "ok" (Ok ()) o.result;
  let ae =
    List.filter_map
      (fun (k, v) ->
        if String.equal (String.lowercase_ascii k) "accept-encoding" then Some v
        else None)
      !seen
  in
  Alcotest.(check (list string)) "only identity" [ "identity" ] ae

let test_header_allowlist env sw =
  let port =
    serve ~sw (Eio.Stdenv.net env) ~callback:(fun _ _ _ ->
        let headers =
          Http.Header.of_list
            [
              ("Content-Type", "text/event-stream");
              ("Retry-After", "7");
              ("X-RateLimit-Remaining-Requests", "99");
              ("anthropic-ratelimit-tokens-limit", "1000");
              ("X-Request-Id", "req_1");
              ("Set-Cookie", "session=secret");
              ("Server", "evil");
              ("X-Custom", "nope");
              ("x-ratelimitless", "nope");
              ("openai-organization", "org");
            ]
        in
        Cohttp_eio.Server.respond_string ~status:`OK ~headers ~body:"hello" ())
  in
  let o = run_call ~env ~sw (get (url port "/")) in
  let got = List.sort compare (some_head o).headers in
  Alcotest.(check (list (pair string string)))
    "allowlisted only"
    [
      ("anthropic-ratelimit-tokens-limit", "1000");
      ("content-type", "text/event-stream");
      ("retry-after", "7");
      ("x-ratelimit-remaining-requests", "99");
      ("x-request-id", "req_1");
    ]
    got

let test_error_status_is_ok env sw =
  let port =
    serve ~sw (Eio.Stdenv.net env) ~callback:(fun _ _ _ ->
        Cohttp_eio.Server.respond_string ~status:`Too_many_requests
          ~headers:(Http.Header.of_list [ ("retry-after", "3") ])
          ~body:"slow down" ())
  in
  let o = run_call ~env ~sw (get (url port "/")) in
  Alcotest.(check (result unit error_testable)) "ok" (Ok ()) o.result;
  Alcotest.(check int) "429" 429 (some_head o).status;
  Alcotest.(check string) "body" "slow down" (body o)

type item = Data of string | End

module Stream_source = struct
  type t = {
    q : item Eio.Stream.t;
    mutable pending : string;
    mutable pos : int;
  }

  let rec single_read t dst =
    if t.pos < String.length t.pending then begin
      let n = min (Cstruct.length dst) (String.length t.pending - t.pos) in
      Cstruct.blit_from_string t.pending t.pos dst 0 n;
      t.pos <- t.pos + n;
      n
    end
    else
      match Eio.Stream.take t.q with
      | Data s ->
          t.pending <- s;
          t.pos <- 0;
          single_read t dst
      | End -> raise End_of_file

  let read_methods = []
end

let source_of_stream q : Eio.Flow.source_ty Eio.Resource.t =
  Eio.Resource.T
    ( { Stream_source.q; pending = ""; pos = 0 },
      Eio.Flow.Pi.source (module Stream_source) )

(* Upstream writing [events] [gap] seconds apart, then ending (or never, when
   [finish] is false). *)
let sse_upstream ~env ~sw ~events ~gap ~finish =
  let clock = Eio.Stdenv.clock env in
  serve ~sw (Eio.Stdenv.net env) ~callback:(fun _ _ _ ->
      let q = Eio.Stream.create max_int in
      Eio.Fiber.fork ~sw (fun () ->
          List.iter
            (fun e ->
              Eio.Stream.add q (Data e);
              Eio.Time.sleep clock gap)
            events;
          if finish then Eio.Stream.add q End);
      Cohttp_eio.Server.respond ~status:`OK
        ~headers:(Http.Header.of_list [ ("content-type", "text/event-stream") ])
        ~body:(source_of_stream q) ())

let gaps times =
  let rec go = function a :: (b :: _ as r) -> (b -. a) :: go r | _ -> [] in
  go times

let test_streamed env sw =
  let events =
    List.init 5 (fun i -> Printf.sprintf "data: event-%d\n\n" (i + 1))
  in
  let port = sse_upstream ~env ~sw ~events ~gap:0.1 ~finish:true in
  let o = run_call ~env ~sw (get (url port "/")) in
  Alcotest.(check (result unit error_testable)) "ok" (Ok ()) o.result;
  Alcotest.(check string) "bytes" (String.concat "" events) (body o);
  Alcotest.(check int) "five chunks" 5 (List.length o.chunks);
  Alcotest.(check bool)
    "arrivals >= 60 ms apart (not buffered)" true
    (List.for_all (fun g -> g >= 0.06) (gaps o.arrivals));
  Alcotest.(check bool)
    "dt_ms tracks the gap" true
    (List.for_all (fun (dt, _) -> dt >= 60 && dt < 1000) (List.tl o.chunks))

let test_large_event env sw =
  let ev = "data: " ^ String.make (200 * 1024) 'x' ^ "\n\n" in
  let port = sse_upstream ~env ~sw ~events:[ ev ] ~gap:0.0 ~finish:true in
  let o = run_call ~env ~sw (get (url port "/")) in
  Alcotest.(check (result unit error_testable)) "ok" (Ok ()) o.result;
  Alcotest.(check bool) "byte-equal" true (String.equal ev (body o))

let test_drop_chunked env sw =
  let reply =
    "HTTP/1.1 200 OK\r\n\
     content-type: text/event-stream\r\n\
     transfer-encoding: chunked\r\n\
     \r\n\
     f\r\n\
     data: event-1\n\n\
     \r\n\
     f\r\n\
     data: event-2\n\n\
     \r\n\
     5\r\n\
     data:"
  in
  let port = serve_raw ~sw (Eio.Stdenv.net env) ~reply in
  let o = run_call ~env ~sw (get (url port "/")) in
  let delivered = body o in
  Alcotest.(check bool)
    "complete chunks delivered" true
    (String.length delivered >= 30
    && String.equal
         (String.sub delivered 0 30)
         "data: event-1\n\ndata: event-2\n\n");
  Alcotest.(check bool) "head delivered" true (o.head <> None);
  Alcotest.(check int)
    "byte count" (String.length delivered) (truncated_bytes o)

let test_drop_content_length env sw =
  let reply =
    "HTTP/1.1 200 OK\r\n\
     content-type: application/json\r\n\
     content-length: 100\r\n\
     \r\n\
     01234567890123456789"
  in
  let port = serve_raw ~sw (Eio.Stdenv.net env) ~reply in
  let o = run_call ~env ~sw (get (url port "/")) in
  Alcotest.(check string)
    "received bytes delivered" "01234567890123456789" (body o);
  Alcotest.(check int) "byte count" 20 (truncated_bytes o)

let test_drop_before_head env sw =
  let port = serve_raw ~sw (Eio.Stdenv.net env) ~reply:"" in
  let o = run_call ~env ~sw (get (url port "/")) in
  Alcotest.(check bool) "no head" true (o.head = None);
  expect_connect_failed o

let test_timeout_no_response env sw =
  let clock = Eio.Stdenv.clock env in
  let port =
    serve ~sw (Eio.Stdenv.net env) ~callback:(fun _ _ _ ->
        Eio.Time.sleep clock 10.0;
        Cohttp_eio.Server.respond_string ~status:`OK ~body:"late" ())
  in
  let t0 = Eio.Time.now clock in
  let o = run_call ~timeout:0.3 ~env ~sw (get (url port "/")) in
  let dt = Eio.Time.now clock -. t0 in
  Alcotest.(check (result unit error_testable))
    "timed out" (Error Timed_out) o.result;
  Alcotest.(check bool) "returned promptly" true (dt < 2.0)

let test_timeout_mid_stream env sw =
  let port =
    sse_upstream ~env ~sw ~events:[ "data: a\n\n" ] ~gap:0.0 ~finish:false
  in
  let o = run_call ~timeout:0.4 ~env ~sw (get (url port "/")) in
  Alcotest.(check string) "first event delivered" "data: a\n\n" (body o);
  Alcotest.(check (result unit error_testable))
    "timed out" (Error Timed_out) o.result

let refused_port env =
  match
    Eio.Switch.run (fun inner ->
        let _, port = listen ~sw:inner (Eio.Stdenv.net env) in
        Eio.Switch.fail inner (Failure (string_of_int port)))
  with
  | () -> Alcotest.fail "unreachable"
  | exception Failure p -> int_of_string p

let test_refused env sw =
  let port = refused_port env in
  let o = run_call ~env ~sw (get (url port "/")) in
  Alcotest.(check bool) "no head" true (o.head = None);
  expect_connect_failed o

let test_https_without_tls env sw =
  let o = run_call ~env ~sw (get (Uri.of_string "https://127.0.0.1:1/")) in
  expect_connect_failed o

let () =
  let t name f = Alcotest.test_case name `Quick (with_env f) in
  Alcotest.run "upstream"
    [
      ( "upstream",
        [
          t "whole body" test_whole_body;
          t "error status is a normal response" test_error_status_is_ok;
          t "accept-encoding identity" test_accept_encoding;
          t "header allowlist" test_header_allowlist;
          t "streamed body arrives as it is sent" test_streamed;
          t "200 KiB event" test_large_event;
          t "drop mid chunked stream" test_drop_chunked;
          t "drop mid content-length body" test_drop_content_length;
          t "drop before head" test_drop_before_head;
          t "timeout with no response" test_timeout_no_response;
          t "timeout mid stream" test_timeout_mid_stream;
          t "connection refused" test_refused;
          t "https without TLS connector" test_https_without_tls;
        ] );
    ]
