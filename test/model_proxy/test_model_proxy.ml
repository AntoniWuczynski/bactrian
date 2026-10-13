open Bactrian_core
open Bactrian_runtime
open Proxy_support

(* Record mode, one fixture per protocol *)

let record_case (protocol, case) =
  let name = protocol ^ "/" ^ case in
  Alcotest.test_case name `Quick
  @@ with_ctx (protocol ^ "-" ^ case)
  @@ fun ctx ->
  let f = fixture protocol case in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let r = send_fixture ctx ~token:token_a f in
  check_ok r;
  Alcotest.(check int) "status" f.status r.status;
  Alcotest.(check string) "bytes to the agent" f.raw r.body;
  let provider = provider_of f in
  let sent = only (F.requests up) in
  let name, value = key_header provider (List.assoc provider real_keys) in
  Alcotest.(check (option string))
    "real key upstream" (Some value)
    (List.assoc_opt name sent.headers);
  Alcotest.(check bool)
    "no placeholder upstream" false
    (List.exists
       (fun (_, v) ->
         String.starts_with ~prefix:"bactrian-" v
         || String.ends_with ~suffix:token_a v)
       sent.headers);
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "exchanges" 1 findings.exchanges;
  Alcotest.(check int) "usage gaps" 0 findings.usage_gaps;
  let e = only (recording ctx run).exchanges in
  Alcotest.(check string) "key" (expected_key f) e.key;
  Alcotest.(check string) "protocol" protocol e.protocol;
  Alcotest.(check string) "recorded bytes" f.raw (stream_bytes (response_of e));
  let spent = Ledger.spent run.ledger ~provider:(segment provider) in
  let cost = expected_cost f in
  if not (close_to spent cost) then
    Alcotest.failf "ledger spent %.12f, expected %.12f" spent cost

let record_cases =
  List.map record_case
    [
      ("openai_chat", "luna_nonstream");
      ("openai_responses", "luna_stream");
      ("anthropic_messages", "haiku_stream");
      ("gemini_generate", "flash_stream_json");
      ("gemini_interactions", "flash_stream");
    ]

(* Streaming arrives event by event *)

let slow_upstream ctx ~chunks ~gap =
  let sock =
    Eio.Net.listen ~sw:ctx.sw ~backlog:16 ~reuse_addr:true
      (Eio.Stdenv.net ctx.env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr sock with
    | `Tcp (_, p) -> p
    | `Unix _ -> Alcotest.fail "unix listener"
  in
  Eio.Fiber.fork_daemon ~sw:ctx.sw (fun () ->
      Eio.Net.run_server sock
        ~on_error:(fun _ -> ())
        (fun flow _ ->
          let r = Eio.Buf_read.of_flow ~max_size:1_000_000 flow in
          let rec head len =
            match Eio.Buf_read.line r with
            | "" -> len
            | line -> (
                match String.split_on_char ':' line with
                | k :: v :: _
                  when String.equal (String.lowercase_ascii k) "content-length"
                  ->
                    head (int_of_string (String.trim v))
                | _ -> head len)
          in
          ignore (Eio.Buf_read.take (head 0) r);
          Eio.Flow.copy_string
            "HTTP/1.1 200 OK\r\n\
             content-type: text/event-stream\r\n\
             transfer-encoding: chunked\r\n\
             \r\n"
            flow;
          List.iter
            (fun c ->
              Eio.Flow.copy_string
                (Printf.sprintf "%x\r\n%s\r\n" (String.length c) c)
                flow;
              Eio.Time.sleep (Eio.Stdenv.clock ctx.env) gap)
            chunks;
          Eio.Flow.copy_string "0\r\n\r\n" flow));
  Printf.sprintf "http://127.0.0.1:%d" port

(* Seconds between the moments each event was complete at the agent. *)
let event_gaps r events =
  let ends =
    List.fold_left_map
      (fun acc e ->
        let n = acc + String.length e in
        (n, n))
      0 events
    |> snd
  in
  let times =
    List.map
      (fun n -> fst (List.find (fun (_, total) -> total >= n) r.arrivals))
      ends
  in
  let rec gaps = function
    | a :: (b :: _ as rest) -> (b -. a) :: gaps rest
    | [ _ ] | [] -> []
  in
  gaps times

let streaming =
  Alcotest.test_case "stream reaches the agent event by event" `Quick
  @@ with_ctx "timing"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_stream" in
  let events =
    List.init 5 (fun i -> Printf.sprintf "data: event-%d\n\n" (i + 1))
  in
  let base = slow_upstream ctx ~chunks:events ~gap:0.1 in
  let run = make_run ctx ~token:token_a ~upstream:base in
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
      Alcotest.(check int) "recorded chunks" 5 (List.length chunks);
      List.iteri
        (fun i (dt, _) ->
          if i > 0 && dt < 60 then Alcotest.failf "chunk %d dt_ms %d" i dt)
        chunks
  | Whole _ -> Alcotest.fail "expected a recorded stream"

(* Placeholder keys *)

let placeholder_keys =
  Alcotest.test_case "wrong, missing and ended keys get 403" `Quick
  @@ with_ctx "keys"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let wrong = send_fixture ctx ~token:token_a ~key:("bactrian-" ^ token_b) f in
  Alcotest.(check int) "wrong key" 403 wrong.status;
  let missing =
    send ctx ~token:token_a ~provider:P.Openai ~meth:f.meth f.path
      ~headers:f.request_headers f.request
  in
  Alcotest.(check int) "missing key" 403 missing.status;
  let other_header =
    send ctx ~token:token_a ~provider:P.Openai ~meth:f.meth f.path
      ~headers:(("x-api-key", "bactrian-" ^ token_a) :: f.request_headers)
      f.request
  in
  Alcotest.(check int)
    "key in another provider's header" 403 other_header.status;
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "refused keys" 3 findings.refused_keys;
  let ended = send_fixture ctx ~token:token_a f in
  Alcotest.(check int) "ended run" 403 ended.status;
  let unknown = send_fixture ctx ~token:token_b f in
  Alcotest.(check int) "unknown token" 403 unknown.status;
  Alcotest.(check int) "nothing forwarded" 0 (List.length (F.requests up))

(* Replay *)

let record_all ctx ~token fixtures =
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run = make_run ctx ~token ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  List.iter (fun f -> check_ok (send_fixture ctx ~token f)) fixtures;
  ignore (P.unregister ctx.proxy ~token);
  recording ctx run

let replay_run ?(record = true) ?(matcher = Matcher.Strict) ?(timing = P.Fast)
    ctx ~token ~upstream recording =
  make_run ctx ~token ~upstream ~record
    ~mode:(P.Replay { recording; matcher; allow_reorder = false; timing })

let replay_strict =
  Alcotest.test_case "strict replay serves the recording" `Quick
  @@ with_ctx "replay"
  @@ fun ctx ->
  let fs =
    [
      fixture "openai_chat" "luna_nonstream";
      fixture "anthropic_messages" "haiku_stream";
    ]
  in
  let recording = record_all ctx ~token:token_a fs in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run = replay_run ctx ~token:token_b ~upstream:(F.base_url up) recording in
  P.register ctx.proxy run;
  List.iter
    (fun f ->
      let r = send_fixture ctx ~token:token_b f in
      check_ok r;
      Alcotest.(check int) "status" f.status r.status;
      Alcotest.(check string) "identical bytes" f.raw r.body)
    fs;
  let findings = P.unregister ctx.proxy ~token:token_b in
  Alcotest.(check int) "divergences" 0 (List.length findings.divergences);
  Alcotest.(check int) "exchanges" 2 findings.exchanges;
  let copies = (run_cassette ctx run).exchanges in
  Alcotest.(check int) "served exchanges copied" 2 (List.length copies);
  List.iteri
    (fun i ((c : Cassette.exchange), (o : Cassette.exchange)) ->
      Alcotest.(check int) "fresh seq" i c.seq;
      Alcotest.(check string) "key" o.key c.key;
      Alcotest.(check int) "occurrence" o.occurrence c.occurrence;
      Alcotest.(check bool)
        "canonical" true
        (Canon.equal o.canonical c.canonical);
      Alcotest.(check bool) "request" true (o.request = c.request);
      Alcotest.(check bool) "outcome" true (o.outcome = c.outcome))
    (List.combine copies recording.exchanges);
  Alcotest.(check int) "nothing upstream" 0 (List.length (F.requests up))

let replay_miss =
  Alcotest.test_case "a changed request gets 409 replay miss" `Quick
  @@ with_ctx "miss"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let recording = record_all ctx ~token:token_a [ f ] in
  let recorded_seq = (only recording.exchanges).seq in
  let up = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy
    (replay_run ctx ~token:token_b ~upstream:(F.base_url up) recording);
  let changed body =
    let v = canon body in
    Canon.to_string
      (Canon.set_member "temperature" (Canon.float 0.5 |> Result.get_ok) v)
  in
  let r = send_fixture ctx ~token:token_b ~body_of:changed f in
  Alcotest.(check int) "status" 409 r.status;
  Alcotest.(check string) "type" "bactrian_replay_miss" (error_type r);
  let divergence =
    Option.get
      (Option.bind
         (Canon.member "error" (canon r.body))
         (Canon.member "divergence"))
  in
  Alcotest.(check (option string))
    "kind" (Some "missing_request")
    (Canon.str_member "kind" divergence);
  Alcotest.(check (option int))
    "nearest seq in body" (Some recorded_seq)
    (Canon.int_member "nearest_seq" divergence);
  let findings = P.unregister ctx.proxy ~token:token_b in
  (match List.filter is_missing findings.divergences with
  | [ Matcher.Missing_request { nearest = Some (e, _, _); _ } ] ->
      Alcotest.(check int) "nearest seq" recorded_seq e.seq
  | [ Matcher.Missing_request { nearest = None; _ } ]
  | [ (Unconsumed _ | Reordered _ | Shape_mismatch _) ]
  | [] | _ :: _ :: _ ->
      Alcotest.fail "expected one Missing_request with a nearest exchange");
  Alcotest.(check int) "nothing upstream" 0 (List.length (F.requests up))

let until_divergence =
  Alcotest.test_case "until-divergence goes live and records the tail" `Quick
  @@ with_ctx "tail"
  @@ fun ctx ->
  let recorded = fixture "openai_chat" "luna_nonstream" in
  let live = fixture "openai_chat" "luna_stream" in
  let prefix = record_all ctx ~token:token_a [ recorded ] in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run =
    replay_run ctx ~record:true ~matcher:Matcher.Until_divergence ~token:token_b
      ~upstream:(F.base_url up) prefix
  in
  P.register ctx.proxy run;
  let r = send_fixture ctx ~token:token_b live in
  check_ok r;
  Alcotest.(check string) "live bytes" live.raw r.body;
  Alcotest.(check int) "forwarded" 1 (List.length (F.requests up));
  let findings = P.unregister ctx.proxy ~token:token_b in
  Alcotest.(check int)
    "missing request reported" 1
    (List.length (List.filter is_missing findings.divergences));
  let e = only (recording ctx run).exchanges in
  Alcotest.(check string) "tail key" (expected_key live) e.key

let drop_script chunks : F.source =
  F.Script
    [
      {
        status = 200;
        headers = [ ("content-type", "text/event-stream") ];
        chunks;
        ending = Drop;
      };
    ]

let replay_truncated =
  Alcotest.test_case "a recorded cut-off stream replays cut off" `Quick
  @@ with_ctx "replay-drop"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_stream" in
  let chunks =
    [ "data: {\"id\":\"x\",\"choices\":[]}\n\n"; "data: {\"id\":" ]
  in
  let up = fake ctx (drop_script chunks) in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let live = send_fixture ctx ~token:token_a f in
  ignore (P.unregister ctx.proxy ~token:token_a);
  let rec_ = recording ctx run in
  let up2 = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy
    (replay_run ctx ~token:token_b ~upstream:(F.base_url up2) rec_);
  let r = send_fixture ctx ~token:token_b f in
  let partial = String.concat "" chunks in
  (match (live.result, r.result) with
  | Error (Truncated a), Error (Truncated b) ->
      Alcotest.(check int) "live bytes" (String.length partial) a.bytes;
      Alcotest.(check int) "replayed bytes" a.bytes b.bytes
  | ( (Ok () | Error (Connect_failed _ | Failed_after_send _ | Timed_out)),
      ( Ok ()
      | Error (Connect_failed _ | Failed_after_send _ | Timed_out | Truncated _)
        ) )
  | ( Error (Truncated _),
      (Ok () | Error (Connect_failed _ | Failed_after_send _ | Timed_out)) ) ->
      Alcotest.fail "expected truncated bodies live and in replay");
  Alcotest.(check string) "partial body" partial r.body;
  let findings = P.unregister ctx.proxy ~token:token_b in
  Alcotest.(check int) "exchanges" 1 findings.exchanges;
  Alcotest.(check int) "no divergences" 0 (List.length findings.divergences);
  Alcotest.(check int) "nothing upstream" 0 (List.length (F.requests up2))

let whole_cut_off =
  Alcotest.test_case "a cut-off whole body is recorded and replayed cut off"
    `Quick
  @@ with_ctx "whole-drop"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let partial = "{\"id\":\"x\",\"choices\":[{\"mess" in
  let up =
    fake ctx
      (F.Script
         [
           {
             status = 200;
             headers = [ ("content-type", "application/json") ];
             chunks = [ partial ];
             ending = Drop;
           };
         ])
  in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let live = send_fixture ctx ~token:token_a f in
  ignore (P.unregister ctx.proxy ~token:token_a);
  let rec_ = recording ctx run in
  (match (response_of (only rec_.exchanges)).body with
  | Whole { payload = Inline s; complete } ->
      Alcotest.(check string) "partial recorded" partial s;
      Alcotest.(check bool) "marked as cut off" false complete
  | Whole { payload = Blob _; complete = _ } | Stream _ ->
      Alcotest.fail "expected an inline whole body");
  let up2 = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy
    (replay_run ctx ~token:token_b ~upstream:(F.base_url up2) rec_);
  let r = send_fixture ctx ~token:token_b f in
  Alcotest.(check (option int))
    "live truncated"
    (Some (String.length partial))
    (truncated_bytes live);
  Alcotest.(check (option int))
    "replay truncated"
    (Some (String.length partial))
    (truncated_bytes r);
  Alcotest.(check string) "partial body" partial r.body;
  Alcotest.(check int) "nothing upstream" 0 (List.length (F.requests up2))

(* A client that reads the first piece of the response and closes its socket. *)
let read_first_piece_then_close ctx ~token (f : fixture) =
  let net = Eio.Stdenv.net ctx.env in
  Eio.Net.with_tcp_connect ~host:"127.0.0.1"
    ~service:(string_of_int (P.port ctx.proxy))
    net
  @@ fun flow ->
  Eio.Flow.copy_string
    (Printf.sprintf
       "%s /r/%s/openai%s HTTP/1.1\r\n\
        host: x\r\n\
        authorization: Bearer bactrian-%s\r\n\
        content-type: application/json\r\n\
        content-length: %d\r\n\
        \r\n\
        %s"
       f.meth token f.path token (String.length f.request) f.request)
    flow;
  let buf = Cstruct.create 4096 in
  let seen = Buffer.create 256 in
  let contains_event () =
    let hay = Buffer.contents seen in
    let rec at i =
      i + 7 <= String.length hay
      && (String.equal (String.sub hay i 7) "event-1" || at (i + 1))
    in
    at 0
  in
  while not (contains_event ()) do
    let n = Eio.Flow.single_read flow buf in
    Buffer.add_string seen (Cstruct.to_string buf ~len:n)
  done

let agent_disconnect =
  Alcotest.test_case "an agent that goes away mid-stream is recorded" `Quick
  @@ with_ctx "agent-gone"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_stream" in
  let events =
    List.init 10 (fun i -> Printf.sprintf "data: event-%d\n\n" (i + 1))
  in
  let base = slow_upstream ctx ~chunks:events ~gap:0.1 in
  let run = make_run ctx ~token:token_a ~upstream:base in
  P.register ctx.proxy run;
  read_first_piece_then_close ctx ~token:token_a f;
  Eio.Time.sleep (Eio.Stdenv.clock ctx.env) 0.6;
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "exchanges" 1 findings.exchanges;
  Alcotest.(check int) "usage gaps" 1 findings.usage_gaps;
  Alcotest.(check bool) "errors noted" true (findings.errors <> []);
  (match (response_of (only (recording ctx run).exchanges)).body with
  | Stream { chunks; complete } ->
      Alcotest.(check bool) "marked as cut off" false complete;
      let n = List.length chunks in
      if n < 1 || n >= 10 then Alcotest.failf "recorded %d chunks" n
  | Whole _ -> Alcotest.fail "expected a recorded stream");
  Alcotest.(check bool)
    "reservation kept" true
    (Ledger.spent run.ledger ~provider:"openai" > 0.)

(* A call still in flight when its run ends settles the ledger but finds the
   cassette closed: nothing is appended and nothing is raised. *)
let late_finish =
  Alcotest.test_case "a call finishing after the run ended records nothing"
    `Quick
  @@ with_ctx "late"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_stream" in
  let events =
    List.init 4 (fun i -> Printf.sprintf "data: event-%d\n\n" (i + 1))
  in
  let base = slow_upstream ctx ~chunks:events ~gap:0.15 in
  let run = make_run ctx ~token:token_a ~upstream:base in
  P.register ctx.proxy run;
  let clock = Eio.Stdenv.clock ctx.env in
  Eio.Fiber.both
    (fun () -> ignore (send_fixture ctx ~token:token_a f))
    (fun () ->
      Eio.Time.sleep clock 0.2;
      ignore (P.unregister ctx.proxy ~token:token_a);
      Option.iter Store.close run.cassette);
  Eio.Time.sleep clock 0.1;
  Alcotest.(check int) "nothing unattributed" 0 (P.unattributed ctx.proxy);
  Alcotest.(check int)
    "nothing recorded" 0
    (List.length (recording ctx run).exchanges)

let body_limit =
  Alcotest.test_case "a request body over 64 MiB is refused" `Slow
  @@ with_ctx "limit"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let r =
    send_fixture ctx ~token:token_a
      ~body_of:(fun b -> b ^ String.make ((64 * 1024 * 1024) + 1) ' ')
      f
  in
  Alcotest.(check bool)
    "refused with 413 or a dropped connection" true
    (r.status = 413 || r.status = 0);
  Alcotest.(check int) "counted" 1 (P.unattributed ctx.proxy);
  Alcotest.(check int) "nothing forwarded" 0 (List.length (F.requests up))

(* Replay pacing *)

let replay_timing =
  Alcotest.test_case "Recorded timing keeps the pacing, Fast does not" `Quick
  @@ with_ctx "pacing"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_stream" in
  let events =
    List.init 4 (fun i -> Printf.sprintf "data: event-%d\n\n" (i + 1))
  in
  let base = slow_upstream ctx ~chunks:events ~gap:0.1 in
  let run = make_run ctx ~token:token_a ~upstream:base in
  P.register ctx.proxy run;
  check_ok (send_fixture ctx ~token:token_a f);
  ignore (P.unregister ctx.proxy ~token:token_a);
  let rec_ = recording ctx run in
  let replayed timing token =
    P.register ctx.proxy (replay_run ctx ~timing ~token ~upstream:base rec_);
    let r = send_fixture ctx ~token f in
    check_ok r;
    Alcotest.(check string) "bytes" (String.concat "" events) r.body;
    ignore (P.unregister ctx.proxy ~token);
    event_gaps r events
  in
  List.iter
    (fun g ->
      if g < 0.06 then
        Alcotest.failf "recorded pacing lost: %.0f ms" (g *. 1000.))
    (replayed P.Recorded token_b);
  List.iter
    (fun g ->
      if g > 0.05 then Alcotest.failf "fast replay paced: %.0f ms" (g *. 1000.))
    (replayed P.Fast token_c)

(* Gemini key in the query *)

let gemini_query_key =
  Alcotest.test_case "Gemini accepts the placeholder in the key query" `Quick
  @@ with_ctx "gemini-key"
  @@ fun ctx ->
  let f = fixture "gemini_generate" "flash_stream_json" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy (make_run ctx ~token:token_a ~upstream:(F.base_url up));
  let with_key k =
    send ctx ~token:token_a ~provider:P.Gemini ~meth:f.meth f.path
      ~query:
        (f.query ^ (if String.equal f.query "" then "" else "&") ^ "key=" ^ k)
      ~headers:f.request_headers f.request
  in
  let r = with_key ("bactrian-" ^ token_a) in
  check_ok r;
  Alcotest.(check int) "status" f.status r.status;
  Alcotest.(check string) "bytes" f.raw r.body;
  let sent = only (F.requests up) in
  Alcotest.(check (option string))
    "real key in the header" (Some "real-gemini-key")
    (List.assoc_opt "x-goog-api-key" sent.headers);
  Alcotest.(check string) "key removed from the query" f.query sent.query;
  Alcotest.(check int) "wrong key" 403 (with_key "bactrian-wrong").status;
  Alcotest.(check int)
    "still one request upstream" 1
    (List.length (F.requests up));
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "refused keys" 1 findings.refused_keys

(* Errors in findings *)

let errors_in_findings =
  Alcotest.test_case "refusals and bad bodies land in the run's errors" `Quick
  @@ with_ctx "errors"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy (make_run ctx ~token:token_a ~upstream:(F.base_url up));
  let bad = send_fixture ctx ~token:token_a ~body_of:(fun _ -> "not json") f in
  Alcotest.(check int) "bad body" 400 bad.status;
  let refused = send_fixture ctx ~token:token_a ~key:"nope" f in
  Alcotest.(check int) "wrong key" 403 refused.status;
  let findings = P.unregister ctx.proxy ~token:token_a in
  match findings.errors with
  | [ first; second ] ->
      Alcotest.(check bool) "bad body first" true (first <> "");
      Alcotest.(check string)
        "key refusal second" "missing or wrong placeholder key in authorization"
        second
  | l -> Alcotest.failf "expected two errors, got %d" (List.length l)

let unattributed_count =
  Alcotest.test_case "unknown tokens and providers are counted proxy-wide"
    `Quick
  @@ with_ctx "unattributed"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy (make_run ctx ~token:token_a ~upstream:(F.base_url up));
  Alcotest.(check int) "none yet" 0 (P.unattributed ctx.proxy);
  ignore (send_fixture ctx ~token:token_b f);
  Alcotest.(check int) "unknown token" 1 (P.unattributed ctx.proxy);
  ignore
    (send ctx ~seg:"nope" ~token:token_a ~provider:P.Openai ~meth:"POST" "/v1/x"
       "{}");
  Alcotest.(check int) "unknown provider" 2 (P.unattributed ctx.proxy);
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check (list string)) "not in the run's errors" [] findings.errors

(* Provider errors and dropped streams *)

let scripted_429 =
  Alcotest.test_case "a 429 passes through and is recorded" `Quick
  @@ with_ctx "429"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let body = {|{"error":{"message":"slow down","type":"rate_limit"}}|} in
  let up =
    fake ctx
      (F.Script
         [
           {
             status = 429;
             headers =
               [ ("content-type", "application/json"); ("retry-after", "7") ];
             chunks = [ body ];
             ending = Complete;
           };
         ])
  in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let r = send_fixture ctx ~token:token_a f in
  check_ok r;
  Alcotest.(check int) "status" 429 r.status;
  Alcotest.(check string) "body" body r.body;
  Alcotest.(check (option string))
    "retry-after" (Some "7")
    (List.assoc_opt "retry-after" r.headers);
  ignore (P.unregister ctx.proxy ~token:token_a);
  let resp = response_of (only (recording ctx run).exchanges) in
  Alcotest.(check int) "recorded status" 429 resp.status;
  Alcotest.(check string) "recorded body" body (stream_bytes resp);
  Alcotest.(check (float 0.))
    "not charged" 0.
    (Ledger.spent run.ledger ~provider:"openai")

let dropped_stream =
  Alcotest.test_case "a dropped stream drops the agent's connection" `Quick
  @@ with_ctx "drop"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_stream" in
  let chunks =
    [ "data: {\"id\":\"x\",\"choices\":[]}\n\n"; "data: {\"id\":" ]
  in
  let up =
    fake ctx
      (F.Script
         [
           {
             status = 200;
             headers = [ ("content-type", "text/event-stream") ];
             chunks;
             ending = Drop;
           };
         ])
  in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let r = send_fixture ctx ~token:token_a f in
  let partial = String.concat "" chunks in
  (match r.result with
  | Error (Truncated { bytes; _ }) ->
      Alcotest.(check int) "bytes before the drop" (String.length partial) bytes
  | Error (Connect_failed _ | Failed_after_send _ | Timed_out) | Ok () ->
      Alcotest.fail "expected a truncated body");
  Alcotest.(check string) "partial body" partial r.body;
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "usage gaps" 1 findings.usage_gaps;
  Alcotest.(check int) "transport errors" 1 findings.transport_errors;
  let resp = response_of (only (recording ctx run).exchanges) in
  Alcotest.(check string) "partial recorded" partial (stream_bytes resp);
  (match resp.body with
  | Stream { complete; _ } ->
      Alcotest.(check bool) "marked as cut off" false complete
  | Whole _ -> Alcotest.fail "expected a recorded stream");
  (match findings.errors with
  | [ e ] ->
      let prefix =
        Printf.sprintf "upstream body cut off after %d bytes"
          (String.length partial)
      in
      Alcotest.(check bool) ("error " ^ e) true (String.starts_with ~prefix e)
  | [] | _ :: _ :: _ -> Alcotest.fail "expected one error");
  Alcotest.(check bool)
    "reservation kept" true
    (Ledger.spent run.ledger ~provider:"openai" > 0.)

let unknown_model =
  Alcotest.test_case "an unknown model gets 402" `Quick
  @@ with_ctx "model"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let r =
    send_fixture ctx ~token:token_a ~body_of:(set_model "no-such-model") f
  in
  Alcotest.(check int) "status" 402 r.status;
  let findings = P.unregister ctx.proxy ~token:token_a in
  (match findings.cap_refusals with
  | [ Ledger.Unknown_model m ] ->
      Alcotest.(check string) "model" "no-such-model" m
  | [ Ledger.Cap_reached _ ] | [] | _ :: _ :: _ ->
      Alcotest.fail "expected one Unknown_model refusal");
  Alcotest.(check int) "nothing forwarded" 0 (List.length (F.requests up));
  Alcotest.(check int)
    "nothing recorded" 0
    (List.length (recording ctx run).exchanges)

(* A provider that reads the request and hangs up before the head: the
   provider may have billed it, so the reservation stays as a usage gap. *)
let failed_after_send =
  Alcotest.test_case "a hang-up after the request keeps the reservation" `Quick
  @@ with_ctx "after-send"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let upstream = raw_upstream ctx ~reply:"" in
  let run = make_run ctx ~token:token_a ~upstream in
  P.register ctx.proxy run;
  let r = send_fixture ctx ~token:token_a f in
  Alcotest.(check int) "agent sees 502" 502 r.status;
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "usage gaps" 1 findings.usage_gaps;
  Alcotest.(check bool)
    "reservation kept" true
    (Ledger.spent run.ledger ~provider:"openai" > 0.)

let refused_connection =
  Alcotest.test_case "a refused connection settles at zero" `Quick
  @@ with_ctx "refused"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let upstream = refused_url ctx in
  let run = make_run ctx ~token:token_a ~upstream in
  P.register ctx.proxy run;
  let r = send_fixture ctx ~token:token_a f in
  Alcotest.(check int) "agent sees 502" 502 r.status;
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "no usage gap" 0 findings.usage_gaps;
  Alcotest.(check (float 0.))
    "not charged" 0.
    (Ledger.spent run.ledger ~provider:"openai")

(* Two runs at once *)

let two_runs =
  Alcotest.test_case "concurrent runs do not see each other's recordings" `Quick
  @@ with_ctx "two"
  @@ fun ctx ->
  let fa = fixture "openai_chat" "luna_nonstream" in
  let fb = fixture "anthropic_messages" "haiku_nonstream" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let ra = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  let rb = make_run ctx ~token:token_b ~upstream:(F.base_url up) in
  P.register ctx.proxy ra;
  P.register ctx.proxy rb;
  Eio.Fiber.both
    (fun () -> check_ok (send_fixture ctx ~token:token_a fa))
    (fun () -> check_ok (send_fixture ctx ~token:token_b fb));
  let cross = send_fixture ctx ~token:token_b ~key:("bactrian-" ^ token_a) fb in
  Alcotest.(check int) "a's key on b's route" 403 cross.status;
  ignore (P.unregister ctx.proxy ~token:token_a);
  ignore (P.unregister ctx.proxy ~token:token_b);
  let ca = recording ctx ra and cb = recording ctx rb in
  Alcotest.(check string)
    "a recorded its own" "openai_chat" (only ca.exchanges).protocol;
  Alcotest.(check string)
    "b recorded its own" "anthropic_messages" (only cb.exchanges).protocol;
  let up2 = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy
    (replay_run ctx ~token:token_c ~upstream:(F.base_url up2) ca);
  P.register ctx.proxy
    (replay_run ctx ~token:token_d ~upstream:(F.base_url up2) cb);
  let c_own, d_other =
    Eio.Fiber.pair
      (fun () -> send_fixture ctx ~token:token_c fa)
      (fun () -> send_fixture ctx ~token:token_d fa)
  in
  Alcotest.(check string) "c serves a's recording" fa.raw c_own.body;
  Alcotest.(check int) "d does not have a's recording" 409 d_other.status;
  ignore (P.unregister ctx.proxy ~token:token_c);
  ignore (P.unregister ctx.proxy ~token:token_d);
  Alcotest.(check int)
    "nothing upstream in replay" 0
    (List.length (F.requests up2))

(* Routing *)

let routing =
  Alcotest.test_case "routes and base URLs" `Quick
  @@ with_ctx "routes"
  @@ fun ctx ->
  let up = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy (make_run ctx ~token:token_a ~upstream:(F.base_url up));
  let port = P.port ctx.proxy in
  Alcotest.(check string)
    "openai base"
    (Printf.sprintf "http://127.0.0.1:%d/r/%s/openai/v1" port token_a)
    (P.base_url ctx.proxy ~token:token_a P.Openai);
  Alcotest.(check string)
    "openrouter base"
    (Printf.sprintf "http://127.0.0.1:%d/r/%s/openrouter/api/v1" port token_a)
    (P.base_url ctx.proxy ~token:token_a P.Openrouter);
  Alcotest.(check string)
    "anthropic base"
    (Printf.sprintf "http://127.0.0.1:%d/r/%s/anthropic" port token_a)
    (P.base_url ctx.proxy ~token:token_a P.Anthropic);
  let auth = [ ("authorization", "Bearer bactrian-" ^ token_a) ] in
  let unknown_provider =
    send ctx ~seg:"nope" ~token:token_a ~provider:P.Openai ~meth:"POST" "/v1/x"
      ~headers:auth "{}"
  in
  Alcotest.(check int) "unknown provider" 404 unknown_provider.status;
  let unknown_endpoint =
    send ctx ~token:token_a ~provider:P.Openai ~meth:"POST" "/v1/embeddings"
      ~headers:auth "{}"
  in
  Alcotest.(check int) "unmodelled endpoint" 404 unknown_endpoint.status;
  ignore (P.unregister ctx.proxy ~token:token_a);
  Alcotest.(check int) "nothing forwarded" 0 (List.length (F.requests up))

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

(* Run settings: normalise, redact, pricing *)

let with_text text body =
  let v = canon body in
  let messages =
    Canon.list
      [
        Result.get_ok
          (Canon.obj
             [ ("role", Canon.string "user"); ("content", Canon.string text) ]);
      ]
  in
  Canon.to_string (Canon.set_member "messages" messages v)

(* Answers any request with the fixture's response. *)
let any_request ctx (f : fixture) =
  fake ctx
    (F.Script
       [
         {
           F.status = f.status;
           headers = [ ("content-type", f.content_type) ];
           chunks = [ f.raw ];
           ending = Complete;
         };
       ])

let date_pattern = {|\d{4}-\d{2}-\d{2}|}
let on date = with_text ("It is " ^ date ^ ". What is the weather?")

let normalise_setting =
  Alcotest.test_case "normalise masks patterns before keying, in both modes"
    `Quick
  @@ with_ctx "normalise"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let up = any_request ctx f in
  let run =
    make_run ctx ~token:token_a ~upstream:(F.base_url up)
      ~normalise:[ date_pattern ]
  in
  P.register ctx.proxy run;
  check_ok (send_fixture ctx ~token:token_a ~body_of:(on "2026-10-01") f);
  let recorded = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "mask fired while recording" 1 recorded.masks_fired;
  let recording = recording ctx run in
  let e = only recording.exchanges in
  Alcotest.(check bool)
    "the canonical request holds the marker" true
    (contains (Canon.to_string e.canonical) Mask.marker);
  let replay ~token ~normalise date =
    let run =
      make_run ctx ~token ~upstream:(F.base_url up) ~normalise
        ~mode:
          (P.Replay
             {
               recording;
               matcher = Matcher.Strict;
               allow_reorder = false;
               timing = P.Fast;
             })
    in
    P.register ctx.proxy run;
    let r = send_fixture ctx ~token ~body_of:(on date) f in
    (r, P.unregister ctx.proxy ~token)
  in
  let r, findings =
    replay ~token:token_b ~normalise:[ date_pattern ] "2027-03-04"
  in
  Alcotest.(check int) "another date matches" 200 r.status;
  Alcotest.(check string) "recorded bytes" f.raw r.body;
  Alcotest.(check int) "mask fired in replay" 1 findings.masks_fired;
  let r, findings = replay ~token:token_c ~normalise:[] "2027-03-04" in
  Alcotest.(check int) "without the pattern it is a miss" 409 r.status;
  Alcotest.(check int) "no masks without a pattern" 0 findings.masks_fired

let secret_pattern = "tok_[a-z]{6}"

let redact_setting =
  Alcotest.test_case "redact patterns never reach the cassette" `Quick
  @@ with_ctx "redact"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let up = any_request ctx f in
  let run =
    make_run ctx ~token:token_a ~upstream:(F.base_url up)
      ~redact:[ secret_pattern ]
  in
  P.register ctx.proxy run;
  check_ok
    (send_fixture ctx ~token:token_a
       ~body_of:(with_text "my token is tok_abcdef, keep it safe")
       f);
  ignore (P.unregister ctx.proxy ~token:token_a);
  Option.iter Store.close run.cassette;
  let path = store_ok (Store.cassette_path ctx.store ~run_id:run.run_id) in
  let text = Eio.Path.load path in
  Alcotest.(check bool) "secret gone" false (contains text "tok_abcdef");
  Alcotest.(check bool) "placeholder present" true (contains text "<secret:1>")

let pricing_setting =
  Alcotest.test_case "the run's price table settles and reserves" `Quick
  @@ with_ctx "pricing"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let spend pricing =
    let token = if Option.is_some pricing then token_b else token_a in
    let run = make_run ctx ~token ~upstream:(F.base_url up) ?pricing in
    P.register ctx.proxy run;
    let r = send_fixture ctx ~token f in
    ignore (P.unregister ctx.proxy ~token);
    (r, Ledger.spent run.ledger ~provider:"openai")
  in
  let _, base = spend None in
  let double : Pricing.entry =
    let e = Option.get (Pricing.find ~model:"gpt-6-luna" ~date:(today ())) in
    {
      e with
      rates =
        {
          e.rates with
          input = e.rates.input *. 10.;
          output = e.rates.output *. 10.;
        };
    }
  in
  let _, tenfold = spend (Some (Pricing.with_overrides [ double ])) in
  Alcotest.(check bool) "default spend is positive" true (base > 0.);
  Alcotest.(check bool)
    "ten times the rates, ten times the spend" true
    (close_to tenfold (base *. 10.))

let () =
  Alcotest.run "model_proxy"
    [
      ("record", record_cases);
      ( "streaming",
        [
          streaming;
          dropped_stream;
          agent_disconnect;
          late_finish;
          replay_timing;
        ] );
      ("keys", [ placeholder_keys; gemini_query_key; unknown_model; routing ]);
      ("after the request", [ failed_after_send; refused_connection ]);
      ( "replay",
        [
          replay_strict;
          replay_miss;
          until_divergence;
          replay_truncated;
          whole_cut_off;
        ] );
      ( "errors",
        [ scripted_429; errors_in_findings; unattributed_count; body_limit ] );
      ("isolation", [ two_runs ]);
      ("run settings", [ normalise_setting; redact_setting; pricing_setting ]);
    ]
