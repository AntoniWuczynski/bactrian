open Bactrian_core
open Proxy_support

(* Replay *)

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

let () =
  Alcotest.run "model_proxy_replay"
    [
      ( "replay",
        [
          replay_strict;
          replay_miss;
          until_divergence;
          replay_truncated;
          whole_cut_off;
          replay_timing;
        ] );
    ]
