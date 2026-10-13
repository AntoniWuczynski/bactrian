open Bactrian_core
open Bactrian_runtime
open Mcp_support

(* Each test orders events through the fake server's script, never by
   sleeping: with --reverse-pairs the server answers a tools/call only once
   the next one has arrived, so the answer to the second call proves that the
   first is on the server's stdin and in flight. *)

let cancel id =
  Mcp_msg.encode
    (Notification
       {
         method_ = "notifications/cancelled";
         params = Some (obj [ ("requestId", Canon.int id); ("reason", s "t") ]);
       })

let in_background ctx f = Eio.Fiber.fork_promise ~sw:ctx.sw f

let city_of params =
  Option.bind (Canon.member "arguments" params) (Canon.str_member "city")

(* The ids bactrian gave the calls for [city], in the order the server got
   them. *)
let server_ids log city =
  List.filter_map
    (fun line ->
      match decode line with
      | Request { id; method_ = "tools/call"; params = Some p }
        when Option.equal String.equal (city_of p) (Some city) ->
          Some id
      | Request _ | Notification _ | Response _ | Error_response _ -> None)
    (lines_of log)

let cancellations log =
  List.filter_map
    (fun line ->
      match decode line with
      | Notification { method_ = "notifications/cancelled"; params = Some p } ->
          Canon.member "requestId" p
      | Request _ | Notification _ | Response _ | Error_response _ -> None)
    (lines_of log)

let mentions what errors =
  List.length (List.filter (fun e -> contains e what) errors)

let cancellation =
  Alcotest.test_case "a cancellation reaches the server with bactrian's id"
    `Quick
  @@ with_ctx "mcp-cancel"
  @@ fun ctx ->
  let log = path ctx "server.log" in
  let args =
    [
      "--log";
      log;
      "--reverse-pairs";
      "--hang-on";
      "Atlantis";
      "--hang-on";
      "Lemuria";
    ]
  in
  let run = record_run ctx ~token:token_a [ stdio ~args "weather" ] in
  let send body = post ctx ~token:token_a ~name:"weather" body in
  (* Two requests first, so that bactrian's ids are not the agent's. *)
  ignore (result_of (send (discover (Int_id 1))).body);
  ignore (result_of (send (tools_list (Int_id 2))).body);
  let atlantis =
    in_background ctx (fun () -> send (call (Int_id 1) "Atlantis"))
  in
  ignore (result_of (send (call (Int_id 2) "Paris")).body);
  check_status "cancel" 202 (send (cancel 1));
  (* The server never answers a cancelled request, so bactrian does. *)
  let id, _, message, _ = error_of (Eio.Promise.await_exn atlantis).body in
  Alcotest.check id_testable "cancelled call's id" (Int_id 1) id;
  if not (contains message "cancelled") then Alcotest.failf "message %s" message;
  (* Two calls in flight with the same agent id, and an id nobody used: neither
     cancellation can be tied to one request, so neither is forwarded. *)
  let first = in_background ctx (fun () -> send (call (Int_id 7) "Atlantis")) in
  ignore (result_of (send (call (Int_id 8) "Paris")).body);
  let second = in_background ctx (fun () -> send (call (Int_id 7) "Lemuria")) in
  ignore (result_of (send (call (Int_id 9) "London")).body);
  check_status "ambiguous" 202 (send (cancel 7));
  check_status "unknown" 202 (send (cancel 99));
  (* The server reads its stdin in order, so once this is answered it has
     logged everything written before it. *)
  ignore (result_of (send (tools_list (Int_id 10))).body);
  let cancelled =
    match server_ids log "Atlantis" with
    | Int_id n :: _ -> n
    | (String_id _ :: _ | []) as ids ->
        Alcotest.failf "Atlantis calls: %d" (List.length ids)
  in
  if cancelled = 1 then Alcotest.fail "bactrian's id is the agent's";
  Alcotest.(check (list canon_testable))
    "forwarded"
    [ Canon.int cancelled ]
    (cancellations log);
  let f = P.unregister ctx.proxy ~token:token_a in
  List.iter
    (fun p -> ignore (error_of (Eio.Promise.await_exn p).body))
    [ first; second ];
  Alcotest.(check int)
    "dropped cancellations noted" 2
    (mentions "cancellation" f.errors);
  Alcotest.(check int) "transport errors" 0 f.transport_errors;
  Alcotest.(check int)
    "cancelled and cut-off calls are not recorded" 6
    (List.length (mcp_exchanges (recording ctx run)))

let cut_off =
  Alcotest.test_case "a request cut off by the end of the run is not recorded"
    `Quick
  @@ with_ctx "mcp-cut-off"
  @@ fun ctx ->
  let args = [ "--reverse-pairs"; "--hang-on"; "Atlantis" ] in
  let run = record_run ctx ~token:token_a [ stdio ~args "weather" ] in
  let send body = post ctx ~token:token_a ~name:"weather" body in
  let atlantis =
    in_background ctx (fun () -> send (call (Int_id 1) "Atlantis"))
  in
  ignore (result_of (send (call (Int_id 2) "Paris")).body);
  let f = P.unregister ctx.proxy ~token:token_a in
  let id, _, message, _ = error_of (Eio.Promise.await_exn atlantis).body in
  Alcotest.check id_testable "id" (Int_id 1) id;
  if not (contains message "ended") then Alcotest.failf "message %s" message;
  Alcotest.(check int) "transport errors" 0 f.transport_errors;
  Alcotest.(check (list string))
    "only Paris recorded" [ "tools/call" ]
    (List.filter_map
       (fun (e : Cassette.exchange) -> Canon.str_member "method" e.canonical)
       (mcp_exchanges (recording ctx run)))

let out_of_order =
  Alcotest.test_case "answers out of order reach the right requests" `Quick
  @@ with_ctx "mcp-reversed"
  @@ fun ctx ->
  let _ =
    record_run ctx ~token:token_a
      [ stdio ~args:[ "--reverse-pairs" ] "weather" ]
  in
  let ask id city () =
    let id', r =
      result_of (post ctx ~token:token_a ~name:"weather" (call id city)).body
    in
    Alcotest.check id_testable "id" id id';
    Alcotest.(check string) "answer" ("Sunny, 21C in " ^ city) (weather_text r)
  in
  Eio.Fiber.both (ask (Int_id 1) "Paris") (ask (Int_id 2) "London");
  no_divergence (P.unregister ctx.proxy ~token:token_a)

let server_request =
  Alcotest.test_case "a request from the server is refused, not left waiting"
    `Quick
  @@ with_ctx "mcp-server-request"
  @@ fun ctx ->
  let _ =
    record_run ctx ~token:token_a [ stdio ~args:[ "--ask-client" ] "weather" ]
  in
  let _, r =
    result_of
      (post ctx ~token:token_a ~name:"weather" (call (Int_id 1) "Paris")).body
  in
  Alcotest.(check string)
    "what the server got" "roots/list: error -32601" (weather_text r);
  ignore (P.unregister ctx.proxy ~token:token_a)

let null_id =
  Alcotest.test_case "an error without an id answers the one pending request"
    `Quick
  @@ with_ctx "mcp-null-id"
  @@ fun ctx ->
  let _ =
    record_run ctx ~token:token_a
      [ stdio ~args:[ "--null-id-error-on"; "Atlantis" ] "weather" ]
  in
  let r =
    post ctx ~token:token_a ~name:"weather" (call (Int_id 4) "Atlantis")
  in
  let id, code, _, _ = error_of r.body in
  Alcotest.check id_testable "id" (Int_id 4) id;
  Alcotest.(check int) "the server's code" (-32603) code;
  ignore (P.unregister ctx.proxy ~token:token_a)

(* Driven without the model proxy, to give the server half a second. *)
let stalled =
  Alcotest.test_case "a server that stops reading its stdin is given up on"
    `Quick
  @@ with_ctx "mcp-no-read"
  @@ fun ctx ->
  let clock = Eio.Stdenv.clock ctx.env and net = Eio.Stdenv.net ctx.env in
  let failures = ref [] in
  let env : M.env =
    {
      sw = ctx.sw;
      process_mgr =
        (Eio.Stdenv.process_mgr ctx.env
          :> Eio_unix.Process.mgr_ty Eio.Resource.t);
      clock :> float Eio.Time.clock_ty Eio.Resource.t;
      call = Upstream.call ~net ~clock ~timeout:5.;
      answer_timeout = 0.5;
    }
  in
  let shared : M.shared =
    {
      serving = Live { record = false };
      store = ctx.store;
      connector = Upstream.plain;
      t_ms = (fun () -> 0);
      scrub = Fun.id;
      mask = Fun.id;
      occurrence = (fun _ -> 0);
      exchange = (fun ~record:_ _ -> ());
      failed = (fun m -> failures := m :: !failures);
      note = ignore;
    }
  in
  let mcp = M.create env shared [ stdio ~args:[ "--no-read" ] "weather" ] in
  let send id city =
    Eio.Time.with_timeout_exn clock 5. (fun () ->
        M.stdio mcp ~name:"weather" (call id city))
  in
  (* Far more than a pipe holds, so the write cannot finish. *)
  let id, _, message, _ =
    error_of (send (Int_id 1) (String.make (1024 * 1024) 'x')).body
  in
  Alcotest.check id_testable "id" (Int_id 1) id;
  if not (contains message "stopped reading") then
    Alcotest.failf "message %s" message;
  let id, _, later, _ = error_of (send (Int_id 2) "Paris").body in
  Alcotest.check id_testable "later id" (Int_id 2) id;
  if not (contains later "stopped reading") then
    Alcotest.failf "later message %s" later;
  Alcotest.(check int) "transport errors" 2 (List.length !failures);
  M.stop mcp

let () =
  Alcotest.run "mcp_concurrency"
    [
      ("cancellation", [ cancellation; cut_off ]);
      ("server behaviour", [ out_of_order; server_request; null_id; stalled ]);
    ]
