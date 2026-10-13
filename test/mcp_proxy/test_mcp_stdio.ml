open Bactrian_core
open Mcp_support

(* Record against the real stdio server, then replay strictly with the server
   unavailable: same results, live ids, no divergence, no spawn. *)

let modern_round_trip =
  Alcotest.test_case "modern: record, then strict replay without the server"
    `Quick
  @@ with_ctx "mcp-modern"
  @@ fun ctx ->
  let log = path ctx "server.log" in
  let server = stdio ~args:[ "--log"; log ] "weather" in
  let run = record_run ctx ~token:token_a [ server ] in
  let send body = post ctx ~token:token_a ~name:"weather" body in
  let r1 = send (discover (Int_id 1)) in
  check_status "discover" 200 r1;
  let id, discovered = result_of r1.body in
  Alcotest.check id_testable "discover id" (Int_id 1) id;
  let r2 = send (call (Int_id 2) "Paris") in
  let id, called = result_of r2.body in
  Alcotest.check id_testable "call id" (Int_id 2) id;
  Alcotest.(check string) "weather" "Sunny, 21C in Paris" (weather_text called);
  let r3 = send (tools_list (String_id "three")) in
  let id, listed = result_of r3.body in
  Alcotest.check id_testable "string id" (String_id "three") id;
  let f = P.unregister ctx.proxy ~token:token_a in
  no_divergence f;
  Alcotest.(check int) "exchanges" 3 f.exchanges;
  Alcotest.(check int) "server saw" 3 (List.length (lines_of log));
  let recording = recording ctx run in
  let recorded = mcp_exchanges recording in
  Alcotest.(check int) "recorded" 3 (List.length recorded);
  List.iter
    (fun (e : Cassette.exchange) ->
      Alcotest.(check string) "path" "/mcp/weather" e.path;
      Alcotest.(check string) "protocol" "mcp" e.protocol)
    recorded;
  Sys.remove log;
  (* Replay: other ids, another client. *)
  let _ = replay_mcp ctx ~token:token_b recording [ server ] in
  let send body = post ctx ~token:token_b ~name:"weather" body in
  let id, r = result_of (send (discover ~client:"other" (Int_id 41))).body in
  Alcotest.check id_testable "replayed discover id" (Int_id 41) id;
  Alcotest.check canon_testable "replayed discover" discovered r;
  let id, r =
    result_of (send (call ~client:"other" (Int_id 42) "Paris")).body
  in
  Alcotest.check id_testable "replayed call id" (Int_id 42) id;
  Alcotest.check canon_testable "replayed call" called r;
  let id, r =
    result_of (send (tools_list ~client:"other" (String_id "x"))).body
  in
  Alcotest.check id_testable "replayed list id" (String_id "x") id;
  Alcotest.check canon_testable "replayed list" listed r;
  let f = P.unregister ctx.proxy ~token:token_b in
  no_divergence f;
  Alcotest.(check int) "served" 3 f.exchanges;
  Alcotest.(check bool) "server never started" false (Sys.file_exists log)

let legacy_round_trip =
  Alcotest.test_case "legacy: initialize, notification, record and replay"
    `Quick
  @@ with_ctx "mcp-legacy"
  @@ fun ctx ->
  let log = path ctx "server.log" in
  let server = stdio ~era:Legacy ~args:[ "--log"; log ] "weather" in
  let run = record_run ctx ~token:token_a [ server ] in
  let send body = post ctx ~token:token_a ~name:"weather" body in
  let _, init = result_of (send (initialize (Int_id 1))).body in
  let note = send initialized in
  check_status "notification" 202 note;
  Alcotest.(check string) "no body" "" note.body;
  let _, called =
    result_of (send (call ~era:Legacy (Int_id 2) "London")).body
  in
  Alcotest.(check string) "weather" "Sunny, 21C in London" (weather_text called);
  ignore (P.unregister ctx.proxy ~token:token_a);
  Alcotest.(check int) "notification forwarded" 3 (List.length (lines_of log));
  let recording = recording ctx run in
  Alcotest.(check int)
    "notifications are not recorded" 2
    (List.length (mcp_exchanges recording));
  Sys.remove log;
  let _ = replay_mcp ctx ~token:token_b recording [ server ] in
  let send body = post ctx ~token:token_b ~name:"weather" body in
  let id, r = result_of (send (initialize ~client:"other" (Int_id 7))).body in
  Alcotest.check id_testable "initialize id" (Int_id 7) id;
  Alcotest.check canon_testable "initialize" init r;
  check_status "notification dropped" 202 (send initialized);
  let _, r = result_of (send (call ~era:Legacy (Int_id 8) "London")).body in
  Alcotest.check canon_testable "call" called r;
  no_divergence (P.unregister ctx.proxy ~token:token_b);
  Alcotest.(check bool) "server never started" false (Sys.file_exists log)

(* A modern client probing a legacy server: server/discover is refused with
   -32601, then initialize. The probe replays like any other exchange. *)
let dual_era_probe =
  Alcotest.test_case "a dual-era probe records and replays" `Quick
  @@ with_ctx "mcp-probe"
  @@ fun ctx ->
  let server = stdio ~era:Legacy "weather" in
  let run = record_run ctx ~token:token_a [ server ] in
  let send token body = post ctx ~token ~name:"weather" body in
  let _, code, _, _ = error_of (send token_a (discover (Int_id 1))).body in
  Alcotest.(check int) "legacy refuses discover" (-32601) code;
  ignore (result_of (send token_a (initialize (Int_id 2))).body);
  ignore (P.unregister ctx.proxy ~token:token_a);
  let _ = replay_mcp ctx ~token:token_b (recording ctx run) [ server ] in
  let id, code, _, _ = error_of (send token_b (discover (Int_id 11))).body in
  Alcotest.check id_testable "error id" (Int_id 11) id;
  Alcotest.(check int) "replayed refusal" (-32601) code;
  ignore (result_of (send token_b (initialize (Int_id 12))).body);
  no_divergence (P.unregister ctx.proxy ~token:token_b)

(* The wrong handshake is a divergence naming it, not a hang. *)
let handshake_divergence =
  let case name ~recorded_era ~record ~replay ~method_ =
    Alcotest.test_case name `Quick
    @@ with_ctx ("mcp-hs-" ^ method_)
    @@ fun ctx ->
    let server = stdio ~era:recorded_era "weather" in
    let run = record_run ctx ~token:token_a [ server ] in
    List.iter
      (fun body -> ignore (post ctx ~token:token_a ~name:"weather" body))
      record;
    ignore (P.unregister ctx.proxy ~token:token_a);
    let _ = replay_mcp ctx ~token:token_b (recording ctx run) [ server ] in
    let started = Unix.gettimeofday () in
    let r = post ctx ~token:token_b ~name:"weather" replay in
    if Unix.gettimeofday () -. started > 5. then Alcotest.fail "slow refusal";
    let id, code, message, data = error_of r.body in
    Alcotest.check id_testable "live id" (Int_id 99) id;
    Alcotest.(check int) "code" (-32000) code;
    Alcotest.(check string) "type" "bactrian_replay_miss" (error_type data);
    if not (contains message ("handshake " ^ method_)) then
      Alcotest.failf "message does not name the handshake: %s" message;
    let f = P.unregister ctx.proxy ~token:token_b in
    let missing =
      List.filter_map
        (function
          | Matcher.Missing_request { request; _ } ->
              Canon.str_member "method" request.canonical
          | Unconsumed _ | Reordered _ | Shape_mismatch _ -> None)
        f.divergences
    in
    Alcotest.(check (list string)) "missing" [ method_ ] missing;
    Alcotest.(check int)
      "recorded calls left unconsumed" (List.length record)
      (List.length f.divergences - 1)
  in
  [
    case "a legacy client against a modern recording"
      ~recorded_era:Fake_mcp.Modern
      ~record:[ discover (Int_id 1); call (Int_id 2) "Paris" ]
      ~replay:(initialize (Int_id 99)) ~method_:"initialize";
    case "a modern client against a legacy recording"
      ~recorded_era:Fake_mcp.Legacy
      ~record:[ initialize (Int_id 1); tools_list ~era:Legacy (Int_id 2) ]
      ~replay:(discover (Int_id 99)) ~method_:"server/discover";
  ]

(* Parallel calls replay when they arrive in another order. *)
let parallel =
  Alcotest.test_case "parallel calls record and replay out of order" `Quick
  @@ with_ctx "mcp-parallel"
  @@ fun ctx ->
  let server = stdio "weather" in
  let run = record_run ctx ~token:token_a [ server ] in
  let both token (id1, city1) (id2, city2) =
    let send id city =
      let id', r =
        result_of (post ctx ~token ~name:"weather" (call id city)).body
      in
      Alcotest.check id_testable "id" id id';
      Alcotest.(check string)
        "answer" ("Sunny, 21C in " ^ city) (weather_text r)
    in
    Eio.Fiber.both (fun () -> send id1 city1) (fun () -> send id2 city2)
  in
  both token_a (Int_id 1, "Paris") (Int_id 2, "London");
  ignore (P.unregister ctx.proxy ~token:token_a);
  let _ = replay_mcp ctx ~token:token_b (recording ctx run) [ server ] in
  both token_b (Int_id 20, "London") (Int_id 10, "Paris");
  no_divergence (P.unregister ctx.proxy ~token:token_b)

(* A crash is a JSON-RPC error and a transport error. *)
let crash =
  Alcotest.test_case "a crashed server gives errors, not a hang" `Quick
  @@ with_ctx "mcp-crash"
  @@ fun ctx ->
  let server = stdio ~args:[ "--crash-on"; "Atlantis" ] "weather" in
  let run = record_run ctx ~token:token_a [ server ] in
  let send body = post ctx ~token:token_a ~name:"weather" body in
  ignore (result_of (send (call (Int_id 1) "Paris")).body);
  let started = Unix.gettimeofday () in
  let id, code, message, data =
    error_of (send (call (Int_id 2) "Atlantis")).body
  in
  if Unix.gettimeofday () -. started > 5. then Alcotest.fail "slow crash report";
  Alcotest.check id_testable "live id" (Int_id 2) id;
  Alcotest.(check int) "code" (-32000) code;
  Alcotest.(check string) "type" "bactrian_mcp_server_error" (error_type data);
  if not (contains message "exited") then Alcotest.failf "message: %s" message;
  let id, _, _, _ = error_of (send (call (Int_id 3) "Paris")).body in
  Alcotest.check id_testable "later call" (Int_id 3) id;
  let f = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "transport errors" 2 f.transport_errors;
  let recording = recording ctx run in
  let outcomes =
    List.map
      (fun (e : Cassette.exchange) ->
        match e.outcome with
        | Response _ -> "response"
        | Transport_error _ -> "error")
      (mcp_exchanges recording)
  in
  Alcotest.(check (list string))
    "recorded"
    [ "response"; "error"; "error" ]
    outcomes;
  (* The recorded failure replays as an error with the live id. *)
  let _ = replay_mcp ctx ~token:token_b recording [ server ] in
  let send body = post ctx ~token:token_b ~name:"weather" body in
  ignore (result_of (send (call (Int_id 1) "Paris")).body);
  let id, _, _, data = error_of (send (call (Int_id 77) "Atlantis")).body in
  Alcotest.check id_testable "replayed error id" (Int_id 77) id;
  Alcotest.(check string)
    "replayed type" "bactrian_mcp_server_error" (error_type data);
  ignore (send (call (Int_id 3) "Paris"));
  no_divergence (P.unregister ctx.proxy ~token:token_b)

let block =
  Alcotest.test_case "block answers -32601 and starts nothing" `Quick
  @@ with_ctx "mcp-block"
  @@ fun ctx ->
  let log = path ctx "server.log" in
  let server = stdio ~policy:Block ~args:[ "--log"; log ] "weather" in
  let _ = record_run ctx ~token:token_a [ server ] in
  let send body = post ctx ~token:token_a ~name:"weather" body in
  let id, code, _, _ = error_of (send (call (Int_id 5) "Paris")).body in
  Alcotest.check id_testable "id" (Int_id 5) id;
  Alcotest.(check int) "code" (-32601) code;
  check_status "notification" 202 (send initialized);
  let f = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "exchanges" 0 f.exchanges;
  Alcotest.(check bool) "never started" false (Sys.file_exists log)

let policies =
  Alcotest.test_case "passthrough, replay without a recording, unknown server"
    `Quick
  @@ with_ctx "mcp-policies"
  @@ fun ctx ->
  let run =
    record_run ctx ~token:token_a
      [
        stdio ~policy:Passthrough "live";
        stdio ~policy:Replay "canned";
        {
          M.name = "missing";
          policy = Record;
          server =
            Stdio { argv = [ "/nonexistent/server" ]; env = []; cwd = None };
        };
      ]
  in
  let send name body = post ctx ~token:token_a ~name body in
  ignore (result_of (send "live" (call (Int_id 1) "Oslo")).body);
  let _, _, _, data = error_of (send "canned" (call (Int_id 2) "Oslo")).body in
  Alcotest.(check string)
    "no recording" "bactrian_replay_miss" (error_type data);
  let unknown = send "nobody" (call (Int_id 3) "Oslo") in
  check_status "unknown" 404 unknown;
  let id, code, _, _ = error_of unknown.body in
  Alcotest.check id_testable "unknown server's id" (Int_id 3) id;
  Alcotest.(check int) "unknown server's code" (-32601) code;
  let _, _, message, data =
    error_of (send "missing" (call (Int_id 4) "Oslo")).body
  in
  Alcotest.(check string)
    "spawn failure" "bactrian_mcp_server_error" (error_type data);
  if not (contains message "could not start") then
    Alcotest.failf "message %s" message;
  let f = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "passthrough and failed spawn counted" 2 f.exchanges;
  Alcotest.(check int) "transport errors" 1 f.transport_errors;
  Alcotest.(check (list string))
    "only the failed spawn is recorded" [ "/mcp/missing" ]
    (List.map
       (fun (e : Cassette.exchange) -> e.path)
       (mcp_exchanges (recording ctx run)))

(* The server gets only its declared environment, and dies with the run. *)
let environment =
  Alcotest.test_case "the server sees only its declared environment" `Quick
  @@ with_ctx "mcp-env"
  @@ fun ctx ->
  Unix.putenv "OPENAI_API_KEY" "real-parent-key";
  Unix.putenv "HTTPS_PROXY" "http://parent-proxy";
  Unix.putenv M.url_variable "http://parent";
  let env_log = path ctx "env.log" and pid_file = path ctx "pid" in
  let server =
    stdio
      ~args:[ "--env-log"; env_log; "--pid-file"; pid_file ]
      ~env:[ ("WEATHER_UNITS", "metric") ]
      "weather"
  in
  let _ = record_run ctx ~token:token_a [ server ] in
  ignore
    (result_of
       (post ctx ~token:token_a ~name:"weather" (call (Int_id 1) "Rome")).body);
  let env = lines_of env_log in
  let names = List.map (fun l -> List.hd (String.split_on_char '=' l)) env in
  List.iter
    (fun n ->
      if not (List.mem n [ "PATH"; "HOME"; "LANG"; "WEATHER_UNITS" ]) then
        Alcotest.failf "unexpected variable %s" n)
    names;
  Alcotest.(check bool) "declared" true (List.mem "WEATHER_UNITS=metric" env);
  Alcotest.(check bool)
    "no key" false
    (contains (String.concat "\n" env) "real-parent-key");
  let pid = int_of_string (String.trim (read pid_file)) in
  ignore (P.unregister ctx.proxy ~token:token_a);
  match Unix.kill pid 0 with
  | () -> Alcotest.failf "server %d still running after unregister" pid
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ()

let () =
  Alcotest.run "mcp_stdio"
    [
      ( "record and replay",
        [ modern_round_trip; legacy_round_trip; dual_era_probe; parallel ] );
      ("handshake", handshake_divergence);
      ("failures", [ crash; block; policies ]);
      ("environment", [ environment ]);
    ]
