module Ledger = Bactrian_runtime.Ledger

let with_tmp name f =
  Eio_main.run @@ fun env ->
  let dir = Eio.Path.(Eio.Stdenv.cwd env / ("tmp-ledger-" ^ name)) in
  Eio.Path.rmtree ~missing_ok:true dir;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
    (fun () -> f dir)

let reserve ?(provider = "openai") ?(owner = "run-a") l usd =
  Ledger.reserve l ~owner ~provider ~model:"gpt-6-luna" ~usd

let granted what = function
  | Ok r -> r
  | Error (Ledger.Cap_reached _ | Ledger.Unknown_model _) ->
      Alcotest.failf "%s: unexpectedly refused" what

let is_refused = function
  | Error (Ledger.Cap_reached _) -> true
  | Ok _ | Error (Ledger.Unknown_model _) -> false

let check_usd what expected actual =
  Alcotest.(check (float 1e-9)) what expected actual

let mk dir ?(caps = [ ("openai", 1.0) ]) () =
  Ledger.create ~path:Eio.Path.(dir / "ledger.jsonl") ~caps

let test_refuses_past_cap () =
  with_tmp "cap" @@ fun dir ->
  let l = mk dir () in
  ignore (granted "first" (reserve l 0.6));
  (match reserve l 0.5 with
  | Error (Ledger.Cap_reached { provider; cap; spent; requested }) ->
      Alcotest.(check string) "provider" "openai" provider;
      check_usd "cap" 1.0 cap;
      check_usd "spent" 0.6 spent;
      check_usd "requested" 0.5 requested
  | Ok _ | Error (Ledger.Unknown_model _) -> Alcotest.fail "expected refusal");
  check_usd "refusal charges nothing" 0.6 (Ledger.spent l ~provider:"openai");
  ignore (granted "exactly the cap" (reserve l 0.4));
  Alcotest.(check bool) "nothing more" true (is_refused (reserve l 0.000001))

let test_refusal_cases () =
  with_tmp "refusals" @@ fun dir ->
  let l = mk dir () in
  Alcotest.(check bool)
    "provider without a cap" true
    (is_refused (reserve ~provider:"gemini" l 0.0001));
  Alcotest.(check bool) "negative" true (is_refused (reserve l (-1.0)));
  Alcotest.(check bool) "nan" true (is_refused (reserve l Float.nan));
  Alcotest.(check bool) "infinity" true (is_refused (reserve l Float.infinity));
  check_usd "nothing charged" 0.0 (Ledger.spent l ~provider:"openai")

let test_settle () =
  with_tmp "settle" @@ fun dir ->
  let l = mk dir () in
  let a = granted "a" (reserve l 0.5) in
  Ledger.settle l a ~actual:(Some 0.1);
  check_usd "actual replaces reservation" 0.1
    (Ledger.spent l ~provider:"openai");
  Ledger.settle l a ~actual:(Some 0.9);
  check_usd "settling twice is a no-op" 0.1 (Ledger.spent l ~provider:"openai");
  let b = granted "b" (reserve l 0.5) in
  Ledger.settle l b ~actual:None;
  check_usd "usage gap keeps the reservation" 0.6
    (Ledger.spent l ~provider:"openai");
  let c = granted "c" (reserve l 0.2) in
  Ledger.settle l c ~actual:(Some Float.nan);
  check_usd "nan actual is a gap" 0.8 (Ledger.spent l ~provider:"openai")

let test_unsettled_stays_charged () =
  with_tmp "unsettled" @@ fun dir ->
  let l = mk dir () in
  ignore (granted "held" (reserve l 0.7));
  check_usd "charged" 0.7 (Ledger.spent l ~provider:"openai");
  Alcotest.(check bool)
    "so it blocks later requests" true
    (is_refused (reserve l 0.4))

let test_concurrent_never_exceeds () =
  with_tmp "concurrent" @@ fun dir ->
  let l = mk dir ~caps:[ ("openai", 100.0) ] () in
  let granted_count = Atomic.make 0 in
  Eio.Fiber.all
    (List.init 1000 (fun _ () ->
         match reserve l 1.0 with
         | Ok r ->
             Atomic.incr granted_count;
             Eio.Fiber.yield ();
             Ledger.settle l r ~actual:None
         | Error (Ledger.Cap_reached _ | Ledger.Unknown_model _) -> ()));
  Alcotest.(check int) "exactly the cap's worth" 100 (Atomic.get granted_count);
  check_usd "spent" 100.0 (Ledger.spent l ~provider:"openai");
  Alcotest.(check bool) "full" true (is_refused (reserve l 1.0))

let test_reload () =
  with_tmp "reload" @@ fun dir ->
  let caps = [ ("openai", 1.0); ("anthropic", 1.0) ] in
  let l = mk dir ~caps () in
  let a = granted "a" (reserve l 0.3) in
  Ledger.settle l a ~actual:(Some 0.1);
  let b = granted "b" (reserve l 0.2) in
  Ledger.settle l b ~actual:None;
  ignore (granted "c, never settled" (reserve l 0.1));
  ignore (granted "other provider" (reserve ~provider:"anthropic" l 0.5));
  let l2 = mk dir ~caps () in
  check_usd "openai" 0.4 (Ledger.spent l2 ~provider:"openai");
  check_usd "anthropic" 0.5 (Ledger.spent l2 ~provider:"anthropic");
  Alcotest.(check bool)
    "cap enforced against reloaded spend" true
    (is_refused (reserve l2 0.7));
  let d = granted "d" (reserve l2 0.5) in
  Ledger.settle l2 d ~actual:(Some 0.25);
  let l3 = mk dir ~caps () in
  check_usd "ids do not collide after reload" 0.65
    (Ledger.spent l3 ~provider:"openai")

let test_reload_legacy () =
  with_tmp "legacy" @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(dir / "ledger.jsonl")
    ({|{"date": "2026-10-13T01:08:01+00:00", "provider": "openai", "model": "gpt-5-nano", "input_tokens": 145, "output_tokens": 23, "usd": 1.645e-05}|}
   ^ "\n"
   ^ {|{"date": "2026-10-13T01:08:09+00:00", "provider": "openai", "model": "gpt-6-luna", "input_tokens": 145, "output_tokens": 17, "usd": 2.3e-05}|}
   ^ "\n");
  let l = mk dir () in
  check_usd "capture-script lines count as settled" 3.945e-05
    (Ledger.spent l ~provider:"openai")

let test_spent_by_owner () =
  with_tmp "owner" @@ fun dir ->
  let caps = [ ("openai", 1.0); ("anthropic", 1.0) ] in
  let l = mk dir ~caps () in
  let a = granted "a" (reserve ~owner:"run-a" l 0.3) in
  let b = granted "b" (reserve ~owner:"run-b" l 0.2) in
  check_usd "outstanding reservation is charged to its owner" 0.3
    (Ledger.spent_by l ~owner:"run-a");
  Ledger.settle l a ~actual:(Some 0.1);
  Ledger.settle l b ~actual:None;
  let c = granted "c" (reserve ~owner:"run-a" ~provider:"anthropic" l 0.4) in
  Ledger.settle l c ~actual:(Some 0.05);
  check_usd "settled plus other provider" 0.15
    (Ledger.spent_by l ~owner:"run-a");
  check_usd "a usage gap stays with its owner" 0.2
    (Ledger.spent_by l ~owner:"run-b");
  check_usd "unknown owner" 0. (Ledger.spent_by l ~owner:"run-z");
  let l2 = mk dir ~caps () in
  check_usd "owner survives a reload" 0.15 (Ledger.spent_by l2 ~owner:"run-a");
  check_usd "gap survives a reload" 0.2 (Ledger.spent_by l2 ~owner:"run-b")

let test_corrupt () =
  with_tmp "corrupt" @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(dir / "ledger.jsonl")
    ({|{"provider": "openai", "usd": 0.5}|} ^ "\n" ^ "not json\n");
  match mk dir () with
  | _ -> Alcotest.fail "corrupt ledger accepted"
  | exception Ledger.Corrupt { line; _ } ->
      Alcotest.(check int) "line number" 2 line

let () =
  Alcotest.run "ledger"
    [
      ( "caps",
        [
          Alcotest.test_case "refuses past the cap" `Quick test_refuses_past_cap;
          Alcotest.test_case "refusal cases" `Quick test_refusal_cases;
          Alcotest.test_case "settle" `Quick test_settle;
          Alcotest.test_case "unsettled stays charged" `Quick
            test_unsettled_stays_charged;
          Alcotest.test_case "1000 fibres never exceed the cap" `Quick
            test_concurrent_never_exceeds;
        ] );
      ( "file",
        [
          Alcotest.test_case "reload spent totals" `Quick test_reload;
          Alcotest.test_case "reload capture-script lines" `Quick
            test_reload_legacy;
          Alcotest.test_case "spend by owner" `Quick test_spent_by_owner;
          Alcotest.test_case "corrupt line" `Quick test_corrupt;
        ] );
    ]
