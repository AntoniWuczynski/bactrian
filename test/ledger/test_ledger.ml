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
  | Error (Ledger.Cap_reached _ | Ledger.Unknown_model _ | Ledger.Unreadable _)
    ->
      Alcotest.failf "%s: unexpectedly refused" what

let is_refused = function
  | Error (Ledger.Cap_reached _) -> true
  | Ok _ | Error (Ledger.Unknown_model _ | Ledger.Unreadable _) -> false

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
  | Ok _ | Error (Ledger.Unknown_model _ | Ledger.Unreadable _) ->
      Alcotest.fail "expected refusal");
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
         | Error
             ( Ledger.Cap_reached _ | Ledger.Unknown_model _
             | Ledger.Unreadable _ ) ->
             ()));
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

(* The ids of every reservation in the file, in order. *)
let reserve_ids dir =
  Eio.Path.load Eio.Path.(dir / "ledger.jsonl")
  |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
      match Bactrian_core.Canon.of_string line with
      | Error _ -> None
      | Ok v -> (
          match Bactrian_core.Canon.str_member "event" v with
          | Some "reserve" -> Bactrian_core.Canon.int_member "id" v
          | Some _ | None -> None))

let check_unique what ids =
  Alcotest.(check int)
    what (List.length ids)
    (List.length (List.sort_uniq Int.compare ids))

(* Two ledgers on one file, as two processes would hold them: each sees what
   the other appended before it reserves. *)
let test_two_handles () =
  with_tmp "two-handles" @@ fun dir ->
  let a = mk dir () and b = mk dir () in
  let ra = granted "a" (reserve a 0.4) in
  let rb = granted "b sees a's 0.4" (reserve b 0.4) in
  Alcotest.(check bool) "a sees b's 0.4" true (is_refused (reserve a 0.3));
  Ledger.settle b rb ~actual:(Some 0.1);
  ignore (granted "a sees b's settlement" (reserve a 0.3));
  Ledger.settle a ra ~actual:None;
  check_usd "a's total" 0.8 (Ledger.spent a ~provider:"openai");
  check_usd "b's total once it reads again" 0.8
    (match reserve b 0.3 with
    | Ok _ -> Alcotest.fail "b exceeded the cap"
    | Error (Ledger.Cap_reached { spent; _ }) -> spent
    | Error (Ledger.Unknown_model _ | Ledger.Unreadable _) ->
        Alcotest.fail "unexpected refusal");
  check_unique "no id is used twice" (reserve_ids dir);
  check_usd "a reload agrees" 0.8 (Ledger.spent (mk dir ()) ~provider:"openai")

(* The same across processes: two workers reserve 0.25 at a time against a cap
   of 25 in one file, 80 times each. Without the lock each would see only its
   own spend and both would get all 80. *)
let test_two_processes () =
  with_tmp "two-processes" @@ fun dir ->
  let file = Eio.Path.native_exn Eio.Path.(dir / "ledger.jsonl") in
  let start owner =
    let stdin_r, stdin_w = Unix.pipe ~cloexec:true () in
    let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
    let pid =
      Unix.create_process "./ledger_worker.exe"
        [| "./ledger_worker.exe"; file; "25"; "0.25"; "80"; owner |]
        stdin_r stdout_w Unix.stderr
    in
    Unix.close stdin_r;
    Unix.close stdout_w;
    (pid, stdin_w, stdout_r)
  in
  let workers = [ start "first"; start "second" ] in
  List.iter (fun (_, w, _) -> Unix.close w) workers;
  let granted =
    List.map
      (fun (pid, _, r) ->
        let ic = Unix.in_channel_of_descr r in
        let line = In_channel.input_line ic in
        In_channel.close ic;
        let rec reap () =
          try Unix.waitpid [] pid
          with Unix.Unix_error (EINTR, _, _) -> reap ()
        in
        (match reap () with
        | _, WEXITED 0 -> ()
        | _, (WEXITED _ | WSIGNALED _ | WSTOPPED _) ->
            Alcotest.fail "a worker failed");
        Option.bind line int_of_string_opt |> Option.value ~default:(-1))
      workers
  in
  Alcotest.(check int)
    "the cap's worth in all" 100
    (List.fold_left ( + ) 0 granted);
  let ids = reserve_ids dir in
  Alcotest.(check int) "one line per grant" 100 (List.length ids);
  check_unique "no id is used twice" ids;
  check_usd "a reload agrees" 25.
    (Ledger.spent (mk dir ~caps:[ ("openai", 25.) ] ()) ~provider:"openai")

(* A line another process wrote that cannot be read stops further spending. *)
let test_unreadable_tail () =
  with_tmp "unreadable" @@ fun dir ->
  let l = mk dir () in
  ignore (granted "before" (reserve l 0.1));
  Eio.Path.save ~append:true ~create:(`If_missing 0o600)
    Eio.Path.(dir / "ledger.jsonl")
    "not json\n";
  match reserve l 0.1 with
  | Error (Ledger.Unreadable { line; _ }) ->
      Alcotest.(check int) "the line" 2 line
  | Ok _ | Error (Ledger.Cap_reached _ | Ledger.Unknown_model _) ->
      Alcotest.fail "expected the reservation to be refused"

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
          Alcotest.test_case "two handles on one file" `Quick test_two_handles;
          Alcotest.test_case "two processes on one file" `Quick
            test_two_processes;
          Alcotest.test_case "a line another process broke" `Quick
            test_unreadable_tail;
        ] );
    ]
