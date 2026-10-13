open Bactrian_core
module P = Pricing

let usage ?cached ?write ~i ~o () : Trace.usage =
  {
    input_tokens = i;
    output_tokens = o;
    cached_input_tokens = cached;
    cache_write_tokens = write;
    reasoning_tokens = None;
  }

let close =
  Alcotest.testable
    (fun ppf f -> Fmt.pf ppf "%.12g" f)
    (fun a b -> Float.abs (a -. b) < 1e-9)

let cost_t = Alcotest.option close
let today = "2026-10-13"
let cost ?(date = today) model u = P.cost ~model ~date u
let per_m = 1e-6

let test_table_sane () =
  List.iter
    (fun (e : P.entry) ->
      Alcotest.(check bool) (e.model ^ " max_output") true (e.max_output > 0);
      Alcotest.(check int)
        (e.model ^ " date length") 10
        (String.length e.effective_from);
      let ok (r : P.rates) =
        r.input >= 0. && r.cached_input >= 0. && r.cache_write >= 0.
        && r.output >= 0.
      in
      Alcotest.(check bool) (e.model ^ " rates") true (ok e.rates);
      match e.long_context with
      | None -> ()
      | Some (n, r) ->
          Alcotest.(check bool)
            (e.model ^ " long ctx") true
            (n > 0 && ok r && r.input >= e.rates.input))
    P.table;
  List.iter
    (fun m ->
      Alcotest.(check bool)
        (m ^ " listed") true
        (List.exists (fun (e : P.entry) -> String.equal e.model m) P.table))
    [
      "gpt-6-luna";
      "gpt-5-nano";
      "gpt-6-sol";
      "gpt-6-astra";
      "claude-haiku-5-5";
      "claude-sonnet-5-5";
      "claude-opus-5-5";
      "claude-fable-5-1";
      "gemini-3.8-flash";
      "gemini-3.5-flash-lite";
      "gemini-3.1-pro-preview";
    ]

let test_basic_costs () =
  (* 1M in, 1M out *)
  let u = usage ~i:1_000_000 ~o:1_000_000 () in
  Alcotest.check cost_t "luna" (Some 0.60) (cost "gpt-6-luna" u);
  Alcotest.check cost_t "nano" (Some 0.45) (cost "gpt-5-nano" u);
  Alcotest.check cost_t "sol" (Some 12.) (cost "gpt-6-sol" u);
  Alcotest.check cost_t "astra" (Some 60.) (cost "gpt-6-astra" u);
  Alcotest.check cost_t "sonnet" (Some 12.) (cost "claude-sonnet-5-5" u);
  Alcotest.check cost_t "opus" (Some 24.) (cost "claude-opus-5-5" u);
  Alcotest.check cost_t "fable" (Some 60.) (cost "claude-fable-5-1" u);
  Alcotest.check cost_t "haiku" (Some 0.06)
    (cost "claude-haiku-5-5" (usage ~i:100_000 ~o:100_000 ()));
  Alcotest.check cost_t "flash lite" (Some 2.80)
    (cost "gemini-3.5-flash-lite" u);
  Alcotest.check cost_t "zero usage" (Some 0.)
    (cost "gpt-6-luna" (usage ~i:0 ~o:0 ()))

let test_cache_split () =
  (* luna: 0.10 in, 0.01 cached, 0.125 write, 0.50 out; 1000 prompt = 600 plain + 300 cached + 100 write *)
  let u = usage ~cached:300 ~write:100 ~i:1000 ~o:200 () in
  let expected =
    ((600. *. 0.10) +. (300. *. 0.01) +. (100. *. 0.125) +. (200. *. 0.50))
    *. per_m
  in
  Alcotest.check cost_t "luna" (Some expected) (cost "gpt-6-luna" u);
  (* only cached *)
  let u2 = usage ~cached:400 ~i:1000 ~o:0 () in
  Alcotest.check cost_t "cached only"
    (Some (((600. *. 0.10) +. (400. *. 0.01)) *. per_m))
    (cost "gpt-6-luna" u2)

let test_anthropic_cache_write () =
  let u = usage ~write:1_000_000 ~i:1_000_000 ~o:0 () in
  Alcotest.check cost_t "sonnet write 2.50" (Some 2.5)
    (cost "claude-sonnet-5-5" u);
  Alcotest.check cost_t "opus write 5" (Some 5.) (cost "claude-opus-5-5" u);
  Alcotest.check cost_t "fable write 12.50" (Some 12.5)
    (cost "claude-fable-5-1" u);
  let r = usage ~cached:1_000_000 ~i:1_000_000 ~o:0 () in
  Alcotest.check cost_t "sonnet read 0.10" (Some 0.1)
    (cost "claude-sonnet-5-5" r);
  Alcotest.check cost_t "fable read 0.25" (Some 0.25)
    (cost "claude-fable-5-1" r)

let test_haiku_long_context () =
  (* at the threshold: still short rates *)
  Alcotest.check cost_t "100000 short"
    (Some (((100_000. *. 0.10) +. (1000. *. 0.50)) *. per_m))
    (cost "claude-haiku-5-5" (usage ~i:100_000 ~o:1000 ()));
  (* above: every rate is 5x, including output *)
  Alcotest.check cost_t "100001 long"
    (Some (((100_001. *. 0.50) +. (1000. *. 2.50)) *. per_m))
    (cost "claude-haiku-5-5" (usage ~i:100_001 ~o:1000 ()));
  (* cache reads and writes count toward the threshold, and use the long cache rates *)
  Alcotest.check cost_t "cache in long"
    (Some
       (((20_000. *. 0.50) +. (60_000. *. 0.05) +. (30_000. *. 0.625)
       +. (10. *. 2.50))
       *. per_m))
    (cost "claude-haiku-5-5"
       (usage ~cached:60_000 ~write:30_000 ~i:110_000 ~o:10 ()))

let test_gemini_pro_tier () =
  Alcotest.check cost_t "short"
    (Some (((200_000. *. 2.00) +. (1000. *. 12.00)) *. per_m))
    (cost "gemini-3.1-pro-preview" (usage ~i:200_000 ~o:1000 ()));
  Alcotest.check cost_t "long"
    (Some (((200_001. *. 4.00) +. (1000. *. 18.00)) *. per_m))
    (cost "gemini-3.1-pro-preview" (usage ~i:200_001 ~o:1000 ()));
  Alcotest.check cost_t "long cached"
    (Some (((100_000. *. 4.00) +. (150_000. *. 0.40)) *. per_m))
    (cost "gemini-3.1-pro-preview" (usage ~cached:150_000 ~i:250_000 ~o:0 ()))

let test_gemini_2027 () =
  let u = usage ~cached:1_000_000 ~i:2_000_000 ~o:1_000_000 () in
  let before =
    ((1_000_000. *. 0.75) +. (1_000_000. *. 0.075) +. (1_000_000. *. 3.75))
    *. per_m
  in
  let after =
    ((1_000_000. *. 1.50) +. (1_000_000. *. 0.15) +. (1_000_000. *. 7.50))
    *. per_m
  in
  let c d = cost ~date:d "gemini-3.8-flash" u in
  Alcotest.check cost_t "2026-12-31" (Some before) (c "2026-12-31");
  Alcotest.check cost_t "2027-01-01" (Some after) (c "2027-01-01");
  Alcotest.check cost_t "2027-06-01" (Some after) (c "2027-06-01");
  Alcotest.check cost_t "2026-10-13" (Some before) (c "2026-10-13");
  Alcotest.check cost_t "dated id too" (Some after)
    (cost ~date:"2027-02-01" "gemini-3.8-flash-001" u)

let test_openrouter_free () =
  let u = usage ~cached:5 ~write:5 ~i:1_000_000 ~o:1_000_000 () in
  Alcotest.check cost_t "free" (Some 0.)
    (cost "nvidia/nemotron-3-super-120b-a12b:free" u);
  Alcotest.check cost_t "free any" (Some 0.) (cost "someone/new-model:free" u);
  Alcotest.check cost_t "not free" None
    (cost "nvidia/nemotron-3-super-120b-a12b" u);
  Alcotest.check cost_t "free in middle" None (cost "a/b:free-ish" u);
  match P.find ~model:"x/y:free" ~date:today with
  | Some e -> Alcotest.(check string) "provider" "openrouter" e.provider
  | None -> Alcotest.fail "free model not found"

let test_find () =
  let model = function Some (e : P.entry) -> Some e.model | None -> None in
  let s = Alcotest.(option string) in
  Alcotest.check s "exact" (Some "gpt-5-nano")
    (model (P.find ~model:"gpt-5-nano" ~date:today));
  Alcotest.check s "dated" (Some "gpt-5-nano")
    (model (P.find ~model:"gpt-5-nano-2025-08-07" ~date:today));
  Alcotest.check s "claude dated" (Some "claude-haiku-5-5")
    (model (P.find ~model:"claude-haiku-5-5-20261001" ~date:today));
  Alcotest.check s "unknown" None (model (P.find ~model:"gpt-9" ~date:today));
  Alcotest.check s "empty" None (model (P.find ~model:"" ~date:today));
  Alcotest.check s "sibling not a prefix" None
    (model (P.find ~model:"gpt-5-nanox" ~date:today));
  Alcotest.check s "shorter than entry" None
    (model (P.find ~model:"gpt-5" ~date:today));
  Alcotest.check s "provider-qualified is unknown" None
    (model (P.find ~model:"openai/gpt-5-nano" ~date:today));
  Alcotest.check s "before any entry" None
    (model (P.find ~model:"gpt-5-nano" ~date:"1960-01-01"))

let test_find_picks_latest () =
  let rate d =
    Option.map
      (fun (e : P.entry) -> e.rates.input)
      (P.find ~model:"gemini-3.8-flash" ~date:d)
  in
  Alcotest.(check (option (float 1e-9)))
    "before" (Some 0.75) (rate "2026-12-31");
  Alcotest.(check (option (float 1e-9))) "after" (Some 1.50) (rate "2027-01-01")

let test_unknown_cost () =
  Alcotest.check cost_t "unknown" None
    (cost "mystery-model" (usage ~i:1 ~o:1 ()))

let test_worst_case () =
  let wc ?(date = today) model ~bytes ~max_output =
    P.worst_case ~model ~date ~input_bytes:bytes ~max_output
  in
  (* 3_000_000 bytes -> 1M tokens; luna: 0.10 + 1000 * 0.50 / 1e6 *)
  Alcotest.check cost_t "luna explicit"
    (Some (0.10 +. (1000. *. 0.50 /. 1e6)))
    (wc "gpt-6-luna" ~bytes:3_000_000 ~max_output:(Some 1000));
  (* default output = model maximum *)
  (match P.find ~model:"gpt-6-luna" ~date:today with
  | None -> Alcotest.fail "luna missing"
  | Some e ->
      Alcotest.check cost_t "luna default max"
        (Some (0.10 +. (float_of_int e.max_output *. 0.50 /. 1e6)))
        (wc "gpt-6-luna" ~bytes:3_000_000 ~max_output:None));
  (* Claude 5.x default maximum is 128000 *)
  Alcotest.check cost_t "sonnet default"
    (Some ((2. *. 0.3) +. (128_000. *. 10. /. 1e6)))
    (wc "claude-sonnet-5-5" ~bytes:900_000 ~max_output:None);
  (* long context applies on the estimate *)
  Alcotest.check cost_t "haiku long"
    (Some ((300_003. /. 3. *. 0.50 /. 1e6) +. (1000. *. 2.50 /. 1e6)))
    (wc "claude-haiku-5-5" ~bytes:300_003 ~max_output:(Some 1000));
  Alcotest.check cost_t "haiku at threshold is short"
    (Some ((100_000. *. 0.10 /. 1e6) +. (1000. *. 0.50 /. 1e6)))
    (wc "claude-haiku-5-5" ~bytes:300_000 ~max_output:(Some 1000));
  Alcotest.check cost_t "free" (Some 0.)
    (wc "x/y:free" ~bytes:1_000_000 ~max_output:None);
  Alcotest.check cost_t "unknown refused" None
    (wc "mystery" ~bytes:1 ~max_output:None);
  Alcotest.check cost_t "gemini 2027"
    (Some ((1.50 *. 1.) +. (1000. *. 7.50 /. 1e6)))
    (wc ~date:"2027-03-01" "gemini-3.8-flash" ~bytes:3_000_000
       ~max_output:(Some 1000))

let test_worst_case_overestimates () =
  (* A real token is at least ~1 byte per token for plain text, so bytes/3 should never be below
     the cost of a prompt of bytes/4 tokens. Property: the reservation covers any actual usage
     whose input tokens do not exceed bytes/3 and whose output is within max_output. *)
  let prop =
    QCheck.Test.make ~name:"reservation covers actual" ~count:300
      QCheck.(
        triple (int_range 0 2_000_000) (int_range 0 5000) (int_range 0 5000))
      (fun (bytes, o, extra) ->
        let max_output = o + extra in
        let tokens = bytes / 3 in
        List.for_all
          (fun (e : P.entry) ->
            match
              ( P.worst_case ~model:e.model ~date:today ~input_bytes:bytes
                  ~max_output:(Some max_output),
                P.cost ~model:e.model ~date:today (usage ~i:tokens ~o ()) )
            with
            | Some w, Some c -> w +. 1e-12 >= c
            | None, _ | _, None -> false)
          P.table)
  in
  QCheck_alcotest.to_alcotest prop |> fun (name, speed, f) ->
  Alcotest.test_case name speed f

let test_monotonic () =
  let prop =
    QCheck.Test.make ~name:"cost monotonic in output" ~count:200
      QCheck.(pair (int_range 0 1_000_000) (int_range 0 1_000_000))
      (fun (a, b) ->
        let lo = min a b and hi = max a b in
        List.for_all
          (fun (e : P.entry) ->
            match
              ( P.cost ~model:e.model ~date:today (usage ~i:1000 ~o:lo ()),
                P.cost ~model:e.model ~date:today (usage ~i:1000 ~o:hi ()) )
            with
            | Some x, Some y -> x <= y
            | None, _ | _, None -> false)
          P.table)
  in
  QCheck_alcotest.to_alcotest prop |> fun (name, speed, f) ->
  Alcotest.test_case name speed f

let () =
  Alcotest.run "pricing"
    [
      ( "table",
        [
          Alcotest.test_case "sane" `Quick test_table_sane;
          Alcotest.test_case "find" `Quick test_find;
          Alcotest.test_case "latest effective entry" `Quick
            test_find_picks_latest;
        ] );
      ( "cost",
        [
          Alcotest.test_case "basic" `Quick test_basic_costs;
          Alcotest.test_case "cache split" `Quick test_cache_split;
          Alcotest.test_case "anthropic cache rates" `Quick
            test_anthropic_cache_write;
          Alcotest.test_case "haiku long context" `Quick test_haiku_long_context;
          Alcotest.test_case "gemini pro tier" `Quick test_gemini_pro_tier;
          Alcotest.test_case "gemini 2027 change" `Quick test_gemini_2027;
          Alcotest.test_case "openrouter free" `Quick test_openrouter_free;
          Alcotest.test_case "unknown model" `Quick test_unknown_cost;
          test_monotonic ();
        ] );
      ( "worst_case",
        [
          Alcotest.test_case "reservations" `Quick test_worst_case;
          test_worst_case_overestimates ();
        ] );
    ]
