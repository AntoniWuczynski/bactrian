open Bactrian_core
module M = Matcher

let doc s =
  match Canon.of_string s with
  | Ok v -> v
  | Error _ -> Alcotest.fail "bad canon"

let body ~stream =
  if stream then Cassette.Stream [ (0, "a") ] else Cassette.Whole (Inline "ok")

let ex ?(channel = Cassette.Model) ?(occ = 0) ?(stream = false) ?canon seq key =
  let canonical =
    match canon with
    | Some c -> c
    | None -> doc (Printf.sprintf {|{"id":"%s","x":"1","y":"2"}|} key)
  in
  {
    Cassette.seq;
    t_ms = seq;
    channel;
    protocol = "p";
    meth = "POST";
    path = "/";
    query = [];
    key;
    occurrence = occ;
    canonical;
    request = Inline "{}";
    outcome = Response { status = 200; headers = []; body = body ~stream };
  }

let req ?(shape = M.Whole_body) ?canon channel key =
  let canonical =
    match canon with
    | Some c -> c
    | None -> doc (Printf.sprintf {|{"id":"%s","x":"1","y":"2"}|} key)
  in
  { M.channel; key; canonical; shape }

let div_tag = function
  | M.Missing_request _ -> "missing"
  | M.Unconsumed _ -> "unconsumed"
  | M.Reordered _ -> "reordered"
  | M.Shape_mismatch _ -> "shape"

let tag = function
  | M.Serve _ -> "serve"
  | M.Go_live -> "live"
  | M.Refuse d -> "refuse:" ^ div_tag d

let tags fs = List.map div_tag fs

let seq_of = function
  | M.Serve { exchange; _ } -> exchange.Cassette.seq
  | M.Go_live | M.Refuse _ -> -1

let sim_of = function
  | M.Serve { similarity; _ } -> similarity
  | M.Go_live | M.Refuse _ -> None

let is_refuse d =
  String.length (tag d) > 6 && String.equal (String.sub (tag d) 0 6) "refuse"

let nearest_of = function
  | M.Refuse (M.Missing_request { nearest; _ }) -> nearest
  | M.Refuse (M.Unconsumed _ | M.Reordered _ | M.Shape_mismatch _)
  | M.Serve _ | M.Go_live ->
      None

let expected_of = function
  | M.Refuse (M.Reordered { expected; _ }) -> Some expected.Cassette.seq
  | M.Refuse (M.Missing_request _ | M.Unconsumed _ | M.Shape_mismatch _)
  | M.Serve _ | M.Go_live ->
      None

let recorded_of = function
  | M.Refuse (M.Shape_mismatch { recorded; _ }) -> Some recorded.Cassette.seq
  | M.Refuse (M.Missing_request _ | M.Unconsumed _ | M.Reordered _)
  | M.Serve _ | M.Go_live ->
      None

let unconsumed_seqs fs =
  List.filter_map
    (function
      | M.Unconsumed e -> Some e.Cassette.seq
      | M.Missing_request _ | M.Reordered _ | M.Shape_mismatch _ -> None)
    fs

let only = function [ x ] -> x | _ -> Alcotest.fail "expected one result"

let run st requests =
  List.fold_left
    (fun (st, acc) r ->
      let st, d, f = M.decide st r in
      (st, (d, f) :: acc))
    (st, []) requests
  |> fun (st, acc) -> (st, List.rev acc)

let start ?(allow_reorder = false) mode exs = M.start ~mode ~allow_reorder exs

let test_key () =
  let c = doc {|{"a":1}|} in
  let k = M.key ~meth:"POST" ~path:"/p" ~query:[ ("a", "1") ] c in
  Alcotest.(check int) "hex" 64 (String.length k);
  Alcotest.(check string)
    "stable" k
    (M.key ~meth:"POST" ~path:"/p" ~query:[ ("a", "1") ] c);
  let differs name k' = Alcotest.(check bool) name false (String.equal k k') in
  differs "meth" (M.key ~meth:"GET" ~path:"/p" ~query:[ ("a", "1") ] c);
  differs "path" (M.key ~meth:"POST" ~path:"/q" ~query:[ ("a", "1") ] c);
  differs "query" (M.key ~meth:"POST" ~path:"/p" ~query:[ ("a", "2") ] c);
  Alcotest.(check string)
    "query order does not matter"
    (M.key ~meth:"POST" ~path:"/p" ~query:[ ("a", "1"); ("b", "2") ] c)
    (M.key ~meth:"POST" ~path:"/p" ~query:[ ("b", "2"); ("a", "1") ] c);
  Alcotest.(check string)
    "repeated names are sorted by value"
    (M.key ~meth:"POST" ~path:"/p" ~query:[ ("a", "1"); ("a", "2") ] c)
    (M.key ~meth:"POST" ~path:"/p" ~query:[ ("a", "2"); ("a", "1") ] c);
  differs "repeated names keep their values"
    (M.key ~meth:"POST" ~path:"/p" ~query:[ ("a", "1"); ("a", "1") ] c);
  differs "body"
    (M.key ~meth:"POST" ~path:"/p" ~query:[ ("a", "1") ] (doc {|{"a":2}|}))

let test_key_drops_api_key () =
  let c = doc {|{"a":1}|} in
  let key query = M.key ~meth:"POST" ~path:"/p" ~query c in
  let base = key [ ("alt", "sse") ] in
  Alcotest.(check string)
    "key parameter ignored" base
    (key [ ("key", "<secret:1>"); ("alt", "sse") ]);
  Alcotest.(check string)
    "whatever its value" base
    (key [ ("alt", "sse"); ("key", "other") ]);
  Alcotest.(check string)
    "every occurrence" base
    (key [ ("key", "a"); ("alt", "sse"); ("key", "b") ]);
  Alcotest.(check bool)
    "case-sensitive" false
    (String.equal base (key [ ("alt", "sse"); ("Key", "x") ]));
  Alcotest.(check bool)
    "other names kept" false
    (String.equal base (key [ ("alt", "sse"); ("keys", "x") ]))

let test_key_headers () =
  let c = doc {|{"a":1}|} in
  let key ?headers () = M.key ?headers ~meth:"POST" ~path:"/p" ~query:[] c in
  let base = key () in
  let same name k = Alcotest.(check string) name base k in
  let differs name k = Alcotest.(check bool) name false (String.equal base k) in
  same "empty list" (key ~headers:[] ());
  same "irrelevant headers"
    (key
       ~headers:[ ("content-type", "application/json"); ("x-request-id", "1") ]
       ());
  differs "anthropic-beta" (key ~headers:[ ("anthropic-beta", "b1") ] ());
  differs "openai-beta" (key ~headers:[ ("openai-beta", "assistants=v2") ] ());
  differs "anthropic-version"
    (key ~headers:[ ("anthropic-version", "2023-06-01") ] ());
  Alcotest.(check string)
    "names are case-insensitive"
    (key ~headers:[ ("anthropic-beta", "b1") ] ())
    (key ~headers:[ ("Anthropic-Beta", "b1") ] ());
  Alcotest.(check bool)
    "values are case-sensitive" false
    (String.equal
       (key ~headers:[ ("anthropic-beta", "b1") ] ())
       (key ~headers:[ ("anthropic-beta", "B1") ] ()));
  Alcotest.(check string)
    "order does not matter"
    (key ~headers:[ ("anthropic-beta", "b"); ("anthropic-version", "v") ] ())
    (key ~headers:[ ("Anthropic-Version", "v"); ("anthropic-beta", "b") ] ());
  same "irrelevant next to none"
    (key ~headers:[ ("Authorization", "Bearer x") ] ())

let test_strict_in_order () =
  let exs =
    [ ex 0 "a"; ex 1 "b"; ex ~channel:Mcp 2 "t"; ex ~channel:Http 3 "h" ]
  in
  let st, rs =
    run (start M.Strict exs)
      [ req Model "a"; req Mcp "t"; req Model "b"; req Http "h" ]
  in
  Alcotest.(check (list int))
    "served" [ 0; 2; 1; 3 ]
    (List.map (fun (d, _) -> seq_of d) rs);
  Alcotest.(check bool)
    "no findings" true
    (List.for_all (fun (_, f) -> f = []) rs);
  Alcotest.(check int) "finish" 0 (List.length (M.finish st))

let test_occurrence () =
  let exs = [ ex 0 "a" ~occ:0; ex 1 "a" ~occ:1 ] in
  let _, rs = run (start M.Strict exs) [ req Model "a"; req Model "a" ] in
  Alcotest.(check (list int))
    "first then second" [ 0; 1 ]
    (List.map (fun (d, _) -> seq_of d) rs);
  let _, rs =
    run (start M.Strict exs) [ req Model "a"; req Model "a"; req Model "a" ]
  in
  Alcotest.(check (list string))
    "third misses"
    [ "serve"; "serve"; "refuse:missing" ]
    (List.map (fun (d, _) -> tag d) rs)

let test_reordered_model () =
  let exs = [ ex 0 "a"; ex 1 "b" ] in
  let _, rs = run (start M.Strict exs) [ req Model "b" ] in
  let d, f = only rs in
  Alcotest.(check string) "refuse" "refuse:reordered" (tag d);
  Alcotest.(check (option int)) "expected" (Some 0) (expected_of d);
  Alcotest.(check (list string)) "no findings" [] (tags f);
  let st, rs = run (start ~allow_reorder:true M.Strict exs) [ req Model "b" ] in
  let d, f = only rs in
  Alcotest.(check int) "served" 1 (seq_of d);
  Alcotest.(check (option (float 0.))) "no sim" None (sim_of d);
  Alcotest.(check (list string)) "finding" [ "reordered" ] (tags f);
  (* the expected exchange is still pending and now the lowest *)
  let _, rs = run st [ req Model "a" ] in
  Alcotest.(check (list int))
    "a still served" [ 0 ]
    (List.map (fun (d, _) -> seq_of d) rs);
  (* Http is ordered too *)
  let _, rs =
    run
      (start M.Strict [ ex ~channel:Http 0 "a"; ex ~channel:Http 1 "b" ])
      [ req Http "b" ]
  in
  Alcotest.(check bool) "http refuses" true (is_refuse (fst (List.hd rs)))

let test_reordered_and_shape () =
  let exs = [ ex 0 "a"; ex ~stream:true 1 "b" ] in
  let request = req ~shape:M.Whole_body Model "b" in
  let _, rs = run (start M.Strict exs) [ request ] in
  let d, f = only rs in
  Alcotest.(check string) "refused as reordered" "refuse:reordered" (tag d);
  Alcotest.(check (list string)) "shape reported too" [ "shape" ] (tags f);
  let _, rs = run (start M.Strict exs) [ req ~shape:M.Streamed Model "b" ] in
  let d, f = only rs in
  Alcotest.(check string) "right shape" "refuse:reordered" (tag d);
  Alcotest.(check (list string)) "no shape finding" [] (tags f);
  let _, rs = run (start M.Until_divergence exs) [ request ] in
  let d, f = only rs in
  Alcotest.(check string) "lenient refuses on shape" "refuse:shape" (tag d);
  Alcotest.(check (list string))
    "and reports the order" [ "reordered" ] (tags f)

let test_mcp_unordered () =
  let exs =
    [ ex ~channel:Mcp 0 "a"; ex ~channel:Mcp 1 "b"; ex ~channel:Mcp 2 "c" ]
  in
  let st, rs =
    run (start M.Strict exs) [ req Mcp "c"; req Mcp "a"; req Mcp "b" ]
  in
  Alcotest.(check (list int))
    "served" [ 2; 0; 1 ]
    (List.map (fun (d, _) -> seq_of d) rs);
  Alcotest.(check bool)
    "no findings" true
    (List.for_all (fun (_, f) -> f = []) rs);
  Alcotest.(check int) "finish" 0 (List.length (M.finish st))

let test_channels_independent () =
  (* same key on two channels *)
  let exs = [ ex 0 "a"; ex ~channel:Mcp 1 "a" ] in
  let _, rs = run (start M.Strict exs) [ req Mcp "a"; req Model "a" ] in
  Alcotest.(check (list int))
    "served" [ 1; 0 ]
    (List.map (fun (d, _) -> seq_of d) rs)

let test_shape () =
  let exs = [ ex ~stream:true 0 "a" ] in
  let st, rs = run (start M.Strict exs) [ req ~shape:M.Whole_body Model "a" ] in
  let d, _ = only rs in
  Alcotest.(check string) "tag" "refuse:shape" (tag d);
  Alcotest.(check (option int)) "recorded" (Some 0) (recorded_of d);
  Alcotest.(check int) "still unconsumed" 1 (List.length (M.finish st));
  let _, rs = run (start M.Strict exs) [ req ~shape:M.Streamed Model "a" ] in
  Alcotest.(check int) "matching shape" 0 (seq_of (fst (List.hd rs)))

let test_miss_nearest () =
  let near = doc {|{"a":"1","b":"2","c":"3"}|} in
  let far = doc {|{"a":"9","b":"8","c":"7"}|} in
  let asked = doc {|{"a":"1","b":"2","c":"X"}|} in
  let exs =
    [
      ex ~canon:far 0 "far";
      ex ~canon:near 1 "near";
      ex ~channel:Mcp ~canon:asked 2 "mcp-exact-content";
    ]
  in
  let _, rs = run (start M.Strict exs) [ req ~canon:asked Model "new" ] in
  let d, f = only rs in
  Alcotest.(check string) "tag" "refuse:missing" (tag d);
  Alcotest.(check (list string)) "no findings" [] (tags f);
  match nearest_of d with
  | Some (e, changes, sim) ->
      Alcotest.(check int) "same channel nearest" 1 e.Cassette.seq;
      Alcotest.(check bool) "diff" true (changes <> []);
      Alcotest.(check bool) "sim in (0,1)" true (sim > 0. && sim < 1.)
  | None -> Alcotest.fail "expected nearest"

let test_miss_no_candidate () =
  let _, rs =
    run (start M.Strict [ ex ~channel:Mcp 0 "a" ]) [ req Model "z" ]
  in
  let d, _ = only rs in
  Alcotest.(check string) "tag" "refuse:missing" (tag d);
  Alcotest.(check bool) "no nearest" true (Option.is_none (nearest_of d))

let test_fuzzy () =
  let rec_c = doc {|{"a":"1","b":"2","c":"3","d":"4"}|} in
  let asked = doc {|{"a":"1","b":"2","c":"3","d":"X"}|} in
  let exs = [ ex ~canon:rec_c 0 "r" ] in
  let _, rs = run (start (M.Fuzzy 0.7) exs) [ req ~canon:asked Model "q" ] in
  let d, f = only rs in
  Alcotest.(check int) "served" 0 (seq_of d);
  Alcotest.(check (option (float 1e-9))) "score" (Some 0.75) (sim_of d);
  Alcotest.(check (list string)) "no findings" [] (tags f);
  let st, rs = run (start (M.Fuzzy 0.8) exs) [ req ~canon:asked Model "q" ] in
  let d, _ = only rs in
  Alcotest.(check string) "tag" "refuse:missing" (tag d);
  Alcotest.(check bool) "nearest" true (Option.is_some (nearest_of d));
  Alcotest.(check int) "unconsumed" 1 (List.length (M.finish st));
  (* exact matches are served without a score *)
  let _, rs = run (start (M.Fuzzy 0.7) exs) [ req ~canon:rec_c Model "r" ] in
  let d, _ = only rs in
  Alcotest.(check string) "tag" "serve" (tag d);
  Alcotest.(check (option (float 0.))) "no sim" None (sim_of d)

let test_fuzzy_threshold_boundary () =
  let rec_c = doc {|{"a":"1","b":"2","c":"3","d":"4"}|} in
  let asked = doc {|{"a":"1","b":"2","c":"3","d":"X"}|} in
  let exs = [ ex ~canon:rec_c 0 "r" ] in
  let go threshold =
    fst
      (only
         (snd
            (run (start (M.Fuzzy threshold) exs) [ req ~canon:asked Model "q" ])))
  in
  Alcotest.(check string) "equal serves" "serve" (tag (go 0.75));
  Alcotest.(check string)
    "just above refuses" "refuse:missing"
    (tag (go (Float.succ 0.75)))

let test_nearest_tie () =
  let asked = doc {|{"a":"1","b":"2"}|} in
  let exs =
    [
      ex ~canon:(doc {|{"a":"1","b":"X"}|}) 5 "late";
      ex ~canon:(doc {|{"a":"X","b":"2"}|}) 3 "early";
    ]
  in
  let nearest_seq st =
    let _, rs = run st [ req ~canon:asked Model "q" ] in
    match nearest_of (fst (only rs)) with
    | Some (e, _, _) -> e.Cassette.seq
    | None -> Alcotest.fail "expected nearest"
  in
  Alcotest.(check int) "strict" 3 (nearest_seq (start M.Strict exs));
  let _, rs = run (start (M.Fuzzy 0.5) exs) [ req ~canon:asked Model "q" ] in
  Alcotest.(check int) "fuzzy serves" 3 (seq_of (fst (only rs)))

let test_transport_error_shape () =
  let e = { (ex 0 "a") with outcome = Transport_error "refused" } in
  List.iter
    (fun shape ->
      let _, rs = run (start M.Strict [ e ]) [ req ~shape Model "a" ] in
      Alcotest.(check string) "served" "serve" (tag (fst (only rs))))
    [ M.Streamed; M.Whole_body ]

(* The nearest search looks at the first 64 unconsumed exchanges only. *)
let test_nearest_window () =
  let far i = doc (Printf.sprintf {|{"a":"%d","b":"%d","c":"%d"}|} i i i) in
  let close = doc {|{"a":"1","b":"2","c":"X"}|} in
  let asked = doc {|{"a":"1","b":"2","c":"3"}|} in
  let build close_at =
    List.init 100 (fun i ->
        ex
          ~canon:(if i = close_at then close else far (i + 10))
          i (Printf.sprintf "k%d" i))
  in
  let nearest_seq exs =
    let _, rs = run (start M.Strict exs) [ req ~canon:asked Model "q" ] in
    Option.map (fun (e, _, _) -> e.Cassette.seq) (nearest_of (fst (only rs)))
  in
  Alcotest.(check (option int))
    "last in window" (Some 63)
    (nearest_seq (build 63));
  Alcotest.(check bool)
    "first outside the window" true
    (match nearest_seq (build 64) with
    | Some 64 -> false
    | Some _ | None -> true);
  (* consumed exchanges do not count towards the window *)
  let exs = build 80 in
  let st, _ =
    run (start M.Strict exs)
      (List.init 20 (fun i -> req Model (Printf.sprintf "k%d" i)))
  in
  let _, rs = run st [ req ~canon:asked Model "q" ] in
  Alcotest.(check (option int))
    "window slides over pending" (Some 80)
    (Option.map (fun (e, _, _) -> e.Cassette.seq) (nearest_of (fst (only rs))))

let time f =
  let t0 = Sys.time () in
  let r = f () in
  (r, Sys.time () -. t0)

let wide i =
  Canon.list
    (List.init 60 (fun j -> Canon.string (Printf.sprintf "v%d.%d" i j)))

let test_perf_many_misses () =
  let exs =
    List.init 10_000 (fun i -> ex ~canon:(wide i) i (Printf.sprintf "k%d" i))
  in
  let st = start M.Strict exs in
  let (), dt =
    time (fun () ->
        for i = 0 to 999 do
          let _, d, _ = M.decide st (req ~canon:(wide (i * 7)) Model "none") in
          if not (is_refuse d) then Alcotest.fail "expected a miss"
        done)
  in
  Alcotest.(check bool)
    (Printf.sprintf "1000 misses in %.2fs" dt)
    true (dt < 2.0)

let test_perf_fuzzy_growing () =
  let history n changed =
    Canon.list
      (List.init n (fun m ->
           let content =
             if changed && m = n - 1 then "changed" else string_of_int m
           in
           doc (Printf.sprintf {|{"role":"user","content":"%s"}|} content)))
  in
  let exs =
    List.init 1000 (fun i ->
        ex ~canon:(history (i + 1) false) i (Printf.sprintf "k%d" i))
  in
  let (), dt =
    time (fun () ->
        let st = ref (start (M.Fuzzy 0.5) exs) in
        for i = 0 to 999 do
          let s, d, _ =
            M.decide !st (req ~canon:(history (i + 1) true) Model "none")
          in
          st := s;
          if not (String.equal (tag d) "serve") then
            Alcotest.fail "expected a serve"
        done)
  in
  Alcotest.(check bool) (Printf.sprintf "1000 fuzzy in %.2fs" dt) true (dt < 2.0)

let test_until_divergence () =
  let exs =
    [
      ex 0 "a";
      ex 1 "b";
      ex ~channel:Mcp 2 "t";
      ex ~channel:Http 3 "h";
      ex ~channel:Mcp 4 "u";
    ]
  in
  let st = start M.Until_divergence exs in
  let st, rs = run st [ req Model "a" ] in
  Alcotest.(check bool) "not yet" false (M.switched st);
  Alcotest.(check int) "a" 0 (seq_of (fst (List.hd rs)));
  let st, rs = run st [ req Model "nope" ] in
  let d, f = only rs in
  Alcotest.(check string) "live" "live" (tag d);
  Alcotest.(check (list string)) "finding" [ "missing" ] (tags f);
  Alcotest.(check bool) "switched" true (M.switched st);
  (* later model request that would match still goes live *)
  let st, rs = run st [ req Model "b" ] in
  Alcotest.(check string) "live" "live" (tag (fst (only rs)));
  (* tools keep replaying *)
  let st, rs = run st [ req Mcp "t"; req Http "h" ] in
  Alcotest.(check (list int))
    "tools" [ 2; 3 ]
    (List.map (fun (d, _) -> seq_of d) rs);
  (* a tool miss after the switch refuses *)
  let st, rs = run st [ req Mcp "zzz"; req Http "zzz" ] in
  List.iter (fun (d, _) -> Alcotest.(check bool) "refuse" true (is_refuse d)) rs;
  (* finish: unconsumed tool exchange only; model exchanges omitted *)
  Alcotest.(check (list string))
    "only unconsumed" [ "unconsumed" ]
    (tags (M.finish st));
  Alcotest.(check (list int)) "u" [ 4 ] (unconsumed_seqs (M.finish st))

let test_tool_miss_no_switch () =
  let st = start M.Until_divergence [ ex ~channel:Mcp 0 "t" ] in
  let st, rs = run st [ req Mcp "zzz" ] in
  Alcotest.(check bool) "refuse" true (is_refuse (fst (List.hd rs)));
  Alcotest.(check bool) "no switch" false (M.switched st)

let test_finish_order () =
  let exs =
    [ ex 0 "a"; ex ~channel:Mcp 1 "t"; ex 2 "b"; ex ~channel:Http 3 "h" ]
  in
  let st, _ = run (start M.Strict exs) [ req Model "a" ] in
  Alcotest.(check (list int))
    "seq order" [ 1; 2; 3 ]
    (unconsumed_seqs (M.finish st))

let test_immutable () =
  let st = start M.Strict [ ex 0 "a" ] in
  let _ = M.decide st (req Model "a") in
  let _, d, _ = M.decide st (req Model "a") in
  Alcotest.(check int) "old state unchanged" 0 (seq_of d)

let () =
  Alcotest.run "matcher"
    [
      ( "matcher",
        [
          Alcotest.test_case "key" `Quick test_key;
          Alcotest.test_case "key drops api key" `Quick test_key_drops_api_key;
          Alcotest.test_case "key headers" `Quick test_key_headers;
          Alcotest.test_case "strict in order" `Quick test_strict_in_order;
          Alcotest.test_case "occurrence" `Quick test_occurrence;
          Alcotest.test_case "reordered" `Quick test_reordered_model;
          Alcotest.test_case "reordered and shape" `Quick
            test_reordered_and_shape;
          Alcotest.test_case "mcp unordered" `Quick test_mcp_unordered;
          Alcotest.test_case "channels independent" `Quick
            test_channels_independent;
          Alcotest.test_case "shape" `Quick test_shape;
          Alcotest.test_case "miss nearest" `Quick test_miss_nearest;
          Alcotest.test_case "miss no candidate" `Quick test_miss_no_candidate;
          Alcotest.test_case "fuzzy" `Quick test_fuzzy;
          Alcotest.test_case "fuzzy threshold boundary" `Quick
            test_fuzzy_threshold_boundary;
          Alcotest.test_case "nearest tie" `Quick test_nearest_tie;
          Alcotest.test_case "transport error shape" `Quick
            test_transport_error_shape;
          Alcotest.test_case "nearest window" `Quick test_nearest_window;
          Alcotest.test_case "perf: many misses" `Slow test_perf_many_misses;
          Alcotest.test_case "perf: fuzzy growing history" `Slow
            test_perf_fuzzy_growing;
          Alcotest.test_case "until divergence" `Quick test_until_divergence;
          Alcotest.test_case "tool miss no switch" `Quick
            test_tool_miss_no_switch;
          Alcotest.test_case "finish order" `Quick test_finish_order;
          Alcotest.test_case "immutable" `Quick test_immutable;
        ] );
    ]
