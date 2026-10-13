open Bactrian_core
module B = Bootstrap

let rng seed = Random.State.make [| seed |]

let bernoulli st n p =
  Array.init n (fun _ -> if Random.State.float st 1. < p then 1. else 0.)

let arms st ~n rates =
  List.mapi
    (fun i p : B.arm ->
      { scenario = Printf.sprintf "s%02d" i; values = bernoulli st n p })
    rates

let mean xs = List.fold_left ( +. ) 0. xs /. float_of_int (List.length xs)
let width (i : Stats.interval) = i.upper -. i.lower

let interval_eq (a : Stats.interval) (b : Stats.interval) =
  Float.equal a.lower b.lower && Float.equal a.upper b.upper

let negated (a : Stats.interval) (b : Stats.interval) =
  Float.equal a.lower (-.b.upper) && Float.equal a.upper (-.b.lower)

(* Eight scenarios with known rates; the true suite difference is the mean of
   the per-scenario differences. *)
let base_rates = [ 0.3; 0.4; 0.5; 0.55; 0.6; 0.65; 0.7; 0.8 ]
let cand_rates = [ 0.35; 0.5; 0.5; 0.6; 0.7; 0.65; 0.8; 0.85 ]
let true_diff = mean (List.map2 ( -. ) cand_rates base_rates)

let simulate_stratified ~seed ~datasets ~n ~resamples ~confidence =
  let st = rng seed in
  List.init datasets (fun _ ->
      let baseline = arms st ~n base_rates in
      let candidate = arms st ~n cand_rates in
      B.stratified_diff ~rng:st ~resamples ~confidence ~baseline ~candidate)

let test_coverage () =
  let datasets = 400 and confidence = 0.9 in
  let covered =
    simulate_stratified ~seed:7 ~datasets ~n:20 ~resamples:1000 ~confidence
    |> List.filter (fun (i : Stats.interval) ->
        i.lower <= true_diff && true_diff <= i.upper)
    |> List.length
  in
  let rate = float_of_int covered /. float_of_int datasets in
  let mc_se =
    sqrt (confidence *. (1. -. confidence) /. float_of_int datasets)
  in
  if Float.abs (rate -. confidence) > 3. *. mc_se then
    Alcotest.failf "coverage %.4f outside %.2f +- %.4f" rate confidence
      (3. *. mc_se)

let test_width_shrinks () =
  let avg n =
    simulate_stratified ~seed:11 ~datasets:100 ~n ~resamples:500 ~confidence:0.9
    |> List.map width |> mean
  in
  let w5 = avg 5 and w40 = avg 40 in
  (* Width scales roughly as 1/sqrt n, so n=40 should be near a third of n=5. *)
  if not (w40 < 0.5 *. w5) then
    Alcotest.failf "width did not shrink: n=5 %.4f, n=40 %.4f" w5 w40

(* Unpaired hierarchical bootstrap, test-only reference: each arm draws its
   own scenarios. *)
let unpaired_hierarchical ~rng ~resamples ~confidence ~(baseline : B.arm list)
    ~(candidate : B.arm list) : Stats.interval =
  let draw_arm (arms : B.arm array) =
    let m = Array.length arms in
    let total = ref 0. in
    for _ = 1 to m do
      let a = arms.(Random.State.int rng m) in
      let n = Array.length a.values in
      let s = ref 0. in
      for _ = 1 to n do
        s := !s +. a.values.(Random.State.int rng n)
      done;
      total := !total +. (!s /. float_of_int n)
    done;
    !total /. float_of_int m
  in
  let b = Array.of_list baseline and c = Array.of_list candidate in
  let reps =
    Array.init resamples (fun _ ->
        let x = draw_arm b in
        draw_arm c -. x)
  in
  let a = (1. -. confidence) /. 2. in
  { lower = Stats.quantile reps a; upper = Stats.quantile reps (1. -. a) }

let test_paired_no_wider () =
  let st = rng 23 in
  let paired = ref [] and unpaired = ref [] in
  for _ = 1 to 60 do
    (* Shared scenario effects: widely spread rates, small common shift. *)
    let rates = List.init 12 (fun _ -> 0.05 +. Random.State.float st 0.8) in
    let baseline = arms st ~n:10 rates in
    let candidate = arms st ~n:10 (List.map (fun p -> p +. 0.1) rates) in
    paired :=
      width
        (B.hierarchical_diff ~rng:st ~resamples:500 ~confidence:0.9 ~baseline
           ~candidate)
      :: !paired;
    unpaired :=
      width
        (unpaired_hierarchical ~rng:st ~resamples:500 ~confidence:0.9 ~baseline
           ~candidate)
      :: !unpaired
  done;
  let p = mean !paired and u = mean !unpaired in
  if not (p <= u) then Alcotest.failf "paired %.4f wider than unpaired %.4f" p u

let test_zero_median () =
  let r ~baseline ~candidate =
    B.log_ratio_of_medians ~rng:(rng 1) ~resamples:100 ~confidence:0.9 ~baseline
      ~candidate
  in
  Alcotest.(check bool)
    "baseline zero" true
    (Option.is_none (r ~baseline:[| 0.; 0.; 1. |] ~candidate:[| 1.; 2.; 3. |]));
  Alcotest.(check bool)
    "candidate zero" true
    (Option.is_none (r ~baseline:[| 1.; 2.; 3. |] ~candidate:[| 0.; 0.; 5. |]))

let test_log_ratio_point () =
  match
    B.log_ratio_of_medians ~rng:(rng 1) ~resamples:200 ~confidence:0.9
      ~baseline:[| 1.; 2.; 3. |] ~candidate:[| 2.; 4.; 6. |]
  with
  | None -> Alcotest.fail "expected Some"
  | Some (e, i) ->
      Alcotest.(check (float 1e-12)) "log 2" (log 2.) e;
      Alcotest.(check bool)
        "contains estimate" true
        (i.lower <= e && e <= i.upper)

let test_invalid () =
  let raises name f =
    match f () with
    | (_ : Stats.interval) ->
        Alcotest.failf "%s: expected Invalid_argument" name
    | exception Invalid_argument _ -> ()
  in
  let a s v : B.arm = { scenario = s; values = v } in
  raises "mismatched scenarios" (fun () ->
      B.stratified_diff ~rng:(rng 1) ~resamples:10 ~confidence:0.9
        ~baseline:[ a "x" [| 1. |] ]
        ~candidate:[ a "y" [| 1. |] ]);
  raises "empty arm" (fun () ->
      B.hierarchical_diff ~rng:(rng 1) ~resamples:10 ~confidence:0.9
        ~baseline:[ a "x" [||] ]
        ~candidate:[ a "x" [| 1. |] ]);
  raises "duplicate scenario" (fun () ->
      B.stratified_diff ~rng:(rng 1) ~resamples:10 ~confidence:0.9
        ~baseline:[ a "x" [| 1. |]; a "x" [| 0. |] ]
        ~candidate:[ a "x" [| 1. |]; a "x" [| 0. |] ])

(* QCheck generators: a dataset of 1 to 6 scenarios, each arm 1 to 8 runs of
   pass/fail or continuous values. *)
let gen_values =
  QCheck.Gen.(
    let* n = int_range 1 8 in
    let* continuous = bool in
    if continuous then array_size (return n) (float_range 0. 10.)
    else array_size (return n) (map (fun b -> if b then 1. else 0.) bool))

let gen_dataset =
  QCheck.Gen.(
    let* m = int_range 1 6 in
    let* pairs = list_size (return m) (pair gen_values gen_values) in
    let* seed = int in
    let mk f =
      List.mapi
        (fun i p : B.arm -> { scenario = Printf.sprintf "s%d" i; values = f p })
        pairs
    in
    (* Scenario order deliberately differs between arms. *)
    return (mk fst, List.rev (mk snd), seed))

let print_dataset (b, c, seed) =
  let arm (a : B.arm) =
    a.scenario ^ ":"
    ^ String.concat "," (Array.to_list (Array.map string_of_float a.values))
  in
  Printf.sprintf "seed=%d baseline=[%s] candidate=[%s]" seed
    (String.concat " " (List.map arm b))
    (String.concat " " (List.map arm c))

let arb_dataset = QCheck.make ~print:print_dataset gen_dataset

let same_data (b : B.arm list) (c : B.arm list) =
  let sorted l =
    List.sort
      (fun (x : B.arm) (y : B.arm) -> String.compare x.scenario y.scenario)
      l
  in
  List.equal
    (fun (x : B.arm) (y : B.arm) ->
      String.equal x.scenario y.scenario
      && Array.length x.values = Array.length y.values
      && Array.for_all2 Float.equal x.values y.values)
    (sorted b) (sorted c)

let diff_fns =
  [ ("stratified", B.stratified_diff); ("hierarchical", B.hierarchical_diff) ]

let swap_negates =
  List.map
    (fun (name, f) ->
      QCheck.Test.make ~name:(name ^ ": swap negates exactly")
        ~count:300 arb_dataset (fun (baseline, candidate, seed) ->
          QCheck.assume (not (same_data baseline candidate));
          let run ~baseline ~candidate =
            f ~rng:(rng seed) ~resamples:200 ~confidence:0.9 ~baseline
              ~candidate
          in
          negated (run ~baseline ~candidate)
            (run ~baseline:candidate ~candidate:baseline)))
    diff_fns

let same_seed_identical =
  List.map
    (fun (name, f) ->
      QCheck.Test.make ~name:(name ^ ": same seed, same interval")
        ~count:200 arb_dataset (fun (baseline, candidate, seed) ->
          let run () =
            f ~rng:(rng seed) ~resamples:200 ~confidence:0.9 ~baseline
              ~candidate
          in
          interval_eq (run ()) (run ())))
    diff_fns

let gen_positive =
  QCheck.Gen.(array_size (int_range 1 15) (float_range 0.01 100.))

let arb_positive_pair =
  QCheck.make
    ~print:QCheck.Print.(triple (array float) (array float) int)
    QCheck.Gen.(triple gen_positive gen_positive int)

let log_ratio_swap =
  QCheck.Test.make ~name:"log ratio: swap negates exactly" ~count:300
    arb_positive_pair (fun (a, b, seed) ->
      QCheck.assume
        (not
           (Array.length a = Array.length b && Array.for_all2 Float.equal a b));
      let run baseline candidate =
        B.log_ratio_of_medians ~rng:(rng seed) ~resamples:200 ~confidence:0.9
          ~baseline ~candidate
      in
      match (run a b, run b a) with
      | Some (e1, i1), Some (e2, i2) -> Float.equal e1 (-.e2) && negated i1 i2
      | (None | Some _), _ -> false)

let log_ratio_same_seed =
  QCheck.Test.make ~name:"log ratio: same seed, same result" ~count:200
    arb_positive_pair (fun (a, b, seed) ->
      let run () =
        B.log_ratio_of_medians ~rng:(rng seed) ~resamples:200 ~confidence:0.9
          ~baseline:a ~candidate:b
      in
      match (run (), run ()) with
      | Some (e1, i1), Some (e2, i2) -> Float.equal e1 e2 && interval_eq i1 i2
      | (None | Some _), _ -> false)

let () =
  Alcotest.run "bootstrap"
    [
      ( "simulation",
        [
          Alcotest.test_case "stratified coverage at 0.9" `Slow test_coverage;
          Alcotest.test_case "width shrinks with n" `Slow test_width_shrinks;
          Alcotest.test_case "paired no wider than unpaired" `Slow
            test_paired_no_wider;
        ] );
      ( "unit",
        [
          Alcotest.test_case "zero median" `Quick test_zero_median;
          Alcotest.test_case "log ratio point" `Quick test_log_ratio_point;
          Alcotest.test_case "invalid arguments" `Quick test_invalid;
        ] );
      ( "properties",
        List.map
          (fun t -> QCheck_alcotest.to_alcotest t)
          (swap_negates @ same_seed_identical
          @ [ log_ratio_swap; log_ratio_same_seed ]) );
    ]
