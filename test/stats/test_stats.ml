open Bactrian_core
module J = Yojson.Safe.Util

let close a b =
  Float.equal a b
  || Float.abs (a -. b) <= 1e-9 *. Float.abs b
  || Float.abs (a -. b) <= 1e-12

let check_close what expected actual =
  if not (close actual expected) then
    Alcotest.failf "%s: expected %.17g, got %.17g" what expected actual

let entries name =
  Yojson.Safe.from_file ("../fixtures/stats/" ^ name)
  |> J.member "entries" |> J.to_list

let num j k = J.member k j |> J.to_number
let int j k = J.member k j |> J.to_int

let fixture_test name file f =
  Alcotest.test_case name `Quick (fun () ->
      let es = entries file in
      Alcotest.(check bool) (file ^ " non-empty") true (es <> []);
      List.iter f es)

let check_interval what (expected : Stats.interval) (actual : Stats.interval) =
  check_close (what ^ " lower") expected.lower actual.lower;
  check_close (what ^ " upper") expected.upper actual.upper

let wilson e =
  let s = int e "successes" and n = int e "trials" and c = num e "confidence" in
  check_interval
    (Printf.sprintf "wilson %d/%d @%g" s n c)
    { lower = num e "lower"; upper = num e "upper" }
    (Stats.wilson ~successes:s ~trials:n ~confidence:c)

let newcombe e =
  let s1 = int e "s1" and n1 = int e "n1" and s2 = int e "s2" in
  let n2 = int e "n2" and c = num e "confidence" in
  check_interval
    (Printf.sprintf "newcombe %d/%d vs %d/%d @%g" s1 n1 s2 n2 c)
    { lower = num e "lower"; upper = num e "upper" }
    (Stats.newcombe_diff ~s1 ~n1 ~s2 ~n2 ~confidence:c)

let pairs j k =
  J.member k j |> J.to_list
  |> List.map (fun x ->
      match J.to_list x with
      | [ s; n ] -> (J.to_int s, J.to_int n)
      | _ -> Alcotest.fail "expected [successes, trials]")

let stratified e =
  let baseline = pairs e "baseline" and candidate = pairs e "candidate" in
  let c = num e "confidence" in
  check_interval
    (Printf.sprintf "stratified newcombe %d scenarios @%g"
       (List.length baseline) c)
    { lower = num e "lower"; upper = num e "upper" }
    (Stats.stratified_newcombe_diff ~baseline ~candidate ~confidence:c)

let normal e =
  let p = num e "p" in
  check_close
    (Printf.sprintf "normal_quantile %g" p)
    (num e "z") (Stats.normal_quantile p)

let quantile e =
  let values = J.member "values" e |> J.to_list |> List.map J.to_number in
  let q = num e "q" in
  check_close
    (Printf.sprintf "quantile %g" q)
    (num e "quantile")
    (Stats.quantile (Array.of_list values) q)

let pass_k e =
  let n = int e "n" and c = int e "c" and k = int e "k" in
  check_close
    (Printf.sprintf "pass^%d %d/%d" k c n)
    (num e "value")
    (Stats.pass_hat_k ~successes:c ~trials:n ~k)

let bonferroni e =
  let c = num e "confidence" and m = int e "family" in
  check_close
    (Printf.sprintf "bonferroni %g/%d" c m)
    (num e "level")
    (Stats.bonferroni_level ~confidence:c ~family:m)

let test_median () =
  check_close "odd" 3. (Stats.median [| 5.; 1.; 3. |]);
  check_close "even" 2.5 (Stats.median [| 4.; 1.; 3.; 2. |])

let test_quantile_does_not_mutate () =
  let xs = [| 3.; 1.; 2. |] in
  ignore (Stats.quantile xs 0.5 : float);
  Alcotest.(check (array (float 0.))) "unchanged" [| 3.; 1.; 2. |] xs

let test_quantile_infinite () =
  check_close "neg inf" neg_infinity
    (Stats.quantile [| neg_infinity; neg_infinity; 1. |] 0.6);
  check_close "pos inf" infinity (Stats.quantile [| 0.; 1.; infinity |] 0.9)

let test_pass_k_large () =
  (* C(1000, 300) overflows a float; the product form must not. *)
  let v = Stats.pass_hat_k ~successes:900 ~trials:1000 ~k:300 in
  Alcotest.(check bool) "finite and positive" true (Float.is_finite v && v > 0.)

let test_invalid () =
  let raises name f =
    match f () with
    | (_ : float) -> Alcotest.failf "%s: expected Invalid_argument" name
    | exception Invalid_argument _ -> ()
  in
  raises "empty quantile" (fun () -> Stats.quantile [||] 0.5);
  raises "q > 1" (fun () -> Stats.quantile [| 1. |] 1.5);
  raises "p < 0" (fun () -> Stats.normal_quantile (-0.1));
  raises "wilson n=0" (fun () ->
      (Stats.wilson ~successes:0 ~trials:0 ~confidence:0.95).lower);
  raises "family 0" (fun () ->
      Stats.bonferroni_level ~confidence:0.95 ~family:0)

let wilson_contains_estimate =
  QCheck.Test.make ~name:"wilson contains s/n and lies in [0,1]" ~count:500
    QCheck.(
      pair (int_range 1 500) (pair (int_range 0 500) (float_range 0.5 0.999)))
    (fun (n, (s, c)) ->
      let s = s mod (n + 1) in
      let i = Stats.wilson ~successes:s ~trials:n ~confidence:c in
      let p = float_of_int s /. float_of_int n in
      0. <= i.lower && i.lower <= p && p <= i.upper && i.upper <= 1.)

let newcombe_swap =
  QCheck.Test.make ~name:"newcombe negates under swap" ~count:500
    QCheck.(
      quad (int_range 1 200) (int_range 1 200) (pair nat_small nat_small)
        (float_range 0.5 0.999))
    (fun (n1, n2, (a, b), c) ->
      let s1 = a mod (n1 + 1) and s2 = b mod (n2 + 1) in
      let i = Stats.newcombe_diff ~s1 ~n1 ~s2 ~n2 ~confidence:c in
      let j = Stats.newcombe_diff ~s1:s2 ~n1:n2 ~s2:s1 ~n2:n1 ~confidence:c in
      let d =
        (float_of_int s2 /. float_of_int n2)
        -. (float_of_int s1 /. float_of_int n1)
      in
      close i.lower (-.j.upper) && close i.upper (-.j.lower) && i.lower <= d
      && d <= i.upper)

(* One to six scenarios, each arm 1 to 30 runs per scenario. *)
let gen_strata =
  QCheck.Gen.(
    let arm k =
      list_size (return k)
        (let* n = int_range 1 30 in
         let* s = int_range 0 n in
         return (s, n))
    in
    let* k = int_range 1 6 in
    let* b = arm k in
    let* c = arm k in
    let* conf = float_range 0.5 0.999 in
    return (b, c, conf))

let arb_strata =
  QCheck.make
    ~print:
      QCheck.Print.(triple (list (pair int int)) (list (pair int int)) float)
    gen_strata

let mean_rate arm =
  List.fold_left
    (fun acc (s, n) -> acc +. (float_of_int s /. float_of_int n))
    0. arm
  /. float_of_int (List.length arm)

let stratified_one_scenario =
  QCheck.Test.make ~name:"stratified newcombe = newcombe with one scenario"
    ~count:1000
    QCheck.(
      quad (int_range 1 300) (int_range 1 300) (pair nat_small nat_small)
        (float_range 0.5 0.999))
    (fun (n1, n2, (a, b), c) ->
      let s1 = a mod (n1 + 1) and s2 = b mod (n2 + 1) in
      let i = Stats.newcombe_diff ~s1 ~n1 ~s2 ~n2 ~confidence:c in
      let j =
        Stats.stratified_newcombe_diff
          ~baseline:[ (s1, n1) ]
          ~candidate:[ (s2, n2) ]
          ~confidence:c
      in
      Float.equal i.lower j.lower && Float.equal i.upper j.upper)

let stratified_swap =
  QCheck.Test.make ~name:"stratified newcombe negates exactly under swap"
    ~count:1000 arb_strata (fun (baseline, candidate, confidence) ->
      let i = Stats.stratified_newcombe_diff ~baseline ~candidate ~confidence in
      let j =
        Stats.stratified_newcombe_diff ~baseline:candidate ~candidate:baseline
          ~confidence
      in
      Float.equal i.lower (-.j.upper) && Float.equal i.upper (-.j.lower))

let stratified_never_zero_width =
  QCheck.Test.make
    ~name:"stratified newcombe contains its estimate, never zero width"
    ~count:1000 arb_strata (fun (baseline, candidate, confidence) ->
      let i = Stats.stratified_newcombe_diff ~baseline ~candidate ~confidence in
      let d = mean_rate candidate -. mean_rate baseline in
      i.lower < i.upper && i.lower <= d && d <= i.upper && -1. <= i.lower
      && i.upper <= 1.)

let test_stratified_unanimous () =
  (* Review §3.1: every run passing in both arms must not look certain. *)
  let all_pass k n = List.init k (fun _ -> (n, n)) in
  List.iter
    (fun (k, n, half_width) ->
      let i =
        Stats.stratified_newcombe_diff ~baseline:(all_pass k n)
          ~candidate:(all_pass k n) ~confidence:0.95
      in
      check_close (Printf.sprintf "K=%d n=%d upper" k n) half_width i.upper;
      check_close (Printf.sprintf "K=%d n=%d lower" k n) (-.half_width) i.lower)
    [
      (1, 3, 0.5614970317550455);
      (10, 3, 0.11351339317396876);
      (10, 10, 0.03699349820698568);
    ]

let test_stratified_invalid () =
  let raises name f =
    match f () with
    | (_ : Stats.interval) ->
        Alcotest.failf "%s: expected Invalid_argument" name
    | exception Invalid_argument _ -> ()
  in
  let sn baseline candidate =
    Stats.stratified_newcombe_diff ~baseline ~candidate ~confidence:0.95
  in
  raises "no scenarios" (fun () -> sn [] []);
  raises "different lengths" (fun () -> sn [ (1, 2) ] [ (1, 2); (1, 2) ]);
  raises "zero trials" (fun () -> sn [ (0, 0) ] [ (1, 2) ]);
  raises "successes above trials" (fun () -> sn [ (1, 2) ] [ (3, 2) ])

let () =
  Alcotest.run "stats"
    [
      ( "fixtures",
        [
          fixture_test "wilson" "wilson.json" wilson;
          fixture_test "newcombe" "newcombe.json" newcombe;
          fixture_test "stratified newcombe" "stratified_newcombe.json"
            stratified;
          fixture_test "normal_quantile" "normal_quantile.json" normal;
          fixture_test "quantile" "quantile.json" quantile;
          fixture_test "pass_hat_k" "pass_k.json" pass_k;
          fixture_test "bonferroni" "bonferroni.json" bonferroni;
        ] );
      ( "unit",
        [
          Alcotest.test_case "median" `Quick test_median;
          Alcotest.test_case "quantile copies" `Quick
            test_quantile_does_not_mutate;
          Alcotest.test_case "quantile infinite" `Quick test_quantile_infinite;
          Alcotest.test_case "pass^k large" `Quick test_pass_k_large;
          Alcotest.test_case "invalid arguments" `Quick test_invalid;
          Alcotest.test_case "stratified unanimous arms" `Quick
            test_stratified_unanimous;
          Alcotest.test_case "stratified invalid arguments" `Quick
            test_stratified_invalid;
        ] );
      ( "properties",
        List.map
          (fun t -> QCheck_alcotest.to_alcotest t)
          [
            wilson_contains_estimate;
            newcombe_swap;
            stratified_one_scenario;
            stratified_swap;
            stratified_never_zero_width;
          ] );
    ]
