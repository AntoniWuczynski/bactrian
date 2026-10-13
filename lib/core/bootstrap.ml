type arm = { scenario : string; values : float array }

let check_common fn ~resamples ~confidence =
  if resamples < 1 then invalid_arg (fn ^ ": resamples < 1");
  if not (confidence > 0. && confidence < 1.) then
    invalid_arg (fn ^ ": confidence outside (0, 1)")

let check_values fn values =
  if Array.length values = 0 then invalid_arg (fn ^ ": empty arm");
  if Array.exists Float.is_nan values then invalid_arg (fn ^ ": NaN value")

(* Total order on arrays: length, then elementwise. Used only to pick a
   canonical orientation of the two arms. *)
let compare_values a b =
  let c = Int.compare (Array.length a) (Array.length b) in
  if c <> 0 then c
  else
    let rec go i =
      if i = Array.length a then 0
      else
        let c = Float.compare a.(i) b.(i) in
        if c <> 0 then c else go (i + 1)
    in
    go 0

(* Pairs the two arms scenario by scenario, in scenario-name order, returning
   [(first, second, flipped)]: [first] is the arm that sorts lower, so the
   draws do not depend on which arm is the baseline. [flipped] is true when
   [first] is the candidate. *)
let canonical_pairs fn ~baseline ~candidate =
  let sorted arms =
    List.iter (fun a -> check_values fn a.values) arms;
    List.sort (fun x y -> String.compare x.scenario y.scenario) arms
  in
  let b = sorted baseline and c = sorted candidate in
  let rec pair acc = function
    | [], [] -> List.rev acc
    | x :: xs, y :: ys when String.equal x.scenario y.scenario -> (
        match acc with
        | (prev, _) :: _ when String.equal prev x.scenario ->
            invalid_arg (fn ^ ": duplicate scenario " ^ x.scenario)
        | _ :: _ | [] ->
            pair ((x.scenario, (x.values, y.values)) :: acc) (xs, ys))
    | _ :: _, _ | [], _ :: _ ->
        invalid_arg (fn ^ ": arms name different scenarios")
  in
  let pairs = pair [] (b, c) |> List.map snd |> Array.of_list in
  if Array.length pairs = 0 then invalid_arg (fn ^ ": no scenarios");
  let order =
    Array.fold_left
      (fun acc (bv, cv) -> if acc <> 0 then acc else compare_values bv cv)
      0 pairs
  in
  if order <= 0 then (pairs, false)
  else (Array.map (fun (bv, cv) -> (cv, bv)) pairs, true)

let resample_mean rng values =
  let n = Array.length values in
  let sum = ref 0. in
  for _ = 1 to n do
    sum := !sum +. values.(Random.State.int rng n)
  done;
  !sum /. float_of_int n

let percentile ~confidence reps : Stats.interval =
  let a = (1. -. confidence) /. 2. in
  { lower = Stats.quantile reps a; upper = Stats.quantile reps (1. -. a) }

let orient flipped (i : Stats.interval) : Stats.interval =
  if flipped then { lower = -.i.upper; upper = -.i.lower } else i

(* Mean over the given scenario indices of (second minus first), each arm's
   runs resampled with replacement; first arm drawn before second. *)
let replicate rng pairs indices =
  let total = ref 0. in
  Array.iter
    (fun s ->
      let first, second = pairs.(s) in
      let m1 = resample_mean rng first in
      let m2 = resample_mean rng second in
      total := !total +. (m2 -. m1))
    indices;
  !total /. float_of_int (Array.length indices)

let diff fn ~draw_scenarios ~rng ~resamples ~confidence ~baseline ~candidate =
  check_common fn ~resamples ~confidence;
  let pairs, flipped = canonical_pairs fn ~baseline ~candidate in
  let reps = Array.make resamples 0. in
  for r = 0 to resamples - 1 do
    reps.(r) <- replicate rng pairs (draw_scenarios rng (Array.length pairs))
  done;
  orient flipped (percentile ~confidence reps)

let hierarchical_diff =
  diff "Bootstrap.hierarchical_diff" ~draw_scenarios:(fun rng m ->
      let idx = Array.make m 0 in
      for i = 0 to m - 1 do
        idx.(i) <- Random.State.int rng m
      done;
      idx)

let resample rng values =
  let n = Array.length values in
  let out = Array.make n 0. in
  for i = 0 to n - 1 do
    out.(i) <- values.(Random.State.int rng n)
  done;
  out

let log_ratio_of_medians ~rng ~resamples ~confidence ~baseline ~candidate =
  let fn = "Bootstrap.log_ratio_of_medians" in
  check_common fn ~resamples ~confidence;
  List.iter
    (fun (a : arm) ->
      if Array.exists (fun x -> x < 0.) a.values then
        invalid_arg (fn ^ ": negative value"))
    (baseline @ candidate);
  let pairs, flipped = canonical_pairs fn ~baseline ~candidate in
  (* One arm's values pooled over scenarios, each scenario's block produced by
     [f] in scenario order. *)
  let pooled f = Array.concat (Array.to_list (Array.map f pairs)) in
  let m1 = Stats.median (pooled fst) and m2 = Stats.median (pooled snd) in
  if Float.equal m1 0. || Float.equal m2 0. then None
  else
    let reps = Array.make resamples 0. in
    for r = 0 to resamples - 1 do
      let r1 = Stats.median (pooled (fun (x, _) -> resample rng x)) in
      let r2 = Stats.median (pooled (fun (_, y) -> resample rng y)) in
      reps.(r) <- log r2 -. log r1
    done;
    (* Both resampled medians zero gives log 0 - log 0 = NaN: the ratio is
       undefined for that replicate, so it is dropped. *)
    let defined =
      Array.of_list
        (List.filter (fun x -> not (Float.is_nan x)) (Array.to_list reps))
    in
    if Array.length defined = 0 then None
    else
      let est = log m2 -. log m1 in
      Some
        ( (if flipped then -.est else est),
          orient flipped (percentile ~confidence defined) )
