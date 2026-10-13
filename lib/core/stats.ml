type interval = { lower : float; upper : float }

(* Horner evaluation of c.(0) + c.(1) x + ... *)
let poly coeffs x = Array.fold_right (fun c acc -> c +. (x *. acc)) coeffs 0.

(* Wichura, Algorithm AS 241 (PPND16), Applied Statistics 37(3), 1988,
   477-484. *)
let as241_a =
  [|
    3.3871328727963666080e0;
    1.3314166789178437745e+2;
    1.9715909503065514427e+3;
    1.3731693765509461125e+4;
    4.5921953931549871457e+4;
    6.7265770927008700853e+4;
    3.3430575583588128105e+4;
    2.5090809287301226727e+3;
  |]

let as241_b =
  [|
    1.0;
    4.2313330701600911252e+1;
    6.8718700749205790830e+2;
    5.3941960214247511077e+3;
    2.1213794301586595867e+4;
    3.9307895800092710610e+4;
    2.8729085735721942674e+4;
    5.2264952788528545610e+3;
  |]

let as241_c =
  [|
    1.42343711074968357734e0;
    4.63033784615654529590e0;
    5.76949722146069140550e0;
    3.64784832476320460504e0;
    1.27045825245236838258e0;
    2.41780725177450611770e-1;
    2.27238449892691845833e-2;
    7.74545014278341407640e-4;
  |]

let as241_d =
  [|
    1.0;
    2.05319162663775882187e0;
    1.67638483018380384940e0;
    6.89767334985100004550e-1;
    1.48103976427480074590e-1;
    1.51986665636164571966e-2;
    5.47593808499534494600e-4;
    1.05075007164441684324e-9;
  |]

let as241_e =
  [|
    6.65790464350110377720e0;
    5.46378491116411436990e0;
    1.78482653991729133580e0;
    2.96560571828504891230e-1;
    2.65321895265761230930e-2;
    1.24266094738807843860e-3;
    2.71155556874348757815e-5;
    2.01033439929228813265e-7;
  |]

let as241_f =
  [|
    1.0;
    5.99832206555887937690e-1;
    1.36929880922735805310e-1;
    1.48753612908506148525e-2;
    7.86869131145613259100e-4;
    1.84631831751005468180e-5;
    1.42151175831644588870e-7;
    2.04426310338993978564e-15;
  |]

let normal_quantile p =
  if Float.is_nan p || p < 0. || p > 1. then
    invalid_arg "Stats.normal_quantile: p outside [0, 1]"
  else if Float.equal p 0. then neg_infinity
  else if Float.equal p 1. then infinity
  else
    let q = p -. 0.5 in
    if Float.abs q <= 0.425 then
      let r = 0.180625 -. (q *. q) in
      q *. poly as241_a r /. poly as241_b r
    else
      let r = sqrt (-.log (Float.min p (1. -. p))) in
      let z =
        if r <= 5. then
          let r = r -. 1.6 in
          poly as241_c r /. poly as241_d r
        else
          let r = r -. 5. in
          poly as241_e r /. poly as241_f r
      in
      if q < 0. then -.z else z

let check_confidence fn confidence =
  if not (confidence > 0. && confidence < 1.) then
    invalid_arg (fn ^ ": confidence outside (0, 1)")

let clamp01 x = Float.min 1. (Float.max 0. x)

(* Wilson score interval at a real-valued sample size [n], so the stratified
   form can use an effective n. [at_zero] and [at_one] mark p = 0 and p = 1,
   where the bound is exactly 0 or 1; rounding would leave it a few ulps
   inside and exclude the point estimate. statsmodels
   proportion_confint(method="wilson"); Wilson 1927, JASA 22. *)
let wilson_at ~p ~n ~at_zero ~at_one ~confidence : interval =
  let z = -.normal_quantile ((1. -. confidence) /. 2.) in
  let z2 = z *. z in
  let denom = 1. +. (z2 /. n) in
  let center = (p +. (z2 /. (2. *. n))) /. denom in
  let dist =
    z *. sqrt ((p *. (1. -. p) /. n) +. (z2 /. (4. *. n *. n))) /. denom
  in
  {
    lower = (if at_zero then 0. else clamp01 (center -. dist));
    upper = (if at_one then 1. else clamp01 (center +. dist));
  }

let check_counts fn ~successes ~trials =
  if trials <= 0 || successes < 0 || successes > trials then
    invalid_arg (fn ^ ": need 0 <= successes <= trials and trials > 0")

let wilson ~successes ~trials ~confidence =
  check_counts "Stats.wilson" ~successes ~trials;
  check_confidence "Stats.wilson" confidence;
  wilson_at
    ~p:(float_of_int successes /. float_of_int trials)
    ~n:(float_of_int trials) ~at_zero:(successes = 0)
    ~at_one:(successes = trials) ~confidence

(* Newcombe's hybrid rule (method 10) for p2 - p1 from each arm's estimate
   and interval. *)
let hybrid ~p1 ~(w1 : interval) ~p2 ~(w2 : interval) =
  let d = p2 -. p1 in
  {
    lower = d -. Float.hypot (p2 -. w2.lower) (w1.upper -. p1);
    upper = d +. Float.hypot (w2.upper -. p2) (p1 -. w1.lower);
  }

let newcombe_diff ~s1 ~n1 ~s2 ~n2 ~confidence =
  let w1 = wilson ~successes:s1 ~trials:n1 ~confidence in
  let w2 = wilson ~successes:s2 ~trials:n2 ~confidence in
  hybrid
    ~p1:(float_of_int s1 /. float_of_int n1)
    ~w1
    ~p2:(float_of_int s2 /. float_of_int n2)
    ~w2

(* Yan and Su stratified Wilson interval of one arm, with its estimate. With
   equal trials in every scenario n_eff is exactly K n; otherwise the general
   formula, whose floating-point value need not equal K n even when it
   should. *)
let stratified_wilson arm ~confidence =
  let fn = "Stats.stratified_newcombe_diff" in
  List.iter (fun (successes, trials) -> check_counts fn ~successes ~trials) arm;
  let k = float_of_int (List.length arm) in
  let p =
    List.fold_left
      (fun acc (s, n) -> acc +. (float_of_int s /. float_of_int n))
      0. arm
    /. k
  in
  let n_eff =
    match arm with
    | (_, n) :: rest when List.for_all (fun (_, m) -> m = n) rest ->
        k *. float_of_int n
    | _ :: _ | [] ->
        1.
        /. List.fold_left
             (fun acc (_, n) -> acc +. (1. /. (k *. k *. float_of_int n)))
             0. arm
  in
  ( p,
    wilson_at ~p ~n:n_eff
      ~at_zero:(List.for_all (fun (s, _) -> s = 0) arm)
      ~at_one:(List.for_all (fun (s, n) -> s = n) arm)
      ~confidence )

let stratified_newcombe_diff ~baseline ~candidate ~confidence =
  let fn = "Stats.stratified_newcombe_diff" in
  check_confidence fn confidence;
  (match baseline with
  | [] -> invalid_arg (fn ^ ": no scenarios")
  | _ :: _ -> ());
  if List.compare_lengths baseline candidate <> 0 then
    invalid_arg (fn ^ ": arms have different numbers of scenarios");
  let p1, w1 = stratified_wilson baseline ~confidence in
  let p2, w2 = stratified_wilson candidate ~confidence in
  hybrid ~p1 ~w1 ~p2 ~w2

let pass_hat_k ~successes ~trials ~k =
  if successes < 0 || successes > trials || k < 1 || k > trials then
    invalid_arg
      "Stats.pass_hat_k: need 0 <= successes <= trials, 1 <= k <= trials";
  if successes < k then 0.
  else
    let rec go i acc =
      if i = k then acc
      else
        go (i + 1)
          (acc *. float_of_int (successes - i) /. float_of_int (trials - i))
    in
    go 0 1.

let quantile xs q =
  if Array.length xs = 0 then invalid_arg "Stats.quantile: empty array";
  if Array.exists Float.is_nan xs then invalid_arg "Stats.quantile: NaN value";
  if not (q >= 0. && q <= 1.) then
    invalid_arg "Stats.quantile: q outside [0, 1]";
  let sorted = Array.copy xs in
  Array.sort Float.compare sorted;
  let h = float_of_int (Array.length sorted - 1) *. q in
  let lo = truncate h in
  let hi = Int.min (lo + 1) (Array.length sorted - 1) in
  let x_lo = sorted.(lo) and x_hi = sorted.(hi) in
  let frac = h -. float_of_int lo in
  if Float.equal frac 0. || Float.equal x_lo x_hi then x_lo
  else if Float.equal x_lo neg_infinity then neg_infinity
  else if Float.equal x_hi infinity then infinity
  else x_lo +. (frac *. (x_hi -. x_lo))

let median xs = quantile xs 0.5

let bonferroni_level ~confidence ~family =
  if family < 1 then invalid_arg "Stats.bonferroni_level: family < 1";
  1. -. ((1. -. confidence) /. float_of_int family)
