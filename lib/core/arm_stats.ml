type outcome = Passed | Failed | Errored | Not_run

type run = {
  outcome : outcome;
  cost : float option;
  tokens : int option;
  latency_ms : float option;
  ttft_ms : float list;
}

type measure = {
  runs : int;
  values : int;
  median : float;
  p90 : float;
  median_interval : Stats.interval option;
  p90_interval : Stats.interval option;
}

type t = {
  finished : int;
  passes : int;
  errored : int;
  not_run : int;
  wilson : Stats.interval option;
  pass_hat_k : float list;
  cost : measure option;
  tokens : measure option;
  latency_ms : measure option;
  ttft_ms : measure option;
}

let min_runs = 5

(* The values of each run that has any, one array per run. *)
let measure ~rng ~resamples ~confidence clusters =
  match List.filter (fun c -> Array.length c > 0) clusters with
  | [] -> None
  | clusters ->
      let pooled = Array.concat clusters in
      let median_interval, p90_interval =
        if List.compare_length_with clusters min_runs < 0 then (None, None)
        else
          let m, p =
            Bootstrap.median_p90_intervals ~rng ~resamples ~confidence clusters
          in
          (Some m, Some p)
      in
      Some
        {
          runs = List.length clusters;
          values = Array.length pooled;
          median = Stats.median pooled;
          p90 = Stats.quantile pooled 0.9;
          median_interval;
          p90_interval;
        }

let compute ~rng ~resamples ~confidence runs =
  let count pred =
    List.length (List.filter (fun (r : run) -> pred r.outcome) runs)
  in
  let passes =
    count (function Passed -> true | Failed | Errored | Not_run -> false)
  in
  let finished =
    passes
    + count (function Failed -> true | Passed | Errored | Not_run -> false)
  in
  let done_ =
    List.filter
      (fun (r : run) ->
        match r.outcome with
        | Passed | Failed -> true
        | Errored | Not_run -> false)
      runs
  in
  let one f = List.map (fun r -> Option.to_list (f r) |> Array.of_list) done_ in
  let measure = measure ~rng ~resamples ~confidence in
  (* Evaluated in this order, so the draws do not depend on the compiler's
     evaluation order of record fields. *)
  let cost = measure (one (fun r -> r.cost)) in
  let tokens = measure (one (fun r -> Option.map float_of_int r.tokens)) in
  let latency_ms = measure (one (fun r -> r.latency_ms)) in
  let ttft_ms =
    measure (List.map (fun (r : run) -> Array.of_list r.ttft_ms) done_)
  in
  {
    finished;
    passes;
    errored =
      count (function Errored -> true | Passed | Failed | Not_run -> false);
    not_run =
      count (function Not_run -> true | Passed | Failed | Errored -> false);
    wilson =
      (if finished = 0 then None
       else Some (Stats.wilson ~successes:passes ~trials:finished ~confidence));
    pass_hat_k =
      List.init finished (fun i ->
          Stats.pass_hat_k ~successes:passes ~trials:finished ~k:(i + 1));
    cost;
    tokens;
    latency_ms;
    ttft_ms;
  }
