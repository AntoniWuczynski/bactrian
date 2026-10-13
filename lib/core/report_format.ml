let signed digits x =
  let s = Printf.sprintf "%+.*f" digits x in
  (* "-0.0" and "+0.0" both read as no change. *)
  if Float.abs x < 0.5 *. (10. ** float_of_int (-digits)) then
    Printf.sprintf "%.*f" digits 0.
  else s

let points x = if Float.is_nan x then "n/a" else signed 1 (x *. 100.) ^ " pp"
let percent x = if Float.is_nan x then "n/a" else signed 1 (x *. 100.) ^ "%"

let usd x =
  if x = 0. then "$0"
  else if Float.abs x < 0.01 then Printf.sprintf "$%.4f" x
  else Printf.sprintf "$%.3f" x

let ms x =
  if x >= 1000. then Printf.sprintf "%.2f s" (x /. 1000.)
  else Printf.sprintf "%.0f ms" x

let share k n = Printf.sprintf "%d/%d" k n

let rate k n =
  if n = 0 then "n/a"
  else Printf.sprintf "%.0f%%" (100. *. float_of_int k /. float_of_int n)

let signature = function
  | [] -> "(no tool calls)"
  | tools -> String.concat " > " tools

let verdict : Compare.verdict -> string = function
  | Regression -> "Regression"
  | Improvement -> "Improvement"
  | No_change -> "No change"
  | Inconclusive { lean = `Worse } -> "Inconclusive, leans worse"
  | Inconclusive { lean = `Better } -> "Inconclusive, leans better"
  | Inconclusive { lean = `None } -> "Inconclusive"

let verdict_class : Compare.verdict -> string = function
  | Regression -> "regression"
  | Improvement -> "improvement"
  | No_change -> "no-change"
  | Inconclusive _ -> "inconclusive"

let metric : Compare.metric -> string = function
  | Pass_rate -> "Pass rate"
  | Cost -> "Cost"
  | Latency_p50 -> "Median latency"
  | Tokens -> "Tokens"

let status : Report_data.status -> string = function
  | Passed -> "passed"
  | Failed -> "failed"
  | Diverged -> "diverged"
  | Errored -> "errored"
  | Not_run -> "not run"
  | Pending -> "pending"
  | Interrupted -> "interrupted"

let truncate n s =
  if String.length s <= n then s
  else
    let cut = ref n in
    (* Back off over UTF-8 continuation bytes (10xxxxxx). *)
    while !cut > 0 && Char.code s.[!cut] land 0xC0 = 0x80 do
      decr cut
    done;
    String.sub s 0 !cut ^ "..."

let clip = truncate 600

let trace_line (e : Trace.entry) =
  let body =
    match e.event with
    | Run_start { run_id; scenario; variant; _ } ->
        Printf.sprintf "run_start %s %s/%s" run_id scenario variant
    | Turn_start { turn; content } ->
        Printf.sprintf "turn_start %d %s" turn (clip content)
    | Model_call { turn; model; status; usage; duration_ms; _ } ->
        Printf.sprintf "model_call %d %s status=%d %dms%s" turn model status
          duration_ms
          (match usage with
          | Some u ->
              Printf.sprintf " tokens=%d/%d" u.input_tokens u.output_tokens
          | None -> "")
    | Tool_call { turn; name; arguments; _ } ->
        Printf.sprintf "tool_call %d %s %s" turn name
          (clip (Canon.to_string arguments))
    | Tool_result { turn; content; is_error; _ } ->
        Printf.sprintf "tool_result %d%s %s" turn
          (if is_error then " error" else "")
          (clip content)
    | Http_call { turn; meth; host; path; status } ->
        Printf.sprintf "http_call %d %s %s%s status=%d" turn meth host path
          status
    | Agent_event { turn; name; data } ->
        Printf.sprintf "agent_event %d %s %s" turn name
          (clip (Canon.to_string data))
    | Reply { turn; content } ->
        Printf.sprintf "reply %d %s" turn (clip content)
    | Sandbox_violation { kind; detail } ->
        Printf.sprintf "sandbox_violation %s %s" kind (clip detail)
    | Done -> "done"
  in
  Printf.sprintf "%3d %6dms %s" e.seq e.t_ms body

let exit_meaning = function
  | 0 -> "no gate metric regressed"
  | 1 -> "a gate metric regressed"
  | 2 -> "a gate metric is inconclusive"
  | 3 -> "missing data may bias the comparison"
  | n -> Printf.sprintf "exit status %d" n

let value_estimand_text =
  "Cost and tokens: the ratio of the candidate's mean per run to the \
   baseline's, each mean weighting every scenario equally. Latency: the \
   average over scenarios of each scenario's ratio of medians (a geometric \
   mean). Both over the scenarios that both arms finished, with bootstrap \
   intervals that resample runs within each scenario."

let interval f (i : Stats.interval) =
  Printf.sprintf "%s to %s" (f i.lower) (f i.upper)

(* A relative threshold δ applies as log (1 + δ) on the log ratio, so a
   regression is a ratio above 1 + δ and an improvement a ratio below
   1 / (1 + δ), a change of -δ / (1 + δ). *)
let threshold_text (metric : Compare.metric) delta =
  let relative worse better =
    Printf.sprintf "%.1f%% %s, %.1f%% %s" (delta *. 100.) worse
      (delta /. (1. +. delta) *. 100.)
      better
  in
  match metric with
  | Pass_rate -> Printf.sprintf "±%.1f pp" (delta *. 100.)
  | Cost -> relative "dearer" "cheaper"
  | Latency_p50 -> relative "slower" "faster"
  | Tokens -> relative "more" "fewer"

let metric_texts (m : Report_data.metric_row) =
  let threshold = threshold_text m.result.metric m.delta in
  match (m.relative, Float.is_nan m.result.estimate) with
  | _, true -> ("undefined", "undefined", "")
  | None, false ->
      (points m.result.estimate, interval points m.result.interval, threshold)
  | Some (e, i), false -> (percent e, interval percent i, threshold)
