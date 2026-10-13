(* A hand-built experiment: two variants, three scenarios, ten repeats each.
   Variant [b] is worse on [refund] (three failures, and a new tool path in
   three runs), costs twice as much on [search], and has an errored and a
   not-run run on [book]. *)

open Bactrian_core
module R = Report_data

let repeats = 10
let scenarios = [ "refund"; "search"; "book" ]

let trace ~tools ~ms ~tokens =
  let events : Trace.event list =
    ([
       Run_start
         {
           run_id = "x";
           scenario = "s";
           variant = "v";
           sim_now = "2026-10-13T09:00:00Z";
           clock = Contract_only;
         };
       Turn_start { turn = 1; content = "Hello" };
       Model_call
         {
           turn = 1;
           protocol = "openai_chat";
           model = "gpt-5-nano";
           status = 200;
           usage =
             Some
               {
                 input_tokens = tokens;
                 output_tokens = 10;
                 cached_input_tokens = None;
                 cache_write_tokens = None;
                 reasoning_tokens = None;
               };
           duration_ms = ms;
           ttft_ms = Some (ms / 4);
         };
     ]
      : Trace.event list)
    @ List.mapi
        (fun i name ->
          Trace.Tool_call
            { turn = 1; index = i; name; arguments = Canon.null; mcp = Joined })
        tools
    @ [ Reply { turn = 1; content = "Done" }; Done ]
  in
  List.mapi (fun seq event -> { Trace.seq; t_ms = seq * 10; event }) events

let fail_check name reason : R.check =
  { name; status = Check_fail; reason = Some reason }

let make ~index ~scenario ~variant ~repeat : R.run =
  let b = String.equal variant "b" in
  let tools, status, checks, reason =
    match (scenario, b, repeat) with
    | "refund", true, (3 | 6 | 9) ->
        ( [ "lookup_order"; "refund"; "notify" ],
          R.Failed,
          [
            fail_check "assertions[0] tool_sequence"
              "expected lookup_order then refund, saw an extra notify";
          ],
          Some "assertions[0] tool_sequence: extra notify" )
    | "refund", _, _ -> ([ "lookup_order"; "refund" ], Passed, [], None)
    | "search", _, _ -> ([ "search" ], Passed, [], None)
    | _, false, (4 | 8) ->
        ( [ "calendar"; "calendar" ],
          Failed,
          [ fail_check "assertions[1] max_steps" "3 model calls, allowed 2" ],
          Some "assertions[1] max_steps: 3 model calls, allowed 2" )
    | _, true, 5 -> ([], Errored, [], Some "proxy fault")
    | _, true, 10 -> ([], Not_run, [], Some "spend cap reached")
    | _ -> ([ "calendar" ], Passed, [], None)
  in
  let finished =
    match status with
    | Passed | Failed | Diverged -> true
    | Errored | Not_run | Pending | Interrupted -> false
  in
  let ms = if b then 900 + (repeat * 10) else 600 + (repeat * 10) in
  let tr =
    if finished then trace ~tools ~ms ~tokens:(if b then 400 else 200) else []
  in
  {
    index;
    repeat;
    scenario;
    variant;
    run_id =
      (if status = Pending then None else Some (Printf.sprintf "run-%03d" index));
    status;
    reason;
    checks;
    trace = tr;
    divergences =
      (if scenario = "refund" && b && repeat = 3 then
         [
           {
             R.description = "request not in the recording";
             changes =
               [
                 { Diff.path = "messages[2].content"; kind = Changed };
                 { Diff.path = "tools[1]"; kind = Added };
               ];
           };
         ]
       else []);
    cost =
      (if finished then
         Some
           ((if b && scenario = "search" then 0.02 else 0.01)
           +. (0.001 *. float_of_int (repeat mod 3)))
       else None);
    latency_ms = (if finished then R.latency_ms tr else None);
    tokens = (if finished then R.tokens tr else None);
    unsandboxed =
      (if scenario = "search" && repeat = 1 then
         Some "unavailable: bubblewrap (bwrap) is not installed"
       else None);
    clock_unenforced =
      (if scenario = "search" && repeat <= 2 then Some "static-binary" else None);
    sandbox_violations =
      (if scenario = "search" && repeat = 2 then
         Some
           "at least 2 (the system log can drop denials, so this is a lower \
            bound)"
       else None);
  }

let runs : R.run list =
  List.concat_map
    (fun scenario ->
      List.concat_map
        (fun repeat ->
          List.map (fun variant -> (scenario, variant, repeat)) [ "a"; "b" ])
        (List.init repeats (fun i -> i + 1)))
    scenarios
  |> List.mapi (fun index (scenario, variant, repeat) ->
      make ~index ~scenario ~variant ~repeat)

let settings ?(seed = 7) () : Compare.input =
  {
    runs = List.map R.compare_run runs;
    baseline = "a";
    candidate = "b";
    population = `Suite;
    confidence = 0.95;
    gate = [ (Pass_rate, 0.1) ];
    report_metrics = [ (Cost, 0.2); (Latency_p50, 0.25) ];
    seed;
    resamples = 500;
    fail_on_inconclusive = false;
  }

let data ?seed () : R.t =
  let settings = settings ?seed () in
  match Compare.run settings with
  | Error m -> failwith m
  | Ok result ->
      R.build ~experiment_id:"20261013T090000Z-0badc0de" ~experiment:"demo"
        ~settings ~result runs
