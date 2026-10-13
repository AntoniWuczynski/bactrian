module H = Tyxml.Html
module D = Report_data
module F = Report_format

type cell =
  | Text of string
  | Num of string
  | Code of string
  | Badge of Compare.verdict
  | New

let cell_elt = function
  | Text s -> H.td [ H.txt s ]
  | Num s -> H.td ~a:[ H.a_class [ "num" ] ] [ H.txt s ]
  | Code s -> H.td [ H.code [ H.txt s ] ]
  | Badge v ->
      H.td
        [
          H.span
            ~a:[ H.a_class [ "badge"; F.verdict_class v ] ]
            [ H.txt (F.verdict v) ];
        ]
  | New -> H.td [ H.span ~a:[ H.a_class [ "badge"; "new" ] ] [ H.txt "new" ] ]

(* The first cell of each row is its header. The caption is shown above the
   scrolling wrapper, so a narrow screen never scrolls it out of view, and kept
   in the table, hidden, as its accessible name. *)
let table ~caption ~head rows =
  H.div
    [
      H.p
        ~a:[ H.a_class [ "caption" ]; H.a_aria "hidden" [ "true" ] ]
        [ H.txt caption ];
      H.div
        ~a:[ H.a_class [ "scroll" ] ]
        [
          H.table
            ~caption:
              (H.caption ~a:[ H.a_class [ "sr-only" ] ] [ H.txt caption ])
            ~thead:
              (H.thead
                 [
                   H.tr
                     (List.map
                        (fun h -> H.th ~a:[ H.a_scope `Col ] [ H.txt h ])
                        head);
                 ])
            (List.map
               (fun (header, cells) ->
                 H.tr
                   (H.th ~a:[ H.a_scope `Row ] [ H.txt header ]
                   :: List.map cell_elt cells))
               rows);
        ];
    ]

let section id title body =
  H.section ~a:[ H.a_id id ] (H.h2 [ H.txt title ] :: body)

let para ?(cls = []) s = H.p ~a:[ H.a_class cls ] [ H.txt s ]
let interval = F.interval

(* Header *)

let estimand_text = function
  | `Suite ->
      "Suite: performance on exactly these scenarios. Uncertainty comes from \
       run-to-run variation only."
  | `Scenarios ->
      "Scenarios: performance on scenarios like these, treating the suite as a \
       sample. Uncertainty comes from both scenarios and runs."

let gate_text (d : D.t) =
  match List.filter (fun (m : D.metric_row) -> m.result.in_gate) d.metrics with
  | [] -> "none"
  | gate ->
      String.concat ", "
        (List.map
           (fun (m : D.metric_row) ->
             match m.result.metric with
             | Pass_rate ->
                 Printf.sprintf "%s (threshold %.1f percentage points)" m.name
                   (m.delta *. 100.)
             | Cost | Latency_p50 | Tokens ->
                 Printf.sprintf "%s (threshold %s%s)" m.name
                   (F.threshold_text m.result.metric m.delta)
                   (match m.result.undefined with
                   | Some _ -> ", undefined, counts as inconclusive"
                   | None -> ""))
           gate)

let fact name value = [ H.dt [ H.txt name ]; H.dd [ H.txt value ] ]

let header (d : D.t) =
  H.header
    [
      H.h1 [ H.txt d.experiment ];
      para ~cls:[ "sub" ]
        (Printf.sprintf "Candidate %s against baseline %s. Experiment %s."
           d.candidate d.baseline d.experiment_id);
      H.dl
        ~a:[ H.a_class [ "facts" ] ]
        (List.concat
           [
             fact "Estimand" (estimand_text d.population);
             fact "Cost, tokens and latency" F.value_estimand_text;
             fact "Confidence" (Printf.sprintf "%.0f%%" (d.confidence *. 100.));
             fact "Gate family" (gate_text d);
             fact "Level per gate interval"
               (Printf.sprintf "%.4f%s" d.gate_level
                  (if List.compare_length_with d.gate 1 > 0 then
                     " (Bonferroni across the gate family)"
                   else ""));
             fact "Seed"
               (Printf.sprintf "%d, %d bootstrap resamples" d.seed d.resamples);
             fact "Gate exit status"
               (Printf.sprintf "%d: %s" d.exit (F.exit_meaning d.exit));
           ]);
    ]

(* Runs outside the sandbox are named before anything else (design §5.3). *)
let unsandboxed_warning (d : D.t) =
  match List.filter_map (fun (r : D.run) -> r.unsandboxed) d.order with
  | [] -> []
  | reasons ->
      [
        para ~cls:[ "warning" ]
          (Printf.sprintf
             "%d of %d runs ran without the operating-system sandbox, so \
              nothing stopped their agents from reaching the network or \
              writing outside their run: %s."
             (List.length reasons) (List.length d.order)
             (String.concat "; " (List.sort_uniq String.compare reasons)));
      ]

let clock_warning (d : D.t) =
  match F.clock_warning d with
  | None -> []
  | Some text -> [ para ~cls:[ "warning" ] text ]

let missing_table (d : D.t) =
  table ~caption:"Missing data per arm"
    ~head:[ "Variant"; "Runs"; "Errored"; "Not run or unfinished" ]
    (List.map
       (fun (v, total, errored, not_run) ->
         ( v,
           [
             Num (string_of_int total);
             Num (string_of_int errored);
             Num (string_of_int not_run);
           ] ))
       d.missing)

let dropped_note (d : D.t) =
  match d.dropped with
  | [] -> []
  | dropped ->
      [
        para ~cls:[ "note" ]
          ("Left out of the comparison, with no finished run in one arm: "
         ^ String.concat ", " dropped
         ^ ". This counts as missing data for the gate.");
      ]

(* Verdicts *)

let verdict_table (d : D.t) =
  let row (m : D.metric_row) =
    let r = m.result in
    let estimate, ival, threshold = F.metric_texts m in
    ( F.metric r.metric,
      [
        Badge r.verdict;
        Text (if r.in_gate then "yes" else "no");
        Num estimate;
        Num ival;
        Num (Printf.sprintf "%.4f" r.level);
        Num threshold;
      ] )
  in
  table
    ~caption:
      "Candidate against baseline. Pass rate is a difference in percentage \
       points. Cost and tokens are relative changes in the scenario-weighted \
       mean, latency in the typical scenario's median."
    ~head:
      (* The verdict comes second, so it is in view on a narrow screen. *)
      [
        "Metric";
        "Verdict";
        "In gate";
        "Estimate";
        "Interval";
        "Level";
        "Threshold";
      ]
    (List.map row d.metrics)

(* Per-scenario forest *)

let pass_rate_delta (d : D.t) =
  List.find_map
    (fun (m : D.metric_row) ->
      match m.result.metric with
      | Pass_rate -> Some m.delta
      | Cost | Latency_p50 | Tokens -> None)
    d.metrics
  |> Option.value ~default:0.

let forest_section (d : D.t) =
  let pass_rate =
    List.find_map
      (fun (m : D.metric_row) ->
        match m.result.metric with
        | Pass_rate when not (Float.is_nan m.result.estimate) -> Some m.result
        | Pass_rate | Cost | Latency_p50 | Tokens -> None)
      d.metrics
  in
  let suite =
    Option.map
      (fun (r : Compare.metric_result) ->
        {
          Report_charts.label =
            Printf.sprintf "All scenarios (level %.4f)" r.level;
          estimate = r.estimate;
          interval = r.interval;
          verdict = r.verdict;
          emphasis = true;
        })
      pass_rate
  in
  let suite_note =
    match pass_rate with
    | None -> ""
    | Some r ->
        Printf.sprintf
          " The first row, all scenarios, is the %sinterval at level %.4f: %s."
          (if r.in_gate then "gate's " else "")
          r.level
          (match d.population with
          | `Suite -> "the stratified Newcombe interval"
          | `Scenarios ->
              "the hull of the hierarchical bootstrap and the stratified \
               Newcombe interval")
  in
  let rows =
    Option.to_list suite
    @ List.map
        (fun (s : D.scenario_row) ->
          {
            Report_charts.label = s.scenario;
            estimate = s.difference;
            interval = s.interval;
            verdict = s.verdict;
            emphasis = false;
          })
        d.scenarios
  in
  let level =
    match d.scenarios with
    | [] -> d.confidence
    | l ->
        Stats.bonferroni_level ~confidence:d.confidence ~family:(List.length l)
  in
  let delta = pass_rate_delta d in
  section "scenarios" "Pass rate per scenario"
    [
      para ~cls:[ "note" ]
        (Printf.sprintf
           "Per-scenario Newcombe intervals at level %.4f (Bonferroni across \
            %d scenarios). Per-scenario results never fail the gate.%s"
           level (List.length d.scenarios) suite_note);
      H.figure
        ~figcaption:
          (`Bottom
             (H.figcaption
                [
                  H.txt
                    (Printf.sprintf
                       "Point and interval of candidate minus baseline. The \
                        shaded band is ±%.1f percentage points, the dashed \
                        line is no difference."
                       (delta *. 100.));
                ]))
        [
          Report_charts.forest ~title:"Pass rate difference per scenario"
            ~threshold:delta rows;
        ];
      table ~caption:"The same results as a table"
        ~head:
          [
            "Scenario";
            "Verdict";
            "Baseline";
            "Candidate";
            "Difference";
            "Interval";
          ]
        (List.map
           (fun (s : D.scenario_row) ->
             ( s.scenario,
               [
                 Badge s.verdict;
                 Num
                   (Printf.sprintf "%s (%s)"
                      (F.share s.baseline_passes s.baseline_runs)
                      (F.rate s.baseline_passes s.baseline_runs));
                 Num
                   (Printf.sprintf "%s (%s)"
                      (F.share s.candidate_passes s.candidate_runs)
                      (F.rate s.candidate_passes s.candidate_runs));
                 Num (F.points s.difference);
                 Num (interval F.points s.interval);
               ] ))
           d.scenarios);
    ]

(* Cost and latency *)

let label (d : D.t) v =
  if String.equal v d.baseline then v ^ " (baseline)"
  else if String.equal v d.candidate then v ^ " (candidate)"
  else v

(* Per scenario and variant *)

let tokens_text x = Printf.sprintf "%.0f" x

let measure_text format (m : Arm_stats.measure option) pick =
  match m with
  | None -> "none recorded"
  | Some m -> (
      let value, ival = pick m in
      match ival with
      | Some i -> Printf.sprintf "%s (%s)" (format value) (interval format i)
      | None -> format value)

let pass_hat_k_text = function
  | [] -> "n/a"
  | ps ->
      String.concat ", "
        (List.mapi (fun i p -> Printf.sprintf "%d: %.3f" (i + 1) p) ps)

let arm_table (d : D.t) scenario (arms : D.arm_row list) =
  (* Text cells, which wrap: a value with its interval, or pass^k for every k,
     would otherwise push the other variant off a narrow screen. *)
  let col f = List.map (fun (a : D.arm_row) -> Text (f a.stats)) arms in
  let measure_rows name format (get : Arm_stats.t -> Arm_stats.measure option) =
    [
      ( name ^ ", median",
        col (fun s ->
            measure_text format (get s) (fun (m : Arm_stats.measure) ->
                (m.median, m.median_interval))) );
      ( name ^ ", 90th percentile",
        col (fun s ->
            measure_text format (get s) (fun (m : Arm_stats.measure) ->
                (m.p90, m.p90_interval))) );
    ]
  in
  table ~caption:scenario
    ~head:("Measure" :: List.map (fun (a : D.arm_row) -> label d a.variant) arms)
    ([
       ( "Finished runs",
         col (fun s ->
             Printf.sprintf "%d (%d errored, %d not run)" s.finished s.errored
               s.not_run) );
       ( "Pass rate",
         col (fun s ->
             match s.wilson with
             | Some i ->
                 Printf.sprintf "%s, %s (%s)"
                   (F.share s.passes s.finished)
                   (F.rate s.passes s.finished)
                   (interval (fun x -> Printf.sprintf "%.0f%%" (100. *. x)) i)
             | None -> "n/a") );
       ("pass^k for k = 1 to n", col (fun s -> pass_hat_k_text s.pass_hat_k));
     ]
    @ measure_rows "Cost" F.usd (fun s -> s.cost)
    @ measure_rows "Tokens" tokens_text (fun s -> s.tokens)
    @ measure_rows "Latency" F.ms (fun s -> s.latency_ms)
    @ measure_rows "Time to first token" F.ms (fun s -> s.ttft_ms))

let arms_section (d : D.t) =
  let scenarios =
    List.sort_uniq String.compare
      (List.map (fun (a : D.arm_row) -> a.scenario) d.arms)
  in
  section "arms" "Per scenario and variant"
    (para ~cls:[ "note" ]
       (Printf.sprintf
          "Over finished runs. Pass rates have Wilson score intervals, the \
           others percentile bootstrap intervals that resample whole runs, all \
           at level %.4f (Bonferroni across %d scenarios), and need at least \
           %d runs. pass^k estimates the chance that k runs in a row all pass. \
           Latency is the time spent in model calls per run, time to first \
           token is per model call."
          d.arms_level (List.length scenarios) Arm_stats.min_runs)
    :: List.map
         (fun s ->
           arm_table d s
             (List.filter
                (fun (a : D.arm_row) -> String.equal a.scenario s)
                d.arms))
         scenarios)

let measure_block (d : D.t) ~id ~title ~noun ~format
    (get : D.summary -> D.dist option) =
  let present =
    List.filter_map
      (fun (s : D.summary) -> Option.map (fun x -> (s.variant, x)) (get s))
      d.summaries
  in
  match present with
  | [] -> [ para ~cls:[ "note" ] (Printf.sprintf "No %s was recorded." noun) ]
  | _ :: _ ->
      [
        H.h3 ~a:[ H.a_id id ] [ H.txt title ];
        H.figure
          ~figcaption:
            (`Bottom
               (H.figcaption [ H.txt "Bar: median. Whisker: 90th percentile." ]))
          [
            Report_charts.bars ~title ~format
              (List.map
                 (fun (v, (x : D.dist)) ->
                   {
                     Report_charts.name = label d v;
                     median = x.median;
                     p90 = x.p90;
                   })
                 present);
          ];
        table ~caption:(title ^ " as a table")
          ~head:[ "Variant"; "Runs"; "Median"; "90th percentile" ]
          (List.map
             (fun (v, (x : D.dist)) ->
               ( label d v,
                 [
                   Num (string_of_int x.n);
                   Num (format x.median);
                   Num (format x.p90);
                 ] ))
             present);
      ]

let measures_section (d : D.t) =
  section "cost-latency" "Cost, latency and tokens"
    (para ~cls:[ "note" ]
       "Over the finished runs of the scenarios both arms finished, the runs \
        the verdicts are about. Latency is the time spent in model calls. The \
        verdict table compares scenario-weighted means of cost and tokens and \
        per-scenario medians of latency, with bootstrap intervals."
    :: (measure_block d ~id:"cost" ~title:"Cost per run" ~noun:"cost"
          ~format:F.usd (fun s -> s.cost)
       @ measure_block d ~id:"latency" ~title:"Latency per run" ~noun:"latency"
           ~format:F.ms (fun s -> s.latency)
       @ measure_block d ~id:"tokens" ~title:"Tokens per run"
           ~noun:"token count" ~format:tokens_text (fun s -> s.tokens)))

(* Behaviour *)

let signatures_table (b : D.behaviour) =
  let count (l : D.signature_count list) s =
    match
      List.find_opt
        (fun (x : D.signature_count) -> List.equal String.equal x.signature s)
        l
    with
    | Some x -> x.count
    | None -> 0
  in
  let all =
    List.map (fun (x : D.signature_count) -> x.signature) b.candidate_signatures
    @ List.map
        (fun (x : D.signature_count) -> x.signature)
        b.baseline_signatures
    |> List.fold_left
         (fun acc s ->
           if List.exists (List.equal String.equal s) acc then acc else s :: acc)
         []
    |> List.rev
  in
  table ~caption:"Tool-call sequences"
    ~head:[ "Sequence"; "Baseline"; "Candidate"; "" ]
    (List.map
       (fun s ->
         let cb = count b.baseline_signatures s
         and cc = count b.candidate_signatures s in
         ( F.signature s,
           [
             Num (Printf.sprintf "%d (%s)" cb (F.rate cb b.baseline_traced));
             Num (Printf.sprintf "%d (%s)" cc (F.rate cc b.candidate_traced));
             (if List.exists (List.equal String.equal s) b.new_signatures then
                New
              else Text "");
           ] ))
       all)

let steps_table (b : D.behaviour) =
  let steps =
    List.sort_uniq Int.compare
      (List.map fst b.baseline_steps @ List.map fst b.candidate_steps)
  in
  let n l s = Option.value (List.assoc_opt s l) ~default:0 in
  table ~caption:"Model calls per run (steps)"
    ~head:[ "Steps"; "Baseline runs"; "Candidate runs" ]
    (List.map
       (fun s ->
         ( string_of_int s,
           [
             Num (string_of_int (n b.baseline_steps s));
             Num (string_of_int (n b.candidate_steps s));
           ] ))
       steps)

let behaviour_section (d : D.t) =
  let one (b : D.behaviour) =
    let fresh = List.length b.new_signatures in
    H.details
      ~a:(if fresh > 0 then [ H.a_open () ] else [])
      (H.summary
         [
           H.txt
             (if fresh = 0 then b.scenario
              else
                Printf.sprintf "%s: %d new behaviour%s" b.scenario fresh
                  (if fresh = 1 then "" else "s"));
         ])
      [ signatures_table b; steps_table b ]
  in
  section "behaviour" "Behaviour"
    (para ~cls:[ "note" ]
       "A trajectory signature is the sequence of tool names in a run. A new \
        behaviour is a signature in at least 10% of the candidate's runs and \
        none of the baseline's. Runs without a trace are left out."
    :: List.map one d.behaviours)

(* Failures and order *)

let failure_elt (f : D.failure) =
  let r = f.run in
  let id = Option.value r.run_id ~default:"no run id" in
  let changes (dv : D.divergence) =
    H.li
      [
        H.txt dv.description;
        H.ul
          (List.map
             (fun (c : Diff.change) ->
               H.li
                 [
                   H.code
                     [
                       H.txt
                         (Printf.sprintf "%s %s"
                            (match c.kind with
                            | Changed -> "changed"
                            | Added -> "added"
                            | Removed -> "removed")
                            (if String.equal c.path "" then "(root)" else c.path));
                     ];
                 ])
             dv.changes);
      ]
  in
  H.details
    (H.summary
       [
         H.txt
           (Printf.sprintf "%s, %s, repeat %d: %s" r.variant r.scenario r.repeat
              (F.status r.status));
       ])
    ([
       para ~cls:[ "note" ] ("Run " ^ id);
       H.ul
         ~a:[ H.a_class [ "messages" ] ]
         (match f.messages with
         | [] -> [ H.li [ H.txt "No message was recorded." ] ]
         | ms -> List.map (fun m -> H.li [ H.txt m ]) ms);
     ]
    @ (match r.divergences with
      | [] -> []
      | ds -> [ H.h3 [ H.txt "Divergences" ]; H.ul (List.map changes ds) ])
    @
    match r.trace with
    | [] -> [ para ~cls:[ "note" ] "This run left no trace." ]
    | t ->
        [
          H.h3 [ H.txt "Trace" ];
          H.pre [ H.txt (String.concat "\n" (List.map F.trace_line t)) ];
        ])

let failures_section (d : D.t) =
  section "failures" "Failures"
    (match d.failures with
    | [] -> [ para "No run of either variant failed." ]
    | fs ->
        para ~cls:[ "note" ]
          (Printf.sprintf
             "%d run%s of the two variants failed, diverged or errored. \
              Errored runs are faults inside bactrian and are excluded from \
              pass rates."
             (List.length fs)
             (if List.compare_length_with fs 1 = 0 then "" else "s"))
        :: List.map failure_elt fs)

let order_section (d : D.t) =
  section "order" "Execution order"
    [
      para ~cls:[ "note" ]
        "Runs are interleaved across scenarios and variants, so drift in the \
         provider spreads over every arm.";
      H.details
        (H.summary [ H.txt (Printf.sprintf "%d runs" (List.length d.order)) ])
        [
          table ~caption:"Runs in the order they started"
            ~head:[ "#"; "Scenario"; "Variant"; "Repeat"; "Status"; "Run" ]
            (List.map
               (fun (r : D.run) ->
                 ( string_of_int r.index,
                   [
                     Text r.scenario;
                     Text r.variant;
                     Num (string_of_int r.repeat);
                     Text
                       (match r.unsandboxed with
                       | Some _ -> F.status r.status ^ ", unsandboxed"
                       | None -> F.status r.status);
                     Code (Option.value r.run_id ~default:"");
                   ] ))
               d.order);
        ];
    ]

(* Page *)

let page (d : D.t) =
  H.html
    ~a:[ H.a_lang "en" ]
    (H.head
       (H.title (H.txt ("bactrian report: " ^ d.experiment)))
       [
         H.meta ~a:[ H.a_charset "utf-8" ] ();
         H.meta
           ~a:
             [
               H.a_name "viewport";
               H.a_content "width=device-width, initial-scale=1";
             ]
           ();
         H.style [ H.txt Report_css.css ];
       ])
    (H.body
       [
         H.main
           ([ header d ]
           @ unsandboxed_warning d @ clock_warning d
           @ [
               section "verdicts" "Verdicts"
                 (verdict_table d
                  :: List.concat_map
                       (fun m -> List.map (para ~cls:[ "note" ]) (F.notes m))
                       d.metrics
                 @ (missing_table d :: dropped_note d));
               forest_section d;
               arms_section d;
               measures_section d;
               behaviour_section d;
               failures_section d;
               order_section d;
             ]);
       ])

(* The printer's own indentation would break lines inside [pre] and the
   stylesheet, so print flat and start a line between adjacent tags. Text in
   [pre] never holds a raw tag boundary, as the printer escapes it. *)
let break_between_tags html =
  let b = Buffer.create (String.length html * 11 / 10) in
  String.iteri
    (fun i c ->
      Buffer.add_char b c;
      if
        Char.equal c '>'
        && i + 1 < String.length html
        && Char.equal html.[i + 1] '<'
      then Buffer.add_char b '\n')
    html;
  Buffer.contents b

let render d =
  break_between_tags (Format.asprintf "%a@." (H.pp ~indent:false ()) (page d))
