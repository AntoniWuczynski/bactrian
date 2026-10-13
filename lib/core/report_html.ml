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

(* The first cell of each row is its header. *)
let table ~caption ~head rows =
  H.div
    ~a:[ H.a_class [ "scroll" ] ]
    [
      H.table
        ~caption:(H.caption [ H.txt caption ])
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
  match d.gate with
  | [] -> "none"
  | gate ->
      String.concat ", "
        (List.map
           (fun (m, delta) ->
             Printf.sprintf "%s (threshold %.1f%s)" m (delta *. 100.)
               (if String.equal m "pass_rate" then " percentage points" else "%"))
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

(* Verdicts *)

let verdict_table (d : D.t) =
  let row (m : D.metric_row) =
    let r = m.result in
    let estimate, ival, threshold = F.metric_texts m in
    ( F.metric r.metric,
      [
        Text (if r.in_gate then "yes" else "no");
        Num estimate;
        Num ival;
        Num (Printf.sprintf "%.4f" r.level);
        Num threshold;
        Badge r.verdict;
      ] )
  in
  table
    ~caption:
      "Candidate against baseline. Pass rate is a difference in percentage \
       points. The others are relative changes in the median."
    ~head:
      [
        "Metric";
        "In gate";
        "Estimate";
        "Interval";
        "Level";
        "Threshold";
        "Verdict";
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
  let suite =
    List.find_map
      (fun (m : D.metric_row) ->
        match m.result.metric with
        | Pass_rate when not (Float.is_nan m.result.estimate) ->
            Some
              {
                Report_charts.label = "All scenarios";
                estimate = m.result.estimate;
                interval = m.result.interval;
                verdict = m.result.verdict;
                emphasis = true;
              }
        | Pass_rate | Cost | Latency_p50 | Tokens -> None)
      d.metrics
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
           "Newcombe intervals at level %.4f (Bonferroni across %d scenarios). \
            Per-scenario results never fail the gate."
           level (List.length d.scenarios));
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
            "Baseline";
            "Candidate";
            "Difference";
            "Interval";
            "Verdict";
          ]
        (List.map
           (fun (s : D.scenario_row) ->
             ( s.scenario,
               [
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
                 Badge s.verdict;
               ] ))
           d.scenarios);
    ]

(* Cost and latency *)

let label (d : D.t) v =
  if String.equal v d.baseline then v ^ " (baseline)"
  else if String.equal v d.candidate then v ^ " (candidate)"
  else v

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
  section "cost-latency" "Cost and latency"
    (para ~cls:[ "note" ]
       "Over finished runs. Latency is the time spent in model calls. The \
        verdict table compares medians with bootstrap intervals."
    :: (measure_block d ~id:"cost" ~title:"Cost per run" ~noun:"cost"
          ~format:F.usd (fun s -> s.cost)
       @ measure_block d ~id:"latency" ~title:"Latency per run" ~noun:"latency"
           ~format:F.ms (fun s -> s.latency)))

(* Behaviour *)

let signatures_table (b : D.behaviour) =
  let count (l : D.signature_count list) s =
    match List.find_opt (fun (x : D.signature_count) -> x.signature = s) l with
    | Some x -> x.count
    | None -> 0
  in
  let all =
    List.map (fun (x : D.signature_count) -> x.signature) b.candidate_signatures
    @ List.map
        (fun (x : D.signature_count) -> x.signature)
        b.baseline_signatures
    |> List.fold_left (fun acc s -> if List.mem s acc then acc else s :: acc) []
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
             (if List.mem s b.new_signatures then New else Text "");
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
                     Text (F.status r.status);
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
           [
             header d;
             section "verdicts" "Verdicts" [ verdict_table d; missing_table d ];
             forest_section d;
             measures_section d;
             behaviour_section d;
             failures_section d;
             order_section d;
           ];
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
