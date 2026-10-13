open Bactrian_core
module D = Report_data
module F = Report_format

(* Left-aligned columns, two spaces apart. *)
let table rows =
  let widths =
    List.fold_left
      (fun ws row ->
        List.mapi
          (fun i cell ->
            max (String.length cell)
              (Option.value (List.nth_opt ws i) ~default:0))
          row)
      [] rows
  in
  String.concat ""
    (List.map
       (fun row ->
         let cells =
           List.mapi
             (fun i cell ->
               if i = List.length row - 1 then cell
               else
                 cell
                 ^ String.make
                     (max 0
                        (Option.value (List.nth_opt widths i) ~default:0
                        - String.length cell))
                     ' ')
             row
         in
         String.concat "  " cells ^ "\n")
       rows)

let verdict_text ~colour v =
  let code =
    match (v : Compare.verdict) with
    | Regression -> "31"
    | Improvement -> "32"
    | No_change -> "34"
    | Inconclusive _ -> "33"
  in
  let text = F.verdict v in
  if colour then Printf.sprintf "\027[%sm%s\027[0m" code text else text

let render_compare_text ~colour (d : D.t) =
  let b = Buffer.create 1024 in
  let line fmt = Printf.bprintf b (fmt ^^ "\n") in
  line "experiment %s (%s): candidate %s against baseline %s" d.experiment
    d.experiment_id d.candidate d.baseline;
  line "estimand %s, confidence %.0f%%, seed %d, %d resamples"
    (match d.population with `Suite -> "suite" | `Scenarios -> "scenarios")
    (d.confidence *. 100.) d.seed d.resamples;
  line
    "cost and tokens compare scenario-weighted means, latency the geometric \
     mean of per-scenario ratios of medians";
  line "gate: %s (level %.4f per interval)"
    (match d.gate with
    | [] -> "none"
    | g -> String.concat ", " (List.map fst g))
    d.gate_level;
  line "";
  (* The verdict is last, so its colour codes do not pad the other columns. *)
  Buffer.add_string b
    (table
       ([
          "metric";
          "gate";
          "estimate";
          "interval";
          "level";
          "threshold";
          "verdict";
        ]
       :: List.map
            (fun (m : D.metric_row) ->
              let estimate, interval, threshold = F.metric_texts m in
              [
                F.metric m.result.metric;
                (if m.result.in_gate then "yes" else "no");
                estimate;
                interval;
                Printf.sprintf "%.4f" m.result.level;
                threshold;
                verdict_text ~colour m.result.verdict;
              ])
            d.metrics));
  List.iter
    (fun m ->
      List.iter
        (fun f -> Option.iter (line "%s") (f m))
        [ F.undefined_text; F.extra_repeats_text ])
    d.metrics;
  line "";
  Buffer.add_string b
    (table
       ([
          "scenario";
          "baseline";
          "candidate";
          "difference";
          "interval";
          "verdict";
        ]
       :: List.map
            (fun (s : D.scenario_row) ->
              [
                s.scenario;
                F.share s.baseline_passes s.baseline_runs;
                F.share s.candidate_passes s.candidate_runs;
                F.points s.difference;
                F.interval F.points s.interval;
                verdict_text ~colour s.verdict;
              ])
            d.scenarios));
  line "";
  List.iter
    (fun (v, total, errored, not_run) ->
      line "%s: %d runs, %d errored, %d not run" v total errored not_run)
    d.missing;
  (match d.dropped with
  | [] -> ()
  | dropped ->
      line "left out, no finished run in one arm: %s"
        (String.concat ", " dropped));
  List.iter
    (fun (bh : D.behaviour) ->
      List.iter
        (fun s -> line "new behaviour in %s: %s" bh.scenario (F.signature s))
        bh.new_signatures)
    d.behaviours;
  line "gate status: %d (%s)" d.exit (F.exit_meaning d.exit);
  Buffer.contents b

let render_compare_json d = Yojson.Safe.to_string (D.summary_json d) ^ "\n"

(* Plan *)

let population_name = function `Suite -> "suite" | `Scenarios -> "scenarios"
let target_name a = D.verdict_name (Plan_sim.target a)

let render_plan_text (a : Plan_sim.assumption) answer =
  let scenarios = List.length a.baseline_rates in
  Printf.sprintf
    "target verdict %s: assumed drop %.3f against threshold %.3f, gate of %d \
     metric%s\n"
    (target_name a) a.true_effect a.threshold a.gate_metrics
    (if a.gate_metrics = 1 then "" else "s")
  ^
  match answer with
  | Plan_sim.Repeats { n; achieved_power; mc_se } ->
      Printf.sprintf
        "%d repeats per scenario and variant (%d scenarios, %d runs per variant)\n\
         simulated power %.3f, Monte Carlo standard error %.3f\n"
        n scenarios (n * scenarios) achieved_power mc_se
  | Unreachable { best_power } ->
      Printf.sprintf
        "unreachable: no repeat count up to %d reaches power %.2f (best \
         simulated power %.3f)\n"
        a.max_repeats a.power best_power

let render_plan_json (a : Plan_sim.assumption) answer =
  let float = Json_float.to_json in
  Yojson.Safe.to_string
    (`Assoc
       [
         ("schema_version", `Int D.schema_version);
         ( "assumptions",
           `Assoc
             [
               ( "baseline_rates",
                 `List
                   (List.map
                      (fun (s, r) ->
                        `Assoc [ ("scenario", `String s); ("rate", float r) ])
                      a.baseline_rates) );
               ("effect", float a.true_effect);
               ("threshold", float a.threshold);
               ( "between_sd",
                 match a.between_sd with Some s -> float s | None -> `Null );
               ("population", `String (population_name a.population));
               ("confidence", float a.confidence);
               ("gate_metrics", `Int a.gate_metrics);
               ("power", float a.power);
               ("max_repeats", `Int a.max_repeats);
               ("seed", `Int a.seed);
               ("simulations", `Int a.simulations);
             ] );
         ("target", `String (target_name a));
         ( "answer",
           match answer with
           | Plan_sim.Repeats { n; achieved_power; mc_se } ->
               `Assoc
                 [
                   ("result", `String "repeats");
                   ("repeats", `Int n);
                   ("achieved_power", float achieved_power);
                   ("mc_se", float mc_se);
                 ]
           | Unreachable { best_power } ->
               `Assoc
                 [
                   ("result", `String "unreachable");
                   ("best_power", float best_power);
                 ] );
       ])
  ^ "\n"
