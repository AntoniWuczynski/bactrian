open Bactrian_core
open Bactrian_runtime

let exit_ok = 0
let exit_failed = 1
let exit_not_run = 2
let exit_usage = 64
let exit_environment = 69
let exit_internal = 70

let exit_code : Run.outcome -> int = function
  | Passed -> exit_ok
  | Failed _ | Diverged -> exit_failed
  | Not_run _ -> exit_not_run
  | Errored _ -> exit_internal

let change_json (c : Diff.change) =
  `Assoc
    [
      ("path", `String c.path);
      ( "change",
        `String
          (match c.kind with
          | Changed -> "changed"
          | Added -> "added"
          | Removed -> "removed") );
    ]

let divergence_json : Matcher.divergence -> Yojson.Safe.t = function
  | Missing_request { request = _; nearest } ->
      `Assoc
        (("kind", `String "missing_request")
        ::
        (match nearest with
        | Some (e, changes, similarity) ->
            [
              ( "nearest",
                `Assoc
                  [
                    ("seq", `Int e.seq);
                    ("similarity", `Float similarity);
                    ("changes", `List (List.map change_json changes));
                  ] );
            ]
        | None -> []))
  | Unconsumed e ->
      `Assoc [ ("kind", `String "unconsumed"); ("seq", `Int e.seq) ]
  | Reordered { request = _; expected } ->
      `Assoc
        [ ("kind", `String "reordered"); ("expected_seq", `Int expected.seq) ]
  | Shape_mismatch { request = _; recorded } ->
      `Assoc
        [
          ("kind", `String "shape_mismatch"); ("recorded_seq", `Int recorded.seq);
        ]

let divergence_text (d : Matcher.divergence) =
  match d with
  | Missing_request { request = _; nearest = Some (e, changes, similarity) } ->
      Printf.sprintf
        "missing request (nearest recorded #%d, similarity %.2f, differs at %s)"
        e.seq similarity
        (String.concat ", "
           (List.map
              (fun (c : Diff.change) ->
                if String.equal c.path "" then "<root>" else c.path)
              changes))
  | Missing_request { request = _; nearest = None } ->
      "missing request (nothing left to compare)"
  | Unconsumed e -> Printf.sprintf "recorded #%d was never requested" e.seq
  | Reordered { request = _; expected } ->
      Printf.sprintf "request out of order (expected #%d)" expected.seq
  | Shape_mismatch { request = _; recorded } ->
      Printf.sprintf "streaming differs from recorded #%d" recorded.seq

let render_json (r : Run.result) =
  let f = r.findings in
  Yojson.Safe.to_string
    (`Assoc
       [
         ("run_id", `String r.run_id);
         ("run_dir", `String r.run_dir);
         ( "outcome",
           `Assoc
             (("kind", `String (Run.outcome_kind r.outcome))
             ::
             (match Run.outcome_reason r.outcome with
             | Some reason -> [ ("reason", `String reason) ]
             | None -> [])) );
         ("divergences", `List (List.map divergence_json f.divergences));
         ( "findings",
           `Assoc
             [
               ("exchanges", `Int f.exchanges);
               ("transport_errors", `Int f.transport_errors);
               ("usage_gaps", `Int f.usage_gaps);
               ("refused_keys", `Int f.refused_keys);
               ("cap_refusals", `Int (List.length f.cap_refusals));
               ("errors", `List (List.map (fun e -> `String e) f.errors));
             ] );
         ( "spend_usd",
           `Assoc (List.map (fun (p, usd) -> (p, `Float usd)) r.spend) );
       ])

let render_text (r : Run.result) =
  let b = Buffer.create 256 in
  let line fmt = Printf.bprintf b (fmt ^^ "\n") in
  line "run %s: %s%s" r.run_id
    (Run.outcome_kind r.outcome)
    (match Run.outcome_reason r.outcome with
    | Some reason -> " (" ^ reason ^ ")"
    | None -> "");
  line "directory: %s" r.run_dir;
  List.iter
    (fun d -> line "divergence: %s" (divergence_text d))
    r.findings.divergences;
  List.iter (fun e -> line "error: %s" e) r.findings.errors;
  List.iter (fun (p, usd) -> line "spent: %.6f USD on %s" usd p) r.spend;
  Buffer.contents b

(* Experiments *)

let status_exit : Executor.status -> int = function
  | Pending | Interrupted | Done (Not_run _) -> exit_not_run
  | Done Passed -> exit_ok
  | Done (Failed _ | Diverged) -> exit_failed
  | Done (Errored _) -> exit_internal

let summary_exit_code (s : Executor.summary) =
  let codes = List.map (fun (_, st, _) -> status_exit st) s.runs in
  List.find_opt
    (fun c -> List.exists (Int.equal c) codes)
    [ exit_internal; exit_failed; exit_not_run ]
  |> Option.value ~default:exit_ok

let colour =
  lazy (Unix.isatty Unix.stdout && Option.is_none (Sys.getenv_opt "NO_COLOR"))

let paint ~colour code text =
  if colour then Printf.sprintf "\027[%sm%s\027[0m" code text else text

let status_kind : Executor.status -> string = function
  | Pending -> "pending"
  | Interrupted -> "interrupted"
  | Done o -> Run.outcome_kind o

let status_reason : Executor.status -> string option = function
  | Pending | Interrupted -> None
  | Done o -> Run.outcome_reason o

let kind_colour = function
  | "passed" -> "32"
  | "failed" | "diverged" | "errored" -> "31"
  | _ -> "33"

let render_summary_text ~colour (s : Executor.summary) =
  let b = Buffer.create 256 in
  let line fmt = Printf.bprintf b (fmt ^^ "\n") in
  line "experiment %s" s.experiment_id;
  List.iter
    (fun ((p : Executor.planned), status, run_id) ->
      let kind = status_kind status in
      line "%3d  %s  %s  #%d  %s%s%s" p.index p.scenario p.variant p.repeat
        (paint ~colour (kind_colour kind) kind)
        (match status_reason status with
        | Some r -> " (" ^ r ^ ")"
        | None -> "")
        (match run_id with Some id -> "  " ^ id | None -> ""))
    s.runs;
  let kinds =
    List.sort_uniq String.compare
      (List.map (fun (_, st, _) -> status_kind st) s.runs)
  in
  line "%s"
    (String.concat ", "
       (List.map
          (fun k ->
            Printf.sprintf "%d %s"
              (List.length
                 (List.filter
                    (fun (_, st, _) -> String.equal (status_kind st) k)
                    s.runs))
              k)
          kinds));
  Buffer.contents b

let render_summary_json (s : Executor.summary) =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("experiment_id", `String s.experiment_id);
         ( "runs",
           `List
             (List.map
                (fun ((p : Executor.planned), status, run_id) ->
                  `Assoc
                    ([
                       ("index", `Int p.index);
                       ("scenario", `String p.scenario);
                       ("variant", `String p.variant);
                       ("repeat", `Int p.repeat);
                       ("status", `String (status_kind status));
                     ]
                    @ (match status_reason status with
                      | Some r -> [ ("reason", `String r) ]
                      | None -> [])
                    @
                    match run_id with
                    | Some id -> [ ("run_id", `String id) ]
                    | None -> []))
                s.runs) );
       ])

let render_golden_text ~colour (r : Golden.report) =
  match r.offenders with
  | [] -> Printf.sprintf "%s %s\n" (paint ~colour "32" "ok") r.scenario
  | offenders ->
      Printf.sprintf "%s %s\n" (paint ~colour "31" "FAIL") r.scenario
      ^ String.concat ""
          (List.map
             (fun (o : Golden.offender) ->
               Printf.sprintf "  %s (golden %s): %s\n" o.trace
                 (match o.expected with Pass -> "pass" | Fail -> "fail")
                 o.reason)
             offenders)
