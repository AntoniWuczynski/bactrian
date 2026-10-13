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

let violations_json : Sandbox.violations -> (string * Yojson.Safe.t) list =
  function
  | Collected vs ->
      [
        ( "sandbox_violations",
          `List
            (List.map
               (fun (v : Bactrian_core.Sandbox_log.violation) ->
                 `Assoc
                   [ ("kind", `String v.kind); ("detail", `String v.detail) ])
               vs) );
      ]
  | Not_collected reason ->
      [
        ("sandbox_violations", `Null);
        ("sandbox_violations_not_collected", `String reason);
      ]

let render_json (r : Run.result) =
  let f = r.findings in
  Yojson.Safe.to_string
    (`Assoc
       ([
          ("run_id", `String r.run_id);
          ("run_dir", `String r.run_dir);
          ( "outcome",
            `Assoc
              (("kind", `String (Run.outcome_kind r.outcome))
              ::
              (match Run.outcome_reason r.outcome with
              | Some reason -> [ ("reason", `String reason) ]
              | None -> [])) );
          ( "divergences",
            `List
              (List.map
                 (fun d -> Divergences.to_json (Divergences.of_matcher d))
                 f.divergences) );
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
          ("spend_usd", `Float r.spend);
          ("sandbox", Sandbox.label_json r.sandbox);
        ]
       @ violations_json r.violations))

let unsandboxed_line (l : Sandbox.label) =
  Printf.sprintf "warning: UNSANDBOXED (sandbox: %s%s)"
    (Sandbox.mode_name l.mode)
    (match l.reason with Some r -> ", " ^ r | None -> "")

let render_text (r : Run.result) =
  let b = Buffer.create 256 in
  let line fmt = Printf.bprintf b (fmt ^^ "\n") in
  line "run %s: %s%s" r.run_id
    (Run.outcome_kind r.outcome)
    (match Run.outcome_reason r.outcome with
    | Some reason -> " (" ^ reason ^ ")"
    | None -> "");
  line "directory: %s" r.run_dir;
  if not r.sandbox.applied then line "%s" (unsandboxed_line r.sandbox);
  (match r.violations with
  | Collected vs ->
      List.iter
        (fun (v : Bactrian_core.Sandbox_log.violation) ->
          line "sandbox violation: %s %s" v.kind v.detail)
        vs
  | Not_collected reason ->
      line "warning: sandbox violations not collected: %s" reason);
  List.iter
    (fun d ->
      line "divergence: %s" (Divergences.describe (Divergences.of_matcher d)))
    r.findings.divergences;
  List.iter (fun e -> line "error: %s" e) r.findings.errors;
  if r.spend > 0. then line "spent: %.6f USD" r.spend;
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

let render_summary_text ?sandbox ~colour (s : Executor.summary) =
  let b = Buffer.create 256 in
  let line fmt = Printf.bprintf b (fmt ^^ "\n") in
  line "experiment %s" s.experiment_id;
  Option.iter
    (fun (l : Sandbox.label) ->
      if not l.applied then
        line "%s" (paint ~colour "31" ("every run: " ^ unsandboxed_line l)))
    sandbox;
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

let render_summary_json ?sandbox (s : Executor.summary) =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("experiment_id", `String s.experiment_id);
         ( "sandbox",
           match sandbox with Some l -> Sandbox.label_json l | None -> `Null );
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
  let head =
    if Golden.passed r then paint ~colour "32" "ok"
    else paint ~colour "31" "FAIL"
  in
  let indented = List.map (fun l -> "  " ^ l ^ "\n") in
  Printf.sprintf "%s %s\n" head r.scenario
  ^ String.concat ""
      (indented r.errors
      @ indented
          (List.map
             (fun (o : Golden.offender) ->
               Printf.sprintf "%s (golden %s): %s" o.trace
                 (match o.expected with Pass -> "pass" | Fail -> "fail")
                 o.reason)
             r.offenders)
      @ indented (List.map (fun w -> "warning: " ^ w) r.warnings))
