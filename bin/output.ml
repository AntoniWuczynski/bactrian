open Bactrian_app
open Bactrian_runtime

let unsandboxed_line (l : Sandbox.label) =
  Printf.sprintf "warning: UNSANDBOXED (sandbox: %s%s)"
    (Sandbox.mode_name l.mode)
    (match l.reason with Some r -> ", " ^ r | None -> "")

(* Every line of a run's text holds agent-chosen text (reasons, denied paths,
   errors), so each is escaped whole. *)
let render_text (r : Run.result) =
  let b = Buffer.create 256 in
  let line fmt =
    Printf.ksprintf
      (fun text ->
        Buffer.add_string b (Bactrian_core.Printable.escape text);
        Buffer.add_char b '\n')
      fmt
  in
  line "run %s: %s%s" r.run_id
    (Run.outcome_kind r.outcome)
    (match Run.outcome_reason r.outcome with
    | Some reason -> " (" ^ reason ^ ")"
    | None -> "");
  line "directory: %s" r.run_dir;
  if not r.sandbox.applied then line "%s" (unsandboxed_line r.sandbox)
  else begin
    (match r.violations with
    | Collected _ -> line "sandbox violations: %s"
    | Not_collected _ -> line "warning: sandbox violations %s")
      (Sandbox.describe_violations r.violations);
    match r.violations with
    | Collected { items; _ } ->
        List.iter
          (fun (v : Bactrian_core.Sandbox_log.violation) ->
            line "sandbox violation: %s %s" v.kind v.detail)
          items
    | Not_collected _ -> ()
  end;
  List.iter
    (fun d ->
      line "divergence: %s" (Divergences.describe (Divergences.of_matcher d)))
    r.findings.divergences;
  List.iter (fun e -> line "error: %s" e) r.findings.errors;
  if r.spend > 0. then line "spent: %.6f USD" r.spend;
  Buffer.contents b

(* Experiments *)

let colour =
  lazy (Unix.isatty Unix.stdout && Option.is_none (Sys.getenv_opt "NO_COLOR"))

let paint ~colour code text =
  if colour then Printf.sprintf "\027[%sm%s\027[0m" code text else text

let kind_colour = function
  | "passed" -> "32"
  | "failed" | "diverged" | "errored" -> "31"
  | _ -> "33"

let render_summary_text ~colour ({ summary = s; sandbox } : Run_command.t) =
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
      let kind = Run_command.status_kind status in
      line "%3d  %s  %s  #%d  %s%s%s" p.index p.scenario p.variant p.repeat
        (paint ~colour (kind_colour kind) kind)
        (match Run_command.status_reason status with
        | Some r -> " (" ^ Bactrian_core.Printable.escape r ^ ")"
        | None -> "")
        (match run_id with Some id -> "  " ^ id | None -> ""))
    s.runs;
  let kinds =
    List.sort_uniq String.compare
      (List.map (fun (_, st, _) -> Run_command.status_kind st) s.runs)
  in
  line "%s"
    (String.concat ", "
       (List.map
          (fun k ->
            Printf.sprintf "%d %s"
              (List.length
                 (List.filter
                    (fun (_, st, _) ->
                      String.equal (Run_command.status_kind st) k)
                    s.runs))
              k)
          kinds));
  Buffer.contents b

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

let render_check_text ~colour (c : Check_command.t) =
  let unsandboxed =
    match c.sandbox with
    | Some l when not l.applied ->
        paint ~colour "31" ("every checker: " ^ unsandboxed_line l) ^ "\n"
    | Some _ | None -> ""
  in
  unsandboxed
  ^ String.concat "" (List.map (render_golden_text ~colour) c.reports)
