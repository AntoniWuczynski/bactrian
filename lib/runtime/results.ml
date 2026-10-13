open Bactrian_core
module D = Report_data

type loaded = {
  manifest : Executor.manifest;
  experiment : Experiment.t option;
  runs : D.run list;
}

type error =
  | No_such_experiment of string
  | Bad_manifest of string
  | Bad_run of { run_id : string; file : string; reason : string }
  | Bad_pricing of string

let pp_error ppf = function
  | No_such_experiment id -> Format.fprintf ppf "no experiment %S" id
  | Bad_manifest m -> Format.fprintf ppf "%s" m
  | Bad_run { run_id; file; reason } ->
      Format.fprintf ppf "run %s: %s: %s" run_id file reason
  | Bad_pricing m -> Format.fprintf ppf "price file: %s" m

let ( let* ) = Result.bind

(* Files *)

let read_opt path =
  match Eio.Path.load path with text -> Some text | exception Eio.Io _ -> None

type verdicts_wire_item = {
  name : string;
  verdict : string;
  reason : string option; [@default None]
}
[@@deriving of_yojson { strict = true }]

(* [passed] is only here so strict decoding accepts the file: the items carry
   the same information. *)
type verdicts_wire = { passed : bool; items : verdicts_wire_item list }
[@@deriving of_yojson { strict = true }] [@@warning "-69"]

let check_of_wire w : (D.check, string) result =
  match w.verdict with
  | "pass" -> Ok { name = w.name; status = Check_pass; reason = w.reason }
  | "fail" -> Ok { name = w.name; status = Check_fail; reason = w.reason }
  | "checker_failed" ->
      Ok { name = w.name; status = Check_error; reason = w.reason }
  | other -> Error (Printf.sprintf "unknown verdict %S" other)

let read_checks dir =
  match read_opt Eio.Path.(dir / "verdicts.json") with
  | None -> Ok []
  | Some text -> (
      match Yojson.Safe.from_string text with
      | exception Yojson.Json_error m -> Error ("verdicts.json", m)
      | json -> (
          match verdicts_wire_of_yojson json with
          | Error m -> Error ("verdicts.json", m)
          | Ok v ->
              List.fold_left
                (fun acc w ->
                  let* l = acc in
                  match check_of_wire w with
                  | Ok c -> Ok (c :: l)
                  | Error m -> Error ("verdicts.json", m))
                (Ok []) v.items
              |> Result.map List.rev))

let read_trace dir =
  match read_opt Eio.Path.(dir / "trace.jsonl") with
  | None -> Ok []
  | Some text ->
      String.split_on_char '\n' text
      |> List.filter (fun l -> not (String.equal l ""))
      |> List.mapi (fun i line -> (i + 1, line))
      |> List.fold_left
           (fun acc (n, line) ->
             let* l = acc in
             match Trace.of_line line with
             | Ok e -> Ok (e :: l)
             | Error e ->
                 Error
                   ( "trace.jsonl",
                     Format.asprintf "line %d: %a" n Trace.pp_error e ))
           (Ok [])
      |> Result.map List.rev

(* The ledger spend in meta.json: a fallback for a run whose calls cannot be
   priced from the trace. *)
let read_spend dir =
  match read_opt Eio.Path.(dir / "meta.json") with
  | None -> None
  | Some text -> (
      match Yojson.Safe.from_string text with
      | exception Yojson.Json_error _ -> None
      | `Assoc fields -> (
          match List.assoc_opt "spend_usd" fields with
          | Some (`Assoc l) ->
              let total =
                List.fold_left
                  (fun acc (_, v) ->
                    match v with
                    | `Float f -> acc +. f
                    | `Int n -> acc +. float_of_int n
                    | `Null | `Bool _ | `Intlit _ | `String _ | `List _
                    | `Assoc _ ->
                        acc)
                  0. l
              in
              if total > 0. then Some total else None
          | Some _ | None -> None)
      | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
          None)

let date_of trace =
  List.find_map
    (fun (e : Trace.entry) ->
      match e.event with
      | Run_start { sim_now; _ } when String.length sim_now >= 10 ->
          Some (String.sub sim_now 0 10)
      | Run_start _ | Turn_start _ | Model_call _ | Tool_call _ | Tool_result _
      | Http_call _ | Agent_event _ | Reply _ | Sandbox_violation _ | Done ->
          None)
    trace

let status_of : Executor.status -> D.status * string option = function
  | Pending -> (Pending, None)
  | Interrupted -> (Interrupted, None)
  | Done o -> (
      let reason = Run.outcome_reason o in
      match o with
      | Passed -> (Passed, reason)
      | Failed _ -> (Failed, reason)
      | Diverged -> (Diverged, reason)
      | Errored _ -> (Errored, reason)
      | Not_run _ -> (Not_run, reason))

let load_run ~table ~root ((p : Executor.planned), status, run_id) =
  let status, reason = status_of status in
  let base : D.run =
    {
      index = p.index;
      repeat = p.repeat;
      scenario = p.scenario;
      variant = p.variant;
      run_id;
      status;
      reason;
      checks = [];
      trace = [];
      divergences = [];
      cost = None;
      latency_ms = None;
      tokens = None;
    }
  in
  match run_id with
  | None -> Ok base
  | Some id -> (
      let dir = Eio.Path.(root / "runs" / id) in
      let bad (file, reason) = Bad_run { run_id = id; file; reason } in
      let* checks = Result.map_error bad (read_checks dir) in
      let* trace = Result.map_error bad (read_trace dir) in
      let cost =
        match (trace, date_of trace) with
        | [], _ | _, None -> None
        | _ :: _, Some date -> (
            match Checks.cost ~table ~date trace with
            | Some c -> Some c
            | None -> read_spend dir)
      in
      match trace with
      | [] -> Ok { base with checks }
      | _ :: _ ->
          Ok
            {
              base with
              checks;
              trace;
              cost;
              latency_ms = D.latency_ms trace;
              tokens = D.tokens trace;
            })

(* Loading *)

let valid_id id =
  not
    (String.equal id "" || String.equal id "." || String.equal id ".."
   || String.contains id '/' || String.contains id '\000')

let load_experiment ~fs file =
  Result.to_option (Config_loader.load_experiment Eio.Path.(fs / file))

let load_table ~fs ~experiment_file (experiment : Experiment.t option) =
  match Option.bind experiment (fun (e : Experiment.t) -> e.pricing) with
  | None -> Ok Pricing.default
  | Some rel -> (
      let path = Filename.concat (Filename.dirname experiment_file) rel in
      match Config_loader.load_pricing Eio.Path.(fs / path) with
      | Ok table -> Ok table
      | Error (e :: _) -> Error (Bad_pricing (path ^ ": " ^ e.message))
      | Error [] -> Error (Bad_pricing path))

let load ~fs ~root ~experiment_id =
  if not (valid_id experiment_id) then Error (No_such_experiment experiment_id)
  else
    let* manifest =
      match
        Executor.read_manifest
          Eio.Path.(root / "experiments" / experiment_id / "manifest.json")
      with
      | Ok m -> Ok m
      | Error None -> Error (No_such_experiment experiment_id)
      | Error (Some m) -> Error (Bad_manifest m)
    in
    let experiment = load_experiment ~fs manifest.experiment_file in
    let* table =
      load_table ~fs ~experiment_file:manifest.experiment_file experiment
    in
    let* runs =
      List.fold_left
        (fun acc r ->
          let* l = acc in
          let* run = load_run ~table ~root r in
          Ok (run :: l))
        (Ok []) manifest.summary.runs
    in
    Ok { manifest; experiment; runs = List.rev runs }

(* Settings *)

let metric_of_name name =
  match D.metric_of_name name with
  | Some m -> Ok m
  | None ->
      Error
        (Printf.sprintf
           "unknown metric %S (expected pass_rate, cost, latency_p50 or tokens)"
           name)

let thresholds (c : Experiment.compare) =
  let* declared =
    List.fold_left
      (fun acc (name, delta) ->
        let* l = acc in
        let* m = metric_of_name name in
        Ok ((m, delta) :: l))
      (Ok []) c.thresholds
    |> Result.map List.rev
  in
  let* gate =
    List.fold_left
      (fun acc name ->
        let* l = acc in
        let* m = metric_of_name name in
        match List.assoc_opt m declared with
        | Some delta -> Ok ((m, delta) :: l)
        | None ->
            Error
              (Printf.sprintf
                 "gate metric %s has no entry in compare.thresholds" name))
      (Ok []) c.gate
    |> Result.map List.rev
  in
  Ok (gate, List.filter (fun (m, _) -> not (List.mem_assoc m gate)) declared)

let choose ~what ~flag ~from_experiment =
  match (flag, from_experiment) with
  | Some v, _ | None, Some v -> Ok v
  | None, None -> Error (Printf.sprintf "--%s is required" what)

let settings (l : loaded) ~baseline ~candidate ~fail_on_inconclusive =
  let compare =
    Option.bind l.experiment (fun (e : Experiment.t) -> e.compare)
  in
  let* baseline =
    choose ~what:"baseline" ~flag:baseline
      ~from_experiment:
        (Option.map (fun (c : Experiment.compare) -> c.baseline) compare)
  in
  let others =
    match l.experiment with
    | Some e ->
        List.filter
          (fun v -> not (String.equal v baseline))
          (List.map (fun (v : Experiment.variant) -> v.name) e.variants)
    | None -> []
  in
  let* candidate =
    choose ~what:"candidate" ~flag:candidate
      ~from_experiment:(match others with [ only ] -> Some only | _ -> None)
  in
  let variants = List.map (fun (r : D.run) -> r.variant) l.runs in
  let* () =
    if String.equal baseline candidate then
      Error "the baseline and the candidate are the same variant"
    else
      match
        List.find_opt
          (fun v -> not (List.mem v variants))
          [ baseline; candidate ]
      with
      | Some v ->
          Error (Printf.sprintf "no runs of variant %S in this experiment" v)
      | None -> Ok ()
  in
  let* population, confidence, gate, report_metrics =
    match compare with
    | Some c ->
        let* gate, report_metrics = thresholds c in
        Ok (c.population, c.confidence, gate, report_metrics)
    | None -> Ok (`Suite, 0.95, [ (Compare.Pass_rate, 0.) ], [])
  in
  Ok
    {
      Compare.runs = List.map D.compare_run l.runs;
      baseline;
      candidate;
      population;
      confidence;
      gate;
      report_metrics;
      seed = l.manifest.seed;
      resamples = 10_000;
      fail_on_inconclusive;
    }
