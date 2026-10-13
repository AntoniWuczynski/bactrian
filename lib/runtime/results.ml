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
  | Bad_experiment of Config_loader.located_error list

let pp_error ppf = function
  | No_such_experiment id -> Format.fprintf ppf "no experiment %S" id
  | Bad_manifest m -> Format.fprintf ppf "%s" m
  | Bad_run { run_id; file; reason } ->
      Format.fprintf ppf "run %s: %s: %s" run_id file reason
  | Bad_experiment errors ->
      Format.pp_print_list ~pp_sep:Format.pp_force_newline
        (fun ppf e -> Format.pp_print_string ppf (Config_loader.located_line e))
        ppf errors

let ( let* ) = Result.bind

(* Files *)

(* [Ok None] when the run did not write the file. Any other failure to read
   it is an error naming the file. *)
let read_opt dir file =
  match Eio.Path.load Eio.Path.(dir / file) with
  | text -> Ok (Some text)
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> Ok None
  | exception (Eio.Io _ as e) -> Error (file, Format.asprintf "%a" Eio.Exn.pp e)

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
  match read_opt dir "verdicts.json" with
  | Error e -> Error e
  | Ok None -> Ok []
  | Ok (Some text) -> (
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
  match read_opt dir "trace.jsonl" with
  | Error e -> Error e
  | Ok None -> Ok []
  | Ok (Some text) ->
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

(* What a run wrote about itself: cost from the ledger's attribution, and the
   latency and token totals of its model calls. A run without a meta.json (it
   crashed before writing one) has none of them. meta.json holds much more
   (run ID, outcome, patterns, unenforced settings), so this decoder reads
   only these three fields of it and lets the others pass. *)
type meta = {
  cost_usd : float option; [@default None]
  latency_ms : float option; [@default None]
  tokens : int option; [@default None]
}
[@@deriving of_yojson { strict = false }]

let read_meta dir =
  match read_opt dir "meta.json" with
  | Error e -> Error e
  | Ok None -> Ok { cost_usd = None; latency_ms = None; tokens = None }
  | Ok (Some text) -> (
      match Yojson.Safe.from_string text with
      | exception Yojson.Json_error m -> Error ("meta.json", m)
      | json -> (
          match meta_of_yojson json with
          | Ok m -> Ok m
          | Error m -> Error ("meta.json", m)))

let read_divergences dir : (D.divergence list, string * string) result =
  match read_opt dir "divergences.json" with
  | Error e -> Error e
  | Ok None -> Ok []
  | Ok (Some text) -> (
      match Yojson.Safe.from_string text with
      | exception Yojson.Json_error m -> Error ("divergences.json", m)
      | json -> (
          match Divergences.of_file_json json with
          | Error m -> Error ("divergences.json", m)
          | Ok ds ->
              Ok
                (List.map
                   (fun (d : Divergences.t) ->
                     {
                       D.description = Divergences.describe d;
                       changes = d.changes;
                     })
                   ds)))

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

let load_run ~root ((p : Executor.planned), status, run_id) =
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
  | Some id ->
      let dir = Eio.Path.(root / "runs" / id) in
      let bad (file, reason) = Bad_run { run_id = id; file; reason } in
      let* checks = Result.map_error bad (read_checks dir) in
      let* trace = Result.map_error bad (read_trace dir) in
      let* meta = Result.map_error bad (read_meta dir) in
      let* divergences = Result.map_error bad (read_divergences dir) in
      Ok
        {
          base with
          checks;
          trace;
          divergences;
          cost = meta.cost_usd;
          latency_ms = meta.latency_ms;
          tokens = meta.tokens;
        }

(* Loading *)

let valid_id id =
  not
    (String.equal id "" || String.equal id "." || String.equal id ".."
   || String.contains id '/' || String.contains id '\000')

(* [None] when the file no longer exists. Any other problem with it is an
   error, so that a mistake in the file is shown where it is rather than as a
   missing setting. *)
let load_experiment ~fs file =
  let path = Eio.Path.(fs / file) in
  match Eio.Path.kind ~follow:true path with
  | `Not_found -> Ok None
  | `Unknown | `Fifo | `Character_special | `Directory | `Block_device
  | `Regular_file | `Symbolic_link | `Socket -> (
      match Config_loader.load_experiment path with
      | Ok e -> Ok (Some e)
      | Error errors -> Error (Bad_experiment errors))

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
    let* experiment = load_experiment ~fs manifest.experiment_file in
    let* runs =
      List.fold_left
        (fun acc r ->
          let* l = acc in
          let* run = load_run ~root r in
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
        match Compare.threshold_of m declared with
        | Some delta -> Ok ((m, delta) :: l)
        | None ->
            Error
              (Printf.sprintf
                 "gate metric %s has no entry in compare.thresholds" name))
      (Ok []) c.gate
    |> Result.map List.rev
  in
  Ok
    ( gate,
      List.filter
        (fun (m, _) -> Option.is_none (Compare.threshold_of m gate))
        declared )

(* Why the experiment file cannot supply a setting, when it cannot. *)
let unavailable (l : loaded) =
  match l.experiment with
  | None ->
      Printf.sprintf "the experiment file %s that the manifest records is gone"
        l.manifest.experiment_file
  | Some _ ->
      Printf.sprintf "the experiment file %s has no compare section"
        l.manifest.experiment_file

let choose l ~what ~flag ~from_experiment =
  match (flag, from_experiment) with
  | Some v, _ | None, Some v -> Ok v
  | None, None ->
      Error (Printf.sprintf "--%s is required: %s" what (unavailable l))

let settings (l : loaded) ~baseline ~candidate ~fail_on_inconclusive =
  let compare =
    Option.bind l.experiment (fun (e : Experiment.t) -> e.compare)
  in
  let* baseline =
    choose l ~what:"baseline" ~flag:baseline
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
    choose l ~what:"candidate" ~flag:candidate
      ~from_experiment:(match others with [ only ] -> Some only | _ -> None)
  in
  let variants = List.map (fun (r : D.run) -> r.variant) l.runs in
  let* () =
    if String.equal baseline candidate then
      Error "the baseline and the candidate are the same variant"
    else
      match
        List.find_opt
          (fun v -> not (List.exists (String.equal v) variants))
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
    | None ->
        Error
          (Printf.sprintf
             "gate metric pass_rate has no threshold, because %s, and a \
              threshold is never assumed"
             (unavailable l))
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
