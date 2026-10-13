(* Prints the fixture's JSON or HTML report, or writes the fixture experiment
   as the executor would have left it (a manifest, and a run directory per
   started run, with its meta.json and divergences.json) under DIR/.bactrian, or
   the experiment file it names, for the command-line tests. *)

open Bactrian_core
module R = Report_data

let experiment_id = "20261013T090000Z-0badc0de"

let write path text =
  let rec mkdirs d =
    if not (Sys.file_exists d) then (
      mkdirs (Filename.dirname d);
      Sys.mkdir d 0o755)
  in
  mkdirs (Filename.dirname path);
  Out_channel.with_open_text path (fun oc -> output_string oc text)

let kind : R.status -> string = function
  | Passed -> "passed"
  | Failed -> "failed"
  | Diverged -> "diverged"
  | Errored -> "errored"
  | Not_run -> "not_run"
  | Pending | Interrupted -> "pending"

let manifest_run (r : R.run) : Yojson.Safe.t =
  `Assoc
    [
      ("index", `Int r.index);
      ("scenario", `String r.scenario);
      ("variant", `String r.variant);
      ("repeat", `Int r.repeat);
      ("status", `String "done");
      ("outcome", `String (kind r.status));
      ("reason", match r.reason with Some s -> `String s | None -> `Null);
      ("run_id", match r.run_id with Some s -> `String s | None -> `Null);
    ]

let verdicts (r : R.run) : Yojson.Safe.t =
  let item (c : R.check) : Yojson.Safe.t =
    `Assoc
      ([
         ("name", `String c.name);
         ( "verdict",
           `String
             (match c.status with
             | Check_pass -> "pass"
             | Check_fail -> "fail"
             | Check_error -> "checker_failed") );
       ]
      @ match c.reason with Some s -> [ ("reason", `String s) ] | None -> [])
  in
  `Assoc
    [
      ("passed", `Bool (r.status = Passed));
      ("items", `List (List.map item r.checks));
    ]

let number f = function Some v -> f v | None -> `Null

(* What the executor's run writes: the totals Results reads back. *)
let meta (r : R.run) : Yojson.Safe.t =
  `Assoc
    [
      ("cost_usd", number (fun c -> `Float c) r.cost);
      ("latency_ms", number (fun l -> `Float l) r.latency_ms);
      ("tokens", number (fun n -> `Int n) r.tokens);
    ]

let divergences (r : R.run) : Yojson.Safe.t =
  `Assoc
    [
      ("version", `Int 1);
      ( "divergences",
        `List
          (List.map
             (fun (d : R.divergence) ->
               `Assoc
                 [
                   ("kind", `String "missing_request");
                   ("seq", `Int 4);
                   ("similarity", `Float 0.8);
                   ( "changes",
                     `List
                       (List.map
                          (fun (c : Diff.change) ->
                            `Assoc
                              [
                                ("path", `String c.path);
                                ( "change",
                                  `String
                                    (match c.kind with
                                    | Changed -> "changed"
                                    | Added -> "added"
                                    | Removed -> "removed") );
                              ])
                          d.changes) );
                 ])
             r.divergences) );
    ]

(* The experiment file the manifest names, with the compare section that
   gives the gate its thresholds. *)
let experiment_file dir =
  write
    (Filename.concat dir "demo.yaml")
    "version: 1\n\
     name: demo\n\
     agent: { protocol: command, command: [agent] }\n\
     scenarios: [scenarios/*.yaml]\n\
     variants: { a: {}, b: {} }\n\
     seed: 7\n\
     compare:\n\
    \  baseline: a\n\
    \  thresholds: { pass_rate: 0.05 }\n"

(* [b-fails]: every run of variant b fails. [clean]: nothing is missing. *)
let transform mode (r : R.run) : R.run =
  match mode with
  | "b-fails" when String.equal r.variant "b" ->
      { r with status = Failed; reason = Some "forced" }
  | "clean" -> (
      match r.status with
      | Errored | Not_run -> { r with status = Passed; reason = None }
      | Passed | Failed | Diverged | Pending | Interrupted -> r)
  | _ -> r

let store dir mode =
  let runs = List.map (transform mode) Fixture.runs in
  let root = Filename.concat dir ".bactrian" in
  write
    (Filename.concat root
       (Printf.sprintf "experiments/%s/manifest.json" experiment_id))
    (Yojson.Safe.to_string
       (`Assoc
          [
            ("version", `Int 1);
            ("experiment_id", `String experiment_id);
            ("experiment", `String "demo");
            ("experiment_file", `String "demo.yaml");
            ("seed", `Int 7);
            ( "fingerprint",
              `Assoc
                [
                  ("experiment", `String "x");
                  ("agent", `String "x");
                  ("scenarios", `List []);
                ] );
            ("runs", `List (List.map manifest_run runs));
          ]));
  List.iter
    (fun (r : R.run) ->
      match r.run_id with
      | Some id when r.trace <> [] ->
          let d = Filename.concat root ("runs/" ^ id) in
          write
            (Filename.concat d "trace.jsonl")
            (String.concat ""
               (List.map (fun e -> Trace.to_line e ^ "\n") r.trace));
          write
            (Filename.concat d "verdicts.json")
            (Yojson.Safe.to_string (verdicts r));
          write (Filename.concat d "meta.json") (Yojson.Safe.to_string (meta r));
          write
            (Filename.concat d "divergences.json")
            (Yojson.Safe.to_string (divergences r))
      | Some _ | None -> ())
    runs

let () =
  match Sys.argv with
  | [| _; "json" |] ->
      print_endline (Yojson.Safe.pretty_to_string (R.to_json (Fixture.data ())))
  | [| _; "html" |] -> print_string (Report_html.render (Fixture.data ()))
  | [| _; "experiment"; dir |] -> experiment_file dir
  | [| _; "store"; dir |] -> store dir "as-is"
  | [| _; "store"; dir; mode |] -> store dir mode
  | _ ->
      prerr_endline
        "usage: snapshot json|html|store DIR [b-fails|clean]|experiment DIR";
      exit 2
