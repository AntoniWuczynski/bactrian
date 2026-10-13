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
