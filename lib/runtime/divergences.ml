open Bactrian_core

type kind = Missing_request | Unconsumed | Reordered | Shape_mismatch

type t = {
  kind : kind;
  seq : int option;
  similarity : float option;
  changes : Diff.change list;
}

let ( let* ) = Result.bind

let kind_name = function
  | Missing_request -> "missing_request"
  | Unconsumed -> "unconsumed"
  | Reordered -> "reordered"
  | Shape_mismatch -> "shape_mismatch"

let kind_of_name = function
  | "missing_request" -> Ok Missing_request
  | "unconsumed" -> Ok Unconsumed
  | "reordered" -> Ok Reordered
  | "shape_mismatch" -> Ok Shape_mismatch
  | other -> Error (Printf.sprintf "unknown divergence kind %S" other)

let change_name : Diff.kind -> string = function
  | Changed -> "changed"
  | Added -> "added"
  | Removed -> "removed"

let change_of_name : string -> (Diff.kind, string) result = function
  | "changed" -> Ok Changed
  | "added" -> Ok Added
  | "removed" -> Ok Removed
  | other -> Error (Printf.sprintf "unknown change %S" other)

let of_matcher : Matcher.divergence -> t = function
  | Missing_request { request = _; nearest = Some (e, changes, similarity) } ->
      {
        kind = Missing_request;
        seq = Some e.seq;
        similarity = Some similarity;
        changes;
      }
  | Missing_request { request = _; nearest = None } ->
      { kind = Missing_request; seq = None; similarity = None; changes = [] }
  | Unconsumed e ->
      { kind = Unconsumed; seq = Some e.seq; similarity = None; changes = [] }
  | Reordered { request = _; expected } ->
      {
        kind = Reordered;
        seq = Some expected.seq;
        similarity = None;
        changes = [];
      }
  | Shape_mismatch { request = _; recorded } ->
      {
        kind = Shape_mismatch;
        seq = Some recorded.seq;
        similarity = None;
        changes = [];
      }

let describe { kind; seq; similarity; changes } =
  let n = Option.value seq ~default:(-1) in
  match (kind, seq, similarity) with
  | Missing_request, Some _, Some similarity ->
      Printf.sprintf
        "missing request (nearest recorded #%d, similarity %.2f, differs at %s)"
        n similarity
        (String.concat ", "
           (List.map
              (fun (c : Diff.change) ->
                if String.equal c.path "" then "<root>" else c.path)
              changes))
  | Missing_request, (Some _ | None), (Some _ | None) ->
      "missing request (nothing left to compare)"
  | Unconsumed, _, _ -> Printf.sprintf "recorded #%d was never requested" n
  | Reordered, _, _ -> Printf.sprintf "request out of order (expected #%d)" n
  | Shape_mismatch, _, _ ->
      Printf.sprintf "streaming differs from recorded #%d" n

type change_wire = { path : string; change : string }
[@@deriving yojson { strict = true }]

type wire = {
  kind : string;
  seq : int option; [@default None]
  similarity : float option; [@default None]
  changes : change_wire list;
}
[@@deriving yojson { strict = true }]

type file_wire = { version : int; divergences : wire list }
[@@deriving yojson { strict = true }]

let to_wire ({ kind; seq; similarity; changes } : t) : wire =
  {
    kind = kind_name kind;
    seq;
    similarity;
    changes =
      List.map
        (fun (c : Diff.change) ->
          { path = c.path; change = change_name c.kind })
        changes;
  }

let to_json t = wire_to_yojson (to_wire t)

let file_json ds =
  file_wire_to_yojson { version = 1; divergences = List.map to_wire ds }

let of_wire (w : wire) =
  let* kind = kind_of_name w.kind in
  let* changes =
    List.fold_right
      (fun (c : change_wire) acc ->
        let* rest = acc in
        let* kind = change_of_name c.change in
        Ok ({ Diff.path = c.path; kind } :: rest))
      w.changes (Ok [])
  in
  Ok ({ kind; seq = w.seq; similarity = w.similarity; changes } : t)

let of_file_json json =
  let* f = file_wire_of_yojson json in
  if f.version <> 1 then Error "unsupported version"
  else
    List.fold_right
      (fun w acc ->
        let* rest = acc in
        let* d = of_wire w in
        Ok (d :: rest))
      f.divergences (Ok [])
