open Bactrian_core

let obj fields =
  match Canon.obj fields with
  | Ok v -> v
  | Error _ -> assert false (* every caller passes distinct literal names *)

let json : Matcher.divergence -> Canon.t = function
  | Missing_request { nearest = None; _ } ->
      obj [ ("kind", Canon.string "missing_request") ]
  | Missing_request { nearest = Some (e, changes, similarity); _ } ->
      obj
        ([
           ("kind", Canon.string "missing_request");
           ("nearest_seq", Canon.int e.seq);
           ( "diff",
             Canon.list
               (List.map (fun (c : Diff.change) -> Canon.string c.path) changes)
           );
         ]
        @
        match Canon.float similarity with
        | Ok v -> [ ("similarity", v) ]
        | Error _ -> [])
  | Reordered { expected; _ } ->
      obj
        [
          ("kind", Canon.string "reordered");
          ("expected_seq", Canon.int expected.seq);
        ]
  | Shape_mismatch { recorded; _ } ->
      obj
        [
          ("kind", Canon.string "shape_mismatch");
          ("recorded_seq", Canon.int recorded.seq);
        ]
  | Unconsumed e ->
      obj [ ("kind", Canon.string "unconsumed"); ("seq", Canon.int e.seq) ]

let describe : Matcher.divergence -> string = function
  | Missing_request { nearest = None; _ } ->
      "the request is not in the recording"
  | Missing_request { nearest = Some (e, _, sim); _ } ->
      Printf.sprintf
        "the request is not in the recording (nearest: seq %d, similarity %.3f)"
        e.seq sim
  | Reordered { expected; _ } ->
      Printf.sprintf "the request arrived out of order (expected seq %d)"
        expected.seq
  | Shape_mismatch { recorded; _ } ->
      Printf.sprintf "streaming differs from the recording (seq %d)"
        recorded.seq
  | Unconsumed e -> Printf.sprintf "seq %d was never requested" e.seq
