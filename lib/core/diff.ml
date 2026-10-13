type kind = Changed | Added | Removed
type change = { path : string; kind : kind }
type segment = Field of string | Index of int
type view = Scalar | Items of Canon.t list | Fields of (string * Canon.t) list

let view v =
  match Canon.to_list_opt v with
  | Some l -> Items l
  | None -> (
      match Canon.to_fields_opt v with Some f -> Fields f | None -> Scalar)

let render rev_path =
  let buf = Buffer.create 32 in
  List.iter
    (function
      | Field k ->
          if Buffer.length buf > 0 then Buffer.add_char buf '.';
          Buffer.add_string buf k
      | Index i -> Buffer.add_string buf (Printf.sprintf "[%d]" i))
    (List.rev rev_path);
  Buffer.contents buf

let changes a b =
  let emit kind rev_path acc = { path = render rev_path; kind } :: acc in
  let rec go rev_path a b acc =
    match (view a, view b) with
    | Scalar, Scalar ->
        if Canon.equal a b then acc else emit Changed rev_path acc
    | Items la, Items lb -> items rev_path 0 la lb acc
    | Fields fa, Fields fb -> fields rev_path fa fb acc
    | Scalar, (Items _ | Fields _)
    | Items _, (Scalar | Fields _)
    | Fields _, (Scalar | Items _) ->
        emit Changed rev_path acc
  and items rev_path i la lb acc =
    let at = Index i :: rev_path in
    match (la, lb) with
    | [], [] -> acc
    | _ :: xs, [] -> items rev_path (i + 1) xs [] (emit Removed at acc)
    | [], _ :: ys -> items rev_path (i + 1) [] ys (emit Added at acc)
    | x :: xs, y :: ys -> items rev_path (i + 1) xs ys (go at x y acc)
  and fields rev_path fa fb acc =
    match (fa, fb) with
    | [], [] -> acc
    | (k, _) :: xs, [] ->
        fields rev_path xs [] (emit Removed (Field k :: rev_path) acc)
    | [], (k, _) :: ys ->
        fields rev_path [] ys (emit Added (Field k :: rev_path) acc)
    | (ka, va) :: xs, (kb, vb) :: ys ->
        let c = String.compare ka kb in
        if c = 0 then
          fields rev_path xs ys (go (Field ka :: rev_path) va vb acc)
        else if c < 0 then
          fields rev_path xs fb (emit Removed (Field ka :: rev_path) acc)
        else fields rev_path fa ys (emit Added (Field kb :: rev_path) acc)
  in
  List.rev (go [] a b [])

(* Similarity treats empty containers as leaves. *)
let sview v =
  match view v with
  | Items [] | Fields [] -> Scalar
  | (Scalar | Items _ | Fields _) as w -> w

let rec leaves v =
  match sview v with
  | Scalar -> 1
  | Items l -> List.fold_left (fun n x -> n + leaves x) 0 l
  | Fields f -> List.fold_left (fun n (_, x) -> n + leaves x) 0 f

let rec matches a b =
  match (sview a, sview b) with
  | Scalar, Scalar -> if Canon.equal a b then 1 else 0
  | Items la, Items lb -> zip_items 0 la lb
  | Fields fa, Fields fb -> merge_fields 0 fa fb
  | Scalar, (Items _ | Fields _)
  | Items _, (Scalar | Fields _)
  | Fields _, (Scalar | Items _) ->
      0

and zip_items n la lb =
  match (la, lb) with
  | x :: xs, y :: ys -> zip_items (n + matches x y) xs ys
  | [], _ | _, [] -> n

and merge_fields n fa fb =
  match (fa, fb) with
  | (ka, va) :: xs, (kb, vb) :: ys ->
      let c = String.compare ka kb in
      if c = 0 then merge_fields (n + matches va vb) xs ys
      else if c < 0 then merge_fields n xs fb
      else merge_fields n fa ys
  | [], _ | _, [] -> n

let similarity a b =
  let total = Int.max (leaves a) (leaves b) in
  float_of_int (matches a b) /. float_of_int total
