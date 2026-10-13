let marker = "<normalised>"

let text patterns s =
  List.fold_left
    (fun (n, s) re ->
      match List.length (Re.all re s) with
      | 0 -> (n, s)
      | k -> (n + k, Re.replace_string re ~by:marker s))
    (0, s) patterns

let rec canon patterns (v : Canon.t) =
  match v with
  | String s ->
      let n, s = text patterns s in
      (n, Canon.string s)
  | List _ | Object _ ->
      Canon.fold_map_children
        (fun total child ->
          let n, child = canon patterns child in
          (total + n, child))
        0 v
  | Null | Bool _ | Int _ | Big_int _ | Float _ -> (0, v)
