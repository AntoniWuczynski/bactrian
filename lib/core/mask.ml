let marker = "<normalised>"

(* Placeholders such as [<secret:1>] are left alone, or a pattern like [\d+]
   would turn two different secrets into one marker and so one key. *)
let text patterns s =
  let mask re n span =
    match List.length (Re.all re span) with
    | 0 -> (n, span)
    | k -> (n + k, Re.replace_string re ~by:marker span)
  in
  let n, s =
    List.fold_left
      (fun (n, s) re -> Redact.fold_plain (mask re) n s)
      (0, s) patterns
  in
  (n, s)

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
