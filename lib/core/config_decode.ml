open Config_error

type 'a decoded = ('a, error list) result
type 'a decoder = path:string -> Canon.t -> 'a decoded

let fail ~path message = Error [ { path; message } ]
let ( let+ ) r f = Result.map f r

let ( and+ ) a b =
  match (a, b) with
  | Ok x, Ok y -> Ok (x, y)
  | Error e, Ok _ | Ok _, Error e -> Error e
  | Error e1, Error e2 -> Error (e1 @ e2)

let ( let* ) r f = Result.bind r f

let nest base sub =
  if String.equal base "" then sub
  else if String.equal sub "" then base
  else if sub.[0] = '[' then base ^ sub
  else base ^ "." ^ sub

let plain_key key =
  key <> ""
  && String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true | _ -> false)
       key

let field_path base key =
  nest base (if plain_key key then key else Printf.sprintf "[%S]" key)

let index_path base i = nest base (Printf.sprintf "[%d]" i)

let collect results =
  List.fold_right
    (fun r acc ->
      let+ x = r and+ xs = acc in
      x :: xs)
    results (Ok [])

let describe : Canon.t -> string = function
  | Null -> "null"
  | Bool _ -> "a boolean"
  | Int _ | Big_int _ | Float _ -> "a number"
  | String _ -> "a string"
  | List _ -> "a list"
  | Object _ -> "an object"

let mismatch ~path expected c =
  fail ~path (Printf.sprintf "expected %s, found %s" expected (describe c))

let string ~path c =
  match Canon.to_string_opt c with
  | Some s -> Ok s
  | None -> mismatch ~path "a string" c

let non_empty_string ~path c =
  let* s = string ~path c in
  if String.equal s "" then fail ~path "must not be empty" else Ok s

let int ~path c =
  match Canon.to_int_opt c with
  | Some n -> Ok n
  | None -> mismatch ~path "an integer" c

let int_at_least min ~path c =
  let* n = int ~path c in
  if n >= min then Ok n
  else fail ~path (Printf.sprintf "must be at least %d, found %d" min n)

let float ~path c =
  match (c : Canon.t) with
  | Int n -> Ok (Float.of_int n)
  | Float f -> Ok f
  | Null | Bool _ | Big_int _ | String _ | List _ | Object _ ->
      mismatch ~path "a number" c

let float_at_least min ~path c =
  let* f = float ~path c in
  if f >= min then Ok f
  else fail ~path (Printf.sprintf "must be at least %g, found %g" min f)

let bool ~path c =
  match Canon.to_bool_opt c with
  | Some b -> Ok b
  | None -> mismatch ~path "a boolean" c

let seconds_of_duration s =
  let n = String.length s in
  if n < 2 then None
  else
    let factor =
      match s.[n - 1] with 's' -> Some 1.0 | 'm' -> Some 60.0 | _ -> None
    in
    let factor, unit_len =
      if n > 2 && String.ends_with ~suffix:"ms" s then (Some 0.001, 2)
      else (factor, 1)
    in
    let digits = String.sub s 0 (n - unit_len) in
    let numeric =
      String.for_all (function '0' .. '9' | '.' -> true | _ -> false) digits
    in
    match (factor, numeric, float_of_string_opt digits) with
    | Some k, true, Some v when v > 0.0 -> Some (v *. k)
    | _ -> None

let duration ~path c =
  let* s = string ~path c in
  match seconds_of_duration s with
  | Some v -> Ok v
  | None ->
      fail ~path
        (Printf.sprintf
           "invalid duration %S, expected a positive number with unit ms, s or \
            m, such as \"250ms\", \"120s\" or \"2m\""
           s)

let regex ~path c =
  let* s = string ~path c in
  match Regex.compile s with
  | Ok _ -> Ok s
  | Error reason ->
      fail ~path (Printf.sprintf "invalid regular expression %S: %s" s reason)

let enum ~what cases ~path c =
  let* s = string ~path c in
  match List.assoc_opt s cases with
  | Some v -> Ok v
  | None ->
      fail ~path
        (Printf.sprintf "unknown %s %S, expected one of: %s" what s
           (String.concat ", " (List.map fst cases)))

let version expected ~path c =
  let* n = int ~path c in
  if n = expected then Ok n
  else
    fail ~path (Printf.sprintf "unsupported version %d, expected %d" n expected)

let list dec ~path c =
  match Canon.to_list_opt c with
  | None -> mismatch ~path "a list" c
  | Some items ->
      collect (List.mapi (fun i x -> dec ~path:(index_path path i) x) items)

let non_empty_list dec ~path c =
  let* items = list dec ~path c in
  match items with
  | [] -> fail ~path "must contain at least one item"
  | _ :: _ -> Ok items

let assoc dec ~path c =
  match Canon.to_fields_opt c with
  | None -> mismatch ~path "an object" c
  | Some fields ->
      collect
        (List.map
           (fun (k, v) ->
             let+ x = dec ~path:(field_path path k) v in
             (k, x))
           fields)

type reader = {
  rpath : string;
  value : Canon.t;
  mutable seen : string list;
  mutable lenient : bool;
}

let path r = r.rpath
let ignore_rest r = r.lenient <- true

let object_ ~path c f =
  match Canon.to_fields_opt c with
  | None -> mismatch ~path "an object" c
  | Some fields -> (
      let r = { rpath = path; value = c; seen = []; lenient = false } in
      let result = f r in
      let unknown =
        if r.lenient then []
        else
          List.filter_map
            (fun (k, _) ->
              if List.exists (String.equal k) r.seen then None
              else Some { path = field_path path k; message = "unknown field" })
            fields
      in
      match (result, unknown) with
      | Ok v, [] -> Ok v
      | Ok _, errs -> Error errs
      | Error e, errs -> Error (e @ errs))

let lookup r key =
  r.seen <- key :: r.seen;
  Canon.member key r.value

let required r key dec =
  match lookup r key with
  | Some v -> dec ~path:(field_path r.rpath key) v
  | None -> fail ~path:(field_path r.rpath key) "missing required field"

let optional r key dec =
  match lookup r key with
  | Some v -> Result.map Option.some (dec ~path:(field_path r.rpath key) v)
  | None -> Ok None

let default r key d dec =
  Result.map (Option.value ~default:d) (optional r key dec)
