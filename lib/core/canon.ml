type t =
  | Null
  | Bool of bool
  | Int of int
  | Big_int of string
  | Float of float
  | String of string
  | List of t list
  | Object of (string * t) list

type error = Syntax of string | Duplicate_key of string | Non_finite_float

let null = Null
let bool b = Bool b
let int i = Int i
let string s = String s
let list l = List l

let float f =
  if not (Float.is_finite f) then Error Non_finite_float
  else if not (Float.is_integer f) then Ok (Float f)
  else if f >= Float.of_int min_int && f < Float.of_int max_int then
    Ok (Int (Float.to_int f))
  else Ok (Big_int (Printf.sprintf "%.0f" f))

let obj fields =
  let sorted =
    List.stable_sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  let rec dup = function
    | (a, _) :: ((b, _) :: _ as rest) ->
        if String.equal a b then Some a else dup rest
    | [] | [ _ ] -> None
  in
  match dup sorted with
  | Some k -> Error (Duplicate_key k)
  | None -> Ok (Object sorted)

let rec map_result f = function
  | [] -> Ok []
  | x :: rest ->
      Result.bind (f x) (fun y ->
          Result.map (fun ys -> y :: ys) (map_result f rest))

let rec of_yojson : Yojson.Safe.t -> (t, error) result = function
  | `Null -> Ok Null
  | `Bool b -> Ok (Bool b)
  | `Int i -> Ok (Int i)
  | `Intlit s -> (
      match int_of_string_opt s with
      | Some i -> Ok (Int i)
      | None -> Ok (Big_int s))
  | `Float f -> float f
  | `String s -> Ok (String s)
  | `List l -> Result.map list (map_result of_yojson l)
  | `Assoc fields ->
      Result.bind
        (map_result
           (fun (k, v) -> Result.map (fun v -> (k, v)) (of_yojson v))
           fields)
        obj

let rec to_yojson : t -> Yojson.Safe.t = function
  | Null -> `Null
  | Bool b -> `Bool b
  | Int i -> `Int i
  | Big_int s -> `Intlit s
  | Float f -> `Float f
  | String s -> `String s
  | List l -> `List (List.map to_yojson l)
  | Object fields -> `Assoc (List.map (fun (k, v) -> (k, to_yojson v)) fields)

let of_string s =
  match Yojson.Safe.from_string s with
  | y -> of_yojson y
  | exception Yojson.Json_error msg -> Error (Syntax msg)

let to_string v =
  let buf = Buffer.create 64 in
  let rec go = function
    | Null -> Buffer.add_string buf "null"
    | Bool b -> Buffer.add_string buf (Bool.to_string b)
    | Int i -> Buffer.add_string buf (Int.to_string i)
    | Big_int s -> Buffer.add_string buf s
    | Float f -> Buffer.add_string buf (Printf.sprintf "%.17g" f)
    | String s -> Yojson.Safe.write_string buf s
    | List l ->
        Buffer.add_char buf '[';
        List.iteri
          (fun i x ->
            if i > 0 then Buffer.add_char buf ',';
            go x)
          l;
        Buffer.add_char buf ']'
    | Object fields ->
        Buffer.add_char buf '{';
        List.iteri
          (fun i (k, x) ->
            if i > 0 then Buffer.add_char buf ',';
            Yojson.Safe.write_string buf k;
            Buffer.add_char buf ':';
            go x)
          fields;
        Buffer.add_char buf '}'
  in
  go v;
  Buffer.contents buf

let tag = function
  | Null -> 0
  | Bool _ -> 1
  | Int _ -> 2
  | Big_int _ -> 3
  | Float _ -> 4
  | String _ -> 5
  | List _ -> 6
  | Object _ -> 7

let rec compare a b =
  match (a, b) with
  | Null, Null -> 0
  | Bool x, Bool y -> Bool.compare x y
  | Int x, Int y -> Int.compare x y
  | Big_int x, Big_int y -> String.compare x y
  | Float x, Float y -> Float.compare x y
  | String x, String y -> String.compare x y
  | List x, List y -> List.compare compare x y
  | Object x, Object y ->
      List.compare
        (fun (k1, v1) (k2, v2) ->
          match String.compare k1 k2 with 0 -> compare v1 v2 | c -> c)
        x y
  | ( ( Null | Bool _ | Int _ | Big_int _ | Float _ | String _ | List _
      | Object _ ),
      _ ) ->
      Int.compare (tag a) (tag b)

let equal a b = Int.equal (compare a b) 0
let digest v = Digest.BLAKE256.(to_hex (string (to_string v)))

let pp_error ppf = function
  | Syntax msg -> Format.fprintf ppf "invalid JSON: %s" msg
  | Duplicate_key k -> Format.fprintf ppf "duplicate key %S" k
  | Non_finite_float -> Format.pp_print_string ppf "non-finite float"

let to_string_opt = function
  | String s -> Some s
  | Null | Bool _ | Int _ | Big_int _ | Float _ | List _ | Object _ -> None

let to_int_opt = function
  | Int n -> Some n
  | Null | Bool _ | Big_int _ | Float _ | String _ | List _ | Object _ -> None

let to_bool_opt = function
  | Bool b -> Some b
  | Null | Int _ | Big_int _ | Float _ | String _ | List _ | Object _ -> None

let to_list_opt = function
  | List xs -> Some xs
  | Null | Bool _ | Int _ | Big_int _ | Float _ | String _ | Object _ -> None

let to_fields_opt = function
  | Object fs -> Some fs
  | Null | Bool _ | Int _ | Big_int _ | Float _ | String _ | List _ -> None

let member k v = Option.bind (to_fields_opt v) (List.assoc_opt k)
let str_member k v = Option.bind (member k v) to_string_opt
let int_member k v = Option.bind (member k v) to_int_opt
let bool_member k v = Option.bind (member k v) to_bool_opt
let list_member k v = Option.bind (member k v) to_list_opt

let set_member k x v =
  match v with
  | Object fs ->
      let rec go = function
        | [] -> [ (k, x) ]
        | ((k', _) as f) :: rest as all ->
            let c = String.compare k k' in
            if c < 0 then (k, x) :: all
            else if c = 0 then (k, x) :: rest
            else f :: go rest
      in
      Object (go fs)
  | Null | Bool _ | Int _ | Big_int _ | Float _ | String _ | List _ -> v

let remove_member k v =
  match v with
  | Object fs ->
      Object (List.filter (fun (k', _) -> not (String.equal k k')) fs)
  | Null | Bool _ | Int _ | Big_int _ | Float _ | String _ | List _ -> v

let map_fields f v =
  match v with
  | Object fs -> Object (List.map (fun (k, x) -> (k, f k x)) fs)
  | Null | Bool _ | Int _ | Big_int _ | Float _ | String _ | List _ -> v

let map_items f v =
  match v with
  | List xs -> List (List.map f xs)
  | Null | Bool _ | Int _ | Big_int _ | Float _ | String _ | Object _ -> v

let empty_object = Object []

let fold_map_children f acc v =
  match v with
  | Object fs ->
      let acc, fs' =
        List.fold_left_map
          (fun acc (k, x) ->
            let acc, x' = f acc x in
            (acc, (k, x')))
          acc fs
      in
      (acc, Object fs')
  | List xs ->
      let acc, xs' = List.fold_left_map f acc xs in
      (acc, List xs')
  | Null | Bool _ | Int _ | Big_int _ | Float _ | String _ -> (acc, v)

type segment = Key of string | Each

let path_of_string p =
  String.split_on_char '.' p
  |> List.concat_map (fun c ->
      match String.ends_with ~suffix:"[]" c with
      | true -> [ Key (String.sub c 0 (String.length c - 2)); Each ]
      | false -> [ Key c ])

let is_empty_object v =
  match v with
  | Object [] -> true
  | Object (_ :: _)
  | Null | Bool _ | Int _ | Big_int _ | Float _ | String _ | List _ ->
      false

let rec remove_at segs v =
  match (segs, v) with
  | [], _ -> v
  | [ Key k ], _ -> remove_member k v
  | Key k :: rest, Object _ -> (
      match member k v with
      | None -> v
      | Some child ->
          let child' = remove_at rest child in
          if is_empty_object child' && not (is_empty_object child) then
            remove_member k v
          else set_member k child' v)
  | Each :: rest, List _ -> map_items (remove_at rest) v
  | ( (Key _ | Each) :: _,
      ( Null | Bool _ | Int _ | Big_int _ | Float _ | String _ | List _
      | Object _ ) ) ->
      v

let of_fields fields =
  List.fold_left (fun o (k, v) -> set_member k v o) empty_object fields
