type segment = Key of string | Index of int

let is_key_char c =
  match c with '.' | '[' | ']' | '"' | '$' -> false | _ -> true

let is_digit c = c >= '0' && c <= '9'

let take_while pred s start =
  let n = String.length s in
  let rec go i = if i < n && pred s.[i] then go (i + 1) else i in
  go start

let at s i = Printf.sprintf "at offset %d of %S" i s

let parse_bracket s i =
  (* [i] is just after '['. Returns the segment and the offset after ']'. *)
  let n = String.length s in
  if i < n && s.[i] = '"' then
    match String.index_from_opt s (i + 1) '"' with
    | None -> Error ("unterminated quoted key " ^ at s i)
    | Some close ->
        if close + 1 < n && s.[close + 1] = ']' then
          Ok (Key (String.sub s (i + 1) (close - i - 1)), close + 2)
        else Error ("expected ']' " ^ at s (close + 1))
  else
    let stop = take_while is_digit s i in
    if stop = i then Error ("expected an index or a quoted key " ^ at s i)
    else if stop < n && s.[stop] = ']' then
      match int_of_string_opt (String.sub s i (stop - i)) with
      | Some k -> Ok (Index k, stop + 1)
      | None -> Error ("index too large " ^ at s i)
    else Error ("expected ']' " ^ at s stop)

let parse s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then Ok (List.rev acc)
    else
      match s.[i] with
      | '.' ->
          let stop = take_while is_key_char s (i + 1) in
          if stop = i + 1 then Error ("expected a key after '.' " ^ at s (i + 1))
          else go stop (Key (String.sub s (i + 1) (stop - i - 1)) :: acc)
      | '[' -> (
          match parse_bracket s (i + 1) with
          | Ok (seg, next) -> go next (seg :: acc)
          | Error _ as e -> e)
      | _ -> Error ("expected '.' or '[' " ^ at s i)
  in
  if n > 0 && s.[0] = '$' then go 1 [] else Error "a JSONPath starts with '$'"

let show = function Key k -> "." ^ k | Index i -> Printf.sprintf "[%d]" i

let eval segments doc =
  let rec go walked v = function
    | [] -> Ok v
    | seg :: rest -> (
        let here = walked ^ show seg in
        let step =
          match seg with
          | Key k -> (
              match Canon.member k v with
              | Some x -> Ok x
              | None -> Error (Printf.sprintf "$%s: no field %S" here k))
          | Index i -> (
              match Canon.to_list_opt v with
              | None -> Error (Printf.sprintf "$%s: not a list" here)
              | Some l -> (
                  match List.nth_opt l i with
                  | Some x -> Ok x
                  | None -> Error (Printf.sprintf "$%s: no element %d" here i)))
        in
        match step with Ok x -> go here x rest | Error _ as e -> e)
  in
  go "" doc segments
