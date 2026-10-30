type libc = Musl | Glibc

let libc_name = function Musl -> "musl" | Glibc -> "glibc"

type report = { libc : libc option; executable : string option }

type wire = { libc : string; found : bool; prefix : string }
[@@deriving of_yojson]

let digit c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | _ -> None

let unhex s =
  let n = String.length s / 2 in
  let out = Bytes.create n in
  let rec go i =
    if i = n then Some (Bytes.to_string out)
    else
      match (digit s.[2 * i], digit s.[(2 * i) + 1]) with
      | Some hi, Some lo ->
          Bytes.set out i (Char.chr ((hi * 16) + lo));
          go (i + 1)
      | None, _ | _, None -> None
  in
  if String.length s mod 2 <> 0 then None else go 0

let of_line line =
  match Yojson.Safe.from_string line with
  | exception Yojson.Json_error m -> Error m
  | json -> (
      match wire_of_yojson json with
      | Error m -> Error m
      | Ok w -> (
          let libc =
            match w.libc with
            | "musl" -> Ok (Some Musl)
            | "glibc" -> Ok (Some Glibc)
            | "none" -> Ok None
            | other -> Error (Printf.sprintf "unknown C library %S" other)
          in
          match (libc, w.found) with
          | Error m, _ -> Error m
          | Ok libc, false -> Ok { libc; executable = None }
          | Ok libc, true -> (
              match unhex w.prefix with
              | Some bytes -> Ok { libc; executable = Some bytes }
              | None -> Error "the executable's prefix is not hexadecimal")))
