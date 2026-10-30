type libc = Musl | Glibc

let libc_name = function Musl -> "musl" | Glibc -> "glibc"
let glibc_floor = "2.34"

type report = { glibc_2_34 : bool; executable : string option }

type wire = { glibc_2_34 : bool; found : bool; prefix : string }
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
      | Ok { glibc_2_34; found = false; prefix = _ } ->
          Ok { glibc_2_34; executable = None }
      | Ok { glibc_2_34; found = true; prefix } -> (
          match unhex prefix with
          | Some bytes -> Ok { glibc_2_34; executable = Some bytes }
          | None -> Error "the executable's prefix is not hexadecimal"))

let libc { glibc_2_34; executable } =
  let loader prefix =
    Option.map Filename.basename (Elf_probe.interpreter prefix)
  in
  match Option.bind executable loader with
  | Some name when String.starts_with ~prefix:"ld-musl-" name -> Some Musl
  | Some name when String.starts_with ~prefix:"ld-linux" name ->
      if glibc_2_34 then Some Glibc else None
  | Some _ | None -> None
