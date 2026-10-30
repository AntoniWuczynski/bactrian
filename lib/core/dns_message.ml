type question = { id : int; name : string; qtype : int }

type error =
  | Truncated
  | Not_a_query
  | Question_count of int
  | Compressed_name
  | Bad_label of int
  | Name_too_long

let ( let* ) = Result.bind
let header_length = 12
let max_name = 255

let byte s i =
  if i >= 0 && i < String.length s then Ok (Char.code s.[i])
  else Error Truncated

let uint16 s i =
  let* hi = byte s i in
  let* lo = byte s (i + 1) in
  Ok ((hi lsl 8) lor lo)

let escape label =
  let b = Buffer.create (String.length label) in
  String.iter
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_') as c ->
          Buffer.add_char b c
      | c -> Buffer.add_string b (Printf.sprintf "\\%03d" (Char.code c)))
    label;
  Buffer.contents b

(* The labels from [i], and the offset just past the name's final zero byte.
   [wire] counts the name's bytes so far, length bytes included. *)
let rec labels s i ~wire acc =
  let* length = byte s i in
  if wire + 1 > max_name then Error Name_too_long
  else
    match length land 0xc0 with
    | 0 when length = 0 -> Ok (List.rev acc, i + 1)
    | 0 ->
        if i + 1 + length > String.length s then Error Truncated
        else
          labels s
            (i + 1 + length)
            ~wire:(wire + 1 + length)
            (String.sub s (i + 1) length :: acc)
    | 0xc0 -> Error Compressed_name
    | _ -> Error (Bad_label length)

let qr_bit = 0x80

let question_end s =
  let* id = uint16 s 0 in
  let* flags = byte s 2 in
  let* count = uint16 s 4 in
  let* () = if flags land qr_bit <> 0 then Error Not_a_query else Ok () in
  let* () = if count <> 1 then Error (Question_count count) else Ok () in
  let* names, after = labels s header_length ~wire:0 [] in
  let* qtype = uint16 s after in
  let* (_ : int) = uint16 s (after + 2) in
  let name =
    match names with
    | [] -> "."
    | _ :: _ -> String.concat "." (List.map escape names)
  in
  Ok ({ id; name; qtype }, after + 4)

let parse s = Result.map fst (question_end s)

let describe_error = function
  | Truncated -> "the message ends inside its header or question"
  | Not_a_query -> "the message is a response, not a query"
  | Question_count n -> Printf.sprintf "%d questions, not one" n
  | Compressed_name -> "the question's name uses a compression pointer"
  | Bad_label b -> Printf.sprintf "label length byte 0x%02x is reserved" b
  | Name_too_long -> Printf.sprintf "the name is longer than %d bytes" max_name

let type_name = function
  | 1 -> "A"
  | 2 -> "NS"
  | 5 -> "CNAME"
  | 6 -> "SOA"
  | 12 -> "PTR"
  | 15 -> "MX"
  | 16 -> "TXT"
  | 28 -> "AAAA"
  | 33 -> "SRV"
  | 64 -> "SVCB"
  | 65 -> "HTTPS"
  | 255 -> "ANY"
  | n -> Printf.sprintf "TYPE%d" n

let refused_code = 5

let refused s =
  match (byte s 2, byte s 3) with
  | Ok flags, Ok _ when String.length s >= header_length ->
      if flags land qr_bit <> 0 then None
      else
        let question, count =
          match question_end s with
          | Ok (_, stop) ->
              (String.sub s header_length (stop - header_length), 1)
          | Error _ -> ("", 0)
        in
        let b = Bytes.make header_length '\000' in
        Bytes.blit_string s 0 b 0 2;
        (* QR, the opcode and RD, then RA clear and the code. *)
        Bytes.set b 2 (Char.chr (qr_bit lor (flags land 0x79)));
        Bytes.set b 3 (Char.chr refused_code);
        Bytes.set b 5 (Char.chr count);
        Some (Bytes.to_string b ^ question)
  | (Ok _ | Error _), (Ok _ | Error _) -> None
