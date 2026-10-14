type kind = Data | Eof | Open | Close | Exit | Log
type t = { channel : int; kind : kind; payload : string }

let header_size = 9
let max_payload = 65536

let code = function
  | Data -> 0
  | Eof -> 1
  | Open -> 2
  | Close -> 3
  | Exit -> 4
  | Log -> 5

let kind_of_code = function
  | 0 -> Some Data
  | 1 -> Some Eof
  | 2 -> Some Open
  | 3 -> Some Close
  | 4 -> Some Exit
  | 5 -> Some Log
  | _ -> None

let make channel kind payload = { channel; kind; payload }

let data ~channel s =
  let n = String.length s in
  List.init
    ((n + max_payload - 1) / max_payload)
    (fun i ->
      let off = i * max_payload in
      make channel Data (String.sub s off (min max_payload (n - off))))

let eof ~channel = make channel Eof ""
let close ~channel = make channel Close ""

let u16 v =
  let b = Bytes.create 2 in
  Bytes.set_uint16_be b 0 v;
  Bytes.to_string b

let u32 v =
  let b = Bytes.create 4 in
  Bytes.set_int32_be b 0 (Int32.of_int v);
  Bytes.to_string b

let open_ ~channel ~port = make channel Open (u16 port)
let exit status = make 0 Exit (u32 status)

let log line =
  make 0 Log
    (if String.length line > max_payload then String.sub line 0 max_payload
     else line)

let port t =
  match t.kind with
  | Open when String.length t.payload = 2 ->
      Some (String.get_uint16_be t.payload 0)
  | Open | Data | Eof | Close | Exit | Log -> None

let get_u32 s off = Int32.to_int (String.get_int32_be s off) land 0xFFFF_FFFF

let status t =
  match t.kind with
  | Exit when String.length t.payload = 4 -> Some (get_u32 t.payload 0)
  | Exit | Data | Eof | Open | Close | Log -> None

let encode t =
  let n = String.length t.payload in
  let b = Bytes.create (header_size + n) in
  Bytes.set_int32_be b 0 (Int32.of_int t.channel);
  Bytes.set_uint8 b 4 (code t.kind);
  Bytes.set_int32_be b 5 (Int32.of_int n);
  Bytes.blit_string t.payload 0 b header_size n;
  Bytes.to_string b

type error = Unknown_kind of int | Too_long of int | Truncated of int

let describe_error = function
  | Unknown_kind k -> Printf.sprintf "unknown frame kind %d" k
  | Too_long n ->
      Printf.sprintf "a frame of %d bytes, more than the %d allowed" n
        max_payload
  | Truncated n -> Printf.sprintf "input ended %d bytes into a frame" n

type decoder = { pending : Buffer.t; mutable failed : error option }

let decoder () = { pending = Buffer.create 1024; failed = None }

(* Frames whole in [s] from [off], and where the unfinished rest starts. *)
let rec split s off acc =
  let left = String.length s - off in
  if left < header_size then Ok (List.rev acc, off)
  else
    let channel = get_u32 s off
    and k = String.get_uint8 s (off + 4)
    and n = get_u32 s (off + 5) in
    match kind_of_code k with
    | None -> Error (Unknown_kind k)
    | Some _ when n > max_payload -> Error (Too_long n)
    | Some kind ->
        if left < header_size + n then Ok (List.rev acc, off)
        else
          split s
            (off + header_size + n)
            (make channel kind (String.sub s (off + header_size) n) :: acc)

let feed d bytes =
  match d.failed with
  | Some e -> Error e
  | None -> (
      Buffer.add_string d.pending bytes;
      let s = Buffer.contents d.pending in
      match split s 0 [] with
      | Error e ->
          d.failed <- Some e;
          Error e
      | Ok (frames, rest) ->
          Buffer.clear d.pending;
          Buffer.add_substring d.pending s rest (String.length s - rest);
          Ok frames)

let finish d =
  match d.failed with
  | Some e -> Error e
  | None ->
      let n = Buffer.length d.pending in
      if n = 0 then Ok () else Error (Truncated n)
