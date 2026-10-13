type kind = Dynamic | Static | Go | Not_elf

let prefix_length = 262144
let ( let* ) = Option.bind
let pt_interp = 3
let pt_note = 4

(* NT_GO_BUILD_ID, the note type of the Go linker's build ID. *)
let nt_go_build_id = 4

(* Bounds-checked little-endian reads. *)
let u16 s off =
  if off >= 0 && off + 2 <= String.length s then
    Some (String.get_uint16_le s off)
  else None

let u32 s off =
  if off >= 0 && off + 4 <= String.length s then
    Some (Int32.to_int (String.get_int32_le s off) land 0xffff_ffff)
  else None

let u64 s off =
  if off >= 0 && off + 8 <= String.length s then
    let v = String.get_int64_le s off in
    if Int64.compare v 0L >= 0 && Int64.compare v (Int64.of_int max_int) <= 0
    then Some (Int64.to_int v)
    else None
  else None

type segment = { kind : int; offset : int; size : int }

(* The program header table of a little-endian ELF file, or [None] when the
   file is not one or the table is cut off. *)
let segments s =
  let* () = if String.starts_with ~prefix:"\x7fELF" s then Some () else None in
  let* class64 =
    match u16 s 4 with
    | Some c when c land 0xff = 2 && c lsr 8 = 1 -> Some true
    | Some c when c land 0xff = 1 && c lsr 8 = 1 -> Some false
    | Some _ | None -> None
  in
  let word off = if class64 then u64 s off else u32 s off in
  let phoff, phentsize, phnum, minimum =
    if class64 then (32, 54, 56, 56) else (28, 42, 44, 32)
  in
  let* table = word phoff in
  let* entry_size = u16 s phentsize in
  let* count = u16 s phnum in
  if entry_size < minimum then None
  else
    let entry i =
      let base = table + (i * entry_size) in
      let* kind = u32 s base in
      let* offset = word (base + if class64 then 8 else 4) in
      let* size = word (base + if class64 then 32 else 16) in
      Some { kind; offset; size }
    in
    let rec collect i acc =
      if i < 0 then Some acc
      else
        let* seg = entry i in
        collect (i - 1) (seg :: acc)
    in
    collect (count - 1) []

let align4 n = (n + 3) land lnot 3

(* Whether the notes in [stop - pos] bytes from [pos] include the Go build ID.
   Each is a 12-byte header (name size, descriptor size, type), then the name
   and the descriptor, each padded to 4 bytes. *)
let rec has_go_note s pos stop =
  let* namesz = u32 s pos in
  let* descsz = u32 s (pos + 4) in
  let* kind = u32 s (pos + 8) in
  let name = pos + 12 in
  if
    kind = nt_go_build_id && namesz = 4
    && name + 4 <= String.length s
    && String.equal (String.sub s name 4) "Go\000\000"
  then Some true
  else
    let next = name + align4 namesz + align4 descsz in
    if next + 12 > stop then Some false else has_go_note s next stop

let go_note s { kind; offset; size } =
  kind = pt_note
  && Option.value
       (has_go_note s offset (min (offset + size) (String.length s)))
       ~default:false

let probe s =
  match segments s with
  | None -> Not_elf
  | Some segs ->
      if List.exists (go_note s) segs then Go
      else if List.exists (fun seg -> seg.kind = pt_interp) segs then Dynamic
      else Static
