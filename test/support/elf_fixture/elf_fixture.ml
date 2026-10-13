let pt_load = 1
let pt_dynamic = 2
let pt_interp = 3
let pt_note = 4

(* [n] bytes of [v], least significant first. *)
let le n v = String.init n (fun i -> Char.chr ((v lsr (8 * i)) land 0xff))
let pad4 s = s ^ String.make (-String.length s land 3) '\000'

let note ~name ~kind ~desc =
  le 4 (String.length name)
  ^ le 4 (String.length desc)
  ^ le 4 kind ^ pad4 name ^ pad4 desc

let interp = "/lib64/ld-linux-x86-64.so.2\000"
let go_note = note ~name:"Go\000\000" ~kind:4 ~desc:(String.make 40 'x')
let gnu_note = note ~name:"GNU\000" ~kind:3 ~desc:(String.make 20 'y')

let elf ?(class64 = true) segments =
  let word = if class64 then le 8 else le 4 in
  let header_size, entry_size = if class64 then (64, 56) else (52, 32) in
  let ident = "\x7fELF" ^ (if class64 then "\002" else "\001") ^ "\001\001" in
  let ident = ident ^ String.make (16 - String.length ident) '\000' in
  let header =
    ident ^ le 2 2
    ^ le 2 (if class64 then 0x3e else 3)
    ^ le 4 1 ^ word 0 ^ word header_size ^ word 0 ^ le 4 0 ^ le 2 header_size
    ^ le 2 entry_size
    ^ le 2 (List.length segments)
    ^ le 2 entry_size ^ le 2 0 ^ le 2 0
  in
  let table_end = header_size + (entry_size * List.length segments) in
  let _, entries, bodies =
    List.fold_left
      (fun (offset, entries, bodies) (kind, body) ->
        let size = String.length body in
        let entry =
          if class64 then
            le 4 kind ^ le 4 0 ^ le 8 offset ^ le 8 0 ^ le 8 0 ^ le 8 size
            ^ le 8 size ^ le 8 1
          else
            le 4 kind ^ le 4 offset ^ le 4 0 ^ le 4 0 ^ le 4 size ^ le 4 size
            ^ le 4 0 ^ le 4 1
        in
        (offset + size, entry :: entries, body :: bodies))
      (table_end, [], []) segments
  in
  String.concat "" ((header :: List.rev entries) @ List.rev bodies)
