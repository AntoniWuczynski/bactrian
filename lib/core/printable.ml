let escape s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i < n then
      let c = Char.code s.[i] in
      if c < 0x20 || c = 0x7f then begin
        Printf.bprintf b "\\x%02x" c;
        go (i + 1)
      end
      else if c = 0xc2 && i + 1 < n && Char.code s.[i + 1] land 0xe0 = 0x80 then begin
        (* U+0080 to U+009F, the C1 controls, in UTF-8. *)
        Printf.bprintf b "\\u%04x" (Char.code s.[i + 1]);
        go (i + 2)
      end
      else begin
        Buffer.add_char b s.[i];
        go (i + 1)
      end
  in
  go 0;
  Buffer.contents b
