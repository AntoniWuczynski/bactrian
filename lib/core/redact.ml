module Smap = Map.Make (String)

(* [group] is the capture group holding the secret: 0 (the whole match) for
   user patterns, 1 for provider patterns whose boundary is part of the match. *)
type pattern = { re : Re.re; group : int }
type error = Invalid_pattern of { source : string; reason : string }

(* [next] is the number the next new secret receives. *)
type state = { ids : int Smap.t; next : int }

let empty = { ids = Smap.empty; next = 1 }
let count st = st.next - 1
let placeholder n = Printf.sprintf "<secret:%d>" n

let compile ~group source =
  match Regex.compile source with
  | Error reason -> Error (Invalid_pattern { source; reason })
  | Ok re -> Ok { re; group }

let pattern source = compile ~group:0 source

(* [re] has no lookbehind, so a left boundary is matched as part of the regex
   and only group 1, the key, is replaced. *)
let provider_key_patterns =
  let key = "[A-Za-z0-9_-]" in
  let unbounded prefix min = "(" ^ prefix ^ key ^ "{" ^ min ^ ",})" in
  let boundary = {|(?:^|[^A-Za-z0-9_-]|\\[ntr"/]|%[0-9A-Fa-f]{2})|} in
  [
    unbounded "sk-ant-" "20";
    unbounded "sk-proj-" "20";
    unbounded "sk-or-" "20";
    unbounded "sk-svcacct-" "20";
    unbounded "AIza" "35";
    boundary ^ unbounded "sk-" "32";
  ]
  |> List.filter_map (fun s -> Result.to_option (compile ~group:1 s))

(* Returns the placeholder for [secret], allocating the next number if new. *)
let intern secret st =
  match Smap.find_opt secret st.ids with
  | Some n -> (placeholder n, st)
  | None ->
      ( placeholder st.next,
        { ids = Smap.add secret st.next st.ids; next = st.next + 1 } )

let placeholder_re =
  Re.compile Re.(seq [ str "<secret:"; rep1 digit; char '>' ])

(* Replace every non-empty match of [re] in [text], numbering secrets in [st]. *)
let replace_matches { re; group } text st =
  let buf = Buffer.create (String.length text) in
  let pos, st =
    List.fold_left
      (fun (pos, st) g ->
        let start, stop = Re.Group.offset g group in
        if Int.equal stop start || start < pos then (pos, st)
        else begin
          Buffer.add_substring buf text pos (start - pos);
          let ph, st = intern (Re.Group.get g group) st in
          Buffer.add_string buf ph;
          (stop, st)
        end)
      (0, st) (Re.all re text)
  in
  Buffer.add_substring buf text pos (String.length text - pos);
  (Buffer.contents buf, st)

(* Apply [re] to the plain spans of [text], leaving existing placeholders. *)
let pass re text st =
  let buf = Buffer.create (String.length text) in
  let plain s st =
    let out, st = replace_matches re s st in
    Buffer.add_string buf out;
    st
  in
  let pos, st =
    List.fold_left
      (fun (pos, st) g ->
        let start, stop = Re.Group.offset g 0 in
        let st = plain (String.sub text pos (start - pos)) st in
        Buffer.add_string buf (Re.Group.get g 0);
        (stop, st))
      (0, st)
      (Re.all placeholder_re text)
  in
  let st = plain (String.sub text pos (String.length text - pos)) st in
  (Buffer.contents buf, st)

(* Passes repeat until the text stops changing: a span that follows a new
   placeholder is a fresh string, so context-dependent patterns can match on
   the next pass. Each change removes plain text, so this terminates. *)
let scrub ~patterns ~known st text =
  let literals =
    known
    |> List.filter (fun s -> not (String.equal s ""))
    |> List.sort_uniq (fun a b ->
        match Int.compare (String.length b) (String.length a) with
        | 0 -> String.compare a b
        | c -> c)
    |> List.map (fun s -> { re = Re.compile (Re.str s); group = 0 })
  in
  let res = literals @ patterns in
  let rec fix text st =
    let text', st' =
      List.fold_left (fun (text, st) re -> pass re text st) (text, st) res
    in
    if String.equal text' text then (st', text') else fix text' st'
  in
  let st, text = fix text st in
  (st, text)

let pp_error ppf (Invalid_pattern { source; reason }) =
  Format.fprintf ppf "invalid pattern %S: %s" source reason
