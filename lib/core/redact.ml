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

(* Applies [f] to the spans of [text] outside existing placeholders, which stay
   as they are. *)
let fold_plain f acc text =
  let buf = Buffer.create (String.length text) in
  let plain acc s =
    let acc, out = f acc s in
    Buffer.add_string buf out;
    acc
  in
  let pos, acc =
    List.fold_left
      (fun (pos, acc) g ->
        let start, stop = Re.Group.offset g 0 in
        let acc = plain acc (String.sub text pos (start - pos)) in
        Buffer.add_string buf (Re.Group.get g 0);
        (stop, acc))
      (0, acc)
      (Re.all placeholder_re text)
  in
  let acc = plain acc (String.sub text pos (String.length text - pos)) in
  (acc, Buffer.contents buf)

(* Apply [re] to the plain spans of [text], leaving existing placeholders. *)
let pass re text st =
  let st, out =
    fold_plain
      (fun st s ->
        let out, st = replace_matches re s st in
        (st, out))
      st text
  in
  (out, st)

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

(* The end (exclusive) of the JSON string literal starting at [i]. *)
let literal_end text i =
  let n = String.length text in
  let rec go j =
    if j >= n then None
    else
      match text.[j] with
      | '\\' -> go (j + 2)
      | '"' -> Some (j + 1)
      | _ -> go (j + 1)
  in
  go (i + 1)

let scrub_json_strings ~patterns st text =
  let n = String.length text in
  let buf = Buffer.create n in
  let rec go pos st =
    match String.index_from_opt text pos '"' with
    | None ->
        Buffer.add_substring buf text pos (n - pos);
        st
    | Some i -> (
        Buffer.add_substring buf text pos (i - pos);
        match literal_end text i with
        | None ->
            Buffer.add_substring buf text i (n - i);
            st
        | Some j ->
            let literal = String.sub text i (j - i) in
            let st =
              match Yojson.Safe.from_string literal with
              | `String s ->
                  let st, s' = scrub ~patterns ~known:[] st s in
                  Buffer.add_string buf
                    (if String.equal s s' then literal
                     else Yojson.Safe.to_string (`String s'));
                  st
              | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `List _
              | `Assoc _
              | (exception Yojson.Json_error _) ->
                  Buffer.add_string buf literal;
                  st
            in
            go j st)
  in
  let st = go 0 st in
  (st, Buffer.contents buf)

let is_json text = Result.is_ok (Canon.of_string text)

let scrub_structured ~patterns st text =
  if is_json text then scrub_json_strings ~patterns st text
  else
    let st, lines =
      List.fold_left_map
        (fun st line ->
          match String.starts_with ~prefix:"data:" line with
          | true when is_json (String.sub line 5 (String.length line - 5)) ->
              let st, payload =
                scrub_json_strings ~patterns st
                  (String.sub line 5 (String.length line - 5))
              in
              (st, "data:" ^ payload)
          | true | false -> scrub ~patterns ~known:[] st line)
        st
        (String.split_on_char '\n' text)
    in
    (st, String.concat "\n" lines)

let pp_error ppf (Invalid_pattern { source; reason }) =
  Format.fprintf ppf "invalid pattern %S: %s" source reason
