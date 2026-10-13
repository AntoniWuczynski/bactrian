module Smap = Map.Make (String)

type pattern = Re.re
type error = Invalid_pattern of { source : string; reason : string }
type state = int Smap.t

let empty = Smap.empty
let count = Smap.cardinal
let placeholder n = Printf.sprintf "<secret:%d>" n

let pattern source =
  match Re.Perl.re_result source with
  | Error `Parse_error ->
      Error (Invalid_pattern { source; reason = "invalid syntax" })
  | Error `Not_supported ->
      Error (Invalid_pattern { source; reason = "unsupported syntax" })
  | Ok core ->
      let re = Re.compile core in
      if Re.execp re "" then
        Error (Invalid_pattern { source; reason = "matches the empty string" })
      else Ok re

let provider_key_patterns =
  let key = "[A-Za-z0-9_-]" in
  [
    "\\bsk-ant-" ^ key ^ "{20,}";
    "\\bsk-or-" ^ key ^ "{20,}";
    "\\bsk-" ^ key ^ "{20,}";
    "\\bAIza" ^ key ^ "{35,}";
  ]
  |> List.filter_map (fun s -> Result.to_option (pattern s))

(* Returns the placeholder for [secret], allocating the next number if new. *)
let intern secret st =
  match Smap.find_opt secret st with
  | Some n -> (placeholder n, st)
  | None ->
      let n = Smap.cardinal st + 1 in
      (placeholder n, Smap.add secret n st)

let placeholder_re =
  Re.compile Re.(seq [ str "<secret:"; rep1 digit; char '>' ])

(* Replace every non-empty match of [re] in [text], numbering secrets in [st]. *)
let replace_matches re text st =
  let buf = Buffer.create (String.length text) in
  let pos, st =
    List.fold_left
      (fun (pos, st) g ->
        let start, stop = Re.Group.offset g 0 in
        if Int.equal stop start || start < pos then (pos, st)
        else begin
          Buffer.add_substring buf text pos (start - pos);
          let ph, st = intern (Re.Group.get g 0) st in
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

let scrub ~patterns ~known st text =
  let literals =
    known
    |> List.filter (fun s -> not (String.equal s ""))
    |> List.sort_uniq (fun a b ->
        match Int.compare (String.length b) (String.length a) with
        | 0 -> String.compare a b
        | c -> c)
    |> List.map (fun s -> Re.compile (Re.str s))
  in
  let res = literals @ patterns in
  let text, st =
    List.fold_left (fun (text, st) re -> pass re text st) (text, st) res
  in
  (st, text)

let pp_error ppf (Invalid_pattern { source; reason }) =
  Format.fprintf ppf "invalid pattern %S: %s" source reason
