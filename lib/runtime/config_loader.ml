open Bactrian_core
module S = Yaml.Stream

type located_error = {
  file : string;
  line : int;
  column : int;
  path : string;
  message : string;
}

(* An error before the file name is known. *)
type problem = { at : int * int; ppath : string; msg : string }

let ( let* ) = Result.bind

(* {1 Positions} *)

let join path key = if String.equal path "" then key else path ^ "." ^ key

let parent path =
  if String.equal path "" then None
  else
    let cut c = Some (String.sub path 0 (String.rindex path c)) in
    if path.[String.length path - 1] = ']' then
      if String.contains path '[' then cut '[' else Some ""
    else if String.contains path '.' then cut '.'
    else Some ""

let rec lookup table path =
  match Hashtbl.find_opt table path with
  | Some _ as found -> found
  | None -> Option.bind (parent path) (lookup table)

(* libyaml's description ends "... character N position N returned: R", where
   the numbers are 0 for syntax errors. Keep the description only. *)
let describe msg =
  let key = " character " in
  let klen = String.length key in
  let n = String.length msg in
  let rec find i =
    if i + klen > n then msg
    else if String.equal (String.sub msg i klen) key then String.sub msg 0 i
    else find (i + 1)
  in
  find 0

(* {1 Scalars} *)

let is_digit c = c >= '0' && c <= '9'
let all_chars p s = String.length s > 0 && String.for_all p s

let resolve_plain v =
  match v with
  | "" | "~" | "null" | "Null" | "NULL" -> Ok Canon.null
  | "true" | "True" | "TRUE" -> Ok (Canon.bool true)
  | "false" | "False" | "FALSE" -> Ok (Canon.bool false)
  | _ ->
      let digits =
        if v.[0] = '-' || v.[0] = '+' then String.sub v 1 (String.length v - 1)
        else v
      in
      if all_chars is_digit digits then
        match int_of_string_opt v with
        | Some i -> Ok (Canon.int i)
        | None -> (
            (* Beyond the native range: Canon keeps the exact digits. *)
            match Canon.of_string (if v.[0] = '+' then digits else v) with
            | Ok c -> Ok c
            | Error _ -> Ok (Canon.string v))
      else if
        all_chars
          (fun c ->
            is_digit c || c = '.' || c = 'e' || c = 'E' || c = '-' || c = '+')
          v
        && String.exists is_digit v
      then
        match float_of_string_opt v with
        | None -> Ok (Canon.string v)
        | Some f -> (
            match Canon.float f with
            | Ok c -> Ok c
            | Error e ->
                Error (Format.asprintf "number %s: %a" v Canon.pp_error e))
      else Ok (Canon.string v)

(* {1 Event stream to Canon.t} *)

type state = {
  next : unit -> (S.Event.t * (int * int), problem) result;
  table : (string, int * int) Hashtbl.t;
}

let mark_of (pos : S.Event.pos) =
  (pos.start_mark.line + 1, pos.start_mark.column + 1)

let problem at ppath msg = Error { at; ppath; msg }
let register st path at = Hashtbl.replace st.table path at

let rec node st path =
  let* ev, at = st.next () in
  node_of st path ev at

and node_of st path ev at =
  match ev with
  | S.Event.Scalar s -> (
      let plain =
        match s.style with
        | `Plain | `Any -> true
        | `Single_quoted | `Double_quoted | `Literal | `Folded -> false
      in
      if not plain then Ok (Canon.string s.value)
      else
        match resolve_plain s.value with
        | Ok c -> Ok c
        | Error m -> problem at path m)
  | Sequence_start _ -> items st path 0 []
  | Mapping_start _ -> fields st path at []
  | Alias { anchor } ->
      problem at path (Printf.sprintf "aliases are not supported (*%s)" anchor)
  | Stream_start _ | Document_start _ | Document_end _ | Mapping_end
  | Stream_end | Sequence_end | Nothing ->
      problem at path "unexpected YAML event"

and items st path i acc =
  let* ev, pos = st.next () in
  match ev with
  | S.Event.Sequence_end -> Ok (Canon.list (List.rev acc))
  | Stream_start _ | Document_start _ | Document_end _ | Mapping_start _
  | Mapping_end | Stream_end | Scalar _ | Sequence_start _ | Alias _ | Nothing
    ->
      let item_path = Printf.sprintf "%s[%d]" path i in
      register st item_path pos;
      let* v = node_of st item_path ev pos in
      items st path (i + 1) (v :: acc)

and fields st path at acc =
  let* ev, kpos = st.next () in
  match ev with
  | S.Event.Mapping_end -> (
      match Canon.obj (List.rev acc) with
      | Ok c -> Ok c
      | Error e -> problem at path (Format.asprintf "%a" Canon.pp_error e))
  | Scalar { value = key; _ } ->
      let kpath = join path key in
      if List.mem_assoc key acc then
        problem kpos kpath (Printf.sprintf "duplicate key %S" key)
      else (
        register st kpath kpos;
        let* v = node st kpath in
        fields st path at ((key, v) :: acc))
  | Stream_start _ | Document_start _ | Document_end _ | Mapping_start _
  | Stream_end | Sequence_start _ | Sequence_end | Alias _ | Nothing ->
      problem kpos path "mapping keys must be plain strings"

let parse_text text =
  let table = Hashtbl.create 64 in
  let fail_at at msg = { at; ppath = ""; msg } in
  (* libyaml gives no error position through this binding, so a syntax error is
     reported where the last well-formed event ended. *)
  let last = ref (1, 1) in
  match S.parser text with
  | Error (`Msg m) -> Error (fail_at (1, 1) m)
  | Ok p ->
      let next () =
        match S.do_parse p with
        | Ok (ev, pos) ->
            last := (pos.end_mark.line + 1, pos.end_mark.column + 1);
            Ok (ev, mark_of pos)
        | Error (`Msg m) -> Error (fail_at !last (describe m))
      in
      let st = { next; table } in
      let expect_end () =
        let* ev, at = next () in
        match ev with
        | S.Event.Stream_end -> Ok ()
        | Document_start _ -> problem at "" "only one YAML document is allowed"
        | Stream_start _ | Document_end _ | Mapping_start _ | Mapping_end
        | Scalar _ | Sequence_start _ | Sequence_end | Alias _ | Nothing ->
            problem at "" "unexpected YAML event"
      in
      let* ev, _ = next () in
      (match ev with
      | S.Event.Stream_start _ -> ()
      | Document_start _ | Document_end _ | Mapping_start _ | Mapping_end
      | Stream_end | Scalar _ | Sequence_start _ | Sequence_end | Alias _
      | Nothing ->
          ());
      let* ev, at = next () in
      let* canon =
        match ev with
        | S.Event.Stream_end ->
            register st "" (1, 1);
            Ok Canon.null
        | Document_start _ ->
            let* ev, at = next () in
            register st "" at;
            let* v = node_of st "" ev at in
            let* ev, at = next () in
            (match ev with
              | S.Event.Document_end _ -> expect_end ()
              | Stream_start _ | Document_start _ | Mapping_start _
              | Mapping_end | Stream_end | Scalar _ | Sequence_start _
              | Sequence_end | Alias _ | Nothing ->
                  problem at "" "unexpected YAML event")
            |> Result.map (fun () -> v)
        | Stream_start _ | Document_end _ | Mapping_start _ | Mapping_end
        | Scalar _ | Sequence_start _ | Sequence_end | Alias _ | Nothing ->
            problem at "" "unexpected YAML event"
      in
      Ok (canon, table)

(* {1 Public API} *)

let file_name path = snd path

let locate ~file (line, column) ~path ~message =
  { file; line; column; path; message }

let read path =
  let file = file_name path in
  match Eio.Path.load path with
  | text -> Ok text
  | exception (Eio.Io _ as e) ->
      Error
        [
          locate ~file (1, 1) ~path:""
            ~message:(Format.asprintf "%a" Eio.Exn.pp e);
        ]

let load path =
  let file = file_name path in
  let* text = read path in
  match parse_text text with
  | Ok (canon, table) -> Ok (canon, lookup table)
  | Error { at; ppath; msg } ->
      Error [ locate ~file at ~path:ppath ~message:msg ]

let load_with ?(validate = fun (_ : 'a) -> []) decode path =
  let file = file_name path in
  let* canon, find = load path in
  match
    Result.bind (decode canon) (fun v ->
        match validate v with [] -> Ok v | errs -> Error errs)
  with
  | Ok v -> Ok v
  | Error errs ->
      Error
        (List.map
           (fun (e : Scenario.error) ->
             locate ~file
               (Option.value ~default:(1, 1) (find e.path))
               ~path:e.path ~message:e.message)
           errs)

(* The fixtures directory is relative to the scenario file. *)
let fixtures_exist path (s : Scenario.t) =
  match (s.fixtures, Eio.Path.split path) with
  | Some dir, Some (parent, _)
    when not (Eio.Path.is_directory Eio.Path.(parent / dir)) ->
      [
        {
          Config_error.path = "fixtures";
          message = Printf.sprintf "%S is not a directory" dir;
        };
      ]
  | Some _, (Some _ | None) | None, (Some _ | None) -> []

let load_scenario path =
  load_with ~validate:(fixtures_exist path) Scenario.decode path

let load_pricing path =
  Result.map Pricing.with_overrides (load_with Price_file.decode path)

let load_experiment path = load_with Experiment.decode path

(* {1 Globs} *)

let has_wildcard seg = String.contains seg '*' || String.contains seg '?'

(* Greedy match with backtracking to the last [*]. *)
let segment_matches pat s =
  let np = String.length pat and ns = String.length s in
  let rec go p i star =
    if i < ns then
      if p < np && (pat.[p] = '?' || (pat.[p] <> '*' && pat.[p] = s.[i])) then
        go (p + 1) (i + 1) star
      else if p < np && pat.[p] = '*' then go (p + 1) i (Some (p, i))
      else
        match star with
        | Some (sp, si) -> go (sp + 1) (si + 1) (Some (sp, si + 1))
        | None -> false
    else
      let rec rest p = p >= np || (pat.[p] = '*' && rest (p + 1)) in
      rest p
  in
  go 0 0 None

let entries dir = try Eio.Path.read_dir_entries dir with Eio.Io _ -> []
let rel_join rel name = if String.equal rel "" then name else rel ^ "/" ^ name

let rec walk dir rel segs =
  match segs with
  | [] -> if Eio.Path.is_file dir then [ rel ] else []
  | "**" :: rest ->
      walk dir rel rest
      @ List.concat_map
          (fun (kind, name) ->
            match kind with
            | `Directory -> walk Eio.Path.(dir / name) (rel_join rel name) segs
            | `Unknown | `Fifo | `Character_special | `Regular_file | `Socket
            | `Symbolic_link | `Block_device ->
                [])
          (entries dir)
  | seg :: rest when has_wildcard seg ->
      List.concat_map
        (fun (_, name) ->
          if segment_matches seg name then
            walk Eio.Path.(dir / name) (rel_join rel name) rest
          else [])
        (entries dir)
  | seg :: rest -> (
      try walk Eio.Path.(dir / seg) (rel_join rel seg) rest
      with Eio.Io _ -> [])

let expand_globs ~base patterns =
  let file = file_name base in
  let expand pattern =
    let segs =
      String.split_on_char '/' pattern
      |> List.filter (fun s -> not (String.equal s "" || String.equal s "."))
    in
    match walk base "" segs with
    | [] ->
        Error
          (locate ~file (1, 1) ~path:pattern
             ~message:(Printf.sprintf "glob %S matches no files" pattern))
    | found -> Ok found
  in
  let results = List.map expand patterns in
  let errors =
    List.filter_map (function Error e -> Some e | Ok _ -> None) results
  in
  match errors with
  | [] ->
      Ok
        (List.concat_map (function Ok l -> l | Error _ -> []) results
        |> List.sort_uniq String.compare)
  | _ :: _ -> Error errors
