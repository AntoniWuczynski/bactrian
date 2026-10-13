type event = {
  event : string option;
  data : string;
  id : string option;
  retry : int option;
}

type item = Event of event | Comment of string

type state = {
  at_start : bool;
      (** Still deciding whether a byte order mark opens the stream. *)
  line : string;  (** Bytes of the current, unterminated line. *)
  last_cr : bool;  (** The previous byte ended a line with ['\r']. *)
  block_bytes : int;
      (** Bytes of completed lines in the current unfinished block. *)
  event_type : string option;
  data_rev : string list;  (** [data:] values, newest first. *)
  id_field : string option;
  retry_field : int option;
}

let init =
  {
    at_start = true;
    line = "";
    last_cr = false;
    block_bytes = 0;
    event_type = None;
    data_rev = [];
    id_field = None;
    retry_field = None;
  }

let bom = "\xEF\xBB\xBF"

let strip_space v =
  if String.starts_with ~prefix:" " v then String.sub v 1 (String.length v - 1)
  else v

let is_digits v = v <> "" && String.for_all (fun c -> c >= '0' && c <= '9') v

let reset st =
  {
    st with
    block_bytes = 0;
    event_type = None;
    data_rev = [];
    id_field = None;
    retry_field = None;
  }

let apply_field st name value =
  match name with
  | "event" -> { st with event_type = Some value }
  | "data" -> { st with data_rev = value :: st.data_rev }
  | "id" ->
      if String.contains value '\000' then st
      else { st with id_field = Some value }
  | "retry" ->
      if is_digits value then { st with retry_field = int_of_string_opt value }
      else st
  | _ -> st

(* Handle one complete line; returns the new state and the item it completes. *)
let end_line st terminator_len =
  let l = st.line in
  let st = { st with line = "" } in
  if l = "" then
    match st.data_rev with
    | [] -> (reset st, None)
    | _ :: _ ->
        let ev =
          {
            event = st.event_type;
            data = String.concat "\n" (List.rev st.data_rev);
            id = st.id_field;
            retry = st.retry_field;
          }
        in
        (reset st, Some (Event ev))
  else if l.[0] = ':' then
    (st, Some (Comment (strip_space (String.sub l 1 (String.length l - 1)))))
  else
    let st =
      {
        st with
        block_bytes = st.block_bytes + String.length l + terminator_len;
      }
    in
    match String.index_opt l ':' with
    | None -> (apply_field st l "", None)
    | Some i ->
        let name = String.sub l 0 i in
        let value =
          strip_space (String.sub l (i + 1) (String.length l - i - 1))
        in
        (apply_field st name value, None)

let next_terminator s i =
  let n = String.length s in
  let rec go j =
    if j >= n then None
    else match s.[j] with '\r' | '\n' -> Some j | _ -> go (j + 1)
  in
  go i

let rec scan st s i acc =
  let n = String.length s in
  if i >= n then (st, acc)
  else if st.last_cr && s.[i] = '\n' then
    let st =
      {
        st with
        last_cr = false;
        block_bytes = (if st.block_bytes > 0 then st.block_bytes + 1 else 0);
      }
    in
    scan st s (i + 1) acc
  else
    let st = { st with last_cr = false } in
    match next_terminator s i with
    | None -> ({ st with line = st.line ^ String.sub s i (n - i) }, acc)
    | Some j ->
        let st = { st with line = st.line ^ String.sub s i (j - i) } in
        let st, item = end_line st 1 in
        let st = { st with last_cr = Char.equal s.[j] '\r' } in
        scan st s (j + 1) (match item with None -> acc | Some it -> it :: acc)

let feed st chunk =
  let st, s =
    if st.at_start then
      let s = st.line ^ chunk in
      let st = { st with line = "" } in
      let n = String.length s in
      if n < 3 && String.starts_with ~prefix:s bom then
        ({ st with line = s }, "")
      else
        let s =
          if String.starts_with ~prefix:bom s then String.sub s 3 (n - 3) else s
        in
        ({ st with at_start = false }, s)
    else (st, chunk)
  in
  let st, acc = scan st s 0 [] in
  (st, List.rev acc)

let finish st =
  match st.block_bytes + String.length st.line with
  | 0 -> `Clean
  | n -> `Truncated n

let no_break s = not (String.contains s '\r' || String.contains s '\n')

let serialise item =
  let b = Buffer.create 64 in
  let field name v = Buffer.add_string b (name ^ ": " ^ v ^ "\n") in
  (match item with
  | Comment c ->
      if not (no_break c) then
        invalid_arg "Sse.serialise: comment contains a line break";
      Buffer.add_string b (":" ^ " " ^ c ^ "\n")
  | Event e ->
      Option.iter
        (fun v ->
          if not (no_break v) then
            invalid_arg "Sse.serialise: event contains a line break")
        e.event;
      Option.iter
        (fun v ->
          if not (no_break v) then
            invalid_arg "Sse.serialise: id contains a line break")
        e.id;
      if String.contains e.data '\r' then
        invalid_arg "Sse.serialise: data contains a carriage return";
      Option.iter (field "event") e.event;
      List.iter (field "data") (String.split_on_char '\n' e.data);
      Option.iter (field "id") e.id;
      Option.iter (fun n -> field "retry" (string_of_int n)) e.retry;
      Buffer.add_char b '\n');
  Buffer.contents b

let equal_item a b =
  match (a, b) with
  | Comment x, Comment y -> String.equal x y
  | Event x, Event y ->
      Option.equal String.equal x.event y.event
      && String.equal x.data y.data
      && Option.equal String.equal x.id y.id
      && Option.equal Int.equal x.retry y.retry
  | Comment _, Event _ | Event _, Comment _ -> false
