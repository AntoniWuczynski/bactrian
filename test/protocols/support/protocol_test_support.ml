open Bactrian_core

let read_file path = In_channel.with_open_bin path In_channel.input_all

let fixture ~protocol name file =
  read_file
    (String.concat "/" [ "../../fixtures/protocols"; protocol; name; file ])

let json s =
  match Canon.of_string s with
  | Ok v -> v
  | Error e -> Alcotest.failf "bad json: %a" Canon.pp_error e

let get k v =
  match Canon.member k v with
  | Some x -> x
  | None -> Alcotest.failf "no field %S in %s" k (Canon.to_string v)

let expect what conv v =
  match conv v with
  | Some x -> x
  | None -> Alcotest.failf "not %s: %s" what (Canon.to_string v)

let string_exn = expect "a string" Canon.to_string_opt
let int_exn = expect "an int" Canon.to_int_opt
let list_exn = expect "a list" Canon.to_list_opt
let str k v = string_exn (get k v)

let ok_exn = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%a" Protocol.pp_error e

let sse_items pieces =
  let st, rev =
    List.fold_left
      (fun (st, acc) piece ->
        let st, items = Sse.feed st piece in
        (st, List.rev_append items acc))
      (Sse.init, []) pieces
  in
  (match Sse.finish st with
  | `Clean -> ()
  | `Truncated _ -> Alcotest.fail "truncated stream");
  List.rev rev

let recorded_pieces ~raw ~chunks =
  List.map
    (fun c ->
      String.sub raw (int_exn (get "offset" c)) (int_exn (get "length" c)))
    (list_exn (json chunks))

let single_bytes s = List.init (String.length s) (fun i -> String.make 1 s.[i])

let canon_t =
  Alcotest.testable
    (fun ppf c -> Fmt.string ppf (Canon.to_string c))
    Canon.equal

let usage_equal (a : Trace.usage) (b : Trace.usage) =
  Int.equal a.input_tokens b.input_tokens
  && Int.equal a.output_tokens b.output_tokens
  && Option.equal Int.equal a.cached_input_tokens b.cached_input_tokens
  && Option.equal Int.equal a.cache_write_tokens b.cache_write_tokens
  && Option.equal Int.equal a.reasoning_tokens b.reasoning_tokens

let summary_t =
  let pp ppf (s : Protocol.summary) =
    Fmt.pf ppf "{model=%s; text=%S; calls=[%s]; usage=%s; stop=%s; error=%s}"
      (Option.value s.model ~default:"-")
      s.text
      (String.concat "; "
         (List.map
            (fun (c : Protocol.tool_call) ->
              c.name ^ Canon.to_string c.arguments)
            s.tool_calls))
      (match s.usage with
      | None -> "-"
      | Some u -> Printf.sprintf "%d/%d" u.input_tokens u.output_tokens)
      (Option.value s.stop_reason ~default:"-")
      (Option.value s.error ~default:"-")
  in
  let equal (a : Protocol.summary) (b : Protocol.summary) =
    Option.equal String.equal a.model b.model
    && String.equal a.text b.text
    && List.equal
         (fun (x : Protocol.tool_call) (y : Protocol.tool_call) ->
           String.equal x.name y.name && Canon.equal x.arguments y.arguments)
         a.tool_calls b.tool_calls
    && Option.equal usage_equal a.usage b.usage
    && Option.equal String.equal a.stop_reason b.stop_reason
    && Option.equal String.equal a.error b.error
  in
  Alcotest.testable pp equal

let obj fields =
  match Canon.obj fields with
  | Ok v -> v
  | Error e -> Alcotest.failf "bad object: %a" Canon.pp_error e

let rec set_at segs value v =
  match segs with
  | [] -> value
  | Canon.Each :: rest -> Canon.map_items (set_at rest value) v
  | Canon.Key k :: rest -> (
      let v =
        if Option.is_some (Canon.to_fields_opt v) then v else Canon.empty_object
      in
      match (Canon.member k v, rest) with
      | None, Canon.Each :: _ -> v
      | None, ([] | Canon.Key _ :: _) ->
          Canon.set_member k (set_at rest value Canon.empty_object) v
      | Some child, _ -> Canon.set_member k (set_at rest value child) v)

let set_path path value v = set_at (Canon.path_of_string path) value v

let find_from s sub i =
  let n = String.length sub in
  let rec go i =
    if i + n > String.length s then None
    else if String.equal (String.sub s i n) sub then Some i
    else go (i + 1)
  in
  go i

let contains ~sub s = Option.is_some (find_from s sub 0)

let replace_all ~sub ~by s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    match find_from s sub i with
    | None -> Buffer.add_string b (String.sub s i (String.length s - i))
    | Some j ->
        Buffer.add_string b (String.sub s i (j - i));
        Buffer.add_string b by;
        go (j + String.length sub)
  in
  go 0;
  Buffer.contents b

let first_half l = List.filteri (fun i _ -> i < List.length l / 2) l

let first_half_body : Protocol.body -> Protocol.body = function
  | Sse items -> Sse (first_half items)
  | Json_stream items -> Json_stream (first_half items)
  | Json _ as b -> b

let check_usage_relations name (s : Protocol.summary) =
  match s.usage with
  | None -> ()
  | Some u ->
      let opt = Option.value ~default:0 in
      Alcotest.(check bool)
        (name ^ ": cached + cache_write <= input")
        true
        (opt u.cached_input_tokens + opt u.cache_write_tokens <= u.input_tokens);
      Alcotest.(check bool)
        (name ^ ": reasoning <= output")
        true
        (opt u.reasoning_tokens <= u.output_tokens)

let check_truncation name summarise body =
  let full : Protocol.summary = ok_exn (summarise body) in
  Alcotest.(check (option string))
    (name ^ ": full has no error")
    None full.error;
  let flagged =
    match summarise (first_half_body body) with
    | Error _ -> true
    | Ok (cut : Protocol.summary) -> Option.is_some cut.error
  in
  Alcotest.(check bool) (name ^ ": truncated stream is an error") true flagged
