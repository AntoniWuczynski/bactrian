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
    && Option.equal ( = ) a.usage b.usage
    && Option.equal String.equal a.stop_reason b.stop_reason
    && Option.equal String.equal a.error b.error
  in
  Alcotest.testable pp equal
