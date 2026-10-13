open Protocol

let name = "anthropic_messages"

let matches ~meth ~path =
  String.equal (String.uppercase_ascii meth) "POST"
  && String.equal path "/v1/messages"

let ignored_fields =
  [
    ("metadata", "caller bookkeeping (user_id), does not change the answer");
    ("service_tier", "capacity routing choice, not part of the prompt");
    ("inference_geo", "data residency choice, not part of the prompt");
    ("container", "server-tool container reuse id, differs per run");
    ( "cache_control",
      "prompt caching hint on any block or tool, affects cost not content" );
    ( "messages[].content[].signature",
      "opaque thinking signature, differs per run for the same thinking" );
  ]

let items k v = Option.value ~default:[] (Canon.list_member k v)
let type_is t v = Option.equal String.equal (Canon.str_member "type" v) (Some t)
let malformed path reason = Error (Malformed { path; reason })

let canon_error path (e : Canon.error) =
  Malformed { path; reason = Format.asprintf "%a" Canon.pp_error e }

let empty_object : Canon.t =
  match Canon.obj [] with
  | Ok v -> v
  | Error (Canon.Syntax _ | Canon.Duplicate_key _ | Canon.Non_finite_float) ->
      Canon.null (* unreachable: an empty field list is always valid *)

let rec map_result f = function
  | [] -> Ok []
  | x :: xs ->
      Result.bind (f x) (fun y ->
          Result.map (fun ys -> y :: ys) (map_result f xs))

(* Request history *)

let call_types = [ "tool_use"; "server_tool_use"; "mcp_tool_use" ]
let is_call b = List.exists (fun t -> type_is t b) call_types

(* Content blocks of every message, in order. *)
let history_blocks req =
  List.concat_map (items "content") (items "messages" req)

let call_ids req =
  List.filter_map
    (fun b -> if is_call b then Canon.str_member "id" b else None)
    (history_blocks req)

let index_of ids id = List.find_index (String.equal id) ids

let result_text c =
  match (Canon.to_string_opt c, Canon.to_list_opt c) with
  | Some s, _ -> s
  | None, Some blocks ->
      String.concat ""
        (List.filter_map
           (fun b ->
             if type_is "text" b then Canon.str_member "text" b else None)
           blocks)
  | None, None -> ""

let tool_results req =
  let ids = call_ids req in
  List.filter_map
    (fun b ->
      if not (type_is "tool_result" b) then None
      else
        Option.bind (Canon.str_member "tool_use_id" b) (index_of ids)
        |> Option.map (fun call_index ->
            {
              call_index;
              content =
                result_text
                  (Option.value ~default:Canon.null (Canon.member "content" b));
              is_error =
                Option.value ~default:false (Canon.bool_member "is_error" b);
            }))
    (history_blocks req)

(* Canonical request *)

let top_level_ignored =
  [ "metadata"; "service_tier"; "inference_geo"; "container" ]

(* Subtrees that hold caller data verbatim: nothing inside is a protocol field. *)
let verbatim_keys = [ "input"; "input_schema" ]
let key_in keys k = List.exists (String.equal k) keys

let rec clean ~ids ~verbatim v =
  let v =
    if verbatim then v
    else
      let v = Canon.remove_member "cache_control" v in
      let v =
        if type_is "thinking" v then Canon.remove_member "signature" v else v
      in
      if is_call v then Canon.remove_member "id" v else v
  in
  let field k child =
    match
      (verbatim, String.equal k "tool_use_id", Canon.to_string_opt child)
    with
    | false, true, Some id ->
        (* A result with no matching call keeps no ID at all. *)
        Option.fold (index_of ids id) ~none:Canon.null ~some:Canon.int
    | _ -> clean ~ids ~verbatim:(verbatim || key_in verbatim_keys k) child
  in
  Canon.map_items (clean ~ids ~verbatim) (Canon.map_fields field v)

let canonical_request ~path:_ req =
  match Canon.to_fields_opt req with
  | None -> malformed "$" "request is not a JSON object"
  | Some _ ->
      let ids = call_ids req in
      let kept =
        List.fold_left
          (fun v k -> Canon.remove_member k v)
          req top_level_ignored
      in
      Ok (Canon.map_fields (fun _ v -> clean ~ids ~verbatim:false v) kept)

let streaming ~path:_ ~query:_ req =
  Option.equal Bool.equal (Canon.bool_member "stream" req) (Some true)

(* Responses *)

type block =
  | Text of Buffer.t
  | Tool of { name : string; start_input : Canon.t; json : Buffer.t }
  | Other

let block_of_start b =
  if type_is "text" b then (
    let buf = Buffer.create 64 in
    Option.iter (Buffer.add_string buf) (Canon.str_member "text" b);
    Text buf)
  else if type_is "tool_use" b then
    Tool
      {
        name = Option.value ~default:"" (Canon.str_member "name" b);
        start_input =
          Option.value ~default:empty_object (Canon.member "input" b);
        json = Buffer.create 64;
      }
  else Other

let finish_blocks blocks =
  let text = Buffer.create 64 in
  let finish = function
    | Text buf ->
        Buffer.add_buffer text buf;
        Ok None
    | Other -> Ok None
    | Tool { name; start_input; json } ->
        if Buffer.length json = 0 then
          Ok (Some { name; arguments = start_input })
        else
          Result.map
            (fun arguments -> Some { name; arguments })
            (Result.map_error
               (canon_error "tool arguments")
               (Canon.of_string (Buffer.contents json)))
  in
  Result.map
    (fun calls -> (Buffer.contents text, List.filter_map Fun.id calls))
    (map_result finish blocks)

let usage_of_fields fields : Trace.usage option =
  match fields with
  | [] -> None
  | _ :: _ ->
      let int k = Option.bind (List.assoc_opt k fields) Canon.to_int_opt in
      Some
        {
          input_tokens = Option.value ~default:0 (int "input_tokens");
          output_tokens = Option.value ~default:0 (int "output_tokens");
          cached_input_tokens = int "cache_read_input_tokens";
          cache_write_tokens = int "cache_creation_input_tokens";
          reasoning_tokens =
            Option.bind
              (Option.bind
                 (List.assoc_opt "output_tokens_details" fields)
                 Canon.to_fields_opt)
              (fun d ->
                Option.bind
                  (List.assoc_opt "thinking_tokens" d)
                  Canon.to_int_opt);
        }

(* [message_delta] usage is cumulative: its fields replace the earlier ones. *)
let merge_usage older newer =
  List.filter (fun (k, _) -> not (List.mem_assoc k newer)) older @ newer

let usage_fields v =
  Option.value ~default:[]
    (Option.bind (Canon.member "usage" v) Canon.to_fields_opt)

let error_text v =
  let e = Option.value ~default:Canon.null (Canon.member "error" v) in
  match (Canon.str_member "type" e, Canon.str_member "message" e) with
  | Some t, Some m -> t ^ ": " ^ m
  | Some t, None -> t
  | None, Some m -> m
  | None, None -> "error"

let make_summary ~model ~usage_fields ~stop_reason ~error blocks =
  Result.map
    (fun (text, tool_calls) ->
      {
        model;
        text;
        tool_calls;
        usage = usage_of_fields usage_fields;
        stop_reason;
        error;
      })
    (finish_blocks blocks)

let summarise_json v =
  if type_is "error" v then
    make_summary ~model:None ~usage_fields:[] ~stop_reason:None
      ~error:(Some (error_text v))
      []
  else
    make_summary
      ~model:(Canon.str_member "model" v)
      ~usage_fields:(usage_fields v)
      ~stop_reason:(Canon.str_member "stop_reason" v)
      ~error:None
      (List.map block_of_start (items "content" v))

type acc = {
  a_model : string option;
  a_usage : (string * Canon.t) list;
  a_stop : string option;
  a_error : string option;
  a_blocks : (int * block) list;  (** Most recent first. *)
}

let acc0 =
  { a_model = None; a_usage = []; a_stop = None; a_error = None; a_blocks = [] }

let block_at acc v =
  Option.bind (Canon.int_member "index" v) (fun i ->
      List.assoc_opt i acc.a_blocks)

let apply_delta acc v =
  let delta = Option.value ~default:Canon.null (Canon.member "delta" v) in
  (match block_at acc v with
  | Some (Text buf) when type_is "text_delta" delta ->
      Option.iter (Buffer.add_string buf) (Canon.str_member "text" delta)
  | Some (Tool { json; _ }) when type_is "input_json_delta" delta ->
      Option.iter (Buffer.add_string json)
        (Canon.str_member "partial_json" delta)
  | Some (Text _ | Tool _ | Other) | None ->
      () (* thinking, signature and citation deltas carry no summary data *));
  acc

let step acc v =
  match Canon.str_member "type" v with
  | Some "message_start" ->
      let m = Option.value ~default:Canon.null (Canon.member "message" v) in
      {
        acc with
        a_model = Canon.str_member "model" m;
        a_usage = merge_usage acc.a_usage (usage_fields m);
      }
  | Some "content_block_start" -> (
      match Canon.int_member "index" v with
      | Some i ->
          let b =
            Option.value ~default:Canon.null (Canon.member "content_block" v)
          in
          { acc with a_blocks = (i, block_of_start b) :: acc.a_blocks }
      | None -> acc)
  | Some "content_block_delta" -> apply_delta acc v
  | Some "message_delta" ->
      let delta = Option.value ~default:Canon.null (Canon.member "delta" v) in
      {
        acc with
        a_stop =
          (match Canon.str_member "stop_reason" delta with
          | Some _ as s -> s
          | None -> acc.a_stop);
        a_usage = merge_usage acc.a_usage (usage_fields v);
      }
  | Some "error" -> { acc with a_error = Some (error_text v) }
  | _ -> acc (* ping, content_block_stop, message_stop and unknown types *)

let summarise_sse items =
  let on_item acc = function
    | Sse.Comment _ -> Ok acc
    | Sse.Event { data; _ } ->
        if String.equal (String.trim data) "" then Ok acc
        else
          Result.map (step acc)
            (Result.map_error (canon_error "sse.data") (Canon.of_string data))
  in
  let rec go acc = function
    | [] -> Ok acc
    | item :: rest -> Result.bind (on_item acc item) (fun acc -> go acc rest)
  in
  Result.bind (go acc0 items) (fun acc ->
      make_summary ~model:acc.a_model ~usage_fields:acc.a_usage
        ~stop_reason:acc.a_stop ~error:acc.a_error
        (List.rev_map snd acc.a_blocks))

let summarise = function
  | Json v -> summarise_json v
  | Sse items -> summarise_sse items
  | Json_stream _ ->
      malformed "body" "Anthropic Messages does not stream a JSON array"
