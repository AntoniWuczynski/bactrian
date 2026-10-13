open Protocol

let name = "anthropic_messages"

let matches ~meth ~path =
  String.equal meth "POST" && String.equal path "/v1/messages"

(* Removed wherever the path matches. *)
let plain_ignored =
  [
    ("metadata", "caller bookkeeping (user_id), does not change the answer");
    ("service_tier", "capacity routing choice, not part of the prompt");
    ("inference_geo", "data residency choice, not part of the prompt");
    ("container", "server-tool container reuse id, differs per run");
    ("cache_control", "automatic prompt caching hint, affects cost not content");
    ("system[].cache_control", "prompt caching hint, affects cost not content");
    ("tools[].cache_control", "prompt caching hint, affects cost not content");
    ( "messages[].content[].cache_control",
      "prompt caching hint, affects cost not content" );
    ( "messages[].content[].content[].cache_control",
      "prompt caching hint on a tool result part, affects cost not content" );
  ]

(* Removed only from blocks of the named type, so a [signature] or [data] field
   on any other block stays in the key. Both are opaque blobs of earlier
   thinking turns that differ per run for the same thinking. *)
let typed_ignored =
  [
    ( "messages[].content[].signature",
      "opaque signature, thinking blocks in the history only" );
    ( "messages[].content[].data",
      "opaque encrypted thinking, redacted_thinking blocks in the history only"
    );
  ]

let ignored_fields = plain_ignored @ typed_ignored
let items k v = Option.value ~default:[] (Canon.list_member k v)
let type_is t v = Option.equal String.equal (Canon.str_member "type" v) (Some t)

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

(* Request history *)

let call_types = [ "tool_use"; "server_tool_use"; "mcp_tool_use" ]
let is_call b = List.exists (fun t -> type_is t b) call_types
let id_or_empty k b = Option.value (Canon.str_member k b) ~default:""

let step_of_block b =
  if is_call b then Some (Called (id_or_empty "id" b))
  else if type_is "tool_result" b then
    Some (Answered (id_or_empty "tool_use_id" b))
  else None

(* Content blocks of every message, in order. *)
let history_blocks req =
  List.concat_map (items "content") (items "messages" req)

let tool_results req =
  history_blocks req
  |> List.filter_map (fun b ->
      Option.map (fun step -> (step, b)) (step_of_block b))
  |> linked_results
  |> List.map (fun (b, call_index) ->
      {
        call_index;
        content =
          result_text
            (Option.value ~default:Canon.null (Canon.member "content" b));
        is_error = Option.value ~default:false (Canon.bool_member "is_error" b);
      })

(* Canonical request *)

(* Call IDs go (calls are known by position) and result IDs become the
   position of the call they answer, or null. Thinking blobs go as in
   [typed_ignored]. *)
let rewrite_block links b =
  match step_of_block b with
  | Some (Called _) ->
      let links, _ = pop_link links in
      (links, Canon.remove_member "id" b)
  | Some (Answered _) -> (
      let links, link = pop_link links in
      let id = Option.fold link ~none:Canon.null ~some:Canon.int in
      ( links,
        match Canon.member "tool_use_id" b with
        | Some _ -> Canon.set_member "tool_use_id" id b
        | None -> b ))
  | None ->
      ( links,
        if type_is "thinking" b then Canon.remove_member "signature" b
        else if type_is "redacted_thinking" b then Canon.remove_member "data" b
        else b )

let rewrite_message links msg =
  match Canon.list_member "content" msg with
  | None -> (links, msg)
  | Some blocks ->
      let links, blocks' = List.fold_left_map rewrite_block links blocks in
      (links, Canon.set_member "content" (Canon.list blocks') msg)

let rewrite_history req =
  match Canon.list_member "messages" req with
  | None -> req
  | Some msgs ->
      let links =
        link_history (List.filter_map step_of_block (history_blocks req))
      in
      let _, msgs' = List.fold_left_map rewrite_message links msgs in
      Canon.set_member "messages" (Canon.list msgs') req

let canonical_request ~path:_ req =
  match Canon.to_fields_opt req with
  | None -> malformed "$" "request is not a JSON object"
  | Some _ -> Ok (remove_ignored plain_ignored (rewrite_history req))

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
          Option.value ~default:Canon.empty_object (Canon.member "input" b);
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
    (traverse finish blocks)

let usage_of_fields fields : Trace.usage option =
  match fields with
  | [] -> None
  | _ :: _ ->
      let int k = Option.bind (List.assoc_opt k fields) Canon.to_int_opt in
      let count k = Option.value ~default:0 (int k) in
      Some
        {
          input_tokens =
            count "input_tokens"
            + count "cache_read_input_tokens"
            + count "cache_creation_input_tokens";
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
  a_complete : bool;  (** [message_stop] arrived. *)
}

let acc0 =
  {
    a_model = None;
    a_usage = [];
    a_stop = None;
    a_error = None;
    a_blocks = [];
    a_complete = false;
  }

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
  | Some "message_stop" -> { acc with a_complete = true }
  | Some "error" -> { acc with a_error = Some (error_text v) }
  | _ -> acc (* ping, content_block_stop and unknown types *)

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
        ~stop_reason:acc.a_stop
        ~error:
          (match acc.a_error with
          | Some _ as e -> e
          | None when acc.a_complete -> None
          | None -> Some "stream ended before message_stop")
        (List.rev_map snd acc.a_blocks))

let summarise = function
  | Json v -> summarise_json v
  | Sse items -> summarise_sse items
  | Json_stream _ ->
      malformed "body" "Anthropic Messages does not stream a JSON array"
