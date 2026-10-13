open Protocol

let name = "openai_responses"

let matches ~meth ~path =
  String.equal meth "POST" && String.equal path "/v1/responses"

let ignored_fields =
  [
    ("metadata", "free-form labels attached to the stored response");
    ("user", "deprecated end-user identifier, differs per run");
    ("safety_identifier", "end-user identifier for abuse detection");
    ("prompt_cache_key", "cache routing hint, affects cost only");
    ("prompt_cache_options", "cache routing hint, affects cost only");
    ("service_tier", "processing tier, affects latency and price only");
    ("truncation", "overflow handling, does not change a fitting request");
    ( "stream_options.include_obfuscation",
      "random padding on stream deltas, noise" );
  ]

let ( let* ) = Result.bind
let is_null v = Canon.equal v Canon.null
let is_object v = Option.is_some (Canon.to_fields_opt v)

let has_type ty item =
  Option.equal String.equal (Canon.str_member "type" item) (Some ty)

(* A Canon.member that is absent or null is [None]. Otherwise [conv] must accept it. *)
let opt_with ~path ~expected conv key obj =
  match Canon.member key obj with
  | None -> Ok None
  | Some v when is_null v -> Ok None
  | Some v -> (
      match conv v with
      | Some x -> Ok (Some x)
      | None -> malformed (path ^ "." ^ key) ("expected " ^ expected))

let opt_string ~path key obj =
  opt_with ~path ~expected:"a string" Canon.to_string_opt key obj

let opt_int ~path key obj =
  opt_with ~path ~expected:"an integer" Canon.to_int_opt key obj

let required ~path key = function
  | Ok (Some x) -> Ok x
  | Ok None -> malformed (path ^ "." ^ key) "missing"
  | Error _ as e -> e

let req_string ~path key obj = required ~path key (opt_string ~path key obj)
let req_int ~path key obj = required ~path key (opt_int ~path key obj)

let opt_list ~path key obj =
  Result.map (Option.value ~default:[])
    (opt_with ~path ~expected:"an array" Canon.to_list_opt key obj)

let nested_int ~path obj key sub =
  match Canon.member key obj with
  | None -> Ok None
  | Some d when is_null d -> Ok None
  | Some d when is_object d -> opt_int ~path:(path ^ "." ^ key) sub d
  | Some _ -> malformed (path ^ "." ^ key) "expected an object"

(* Request side *)

(* Every call type the Responses API has ([function_call], [custom_tool_call],
   [computer_call], [shell_call], …) pairs with a [<type>_output] item through
   [call_id], so items are classified by suffix rather than by a fixed list. *)
let step_of_item item =
  let call_id () = Option.value (Canon.str_member "call_id" item) ~default:"" in
  match Canon.str_member "type" item with
  | Some t when String.ends_with ~suffix:"_call_output" t ->
      Some (Answered (call_id ()))
  | Some t
    when String.ends_with ~suffix:"_call" t
         && Option.is_some (Canon.member "call_id" item) ->
      Some (Called (call_id ()))
  | Some _ | None -> None

let items req = Option.value (Canon.list_member "input" req) ~default:[]

(* Drops the server-generated item [id] and replaces [call_id] by the position
   of the call it names, so renaming IDs keeps the pairing of calls and outputs
   but not the names. An [item_reference] is only its [id], so that stays. The
   encrypted_content of a reasoning item is an opaque blob that differs per run
   for the same reasoning, so it goes too. *)
let normalise_item links item =
  let item =
    if has_type "item_reference" item then item
    else Canon.remove_member "id" item
  in
  let item =
    if has_type "reasoning" item then
      Canon.remove_member "encrypted_content" item
    else item
  in
  match step_of_item item with
  | None -> (links, item)
  | Some _ -> (
      let links, link = pop_link links in
      match Canon.member "call_id" item with
      | None -> (links, item)
      | Some _ ->
          let id = Option.fold link ~none:Canon.null ~some:Canon.int in
          (links, Canon.set_member "call_id" id item))

let normalise_input req =
  match Canon.list_member "input" req with
  | Some items ->
      let links = link_history (List.filter_map step_of_item items) in
      let _, items' = List.fold_left_map normalise_item links items in
      Canon.set_member "input" (Canon.list items') req
  | None -> req

let canonical_request ~path:_ req =
  if is_object req then Ok (normalise_input (remove_ignored ignored_fields req))
  else malformed "$" "request is not a JSON object"

let streaming ~path:_ ~query:_ req =
  Option.value (Canon.bool_member "stream" req) ~default:false

let tool_results req =
  items req
  |> List.filter_map (fun it ->
      Option.map (fun step -> (step, it)) (step_of_item it))
  |> linked_results
  |> List.map (fun (it, call_index) ->
      let content =
        match Canon.member "output" it with
        | None -> ""
        | Some o ->
            Option.value (Canon.to_string_opt o) ~default:(Canon.to_string o)
      in
      { call_index; content; is_error = false })

(* Response side *)

type piece = Text of string | Call of tool_call | Skip

let content_part ~path part =
  if has_type "output_text" part then req_string ~path "text" part
  else if has_type "refusal" part then req_string ~path "refusal" part
  else Ok ""

let output_item ~path item =
  if has_type "message" item then
    let* parts = opt_list ~path "content" item in
    let* texts =
      traverse
        (fun (i, p) ->
          content_part ~path:(Printf.sprintf "%s.content[%d]" path i) p)
        (List.mapi (fun i p -> (i, p)) parts)
    in
    Ok (Text (String.concat "" texts))
  else if has_type "function_call" item then
    let* name = req_string ~path "name" item in
    let* args = req_string ~path "arguments" item in
    let* arguments =
      Result.map_error
        (fun e -> canon_error (path ^ ".arguments") e)
        (Canon.of_string args)
    in
    Ok (Call { name; arguments })
  else Ok Skip

let usage ~path r =
  match Canon.member "usage" r with
  | None -> Ok None
  | Some u when is_null u -> Ok None
  | Some u when is_object u ->
      let path = path ^ ".usage" in
      let* input_tokens = req_int ~path "input_tokens" u in
      let* output_tokens = req_int ~path "output_tokens" u in
      let* cached_input_tokens =
        nested_int ~path u "input_tokens_details" "cached_tokens"
      in
      let* cache_write_tokens =
        nested_int ~path u "input_tokens_details" "cache_write_tokens"
      in
      let* reasoning_tokens =
        nested_int ~path u "output_tokens_details" "reasoning_tokens"
      in
      Ok
        (Some
           {
             Trace.input_tokens;
             output_tokens;
             cached_input_tokens;
             cache_write_tokens;
             reasoning_tokens;
           })
  | Some _ -> malformed (path ^ ".usage") "expected an object"

let error_message r =
  match Canon.member "error" r with
  | None -> None
  | Some e when is_null e -> None
  | Some e -> (
      match Canon.str_member "message" e with
      | Some m -> Some m
      | None -> Some (Canon.to_string e))

let summarise_response ~path r =
  match Canon.to_fields_opt r with
  | Some _ ->
      let* model = opt_string ~path "model" r in
      let* status = opt_string ~path "status" r in
      let* output = opt_list ~path "output" r in
      let* pieces =
        traverse
          (fun (i, it) ->
            output_item ~path:(Printf.sprintf "%s.output[%d]" path i) it)
          (List.mapi (fun i it -> (i, it)) output)
      in
      let* usage = usage ~path r in
      Ok
        {
          model;
          text =
            String.concat ""
              (List.filter_map
                 (function Text s -> Some s | Call _ | Skip -> None)
                 pieces);
          tool_calls =
            List.filter_map
              (function Call c -> Some c | Text _ | Skip -> None)
              pieces;
          usage;
          stop_reason = status;
          error = error_message r;
        }
  | None -> malformed path "expected an object"

type stream = {
  terminal : Canon.t option;
  latest : Canon.t option;
  frame_error : string option;
}

let stream_step st (i, item) =
  match item with
  | Sse.Comment _ -> Ok st
  | Sse.Event { data; _ } when String.equal data "[DONE]" -> Ok st
  | Sse.Event { data; _ } -> (
      let path = Printf.sprintf "$sse[%d]" i in
      let* payload =
        Result.map_error (canon_error path) (Canon.of_string data)
      in
      let response () =
        match Canon.member "response" payload with
        | Some r when is_object r -> Ok r
        | Some _ | None -> malformed (path ^ ".response") "expected an object"
      in
      match Canon.str_member "type" payload with
      | Some ("response.completed" | "response.incomplete" | "response.failed")
        ->
          let* r = response () in
          Ok { st with terminal = Some r; latest = Some r }
      | Some ("response.created" | "response.in_progress" | "response.queued")
        ->
          let* r = response () in
          Ok { st with latest = Some r }
      | Some "error" ->
          let* message = opt_string ~path "message" payload in
          Ok
            {
              st with
              frame_error =
                Some (Option.value message ~default:(Canon.to_string payload));
            }
      | Some _ | None -> Ok st)

let summarise_stream items =
  let* st =
    List.fold_left
      (fun acc it ->
        let* st = acc in
        stream_step st it)
      (Ok { terminal = None; latest = None; frame_error = None })
      (List.mapi (fun i it -> (i, it)) items)
  in
  let empty =
    {
      model = None;
      text = "";
      tool_calls = [];
      usage = None;
      stop_reason = None;
      error = None;
    }
  in
  let* base =
    match (st.terminal, st.latest) with
    | Some r, _ -> summarise_response ~path:"$sse.response" r
    | None, Some r ->
        let* s = summarise_response ~path:"$sse.response" r in
        Ok { s with stop_reason = None }
    | None, None -> Ok empty
  in
  let error =
    match (st.frame_error, st.terminal) with
    | Some e, _ -> Some e
    | None, Some _ -> base.error
    | None, None ->
        Some
          (Option.value base.error
             ~default:"stream ended before a terminal response event")
  in
  Ok { base with error }

let summarise = function
  | Json c -> summarise_response ~path:"$" c
  | Sse items -> summarise_stream items
  | Json_stream _ -> malformed "$" "Responses streams are SSE, not a JSON array"
