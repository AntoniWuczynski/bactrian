open Protocol

let name = "openai_chat"

let matches ~meth ~path =
  String.equal meth "POST"
  && List.exists (String.equal path)
       [ "/v1/chat/completions"; "/api/v1/chat/completions" ]

let ignored_fields =
  [
    ("metadata", "caller bookkeeping");
    ("user", "end-user identifier, abuse tracking only");
    ("safety_identifier", "end-user identifier, abuse tracking only");
    ("prompt_cache_key", "cache routing hint");
    ("prompt_cache_retention", "cache lifetime hint");
    ("service_tier", "capacity tier, not content");
    ("stream_options.include_obfuscation", "padding noise in the stream");
  ]

let ( let* ) = Result.bind

let call_steps msg =
  match (Canon.str_member "role" msg, Canon.list_member "tool_calls" msg) with
  | Some "assistant", Some calls ->
      List.map
        (fun c -> Called (Option.value (Canon.str_member "id" c) ~default:""))
        calls
  | Some "tool", _ ->
      [
        Answered
          (Option.value (Canon.str_member "tool_call_id" msg) ~default:"");
      ]
  | _ -> []

let messages req = Option.value (Canon.list_member "messages" req) ~default:[]

(* Replace provider call IDs by the position of the call they name, using the
   shared linking rule. A call without an ID has none to replace. *)
let rewrite_message links msg =
  match (Canon.str_member "role" msg, Canon.list_member "tool_calls" msg) with
  | Some "assistant", Some calls ->
      let links, calls' =
        List.fold_left_map
          (fun links call ->
            let links, link = pop_link links in
            match (Canon.member "id" call, link) with
            | Some _, Some i -> (links, Canon.set_member "id" (Canon.int i) call)
            | (Some _ | None), (Some _ | None) -> (links, call))
          links calls
      in
      (links, Canon.set_member "tool_calls" (Canon.list calls') msg)
  | Some "tool", _ -> (
      let links, link = pop_link links in
      match Canon.member "tool_call_id" msg with
      | None -> (links, msg)
      | Some _ ->
          let id = Option.fold link ~none:Canon.null ~some:Canon.int in
          (links, Canon.set_member "tool_call_id" id msg))
  | _ -> (links, msg)

let rewrite_ids root =
  match Canon.list_member "messages" root with
  | None -> root
  | Some msgs ->
      let links = link_history (List.concat_map call_steps msgs) in
      let _, msgs' = List.fold_left_map rewrite_message links msgs in
      Canon.set_member "messages" (Canon.list msgs') root

let canonical_request ~path:_ req =
  match Canon.to_fields_opt req with
  | None -> malformed "$" "request is not a JSON object"
  | Some _ -> Ok (rewrite_ids (remove_ignored ignored_fields req))

let streaming ~path:_ ~query:_ req =
  Option.value (Canon.bool_member "stream" req) ~default:false

let content_text c =
  match Canon.to_string_opt c with
  | Some s -> s
  | None ->
      Canon.to_list_opt c |> Option.value ~default:[]
      |> List.filter_map (Canon.str_member "text")
      |> String.concat ""

let tool_results req =
  messages req
  |> List.concat_map (fun m -> List.map (fun step -> (step, m)) (call_steps m))
  |> linked_results
  |> List.map (fun (m, call_index) ->
      {
        call_index;
        content =
          Option.fold ~none:"" ~some:content_text (Canon.member "content" m);
        is_error = false;
      })

module IM = Map.Make (Int)

type call_acc = { cname : string; cargs_rev : string list }

type acc = {
  model : string option;
  text_rev : string list;
  calls : call_acc IM.t;
  usage : Trace.usage option;
  stop_reason : string option;
  error : string option;
  seen_choices : bool;
  complete : bool;  (** [[DONE]] or a finish_reason arrived. *)
}

let empty =
  {
    model = None;
    text_rev = [];
    calls = IM.empty;
    usage = None;
    stop_reason = None;
    error = None;
    seen_choices = false;
    complete = false;
  }

let error_message e =
  match Canon.to_string_opt e with
  | Some s -> s
  | None -> (
      match Canon.str_member "message" e with
      | Some m -> m
      | None -> Canon.to_string e)

let parse_usage u =
  let req k =
    match Canon.int_member k u with
    | Some n -> Ok n
    | None -> malformed ("usage." ^ k) "missing or not an integer"
  in
  let* input_tokens = req "prompt_tokens" in
  let* output_tokens = req "completion_tokens" in
  let detail d k = Option.bind (Canon.member d u) (Canon.int_member k) in
  Ok
    {
      Trace.input_tokens;
      output_tokens;
      cached_input_tokens = detail "prompt_tokens_details" "cached_tokens";
      cache_write_tokens = detail "prompt_tokens_details" "cache_write_tokens";
      reasoning_tokens = detail "completion_tokens_details" "reasoning_tokens";
    }

let add_call acc pos call =
  let idx = Option.value (Canon.int_member "index" call) ~default:pos in
  let fn = Option.value (Canon.member "function" call) ~default:Canon.null in
  let prev =
    Option.value
      (IM.find_opt idx acc.calls)
      ~default:{ cname = ""; cargs_rev = [] }
  in
  let cname = Option.value (Canon.str_member "name" fn) ~default:prev.cname in
  let cargs_rev =
    match Canon.str_member "arguments" fn with
    | Some a -> a :: prev.cargs_rev
    | None -> prev.cargs_rev
  in
  { acc with calls = IM.add idx { cname; cargs_rev } acc.calls }

(* Only choice 0 is summarised: agents do not use [n > 1], and the text and
   tool calls of other choices are dropped (their tokens still count in the
   usage). *)
let apply_choice acc choice =
  let acc =
    match Canon.str_member "finish_reason" choice with
    | Some _ -> { acc with complete = true }
    | None -> acc
  in
  if
    not
      (Int.equal (Option.value (Canon.int_member "index" choice) ~default:0) 0)
  then acc
  else
    let body =
      match Canon.member "delta" choice with
      | Some d -> d
      | None -> Option.value (Canon.member "message" choice) ~default:Canon.null
    in
    let acc =
      match Option.bind (Canon.member "content" body) Canon.to_string_opt with
      | Some t -> { acc with text_rev = t :: acc.text_rev }
      | None -> acc
    in
    let acc =
      Canon.list_member "tool_calls" body
      |> Option.value ~default:[]
      |> List.mapi (fun i c -> (i, c))
      |> List.fold_left (fun a (i, c) -> add_call a i c) acc
    in
    let finish = Canon.str_member "finish_reason" choice in
    let acc =
      match finish with
      | Some _ -> { acc with stop_reason = finish }
      | None -> acc
    in
    match (finish, acc.error) with
    | Some "error", None ->
        let msg =
          Option.fold
            (Canon.member "error" choice)
            ~none:"finish_reason: error" ~some:error_message
        in
        { acc with error = Some msg }
    | _ -> acc

let apply acc chunk =
  let acc =
    match (acc.model, Canon.str_member "model" chunk) with
    | None, (Some _ as m) -> { acc with model = m }
    | _ -> acc
  in
  let acc =
    match (acc.error, Canon.member "error" chunk) with
    | None, Some e when not (Canon.equal e Canon.null) ->
        { acc with error = Some (error_message e) }
    | _ -> acc
  in
  let acc =
    match Canon.list_member "choices" chunk with
    | Some cs -> List.fold_left apply_choice { acc with seen_choices = true } cs
    | None -> acc
  in
  match Canon.member "usage" chunk with
  | None -> Ok acc
  | Some u when Canon.equal u Canon.null -> Ok acc
  | Some u ->
      let* usage = parse_usage u in
      Ok { acc with usage = Some usage }

let finish acc =
  let* tool_calls =
    IM.bindings acc.calls
    |> traverse (fun (i, c) ->
        let args = String.concat "" (List.rev c.cargs_rev) in
        let* arguments =
          if String.equal args "" then Ok Canon.empty_object
          else
            Result.map_error
              (canon_error
                 (Printf.sprintf "tool_calls[%d].function.arguments" i))
              (Canon.of_string args)
        in
        Ok { name = c.cname; arguments })
  in
  Ok
    {
      model = acc.model;
      text = String.concat "" (List.rev acc.text_rev);
      tool_calls;
      usage = acc.usage;
      stop_reason = acc.stop_reason;
      error = acc.error;
    }

let truncated acc =
  if acc.complete || Option.is_some acc.error then acc
  else { acc with error = Some "stream ended before [DONE] or a finish_reason" }

let apply_item acc = function
  | Sse.Comment _ -> Ok (`Continue acc)
  | Sse.Event { event = Some "error"; data; _ } ->
      Ok (`Continue { acc with error = Some data })
  | Sse.Event { data; _ } when String.equal (String.trim data) "[DONE]" ->
      Ok (`Stop { acc with complete = true })
  | Sse.Event { data; _ } -> (
      match Canon.of_string data with
      | Error e -> Error (canon_error "data" e)
      | Ok chunk ->
          let* acc' = apply acc chunk in
          Ok (`Continue acc'))

let rec fold_items acc = function
  | [] -> Ok acc
  | it :: rest -> (
      let* r = apply_item acc it in
      match r with `Stop a -> Ok a | `Continue a -> fold_items a rest)

let summarise = function
  | Json j ->
      let* acc = apply empty j in
      if (not acc.seen_choices) && Option.is_none acc.error then
        malformed "choices" "missing"
      else finish acc
  | Sse its ->
      let* acc = fold_items empty its in
      finish (truncated acc)
  | Json_stream _ -> malformed "$" "a JSON stream is not an OpenAI chat body"
