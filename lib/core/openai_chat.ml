open Protocol

let name = "openai_chat"

let matches ~meth ~path =
  String.equal meth "POST"
  && List.exists (String.equal path)
       [ "/v1/chat/completions"; "/api/v1/chat/completions" ]

let ignored_fields =
  [
    ("store", "storage opt-in, does not change the completion");
    ("metadata", "caller bookkeeping");
    ("user", "end-user identifier, abuse tracking only");
    ("safety_identifier", "end-user identifier, abuse tracking only");
    ("prompt_cache_key", "cache routing hint");
    ("prompt_cache_retention", "cache lifetime hint");
    ("service_tier", "capacity tier, not content");
    ("stream_options.include_obfuscation", "padding noise in the stream");
    ("provider", "OpenRouter upstream routing");
    ("models", "OpenRouter fallback model list");
    ("route", "OpenRouter routing mode");
    ("plugins", "OpenRouter plugin list");
    ("debug", "OpenRouter debug echo");
  ]

let ( let* ) = Result.bind
let malformed path reason = Error (Malformed { path; reason })

let canon_obj path fs =
  Result.map_error
    (fun e ->
      Malformed { path; reason = Format.asprintf "%a" Canon.pp_error e })
    (Canon.obj fs)

let rec map_result f = function
  | [] -> Ok []
  | x :: rest ->
      let* y = f x in
      let* ys = map_result f rest in
      Ok (y :: ys)

(* Replace provider call IDs by the call's position in the history. State is
   threaded as (ids seen so far, next call position). *)
let rewrite_call (ids, next) call =
  match str_member "id" call with
  | None -> ((ids, next + 1), call)
  | Some id ->
      ( ((id, next) :: ids, next + 1),
        Canon.set_member "id" (Canon.int next) call )

let rewrite_message ((ids, _) as state) msg =
  match (str_member "role" msg, list_member "tool_calls" msg) with
  | Some "assistant", Some calls ->
      let state', calls' = List.fold_left_map rewrite_call state calls in
      (state', Canon.set_member "tool_calls" (Canon.list calls') msg)
  | Some "tool", _ -> (
      match
        Option.bind (str_member "tool_call_id" msg) (fun id ->
            List.assoc_opt id ids)
      with
      | None -> (state, msg)
      | Some n -> (state, Canon.set_member "tool_call_id" (Canon.int n) msg))
  | _ -> (state, msg)

let rewrite_ids root =
  match list_member "messages" root with
  | None -> root
  | Some msgs ->
      let _, msgs' = List.fold_left_map rewrite_message ([], 0) msgs in
      Canon.set_member "messages" (Canon.list msgs') root

let canonical_request ~path:_ req =
  match Canon.to_fields_opt req with
  | None -> malformed "$" "request is not a JSON object"
  | Some _ ->
      Ok
        (rewrite_ids
           (List.fold_left
              (fun v (p, _) -> Canon.remove_at (Canon.path_of_string p) v)
              req ignored_fields))

let streaming ~path:_ ~query:_ req =
  Option.value (bool_member "stream" req) ~default:false

let content_text c =
  match Canon.to_string_opt c with
  | Some s -> s
  | None ->
      Canon.to_list_opt c |> Option.value ~default:[]
      |> List.filter_map (str_member "text")
      |> String.concat ""

let tool_results req =
  list_member "messages" (rewrite_ids req)
  |> Option.value ~default:[]
  |> List.filter_map (fun m ->
      match (str_member "role" m, int_member "tool_call_id" m) with
      | Some "tool", Some call_index ->
          Some
            {
              call_index;
              content =
                Option.fold ~none:"" ~some:content_text
                  (Canon.member "content" m);
              is_error = false;
            }
      | _ -> None)

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
  }

let error_message e =
  match Canon.to_string_opt e with
  | Some s -> s
  | None -> (
      match str_member "message" e with
      | Some m -> m
      | None -> Canon.to_string e)

let parse_usage u =
  let req k =
    match int_member k u with
    | Some n -> Ok n
    | None -> malformed ("usage." ^ k) "missing or not an integer"
  in
  let* input_tokens = req "prompt_tokens" in
  let* output_tokens = req "completion_tokens" in
  let detail d k = Option.bind (Canon.member d u) (int_member k) in
  Ok
    {
      Trace.input_tokens;
      output_tokens;
      cached_input_tokens = detail "prompt_tokens_details" "cached_tokens";
      cache_write_tokens = detail "prompt_tokens_details" "cache_write_tokens";
      reasoning_tokens = detail "completion_tokens_details" "reasoning_tokens";
    }

let add_call acc pos call =
  let idx = Option.value (int_member "index" call) ~default:pos in
  let fn = Option.value (Canon.member "function" call) ~default:Canon.null in
  let prev =
    Option.value
      (IM.find_opt idx acc.calls)
      ~default:{ cname = ""; cargs_rev = [] }
  in
  let cname = Option.value (str_member "name" fn) ~default:prev.cname in
  let cargs_rev =
    match str_member "arguments" fn with
    | Some a -> a :: prev.cargs_rev
    | None -> prev.cargs_rev
  in
  { acc with calls = IM.add idx { cname; cargs_rev } acc.calls }

let apply_choice acc choice =
  if Option.value (int_member "index" choice) ~default:0 <> 0 then acc
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
      list_member "tool_calls" body
      |> Option.value ~default:[]
      |> List.mapi (fun i c -> (i, c))
      |> List.fold_left (fun a (i, c) -> add_call a i c) acc
    in
    let finish = str_member "finish_reason" choice in
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
    match (acc.model, str_member "model" chunk) with
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
    match list_member "choices" chunk with
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
    |> map_result (fun (i, c) ->
        let args = String.concat "" (List.rev c.cargs_rev) in
        let* arguments =
          if String.equal args "" then canon_obj "arguments" []
          else
            Result.map_error
              (fun e ->
                Malformed
                  {
                    path = Printf.sprintf "tool_calls[%d].function.arguments" i;
                    reason = Format.asprintf "%a" Canon.pp_error e;
                  })
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

let apply_item acc = function
  | Sse.Comment _ -> Ok (`Continue acc)
  | Sse.Event { event = Some "error"; data; _ } ->
      Ok (`Continue { acc with error = Some data })
  | Sse.Event { data; _ } when String.equal (String.trim data) "[DONE]" ->
      Ok (`Stop acc)
  | Sse.Event { data; _ } -> (
      match Canon.of_string data with
      | Error e -> malformed "data" (Format.asprintf "%a" Canon.pp_error e)
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
      finish acc
  | Json_stream _ -> malformed "$" "a JSON stream is not an OpenAI chat body"
