open Protocol

let name = "gemini_interactions"

let matches ~meth ~path =
  String.equal meth "POST" && String.equal path "/v1beta/interactions"

let ignored_fields =
  [
    ("labels", "billing metadata, does not change the answer");
    ("service_tier", "routing and billing tier, does not change the answer");
    ("continuation_token", "opaque pagination handle, differs per run");
  ]

(* Helpers over Canon.t *)

let obj path fields = Result.map_error (canon_error path) (Canon.obj fields)
let string_field = Canon.str_member
let int_field = Canon.int_member
let type_is t v = Option.equal String.equal (string_field "type" v) (Some t)

let step_of v =
  let id k = Option.value (string_field k v) ~default:"" in
  match string_field "type" v with
  | Some "function_call" -> Some (Called (id "id"))
  | Some "function_result" -> Some (Answered (id "call_id"))
  | Some _ | None -> None

let children v =
  match Canon.to_fields_opt v with
  | Some fs -> List.map snd fs
  | None -> Option.value (Canon.to_list_opt v) ~default:[]

(* Calls and results in document order, outermost first, with the value. *)
let rec steps_of v =
  match step_of v with
  | Some step -> [ (step, v) ]
  | None -> List.concat_map steps_of (children v)

(* Replace provider call ids by positions and drop thought signatures. *)
let rec rewrite links v =
  match string_field "type" v with
  | Some "function_call" ->
      let links, _ = pop_link links in
      (links, Canon.remove_member "id" (Canon.remove_member "signature" v))
  | Some "thought" -> (links, Canon.remove_member "signature" v)
  | Some "function_result" ->
      let links, link = pop_link links in
      ( links,
        Canon.set_member "call_id"
          (Option.fold link ~none:Canon.null ~some:Canon.int)
          v )
  | Some _ | None -> Canon.fold_map_children rewrite links v

let canonical_request ~path:_ req =
  match Canon.to_fields_opt req with
  | None -> malformed "" "request body is not a JSON object"
  | Some _ ->
      let kept = remove_ignored ignored_fields req in
      Ok
        (match Canon.member "input" kept with
        | Some x ->
            let links = link_history (List.map fst (steps_of x)) in
            Canon.set_member "input" (snd (rewrite links x)) kept
        | None -> kept)

let streaming ~path:_ ~query:_ req =
  Option.equal Bool.equal (Canon.bool_member "stream" req) (Some true)

(* A result is a string or a list of content parts: the text parts joined, or
   the JSON text when there are none. *)
let result_text v =
  match Canon.to_string_opt v with
  | Some s -> s
  | None -> (
      let texts =
        List.filter_map
          (fun part ->
            if type_is "text" part then string_field "text" part else None)
          (Option.value (Canon.to_list_opt v) ~default:[])
      in
      match texts with
      | [] -> Canon.to_string v
      | _ :: _ -> String.concat "" texts)

let tool_results req =
  let input = Option.value (Canon.member "input" req) ~default:Canon.null in
  linked_results (steps_of input)
  |> List.map (fun (r, call_index) ->
      let content =
        match Canon.member "result" r with
        | Some v -> result_text v
        | None -> ""
      in
      let is_error =
        Option.value (Canon.bool_member "is_error" r) ~default:false
      in
      { call_index; content; is_error })

(* Summaries *)

type call = {
  index : int;
  call_name : string;
  start_args : Canon.t option;
  chunks : string list;  (** Reversed. *)
}

type acc = {
  model : string option;
  texts : string list;  (** Reversed. *)
  calls : call list;  (** Reversed. *)
  usage : Trace.usage option;
  stop_reason : string option;
  error : string option;
  complete : bool;  (** [interaction.completed] or [done] arrived. *)
}

let empty =
  {
    model = None;
    texts = [];
    calls = [];
    usage = None;
    stop_reason = None;
    error = None;
    complete = false;
  }

let usage_of path interaction =
  match Canon.member "usage" interaction with
  | None -> Ok None
  | Some u -> (
      match
        (int_field "total_input_tokens" u, int_field "total_output_tokens" u)
      with
      | Some input_tokens, Some output_tokens ->
          Ok
            (Some
               {
                 Trace.input_tokens =
                   input_tokens
                   + Option.value ~default:0
                       (int_field "total_tool_use_tokens" u);
                 output_tokens =
                   output_tokens
                   + Option.value ~default:0
                       (int_field "total_thought_tokens" u);
                 cached_input_tokens = int_field "total_cached_tokens" u;
                 cache_write_tokens = None;
                 reasoning_tokens = int_field "total_thought_tokens" u;
               })
      | None, _ | _, None ->
          malformed path "usage lacks total_input_tokens or total_output_tokens"
      )

let failed_status = function "failed" | "cancelled" -> true | _ -> false

let with_status acc status =
  match status with
  | Some s when failed_status s && Option.is_none acc.error ->
      { acc with error = Some ("interaction " ^ s) }
  | Some _ | None -> acc

(* Model, status and usage carried by an interaction object. *)
let apply_interaction path acc interaction =
  let ( let* ) = Result.bind in
  let* usage = usage_of path interaction in
  let status = string_field "status" interaction in
  let acc = with_status acc status in
  Ok
    {
      acc with
      model =
        (match string_field "model" interaction with
        | Some _ as m -> m
        | None -> acc.model);
      usage = (match usage with Some _ as u -> u | None -> acc.usage);
      stop_reason =
        (match status with Some _ as s -> s | None -> acc.stop_reason);
    }

let interaction_of path payload =
  match Canon.member "interaction" payload with
  | Some i -> Ok i
  | None -> malformed path "event has no interaction object"

let error_message payload data =
  match Canon.member "error" payload with
  | Some e -> (
      match string_field "message" e with
      | Some m -> m
      | None -> Option.value (Canon.to_string_opt e) ~default:data)
  | None -> Option.value (string_field "message" payload) ~default:data

let start_call path acc payload =
  match (int_field "index" payload, Canon.member "step" payload) with
  | Some index, Some step when type_is "function_call" step -> (
      match string_field "name" step with
      | None -> malformed path "function_call step has no name"
      | Some call_name ->
          let start_args =
            match Canon.member "arguments" step with
            | Some a when not (String.equal (Canon.to_string a) "{}") -> Some a
            | Some _ | None -> None
          in
          Ok
            {
              acc with
              calls = { index; call_name; start_args; chunks = [] } :: acc.calls;
            })
  | Some _, Some _ -> Ok acc
  | None, _ | _, None -> malformed path "step.start lacks index or step"

let apply_delta path acc payload =
  match (int_field "index" payload, Canon.member "delta" payload) with
  | Some index, Some delta -> (
      match string_field "type" delta with
      | Some "text" ->
          Ok
            {
              acc with
              texts =
                Option.value (string_field "text" delta) ~default:""
                :: acc.texts;
            }
      | Some "arguments_delta" ->
          let part =
            Option.value (string_field "arguments" delta) ~default:""
          in
          if List.exists (fun c -> Int.equal c.index index) acc.calls then
            Ok
              {
                acc with
                calls =
                  List.map
                    (fun c ->
                      if Int.equal c.index index then
                        { c with chunks = part :: c.chunks }
                      else c)
                    acc.calls;
              }
          else malformed path "arguments_delta for an unknown step"
      | Some _ | None -> Ok acc)
  | None, _ | _, None -> malformed path "step.delta lacks index or delta"

let apply_event acc (ev : Sse.event) =
  let ( let* ) = Result.bind in
  if
    String.equal ev.data "[DONE]"
    || Option.equal String.equal ev.event (Some "done")
  then Ok { acc with complete = true }
  else
    let path = "data" in
    let* payload =
      Result.map_error (canon_error path) (Canon.of_string ev.data)
    in
    let kind =
      match ev.event with
      | Some e -> e
      | None -> Option.value (string_field "event_type" payload) ~default:""
    in
    match kind with
    | "interaction.created" ->
        let* i = interaction_of path payload in
        apply_interaction path acc i
    | "interaction.completed" ->
        let* i = interaction_of path payload in
        let* acc = apply_interaction path acc i in
        Ok { acc with complete = true }
    | "interaction.status_update" ->
        Ok (with_status acc (string_field "status" payload))
    | "step.start" -> start_call path acc payload
    | "step.delta" -> apply_delta path acc payload
    | "error" -> Ok { acc with error = Some (error_message payload ev.data) }
    | _ -> Ok acc

let finish_call c =
  let ( let* ) = Result.bind in
  let* arguments =
    match c.chunks with
    | [] -> (
        match c.start_args with Some a -> Ok a | None -> obj "arguments" [])
    | chunks ->
        Result.map_error (canon_error "arguments")
          (Canon.of_string (String.concat "" (List.rev chunks)))
  in
  Ok { Protocol.name = c.call_name; arguments }

let truncated acc =
  if acc.complete || Option.is_some acc.error then acc
  else
    {
      acc with
      error = Some "stream ended before interaction.completed or done";
    }

let finish acc =
  let ( let* ) = Result.bind in
  let* tool_calls = traverse finish_call (List.rev acc.calls) in
  Ok
    {
      Protocol.model = acc.model;
      text = String.concat "" (List.rev acc.texts);
      tool_calls;
      usage = acc.usage;
      stop_reason = acc.stop_reason;
      error = acc.error;
    }

(* A whole (non-streamed) interaction: its steps are read best-effort. *)
let summarise_json v =
  let ( let* ) = Result.bind in
  let path = "interaction" in
  match Canon.to_fields_opt v with
  | Some _ ->
      let* acc = apply_interaction path empty v in
      let acc =
        match Canon.member "error" v with
        | Some _ -> { acc with error = Some (error_message v "error") }
        | None -> acc
      in
      let steps = Option.value (Canon.list_member "steps" v) ~default:[] in
      let acc =
        List.fold_left
          (fun acc step ->
            if type_is "function_call" step then
              let start_args = Canon.member "arguments" step in
              let call_name =
                Option.value (string_field "name" step) ~default:""
              in
              {
                acc with
                calls =
                  { index = 0; call_name; start_args; chunks = [] } :: acc.calls;
              }
            else if type_is "text" step then
              {
                acc with
                texts =
                  Option.value (string_field "text" step) ~default:""
                  :: acc.texts;
              }
            else acc)
          acc steps
      in
      finish acc
  | None -> malformed path "response is not a JSON object"

let summarise = function
  | Protocol.Json v -> summarise_json v
  | Protocol.Json_stream _ ->
      malformed "" "Interactions never streams a JSON array"
  | Protocol.Sse items ->
      let ( let* ) = Result.bind in
      let* acc =
        List.fold_left
          (fun acc item ->
            let* acc = acc in
            match item with
            | Sse.Comment _ -> Ok acc
            | Sse.Event ev -> apply_event acc ev)
          (Ok empty) items
      in
      finish (truncated acc)
