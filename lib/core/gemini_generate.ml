open Protocol

let name = "gemini_generate"
let ( let* ) = Result.bind

(* Paths *)

type verb = Generate | Stream

let verb_of_string = function
  | "generateContent" -> Some Generate
  | "streamGenerateContent" -> Some Stream
  | _ -> None

(* "/v1beta/models/<model>:<verb>", also under "/v1". *)
let parse_path path =
  match String.split_on_char '/' path with
  | [ ""; ("v1beta" | "v1"); "models"; last ] -> (
      match String.index_opt last ':' with
      | None -> None
      | Some i ->
          let model = String.sub last 0 i in
          let verb = String.sub last (i + 1) (String.length last - i - 1) in
          if String.equal model "" then None
          else Option.map (fun v -> (model, v)) (verb_of_string verb))
  | _ -> None

let matches ~meth ~path =
  String.equal meth "POST" && Option.is_some (parse_path path)

let streaming ~path ~query:_ _ =
  match parse_path path with
  | Some (_, Stream) -> true
  | Some (_, Generate) | None -> false

(* Errors and JSON access *)

let canon path r = Result.map_error (canon_error path) r
let mk_obj path fields = canon path (Canon.obj fields)

(* Tool calls and results in the request history *)

let step_of_part part =
  let id v = Option.value (Canon.str_member "id" v) ~default:"" in
  match
    (Canon.member "functionCall" part, Canon.member "functionResponse" part)
  with
  | Some fc, _ -> Some (Called (id fc))
  | None, Some response -> Some (Answered (id response))
  | None, None -> None

let contents req = Option.value ~default:[] (Canon.list_member "contents" req)

let parts_of content =
  Option.value ~default:[] (Canon.list_member "parts" content)

let result_content response =
  match Canon.member "response" response with
  | None -> ""
  | Some r -> (
      match Canon.str_member "result" r with
      | Some s -> s
      | None -> Canon.to_string r)

let result_is_error response =
  match Canon.member "response" response with
  | None -> false
  | Some r -> Option.is_some (Canon.member "error" r)

let tool_results req =
  List.concat_map parts_of (contents req)
  |> List.filter_map (fun part ->
      Option.map (fun step -> (step, part)) (step_of_part part))
  |> linked_results
  |> List.filter_map (fun (part, call_index) ->
      Option.map
        (fun response ->
          {
            call_index;
            content = result_content response;
            is_error = result_is_error response;
          })
        (Canon.member "functionResponse" part))

(* Canonical request *)

let ignored_fields =
  [
    ("labels", "billing metadata, no effect on output");
    ("serviceTier", "capacity routing, no effect on output");
    ("continuationToken", "server-side continuation state, volatile");
    ( "contents[].parts[].thoughtSignature",
      "opaque signature of a previous turn, changes on every run" );
  ]

(* Replace the [id] inside [key] (a functionCall or functionResponse) *)
let replace_id key replacement part =
  match Canon.member key part with
  | Some inner when Option.is_some (Canon.member "id" inner) ->
      Canon.set_member key (Canon.set_member "id" replacement inner) part
  | Some _ | None -> part

(* Call IDs become the call's position, result IDs the position of the call
   they answer (null if none), by the shared linking rule. *)
let rewrite_part links part =
  match step_of_part part with
  | None -> (links, part)
  | Some (Called _) ->
      let links, link = pop_link links in
      ( links,
        replace_id "functionCall"
          (Option.fold link ~none:Canon.null ~some:Canon.int)
          part )
  | Some (Answered _) ->
      let links, link = pop_link links in
      ( links,
        replace_id "functionResponse"
          (Option.fold link ~none:Canon.null ~some:Canon.int)
          part )

let rewrite_content links content =
  match Canon.list_member "parts" content with
  | Some parts ->
      let links, parts = List.fold_left_map rewrite_part links parts in
      (links, Canon.set_member "parts" (Canon.list parts) content)
  | None -> (links, content)

let rewrite_ids body =
  match Canon.list_member "contents" body with
  | None -> body
  | Some cs ->
      let links =
        link_history
          (List.filter_map step_of_part (List.concat_map parts_of cs))
      in
      let _, cs = List.fold_left_map rewrite_content links cs in
      Canon.set_member "contents" (Canon.list cs) body

let canonical_request ~path body =
  let* model, verb =
    match parse_path path with
    | Some mv -> Ok mv
    | None -> malformed path "not a Gemini generateContent path"
  in
  let body = remove_ignored ignored_fields body in
  if Option.is_none (Canon.to_fields_opt body) then
    malformed "$" "request body is not an object"
  else
    let stream = match verb with Stream -> true | Generate -> false in
    Ok
      (Canon.set_member "model" (Canon.string model)
         (Canon.set_member "stream" (Canon.bool stream) (rewrite_ids body)))

(* Summaries *)

type acc = {
  model : string option;
  texts : string list;
  calls : tool_call list;
  usage : Trace.usage option;
  stop : string option;
  err : string option;
}

let empty =
  {
    model = None;
    texts = [];
    calls = [];
    usage = None;
    stop = None;
    err = None;
  }

let usage_of um =
  let int k = Canon.int_member k um in
  let count k = Option.value ~default:0 (int k) in
  let thoughts = int "thoughtsTokenCount" in
  {
    Trace.input_tokens =
      count "promptTokenCount" + count "toolUsePromptTokenCount";
    output_tokens = count "candidatesTokenCount" + count "thoughtsTokenCount";
    cached_input_tokens = int "cachedContentTokenCount";
    cache_write_tokens = None;
    reasoning_tokens = thoughts;
  }

let error_text e =
  match Canon.str_member "message" e with
  | None -> Canon.to_string e
  | Some m -> (
      match Canon.str_member "status" e with
      | Some s -> s ^ ": " ^ m
      | None -> m)

let add_part acc part =
  match Canon.member "functionCall" part with
  | Some fc ->
      let* name =
        match Canon.str_member "name" fc with
        | Some n -> Ok n
        | None -> malformed "functionCall.name" "missing or not a string"
      in
      let* arguments =
        match Canon.member "args" fc with
        | Some a -> Ok a
        | None -> mk_obj "functionCall.args" []
      in
      Ok { acc with calls = { name; arguments } :: acc.calls }
  | None -> (
      let thought =
        Option.value (Canon.bool_member "thought" part) ~default:false
      in
      match Canon.str_member "text" part with
      | Some t when not thought -> Ok { acc with texts = t :: acc.texts }
      | Some _ | None -> Ok acc)

let add_candidate acc cand =
  let parts =
    Option.value ~default:[]
      (Option.bind (Canon.member "content" cand) (Canon.list_member "parts"))
  in
  let* acc =
    List.fold_left
      (fun a p ->
        let* a = a in
        add_part a p)
      (Ok acc) parts
  in
  let stop =
    match Canon.str_member "finishReason" cand with
    | Some _ as s -> s
    | None -> acc.stop
  in
  Ok { acc with stop }

let add_chunk acc chunk =
  match Canon.to_fields_opt chunk with
  | None -> malformed "$" "response chunk is not an object"
  | Some _ -> (
      let block =
        Option.bind
          (Canon.member "promptFeedback" chunk)
          (Canon.str_member "blockReason")
        |> Option.map (fun r -> "prompt blocked: " ^ r)
      in
      let err =
        match acc.err with
        | Some _ as e -> e
        | None -> (
            match Option.map error_text (Canon.member "error" chunk) with
            | Some _ as e -> e
            | None -> block)
      in
      let acc =
        {
          acc with
          err;
          model =
            (match Canon.str_member "modelVersion" chunk with
            | Some _ as m -> m
            | None -> acc.model);
          usage =
            (match Canon.member "usageMetadata" chunk with
            | Some um -> Some (usage_of um)
            | None -> acc.usage);
        }
      in
      (* Only the first candidate: n > 1 is not used by agents. *)
      let cands =
        Option.value ~default:[] (Canon.list_member "candidates" chunk)
      in
      match cands with [] -> Ok acc | first :: _ -> add_candidate acc first)

let chunks_of_body = function
  | Json j -> Ok [ j ]
  | Json_stream l -> Ok l
  | Sse items ->
      let events =
        List.filter_map
          (function Sse.Event e -> Some e | Sse.Comment _ -> None)
          items
      in
      traverse
        (fun (e : Sse.event) -> canon "sse.data" (Canon.of_string e.data))
        events

let summarise body =
  let* chunks = chunks_of_body body in
  let* acc =
    List.fold_left
      (fun a c ->
        let* a = a in
        add_chunk a c)
      (Ok empty) chunks
  in
  let streamed =
    match body with Json _ -> false | Sse _ | Json_stream _ -> true
  in
  let err =
    match (acc.err, acc.stop) with
    | None, None when streamed -> Some "stream ended before a finishReason"
    | _ -> acc.err
  in
  Ok
    {
      model = acc.model;
      text = String.concat "" (List.rev acc.texts);
      tool_calls = List.rev acc.calls;
      usage = acc.usage;
      stop_reason = acc.stop;
      error = err;
    }
