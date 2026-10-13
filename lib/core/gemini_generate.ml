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
  String.equal (String.uppercase_ascii meth) "POST"
  && Option.is_some (parse_path path)

let streaming ~path ~query:_ _ =
  match parse_path path with
  | Some (_, Stream) -> true
  | Some (_, Generate) | None -> false

(* Errors and JSON access *)

let malformed path reason = Error (Malformed { path; reason })

let canon path r =
  Result.map_error
    (fun e ->
      Malformed { path; reason = Format.asprintf "%a" Canon.pp_error e })
    r

let mk_obj path fields = canon path (Canon.obj fields)

let map_r f xs =
  let* rev =
    List.fold_left
      (fun acc x ->
        let* ys = acc in
        let* y = f x in
        Ok (y :: ys))
      (Ok []) xs
  in
  Ok (List.rev rev)

(* Tool calls and results in the request history *)

type history = { n_calls : int; ids : (string * int) list; n_responses : int }

type role =
  | Call of int
  | Response of { index : int option; response : Canon.t }
  | Other

let start = { n_calls = 0; ids = []; n_responses = 0 }
let id_of v = str_member "id" v

(* Classify one part. A response is linked to its call by id, or by order when
   Gemini sent no ids. *)
let step h part =
  match
    (Canon.member "functionCall" part, Canon.member "functionResponse" part)
  with
  | Some fc, _ ->
      let ids =
        match id_of fc with
        | Some id -> (id, h.n_calls) :: h.ids
        | None -> h.ids
      in
      ({ h with n_calls = h.n_calls + 1; ids }, Call h.n_calls)
  | None, Some response ->
      let index =
        match id_of response with
        | Some id ->
            List.find_map
              (fun (k, i) -> if String.equal k id then Some i else None)
              h.ids
        | None -> Some h.n_responses
      in
      ({ h with n_responses = h.n_responses + 1 }, Response { index; response })
  | None, None -> (h, Other)

let contents req = Option.value ~default:[] (list_member "contents" req)
let parts_of content = Option.value ~default:[] (list_member "parts" content)

let result_content response =
  match Canon.member "response" response with
  | None -> ""
  | Some r -> (
      match str_member "result" r with Some s -> s | None -> Canon.to_string r)

let result_is_error response =
  match Canon.member "response" response with
  | None -> false
  | Some r -> Option.is_some (Canon.member "error" r)

let tool_results req =
  let _, rev =
    List.fold_left
      (fun (h, acc) part ->
        let h, role = step h part in
        match role with
        | Response { index = Some call_index; response } ->
            ( h,
              {
                call_index;
                content = result_content response;
                is_error = result_is_error response;
              }
              :: acc )
        | Response { index = None; response = _ } | Call _ | Other -> (h, acc))
      (start, [])
      (List.concat_map parts_of (contents req))
  in
  List.rev rev

(* Canonical request *)

let ignored_fields =
  [
    ("labels", "billing metadata, no effect on output");
    ("store", "server-side logging flag, no effect on output");
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

let rewrite_part h part =
  let h, role = step h part in
  match role with
  | Other -> (h, part)
  | Call index -> (h, replace_id "functionCall" (Canon.int index) part)
  | Response { index; response = _ } ->
      let replacement =
        match index with Some i -> Canon.int i | None -> Canon.null
      in
      (h, replace_id "functionResponse" replacement part)

let rewrite_content h content =
  match list_member "parts" content with
  | Some parts ->
      let h, parts = List.fold_left_map rewrite_part h parts in
      (h, Canon.set_member "parts" (Canon.list parts) content)
  | None -> (h, content)

let rewrite_ids body =
  match list_member "contents" body with
  | None -> body
  | Some cs ->
      let _, cs = List.fold_left_map rewrite_content start cs in
      Canon.set_member "contents" (Canon.list cs) body

let canonical_request ~path body =
  let* model, verb =
    match parse_path path with
    | Some mv -> Ok mv
    | None -> malformed path "not a Gemini generateContent path"
  in
  let body =
    List.fold_left
      (fun v (p, _) -> Canon.remove_at (Canon.path_of_string p) v)
      body ignored_fields
  in
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
  let int k = int_member k um in
  let count k = Option.value ~default:0 (int k) in
  let thoughts = int "thoughtsTokenCount" in
  {
    Trace.input_tokens = count "promptTokenCount";
    output_tokens = count "candidatesTokenCount" + count "thoughtsTokenCount";
    cached_input_tokens = int "cachedContentTokenCount";
    cache_write_tokens = None;
    reasoning_tokens = thoughts;
  }

let error_text e =
  match str_member "message" e with
  | None -> Canon.to_string e
  | Some m -> (
      match str_member "status" e with Some s -> s ^ ": " ^ m | None -> m)

let add_part acc part =
  match Canon.member "functionCall" part with
  | Some fc ->
      let* name =
        match str_member "name" fc with
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
      let thought = Option.value (bool_member "thought" part) ~default:false in
      match str_member "text" part with
      | Some t when not thought -> Ok { acc with texts = t :: acc.texts }
      | Some _ | None -> Ok acc)

let add_candidate acc cand =
  let parts =
    Option.value ~default:[]
      (Option.bind (Canon.member "content" cand) (list_member "parts"))
  in
  let* acc =
    List.fold_left
      (fun a p ->
        let* a = a in
        add_part a p)
      (Ok acc) parts
  in
  let stop =
    match str_member "finishReason" cand with
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
          (str_member "blockReason")
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
            (match str_member "modelVersion" chunk with
            | Some _ as m -> m
            | None -> acc.model);
          usage =
            (match Canon.member "usageMetadata" chunk with
            | Some um -> Some (usage_of um)
            | None -> acc.usage);
        }
      in
      (* Only the first candidate: n > 1 is not used by agents. *)
      let cands = Option.value ~default:[] (list_member "candidates" chunk) in
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
      map_r
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
  Ok
    {
      model = acc.model;
      text = String.concat "" (List.rev acc.texts);
      tool_calls = List.rev acc.calls;
      usage = acc.usage;
      stop_reason = acc.stop;
      error = acc.err;
    }
