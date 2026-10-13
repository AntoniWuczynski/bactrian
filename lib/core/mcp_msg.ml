type id = Int_id of int | String_id of string

type message =
  | Request of { id : id; method_ : string; params : Canon.t option }
  | Notification of { method_ : string; params : Canon.t option }
  | Response of { id : id; result : Canon.t }
  | Error_response of {
      id : id option;
      code : int;
      message : string;
      data : Canon.t option;
    }

type era = Modern | Legacy
type error = Invalid_json of string | Not_jsonrpc of string

let protocol_version_key = "io.modelcontextprotocol/protocolVersion"

(* Entries of [_meta] that differ between clients or runs without changing what
   the server answers. *)
let varying_meta_keys =
  [
    "io.modelcontextprotocol/clientInfo";
    "traceparent";
    "tracestate";
    "baggage";
    "progressToken";
  ]

let known_members = [ "jsonrpc"; "id"; "method"; "params"; "result"; "error" ]

(* {1 Decoding} *)

type id_field = Absent | Null_id | Id of id

let ( let* ) = Result.bind
let bad reason = Error (Not_jsonrpc reason)

let id_field fields =
  match List.assoc_opt "id" fields with
  | None -> Ok Absent
  | Some v when Canon.equal v Canon.null -> Ok Null_id
  | Some v -> (
      match (Canon.to_int_opt v, Canon.to_string_opt v) with
      | Some i, _ -> Ok (Id (Int_id i))
      | None, Some s -> Ok (Id (String_id s))
      | None, None -> bad "id must be an integer, a string or null")

let params_field fields =
  match List.assoc_opt "params" fields with
  | None -> Ok None
  | Some p ->
      if
        Option.is_some (Canon.to_fields_opt p)
        || Option.is_some (Canon.to_list_opt p)
      then Ok (Some p)
      else bad "params must be an object or an array"

let error_field e =
  match
    ( Canon.int_member "code" e,
      Canon.str_member "message" e,
      Canon.to_fields_opt e )
  with
  | Some code, Some message, Some fields ->
      Ok (code, message, List.assoc_opt "data" fields)
  | None, _, _ | _, None, _ | _, _, None ->
      bad "error needs an integer code and a string message"

let of_fields fields =
  let* () =
    match
      List.find_opt (fun (k, _) -> not (List.mem k known_members)) fields
    with
    | Some (k, _) -> bad ("unexpected member " ^ k)
    | None -> Ok ()
  in
  let* () =
    match List.assoc_opt "jsonrpc" fields with
    | Some v when Option.equal String.equal (Canon.to_string_opt v) (Some "2.0")
      ->
        Ok ()
    | Some _ | None -> bad "jsonrpc must be \"2.0\""
  in
  let* id = id_field fields in
  let result = List.assoc_opt "result" fields in
  let error = List.assoc_opt "error" fields in
  match (List.assoc_opt "method" fields, result, error) with
  | Some m, None, None -> (
      let* params = params_field fields in
      match (Canon.to_string_opt m, id) with
      | None, _ -> bad "method must be a string"
      | Some method_, Absent -> Ok (Notification { method_; params })
      | Some method_, Id id -> Ok (Request { id; method_; params })
      | Some _, Null_id -> bad "a request id must not be null")
  | Some _, _, _ -> bad "a message with method has no result or error"
  | None, Some _, Some _ -> bad "result and error are exclusive"
  | None, Some result, None -> (
      match id with
      | Id id -> Ok (Response { id; result })
      | Absent | Null_id -> bad "a response needs an id")
  | None, None, Some e ->
      let* code, message, data = error_field e in
      let id = match id with Id i -> Some i | Absent | Null_id -> None in
      Ok (Error_response { id; code; message; data })
  | None, None, None -> bad "neither method, result nor error"

let decode text =
  match Canon.of_string text with
  | Error e -> Error (Invalid_json (Format.asprintf "%a" Canon.pp_error e))
  | Ok v -> (
      match (Canon.to_fields_opt v, Canon.to_list_opt v) with
      | Some fields, _ -> of_fields fields
      | None, Some _ ->
          bad "batches are not supported (removed in MCP 2026-07-28)"
      | None, None -> bad "a message must be a JSON object")

(* {1 Encoding} *)

let object_of = Canon.of_fields
let id_json = function Int_id i -> Canon.int i | String_id s -> Canon.string s
let opt_field name = Option.map (fun v -> (name, v))

let to_canon message =
  let base = [ ("jsonrpc", Canon.string "2.0") ] in
  let fields =
    match message with
    | Request { id; method_; params } ->
        base
        @ [ ("id", id_json id); ("method", Canon.string method_) ]
        @ Option.to_list (opt_field "params" params)
    | Notification { method_; params } ->
        base
        @ [ ("method", Canon.string method_) ]
        @ Option.to_list (opt_field "params" params)
    | Response { id; result } ->
        base @ [ ("id", id_json id); ("result", result) ]
    | Error_response { id; code; message; data } ->
        let error =
          object_of
            ([ ("code", Canon.int code); ("message", Canon.string message) ]
            @ Option.to_list (opt_field "data" data))
        in
        base
        @ [
            ("id", Option.fold ~none:Canon.null ~some:id_json id);
            ("error", error);
          ]
  in
  object_of fields

let encode message = Canon.to_string (to_canon message)

(* {1 Protocol knowledge} *)

let era_of_request = function
  | Request { method_ = "initialize"; _ } -> Some Legacy
  | Request { params; _ } -> (
      let version =
        Option.bind params (fun p ->
            Option.bind (Canon.member "_meta" p)
              (Canon.str_member protocol_version_key))
      in
      match version with Some _ -> Some Modern | None -> None)
  | Notification _ | Response _ | Error_response _ -> None

let strip_varying_meta params =
  match Canon.member "_meta" params with
  | None -> params
  | Some meta ->
      let meta =
        List.fold_left
          (fun m k -> Canon.remove_member k m)
          meta varying_meta_keys
      in
      if Canon.equal meta Canon.empty_object then
        Canon.remove_member "_meta" params
      else Canon.set_member "_meta" meta params

let clean_params ~method_ params =
  let params =
    if String.equal method_ "initialize" then
      Canon.remove_member "clientInfo" params
    else params
  in
  let params = strip_varying_meta params in
  if Canon.equal params Canon.empty_object then None else Some params

let canonical_request = function
  | Request { method_; params; _ } ->
      let params = Option.bind params (clean_params ~method_) in
      Some
        (object_of
           (("method", Canon.string method_)
           :: Option.to_list (opt_field "params" params)))
  | Notification _ | Response _ | Error_response _ -> None

let tool_call = function
  | Request { method_ = "tools/call"; params = Some p; _ } ->
      Option.map
        (fun name ->
          ( name,
            Option.value
              (Canon.member "arguments" p)
              ~default:Canon.empty_object ))
        (Canon.str_member "name" p)
  | Request _ | Notification _ | Response _ | Error_response _ -> None

let with_id new_id = function
  | Request r -> Request { r with id = new_id }
  | Response r -> Response { r with id = new_id }
  | Error_response r -> Error_response { r with id = Some new_id }
  | Notification _ as n -> n
