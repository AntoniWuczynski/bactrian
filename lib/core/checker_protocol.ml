type verdict = { pass : bool; score : float option; message : string option }

let trace_json entries =
  "[" ^ String.concat "," (List.map Trace.to_line entries) ^ "]"

let optional name convert = function
  | None | Some `Null -> Ok None
  | Some v -> (
      match convert v with
      | Some x -> Ok (Some x)
      | None -> Error (Printf.sprintf "field %S has the wrong type" name))

let to_number = function
  | `Int i -> Some (float_of_int i)
  | `Float f -> Some f
  | `Null | `Bool _ | `Intlit _ | `String _ | `Assoc _ | `List _ -> None

let to_text = function
  | `String s -> Some s
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `Assoc _ | `List _ -> None

let of_fields fields =
  let open Result.Syntax in
  let known = [ "pass"; "score"; "message" ] in
  match List.find_opt (fun (k, _) -> not (List.mem k known)) fields with
  | Some (k, _) -> Error (Printf.sprintf "unknown field %S" k)
  | None ->
      let* pass =
        match List.assoc_opt "pass" fields with
        | Some (`Bool b) -> Ok b
        | Some _ -> Error "field \"pass\" must be a boolean"
        | None -> Error "missing field \"pass\""
      in
      let* score = optional "score" to_number (List.assoc_opt "score" fields) in
      let* message =
        optional "message" to_text (List.assoc_opt "message" fields)
      in
      Ok { pass; score; message }

let decode_verdict text =
  match Yojson.Safe.from_string text with
  | `Assoc fields -> of_fields fields
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
      Error "the verdict must be a JSON object"
  | exception Yojson.Json_error msg -> Error ("not valid JSON: " ^ msg)
