type verdict = { pass : bool; score : float option; message : string option }

let trace_json entries =
  "[" ^ String.concat "," (List.map Trace.to_line entries) ^ "]"

let optional name convert = function
  | None -> Ok None
  | Some (v : Canon.t) -> (
      match v with
      | Null -> Ok None
      | Bool _ | Int _ | Big_int _ | Float _ | String _ | List _ | Object _ -> (
          match convert v with
          | Some x -> Ok (Some x)
          | None -> Error (Printf.sprintf "field %S has the wrong type" name)))

let to_number : Canon.t -> float option = function
  | Int i -> Some (float_of_int i)
  | Float f -> Some f
  | Big_int digits -> float_of_string_opt digits
  | Null | Bool _ | String _ | List _ | Object _ -> None

let of_fields fields =
  let open Result.Syntax in
  let known = [ "pass"; "score"; "message" ] in
  match
    List.find_opt
      (fun (k, _) -> not (List.exists (String.equal k) known))
      fields
  with
  | Some (k, _) -> Error (Printf.sprintf "unknown field %S" k)
  | None ->
      let field name = List.assoc_opt name fields in
      let* pass =
        match field "pass" with
        | Some v -> (
            match Canon.to_bool_opt v with
            | Some b -> Ok b
            | None -> Error "field \"pass\" must be a boolean")
        | None -> Error "missing field \"pass\""
      in
      let* score = optional "score" to_number (field "score") in
      let* message = optional "message" Canon.to_string_opt (field "message") in
      Ok { pass; score; message }

(* Parsed as canonical JSON, which refuses a duplicate key: a checker printing
   {"pass": true, "pass": false} has not given one verdict. *)
let decode_verdict text =
  match Canon.of_string text with
  | Error (Duplicate_key k) -> Error (Printf.sprintf "duplicate field %S" k)
  | Error (Syntax msg) -> Error ("not valid JSON: " ^ msg)
  | Error Non_finite_float -> Error "not valid JSON: a number is not finite"
  | Ok v -> (
      match Canon.to_fields_opt v with
      | Some fields -> of_fields fields
      | None -> Error "the verdict must be a JSON object")
