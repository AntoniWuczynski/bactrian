open Config_error

let type_names =
  [ "object"; "array"; "string"; "number"; "integer"; "boolean"; "null" ]

let keywords =
  [
    "type";
    "properties";
    "required";
    "enum";
    "items";
    "additionalProperties";
    "title";
    "description";
  ]

let join p k = if p = "" then k else p ^ "." ^ k
let err path message = { path; message }

let strings_of v =
  Option.bind (Canon.to_list_opt v) (fun l ->
      let ss = List.filter_map Canon.to_string_opt l in
      if List.compare_lengths l ss = 0 then Some ss else None)

let rec check_at path schema =
  match Canon.to_fields_opt schema with
  | None -> [ err path "a schema must be an object" ]
  | Some fields -> List.concat_map (check_keyword path) fields

and check_keyword path (k, v) =
  let p = join path k in
  match k with
  | "type" -> (
      let names =
        match Canon.to_string_opt v with
        | Some s -> Some [ s ]
        | None -> strings_of v
      in
      match names with
      | Some ns
        when ns <> [] && List.for_all (fun n -> List.mem n type_names) ns ->
          []
      | Some _ | None ->
          [
            err p
              ("expected one of "
              ^ String.concat ", " type_names
              ^ " or a list of them");
          ])
  | "properties" -> (
      match Canon.to_fields_opt v with
      | None -> [ err p "expected an object of schemas" ]
      | Some props ->
          List.concat_map (fun (n, s) -> check_at (join p n) s) props)
  | "required" -> (
      match strings_of v with
      | Some _ -> []
      | None -> [ err p "expected a list of strings" ])
  | "enum" -> (
      match Canon.to_list_opt v with
      | Some _ -> []
      | None -> [ err p "expected a list" ])
  | "items" -> check_at p v
  | "additionalProperties" -> (
      match Canon.to_bool_opt v with
      | Some _ -> []
      | None -> [ err p "expected true or false" ])
  | "title" | "description" -> []
  | _ ->
      [
        err p ("unsupported keyword, supported: " ^ String.concat ", " keywords);
      ]

let check schema = check_at "" schema

let type_matches name v =
  match v with
  | Canon.Object _ -> String.equal name "object"
  | List _ -> String.equal name "array"
  | String _ -> String.equal name "string"
  | Bool _ -> String.equal name "boolean"
  | Null -> String.equal name "null"
  | Int _ | Big_int _ ->
      String.equal name "integer" || String.equal name "number"
  | Float _ -> String.equal name "number"

let describe = function
  | Canon.Null -> "null"
  | Bool _ -> "boolean"
  | Int _ | Big_int _ -> "integer"
  | Float _ -> "number"
  | String _ -> "string"
  | List _ -> "array"
  | Object _ -> "object"

let rec violations path schema inst =
  let field k = Canon.member k schema in
  let type_errors =
    match field "type" with
    | None -> []
    | Some t ->
        let names =
          match Canon.to_string_opt t with
          | Some s -> [ s ]
          | None -> Option.value (strings_of t) ~default:[]
        in
        if List.exists (fun n -> type_matches n inst) names then []
        else
          [
            Printf.sprintf "%s: expected %s, got %s" path
              (String.concat " or " names)
              (describe inst);
          ]
  in
  let enum_errors =
    match Option.bind (field "enum") Canon.to_list_opt with
    | Some options when not (List.exists (Canon.equal inst) options) ->
        [
          Printf.sprintf "%s: %s is not one of the allowed values" path
            (Canon.to_string inst);
        ]
    | Some _ | None -> []
  in
  let props =
    Option.value
      (Option.bind (field "properties") Canon.to_fields_opt)
      ~default:[]
  in
  let object_errors =
    match Canon.to_fields_opt inst with
    | None -> []
    | Some fields ->
        let required =
          Option.value (Option.bind (field "required") strings_of) ~default:[]
        in
        let missing =
          List.filter_map
            (fun k ->
              if List.mem_assoc k fields then None
              else
                Some (Printf.sprintf "%s: required field %S is missing" path k))
            required
        in
        let nested =
          List.concat_map
            (fun (k, sub) ->
              match List.assoc_opt k fields with
              | Some v -> violations (path ^ "." ^ k) sub v
              | None -> [])
            props
        in
        let extra =
          match
            Option.bind (field "additionalProperties") Canon.to_bool_opt
          with
          | Some false ->
              List.filter_map
                (fun (k, _) ->
                  if List.mem_assoc k props then None
                  else Some (Printf.sprintf "%s: unexpected field %S" path k))
                fields
          | Some true | None -> []
        in
        missing @ nested @ extra
  in
  let item_errors =
    match (field "items", Canon.to_list_opt inst) with
    | Some sub, Some l ->
        List.concat
          (List.mapi
             (fun i v -> violations (Printf.sprintf "%s[%d]" path i) sub v)
             l)
    | Some _, None | None, _ -> []
  in
  type_errors @ enum_errors @ object_errors @ item_errors

let validate ~schema inst =
  match check schema with
  | [] -> violations "$" schema inst
  | problems ->
      List.map
        (fun e -> Format.asprintf "invalid schema: %a" Config_error.pp e)
        problems
