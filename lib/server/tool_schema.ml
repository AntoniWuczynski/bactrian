open Bactrian_core

let ( let* ) = Result.bind

type kind =
  | String
  | Strings
  | Nonempty_strings
  | Number
  | Numbers
  | Integer
  | Boolean
  | Enum of string list

type property = {
  name : string;
  kind : kind;
  required : bool;
  description : string;
}

let obj fields =
  List.fold_left
    (fun o (k, v) -> Canon.set_member k v o)
    Canon.empty_object fields

let typed t = [ ("type", Canon.string t) ]
let array_of t = [ ("type", Canon.string "array"); ("items", obj (typed t)) ]

let kind_schema = function
  | String -> typed "string"
  | Strings -> array_of "string"
  | Nonempty_strings -> ("minItems", Canon.int 1) :: array_of "string"
  | Number -> typed "number"
  | Numbers -> array_of "number"
  | Integer -> typed "integer"
  | Boolean -> typed "boolean"
  | Enum values ->
      ("enum", Canon.list (List.map Canon.string values)) :: typed "string"

let input_schema properties =
  let required =
    List.filter_map
      (fun p -> if p.required then Some (Canon.string p.name) else None)
      properties
  in
  obj
    ([
       ("type", Canon.string "object");
       ( "properties",
         obj
           (List.map
              (fun p ->
                ( p.name,
                  obj
                    (("description", Canon.string p.description)
                    :: kind_schema p.kind) ))
              properties) );
       ("additionalProperties", Canon.bool false);
     ]
    @
    match required with
    | [] -> []
    | _ :: _ -> [ ("required", Canon.list required) ])

let describe = function
  | String -> "a string"
  | Strings -> "an array of strings"
  | Nonempty_strings -> "an array of at least one string"
  | Number -> "a number"
  | Numbers -> "an array of numbers"
  | Integer -> "an integer"
  | Boolean -> "true or false"
  | Enum values -> "one of " ^ String.concat ", " values

let wrong p =
  Error (Printf.sprintf "argument %s: must be %s" p.name (describe p.kind))

let find properties name =
  List.find_opt (fun p -> String.equal p.name name) properties

let check_member properties (name, value) =
  match find properties name with
  | None -> Error (Printf.sprintf "unknown argument %s" name)
  | Some ({ kind = Enum values; _ } as p) -> (
      match Canon.to_string_opt value with
      | Some v when List.exists (String.equal v) values -> Ok ()
      | Some _ -> wrong p
      | None -> Ok ())
  | Some ({ kind = Nonempty_strings; _ } as p) -> (
      match Canon.to_list_opt value with
      | Some [] -> wrong p
      | Some _ | None -> Ok ())
  | Some { kind = String | Strings | Number | Numbers | Integer | Boolean; _ }
    ->
      Ok ()

let rec all f = function
  | [] -> Ok ()
  | x :: rest ->
      let* () = f x in
      all f rest

(* A decoder's error is the path of the field it failed on, such as
   [Mcp_tools.run_args.experiment]. A field named after an OCaml keyword
   carries a trailing underscore. *)
let field_of_path path =
  let last =
    match List.rev (String.split_on_char '.' path) with
    | last :: _ -> last
    | [] -> path
  in
  if String.ends_with ~suffix:"_" last then
    String.sub last 0 (String.length last - 1)
  else last

let decode properties of_yojson arguments =
  let arguments = Option.value ~default:Canon.empty_object arguments in
  match Canon.to_fields_opt arguments with
  | None -> Error "arguments: must be an object"
  | Some fields -> (
      let* () = all (check_member properties) fields in
      let* () =
        all
          (fun p ->
            if p.required && not (List.mem_assoc p.name fields) then
              Error (Printf.sprintf "missing argument %s" p.name)
            else Ok ())
          properties
      in
      match of_yojson (Canon.to_yojson arguments) with
      | Ok v -> Ok v
      | Error path -> (
          match find properties (field_of_path path) with
          | Some p -> wrong p
          | None -> Error ("invalid arguments: " ^ path)))
