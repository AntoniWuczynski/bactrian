let ( let* ) = Result.bind

let parse text decode =
  match Yojson.Safe.from_string text with
  | exception Yojson.Json_error m -> Error m
  | json -> decode json

let first what = function
  | x :: _ -> Ok x
  | [] -> Error (what ^ " gave an empty list")

module Status = struct
  type host = { architecture : string }
  [@@deriving of_yojson { strict = false }]

  type t = { status : string; host : host }
  [@@deriving of_yojson { strict = false }]
end

type status = { running : bool; state : string; architecture : string }

let status text =
  let* (s : Status.t) = parse text Status.of_yojson in
  Ok
    {
      running = String.equal s.status "running";
      state = s.status;
      architecture = s.host.architecture;
    }

module Descriptor = struct
  type t = { digest : string } [@@deriving of_yojson { strict = false }]
end

module Image = struct
  type configuration = { descriptor : Descriptor.t }
  [@@deriving of_yojson { strict = false }]

  type t = { configuration : configuration }
  [@@deriving of_yojson { strict = false }]

  type many = t list [@@deriving of_yojson]
end

let image_digest text =
  let* images = parse text Image.many_of_yojson in
  let* (i : Image.t) = first "image inspect" images in
  Ok i.configuration.descriptor.digest

module Container = struct
  type image = { descriptor : Descriptor.t }
  [@@deriving of_yojson { strict = false }]

  type configuration = { image : image }
  [@@deriving of_yojson { strict = false }]

  type t = { configuration : configuration }
  [@@deriving of_yojson { strict = false }]

  type many = t list [@@deriving of_yojson]
end

let container_digest text =
  let* containers = parse text Container.many_of_yojson in
  let* (c : Container.t) = first "inspect" containers in
  Ok c.configuration.image.descriptor.digest

(* An object of strings, as container prints labels. *)
let labels_of_yojson = function
  | `Assoc fields ->
      List.fold_right
        (fun (k, v) acc ->
          let* rest = acc in
          match v with
          | `String s -> Ok ((k, s) :: rest)
          | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `List _ | `Assoc _
          | `Tuple _ | `Variant _ ->
              Error (Printf.sprintf "the label %S is not a string" k))
        fields (Ok [])
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _
  | `Tuple _ | `Variant _ ->
      Error "labels: expected an object"

module Listed = struct
  type configuration = {
    id : string;
    labels : (string * string) list; [@of_yojson labels_of_yojson]
  }
  [@@deriving of_yojson { strict = false }]

  type t = { configuration : configuration }
  [@@deriving of_yojson { strict = false }]
end

type listed = { id : string; labels : (string * string) list }

(* One by one, so a container of another tool that does not decode is left
   out alone. *)
let containers text =
  parse text (function
    | `List items ->
        Ok
          (List.filter_map
             (fun item ->
               match Listed.of_yojson item with
               | Ok (l : Listed.t) ->
                   Some
                     {
                       id = l.configuration.id;
                       labels = l.configuration.labels;
                     }
               | Error (_ : string) -> None)
             items)
    | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ ->
        Error "container list: expected a list")
