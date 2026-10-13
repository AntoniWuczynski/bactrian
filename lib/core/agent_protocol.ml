type to_agent =
  | Start of {
      run_id : string;
      scenario : string;
      variant : string;
      sim_now : string;
      state_dir : string;
    }
  | Turn of { turn : int; content : string }
  | End

type from_agent =
  | Reply of { turn : int; content : string }
  | Event of { turn : int; name : string; data : Canon.t }
  | Agent_error of { turn : int; message : string }

type error =
  | Invalid_json of string
  | Unsupported_version of int
  | Unknown_type of string
  | Bad_message of { type_ : string; reason : string }

let version = 1

let line type_ fields =
  Yojson.Safe.to_string
    (`Assoc (("v", `Int version) :: ("type", `String type_) :: fields))

let encode = function
  | Start { run_id; scenario; variant; sim_now; state_dir } ->
      line "start"
        [
          ("run_id", `String run_id);
          ("scenario", `String scenario);
          ("variant", `String variant);
          ("sim_now", `String sim_now);
          ("state_dir", `String state_dir);
        ]
  | Turn { turn; content } ->
      line "turn" [ ("turn", `Int turn); ("content", `String content) ]
  | End -> line "end" []

let encode_from_agent = function
  | Reply { turn; content } ->
      line "reply" [ ("turn", `Int turn); ("content", `String content) ]
  | Event { turn; name; data } ->
      line "event"
        [
          ("turn", `Int turn);
          ("name", `String name);
          ("data", Canon.to_yojson data);
        ]
  | Agent_error { turn; message } ->
      line "error" [ ("turn", `Int turn); ("message", `String message) ]

(* Decoding *)

let ( let* ) = Result.bind

type envelope = { type_ : string; fields : (string * Canon.t) list }

let bad type_ reason = Error (Bad_message { type_; reason })

let parse_envelope text =
  let* doc =
    Result.map_error
      (fun e -> Invalid_json (Format.asprintf "%a" Canon.pp_error e))
      (Canon.of_string text)
  in
  match Canon.to_fields_opt doc with
  | None -> bad "" "not a JSON object"
  | Some fields -> (
      match List.assoc_opt "v" fields with
      | None -> bad "" "missing field \"v\""
      | Some v -> (
          match Canon.to_int_opt v with
          | None -> bad "" "field \"v\" is not an integer"
          | Some n when not (Int.equal n version) ->
              Error (Unsupported_version n)
          | Some _ -> (
              match
                Option.bind (List.assoc_opt "type" fields) Canon.to_string_opt
              with
              | None -> bad "" "missing or non-string field \"type\""
              | Some type_ -> Ok { type_; fields })))

let field env key conv what =
  match Option.bind (List.assoc_opt key env.fields) conv with
  | Some x -> Ok x
  | None -> bad env.type_ (Printf.sprintf "field %S missing or not %s" key what)

let str env key = field env key Canon.to_string_opt "a string"
let int env key = field env key Canon.to_int_opt "an integer"

let obj env key =
  field env key
    (fun c -> Option.map (fun _ -> c) (Canon.to_fields_opt c))
    "an object"

let only env allowed =
  match
    List.find_opt
      (fun (k, _) ->
        not (List.exists (String.equal k) ("v" :: "type" :: allowed)))
      env.fields
  with
  | None -> Ok ()
  | Some (k, _) -> bad env.type_ (Printf.sprintf "unknown field %S" k)

let decode text =
  let* env = parse_envelope text in
  match env.type_ with
  | "reply" ->
      let* () = only env [ "turn"; "content" ] in
      let* turn = int env "turn" in
      let* content = str env "content" in
      Ok (Reply { turn; content })
  | "event" ->
      let* () = only env [ "turn"; "name"; "data" ] in
      let* turn = int env "turn" in
      let* name = str env "name" in
      let* data = obj env "data" in
      Ok (Event { turn; name; data })
  | "error" ->
      let* () = only env [ "turn"; "message" ] in
      let* turn = int env "turn" in
      let* message = str env "message" in
      Ok (Agent_error { turn; message })
  | other -> Error (Unknown_type other)

let decode_to_agent text =
  let* env = parse_envelope text in
  match env.type_ with
  | "start" ->
      let* () =
        only env [ "run_id"; "scenario"; "variant"; "sim_now"; "state_dir" ]
      in
      let* run_id = str env "run_id" in
      let* scenario = str env "scenario" in
      let* variant = str env "variant" in
      let* sim_now = str env "sim_now" in
      let* state_dir = str env "state_dir" in
      Ok (Start { run_id; scenario; variant; sim_now; state_dir })
  | "turn" ->
      let* () = only env [ "turn"; "content" ] in
      let* turn = int env "turn" in
      let* content = str env "content" in
      Ok (Turn { turn; content })
  | "end" ->
      let* () = only env [] in
      Ok End
  | other -> Error (Unknown_type other)

let pp_error ppf = function
  | Invalid_json msg -> Format.fprintf ppf "invalid JSON: %s" msg
  | Unsupported_version n ->
      Format.fprintf ppf "unsupported protocol version %d" n
  | Unknown_type t -> Format.fprintf ppf "unknown message type %S" t
  | Bad_message { type_; reason } ->
      if String.equal type_ "" then Format.fprintf ppf "bad message: %s" reason
      else Format.fprintf ppf "bad %S message: %s" type_ reason
