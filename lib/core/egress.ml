type policy = Experiment.policy
type t = { default : policy; hosts : (string * policy) list }

let block_all = { default = Block; hosts = [] }

let host_port host port =
  if String.contains host ':' then Printf.sprintf "[%s]:%d" host port
  else Printf.sprintf "%s:%d" host port

let policy t ~host ~port =
  let find name =
    List.find_map
      (fun (k, p) ->
        if String.equal (String.lowercase_ascii k) name then Some p else None)
      t.hosts
  in
  let host = String.lowercase_ascii host in
  match find (host_port host port) with
  | Some p -> p
  | None -> Option.value (find host) ~default:t.default

let port_of text =
  match int_of_string_opt text with
  | Some p
    when p >= 1 && p <= 65535
         && String.for_all (fun c -> c >= '0' && c <= '9') text ->
      Ok p
  | Some _ | None -> Error (Printf.sprintf "%S is not a port" text)

let authority target =
  let bad () = Error (Printf.sprintf "%S is not host:port" target) in
  let ( let* ) = Result.bind in
  if String.length target > 0 && Char.equal target.[0] '[' then
    match String.index_opt target ']' with
    | Some close
      when close + 1 < String.length target && Char.equal target.[close + 1] ':'
      ->
        let host = String.sub target 1 (close - 1) in
        let* port =
          port_of
            (String.sub target (close + 2) (String.length target - close - 2))
        in
        if String.equal host "" then bad ()
        else Ok (String.lowercase_ascii host, port)
    | Some _ | None -> bad ()
  else
    match String.rindex_opt target ':' with
    | Some i when i > 0 && not (String.contains (String.sub target 0 i) ':') ->
        let* port =
          port_of (String.sub target (i + 1) (String.length target - i - 1))
        in
        Ok (String.lowercase_ascii (String.sub target 0 i), port)
    | Some _ | None -> bad ()

let canonical ~authority ~content_type body =
  let body =
    if String.equal body "" then Canon.null
    else
      match Canon.of_string body with
      | Ok v -> v
      | Error _ ->
          Canon.of_fields
            [
              ( "blake2b-256",
                Canon.string Digest.BLAKE256.(to_hex (string body)) );
            ]
  in
  Canon.of_fields
    [
      ("authority", Canon.string authority);
      ( "content-type",
        match content_type with Some c -> Canon.string c | None -> Canon.null );
      ("body", body);
    ]

let to_json t =
  `Assoc
    [
      ("default", `String (Experiment.policy_name t.default));
      ( "hosts",
        `Assoc
          (List.map
             (fun (h, p) -> (h, `String (Experiment.policy_name p)))
             t.hosts) );
    ]

let policy_of_json what : Yojson.Safe.t -> (policy, string) result = function
  | `String s -> (
      match Experiment.policy_of_name s with
      | Some p -> Ok p
      | None -> Error (Printf.sprintf "%s: unknown policy %S" what s))
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `List _ | `Assoc _ ->
      Error (what ^ ": not a string")

let of_json : Yojson.Safe.t -> (t, string) result = function
  | `Assoc fields -> (
      let ( let* ) = Result.bind in
      match
        (List.assoc_opt "default" fields, List.assoc_opt "hosts" fields)
      with
      | Some d, Some (`Assoc hosts) ->
          let* default = policy_of_json "default" d in
          let* hosts =
            List.fold_right
              (fun (h, p) acc ->
                let* rest = acc in
                let* p = policy_of_json h p in
                Ok ((h, p) :: rest))
              hosts (Ok [])
          in
          Ok { default; hosts }
      | _ -> Error "expected {\"default\": ..., \"hosts\": {...}}")
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
      Error "not an object"

let node_env_proxy text =
  let text = String.trim text in
  let digits =
    if String.starts_with ~prefix:"v" text then
      String.sub text 1 (String.length text - 1)
    else text
  in
  match List.map int_of_string_opt (String.split_on_char '.' digits) with
  | [ Some major; Some minor; Some _ ] ->
      Ok (major >= 24 || (major = 22 && minor >= 21))
  | _ -> Error (Printf.sprintf "%S is not a Node.js version" text)

let of_flags ~default flags =
  let ( let* ) = Result.bind in
  let policy_named what name =
    match Experiment.policy_of_name name with
    | Some p -> Ok p
    | None ->
        Error
          (Printf.sprintf
             "%s: unknown policy %S (record, replay, passthrough or block)" what
             name)
  in
  let* default =
    match default with
    | None -> Ok Experiment.Block
    | Some name -> policy_named "--egress-default" name
  in
  let* hosts =
    List.fold_right
      (fun flag acc ->
        let* rest = acc in
        match String.rindex_opt flag '=' with
        | Some i when i > 0 ->
            let host = String.sub flag 0 i in
            let* p =
              policy_named ("--egress " ^ flag)
                (String.sub flag (i + 1) (String.length flag - i - 1))
            in
            Ok ((host, p) :: rest)
        | Some _ | None ->
            Error (Printf.sprintf "--egress %S: expected HOST=POLICY" flag))
      flags (Ok [])
  in
  Ok { default; hosts }
