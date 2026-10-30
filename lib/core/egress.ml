type policy = Experiment.policy
type t = { default : policy; hosts : (string * policy) list }

let block_all = { default = Block; hosts = [] }
let host_port = Trace.host_port
let authority = Trace.authority

let policy t ~host ~port =
  let find name =
    List.find_map
      (fun (k, p) ->
        if String.equal (Trace.host_key k) name then Some p else None)
      t.hosts
  in
  let host = Trace.host_name host in
  match find (host_port host port) with
  | Some p -> p
  | None -> Option.value (find host) ~default:t.default

let split_query text =
  match String.index_opt text '?' with
  | None -> (text, None)
  | Some i ->
      ( String.sub text 0 i,
        Some (String.sub text (i + 1) (String.length text - i - 1)) )

let target ~host ~port text =
  let bad why = Error (Printf.sprintf "request target %S: %s" text why) in
  let scheme = "https://" in
  let n = String.length scheme in
  if String.contains text '#' then bad "a fragment is not sent to a server"
  else if String.starts_with ~prefix:"/" text then Ok (split_query text)
  else if
    String.length text >= n
    && String.equal (String.lowercase_ascii (String.sub text 0 n)) scheme
  then
    let rest = String.sub text n (String.length text - n) in
    let stop =
      String.to_seq rest
      |> Seq.find_index (fun c -> Char.equal c '/' || Char.equal c '?')
      |> Option.value ~default:(String.length rest)
    in
    let named = String.sub rest 0 stop in
    let path, query =
      split_query (String.sub rest stop (String.length rest - stop))
    in
    let path = if String.equal path "" then "/" else path in
    let parsed =
      match authority named with
      | Ok v -> Ok v
      | Error _ -> authority (named ^ ":443")
    in
    match parsed with
    | Ok (h, p) when String.equal h (Trace.host_name host) && p = port ->
        Ok (path, query)
    | Ok _ | Error _ ->
        bad
          (Printf.sprintf "only %s can be asked for on this tunnel"
             (host_port host port))
  else bad "only a path, or an https URL of the tunnel's host, is accepted"

let canonical ~authority ~meth ~path ~query ~content_type body =
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
      ("method", Canon.string meth);
      ("path", Canon.string path);
      ( "query",
        Canon.list
          (List.map
             (fun (k, v) -> Canon.list [ Canon.string k; Canon.string v ])
             query) );
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
  match Experiment.egress_host_errors hosts with
  | [] -> Ok { default; hosts }
  | (key, m) :: _ -> Error (Printf.sprintf "--egress %s: %s" key m)
