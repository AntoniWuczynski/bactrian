open Bactrian_core

let ( let* ) = Result.bind

let transport_name : Mcp_proxy.server -> string = function
  | Stdio _ -> "stdio"
  | Http _ -> "http"

let to_json configs =
  `List
    (List.map
       (fun (c : Mcp_proxy.config) ->
         `Assoc
           [
             ("name", `String c.name);
             ("transport", `String (transport_name c.server));
             ("policy", `String (Experiment.policy_name c.policy));
           ])
       configs)

let string_field fields key =
  match List.assoc_opt key fields with
  | Some (`String s) -> Ok s
  | Some _ -> Error (Printf.sprintf "%s is not a string" key)
  | None -> Error (Printf.sprintf "%s is missing" key)

let server_of_transport : string -> (Mcp_proxy.server, string) result = function
  | "stdio" -> Ok (Stdio { argv = []; env = []; cwd = None })
  | "http" -> Ok (Http { url = Uri.of_string "http://recorded.invalid/" })
  | other -> Error (Printf.sprintf "unknown transport %S" other)

let replay_policy name : (Mcp_proxy.policy, string) result =
  match Experiment.policy_of_name name with
  | Some Block -> Ok Block
  | Some (Record | Replay | Passthrough) -> Ok Replay
  | None -> Error (Printf.sprintf "unknown policy %S" name)

let entry : Yojson.Safe.t -> (Mcp_proxy.config, string) result = function
  | `Assoc fields ->
      let* name = string_field fields "name" in
      let* () =
        if Experiment.valid_mcp_name name then Ok ()
        else Error (Printf.sprintf "invalid server name %S" name)
      in
      let* server =
        Result.bind (string_field fields "transport") server_of_transport
      in
      let* policy = Result.bind (string_field fields "policy") replay_policy in
      Ok { Mcp_proxy.name; server; policy }
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
      Error "a server is not an object"

let replaying : Yojson.Safe.t -> (Mcp_proxy.config list, string) result =
  function
  | `List items ->
      List.fold_right
        (fun item acc ->
          let* rest = acc in
          let* c = entry item in
          Ok (c :: rest))
        items (Ok [])
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ ->
      Error "not a list"
