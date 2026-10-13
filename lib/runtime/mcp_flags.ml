open Bactrian_core

let ( let* ) = Result.bind
let stdio_name = "mcp-stdio"
let http_name = "mcp-http"
let policy_name = "mcp-policy"
let stdio_flag = "--" ^ stdio_name
let http_flag = "--" ^ http_name
let policy_flag = "--" ^ policy_name

(* NAME=VALUE: a name has no '=', so the first one splits. *)
let assignment flag s =
  match String.index_opt s '=' with
  | Some i ->
      Ok (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> Error (Printf.sprintf "%s %S: expected NAME=VALUE" flag s)

let named flag name =
  if Experiment.valid_mcp_name name then Ok ()
  else
    Error
      (Printf.sprintf
         "%s: the server name %S has letters, digits, '_' and '-' only" flag
         name)

let argv name value =
  let fail reason =
    Error (Printf.sprintf "%s %s: the command %s" stdio_flag name reason)
  in
  match Yojson.Safe.from_string value with
  | exception Yojson.Json_error m ->
      fail (Printf.sprintf "is not valid JSON (%s)" m)
  | `List [] -> fail "is empty"
  | `List items ->
      List.fold_right
        (fun item acc ->
          let* rest = acc in
          match item with
          | `String s -> Ok (s :: rest)
          | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `List _ | `Assoc _
            ->
              fail "has an item that is not a string")
        items (Ok [])
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ ->
      fail "is not a JSON array of strings"

let declared flag parse_value values =
  List.fold_left
    (fun acc s ->
      let* declared = acc in
      let* name, value = assignment flag s in
      let* () = named flag name in
      let* server = parse_value name value in
      Ok ((name, server, flag) :: declared))
    (Ok []) values
  |> Result.map List.rev

let stdio_server name value : (Mcp_proxy.server, string) result =
  let* argv = argv name value in
  Ok (Mcp_proxy.Stdio { argv; env = []; cwd = None })

let http_server name value : (Mcp_proxy.server, string) result =
  if Experiment.valid_mcp_url value then
    Ok (Mcp_proxy.Http { url = Uri.of_string value })
  else
    Error
      (Printf.sprintf "%s %s: %S is not an http:// or https:// URL" http_flag
         name value)

(* A name is declared once, and no two HTTP servers share a variable. *)
let check_names servers =
  let http_names =
    List.filter_map
      (fun (name, (server : Mcp_proxy.server), _) ->
        match server with Http _ -> Some name | Stdio _ -> None)
      servers
  in
  let rec go seen = function
    | [] -> Ok ()
    | (name, server, flag) :: rest -> (
        match List.assoc_opt name seen with
        | Some first ->
            Error
              (Printf.sprintf "%s %s: the server %s is already declared by %s"
                 flag name name first)
        | None -> (
            match (server : Mcp_proxy.server) with
            | Http _ -> (
                match Experiment.mcp_http_clash ~http_names name with
                | Some v ->
                    Error
                      (Printf.sprintf
                         "%s %s: %s would also be another server's variable"
                         flag name v)
                | None -> go ((name, flag) :: seen) rest)
            | Stdio _ -> go ((name, flag) :: seen) rest))
  in
  go [] servers

let policies servers values =
  List.fold_left
    (fun acc s ->
      let* chosen = acc in
      let* name, value = assignment policy_flag s in
      let fail reason =
        Error (Printf.sprintf "%s %s: %s" policy_flag name reason)
      in
      if not (List.exists (fun (n, _, _) -> String.equal n name) servers) then
        fail "no such server: declare it with --mcp-stdio or --mcp-http"
      else if List.exists (fun (n, _) -> String.equal n name) chosen then
        fail "given twice"
      else
        match Experiment.policy_of_name value with
        | Some Replay ->
            fail
              "replay needs a recording and a record run makes one: use \
               record, passthrough or block"
        | Some ((Record | Passthrough | Block) as p) -> Ok ((name, p) :: chosen)
        | None ->
            fail
              (Printf.sprintf
                 "%S is not a policy: use record, passthrough or block" value))
    (Ok []) values

let parse ~stdio ~http ~policy =
  let* stdio = declared stdio_flag stdio_server stdio in
  let* http = declared http_flag http_server http in
  let servers = stdio @ http in
  let* () = check_names servers in
  let* chosen = policies servers policy in
  Ok
    (List.map
       (fun (name, server, _) : Mcp_proxy.config ->
         {
           name;
           server;
           policy =
             Option.value (List.assoc_opt name chosen) ~default:Mcp_proxy.Record;
         })
       servers
    |> List.sort (fun (a : Mcp_proxy.config) b -> String.compare a.name b.name)
    )
