module Canon = Bactrian_core.Canon

type era = Modern | Legacy

let protocol_version = function
  | Modern -> "2026-07-28"
  | Legacy -> "2025-11-25"

let session_id = "fake-mcp-session"

(* The built-in documents below are literals, so a parse failure is a bug in
   this file. *)
let literal text =
  match Canon.of_string text with
  | Ok v -> v
  | Error _ -> invalid_arg "Fake_mcp: malformed built-in JSON"

let server_info = literal {|{"name":"fake-weather","version":"1.0.0"}|}

let tool =
  literal
    {|{"name":"get_weather","description":"Get the current weather for a city.","inputSchema":{"type":"object","properties":{"city":{"type":"string"}},"required":["city"]}}|}

type outcome =
  | Result of Canon.t
  | Fail of { code : int; message : string; data : Canon.t option }
  | No_reply

let fail ?data code message = Fail { code; message; data }
let set = Canon.set_member

(* Every modern result says it is complete. *)
let complete era result =
  match era with
  | Modern -> set "resultType" (Canon.string "complete") result
  | Legacy -> result

let discover =
  literal
    {|{"supportedVersions":["2026-07-28"],"capabilities":{"tools":{}},"cacheScope":"public","ttlMs":60000,"_meta":{"io.modelcontextprotocol/serverInfo":{"name":"fake-weather","version":"1.0.0"}}}|}

let initialize =
  Canon.empty_object
  |> set "protocolVersion" (Canon.string "2025-11-25")
  |> set "capabilities" (literal {|{"tools":{}}|})
  |> set "serverInfo" server_info

let tools_list era =
  let tools = set "tools" (Canon.list [ tool ]) Canon.empty_object in
  match era with
  | Modern ->
      complete era
        (tools
        |> set "ttlMs" (Canon.int 60000)
        |> set "cacheScope" (Canon.string "public"))
  | Legacy -> tools

let text_result ~is_error text =
  let item =
    Canon.empty_object
    |> set "type" (Canon.string "text")
    |> set "text" (Canon.string text)
  in
  Canon.empty_object
  |> set "content" (Canon.list [ item ])
  |> set "isError" (Canon.bool is_error)

let tools_call era params =
  match Option.bind (Canon.member "name" params) Canon.to_string_opt with
  | Some "get_weather" -> (
      let city =
        Option.bind (Canon.member "arguments" params) (Canon.str_member "city")
      in
      match city with
      | Some city ->
          Result
            (complete era
               (text_result ~is_error:false ("Sunny, 21C in " ^ city)))
      | None ->
          Result (complete era (text_result ~is_error:true "city is required")))
  | Some other -> fail (-32602) ("Unknown tool: " ^ other)
  | None -> fail (-32602) "Missing tool name"

let requested_version params =
  Option.bind
    (Canon.member "_meta" params)
    (Canon.str_member "io.modelcontextprotocol/protocolVersion")

let unsupported_version requested =
  let data =
    Canon.empty_object
    |> set "supported" (Canon.list [ Canon.string (protocol_version Modern) ])
    |> set "requested"
         (Option.fold ~none:Canon.null ~some:Canon.string requested)
  in
  fail ~data (-32022) "Unsupported protocol version"

let method_not_found m = fail (-32601) ("Method not found: " ^ m)

(* [message] is a JSON-RPC object. *)
let handle era message =
  let params =
    Option.value ~default:Canon.empty_object (Canon.member "params" message)
  in
  match (Canon.str_member "method" message, Canon.member "id" message) with
  | None, _ -> fail (-32600) "Invalid request"
  | Some _, None -> No_reply
  | Some m, Some _ -> (
      match (era, m) with
      | Modern, "server/discover" -> Result (complete era discover)
      | Modern, ("tools/list" | "tools/call") -> (
          match requested_version params with
          | Some v when String.equal v (protocol_version Modern) ->
              if String.equal m "tools/list" then Result (tools_list era)
              else tools_call era params
          | (Some _ | None) as v -> unsupported_version v)
      | Legacy, "initialize" -> Result initialize
      | Legacy, "tools/list" -> Result (tools_list era)
      | Legacy, "tools/call" -> tools_call era params
      | Modern, _ | Legacy, _ -> method_not_found m)

let envelope ~id outcome =
  let base = set "id" id (literal {|{"jsonrpc":"2.0"}|}) in
  match outcome with
  | No_reply -> None
  | Result r -> Some (set "result" r base)
  | Fail { code; message; data } ->
      let error =
        Canon.empty_object
        |> set "code" (Canon.int code)
        |> set "message" (Canon.string message)
      in
      let error =
        Option.fold ~none:error ~some:(fun d -> set "data" d error) data
      in
      Some (set "error" error base)

let id_of message = Option.value ~default:Canon.null (Canon.member "id" message)

type parsed = Message of Canon.t | Rejected of outcome

let parse text =
  match Canon.of_string text with
  | Error _ -> Rejected (fail (-32700) "Parse error")
  | Ok (Canon.Object _ as m) -> Message m
  | Ok
      ( Canon.Null | Canon.Bool _ | Canon.Int _ | Canon.Big_int _
      | Canon.Float _ | Canon.String _ | Canon.List _ ) ->
      Rejected (fail (-32600) "Batches and non-objects are not supported")

(* Stdio *)

let append_line path line =
  Out_channel.with_open_gen [ Open_append; Open_creat; Open_text ] 0o644 path
    (fun oc -> output_string oc (line ^ "\n"))

let answer era line =
  match parse line with
  | Rejected outcome -> envelope ~id:Canon.null outcome
  | Message m -> envelope ~id:(id_of m) (handle era m)

let serve_stdio ~era ~log ic oc =
  let rec loop () =
    match In_channel.input_line ic with
    | None -> ()
    | Some line when String.equal (String.trim line) "" -> loop ()
    | Some line ->
        Option.iter (fun path -> append_line path line) log;
        Option.iter
          (fun reply ->
            output_string oc (Canon.to_string reply ^ "\n");
            flush oc)
          (answer era line);
        loop ()
  in
  loop ()

(* Streamable HTTP *)

type request = {
  meth : string;
  path : string;
  headers : (string * string) list;
  body : string;
}

type t = { port : int; log : request list ref  (** Newest first. *) }

type http_reply = {
  status : int;
  extra : (string * string) list;
  payload : string;
}

let json_reply ?(extra = []) status doc =
  {
    status;
    extra = ("content-type", "application/json") :: extra;
    payload = Canon.to_string doc;
  }

let no_body status = { status; extra = []; payload = "" }

let error_reply status ~id code message =
  match envelope ~id (fail code message) with
  | Some doc -> json_reply status doc
  | None -> no_body status

let mismatch ~id what = error_reply 400 ~id (-32020) ("Header mismatch: " ^ what)

let status_of_code = function
  | -32601 -> 404
  | -32022 | -32600 | -32700 -> 400
  | _ -> 200

let reply_of_outcome ~era ~id = function
  | No_reply -> no_body 202
  | Result _ as o ->
      Option.fold ~none:(no_body 202) ~some:(json_reply 200) (envelope ~id o)
  | Fail { code; _ } as o ->
      let status =
        match era with Modern -> status_of_code code | Legacy -> 200
      in
      Option.fold ~none:(no_body 202) ~some:(json_reply status) (envelope ~id o)

(* The header checks a modern server makes before it looks at the method. *)
let modern_header_error header message =
  let id = id_of message in
  let method_ = Canon.str_member "method" message in
  let params =
    Option.value ~default:Canon.empty_object (Canon.member "params" message)
  in
  match (header "mcp-protocol-version", header "mcp-method") with
  | None, _ -> Some (mismatch ~id "MCP-Protocol-Version is missing")
  | _, None -> Some (mismatch ~id "Mcp-Method is missing")
  | Some version, Some hm -> (
      let body_version = requested_version params in
      if
        Option.fold ~none:false
          ~some:(fun v -> not (String.equal v version))
          body_version
      then Some (mismatch ~id "MCP-Protocol-Version differs from the body")
      else if not (Option.equal String.equal (Some hm) method_) then
        Some (mismatch ~id "Mcp-Method differs from the body")
      else
        match method_ with
        | Some "tools/call"
          when not
                 (Option.equal String.equal (header "mcp-name")
                    (Canon.str_member "name" params)) ->
            Some (mismatch ~id "Mcp-Name differs from the body")
        | Some _ | None -> None)

let legacy_session_error header message =
  match Canon.str_member "method" message with
  | Some "initialize" -> None
  | Some _ | None -> (
      match header "mcp-session-id" with
      | None ->
          Some (error_reply 400 ~id:(id_of message) (-32600) "Missing session")
      | Some s when String.equal s session_id -> None
      | Some _ ->
          Some (error_reply 404 ~id:(id_of message) (-32600) "Unknown session"))

let route era (r : request) =
  let header name = List.assoc_opt name r.headers in
  if not (String.equal r.meth "POST") then
    { (no_body 405) with extra = [ ("allow", "POST") ] }
  else
    match parse r.body with
    | Rejected (Fail { code; message; _ }) ->
        error_reply 400 ~id:Canon.null code message
    | Rejected (Result _ | No_reply) -> no_body 400
    | Message m -> (
        let check =
          match era with
          | Modern -> modern_header_error header m
          | Legacy -> legacy_session_error header m
        in
        match check with
        | Some reply -> reply
        | None -> (
            let reply = reply_of_outcome ~era ~id:(id_of m) (handle era m) in
            let initialising =
              Option.equal String.equal
                (Canon.str_member "method" m)
                (Some "initialize")
            in
            match era with
            | Legacy when initialising && reply.status = 200 ->
                {
                  reply with
                  extra = ("mcp-session-id", session_id) :: reply.extra;
                }
            | Legacy | Modern -> reply))

let write_reply (reply : http_reply) =
  let headers =
    Http.Header.of_list
      (("content-length", string_of_int (String.length reply.payload))
      :: reply.extra)
  in
  let response =
    Http.Response.make ~status:(Http.Status.of_int reply.status) ~headers ()
  in
  ( response,
    fun _ic oc ->
      Eio.Buf_write.string oc reply.payload;
      Eio.Buf_write.flush oc )

let start ~sw ~net era =
  let log = ref [] in
  let callback _conn http_request body =
    let body =
      match Eio.Buf_read.parse ~max_size:max_int Eio.Buf_read.take_all body with
      | Ok s -> s
      | Error (`Msg _) -> ""
    in
    let r =
      {
        meth = Http.Method.to_string (Http.Request.meth http_request);
        path = Uri.path (Uri.of_string (Http.Request.resource http_request));
        headers =
          List.map
            (fun (k, v) -> (String.lowercase_ascii k, v))
            (Http.Header.to_list (Http.Request.headers http_request));
        body;
      }
    in
    log := r :: !log;
    write_reply (route era r)
  in
  let sock =
    Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr sock with
    | `Tcp (_, port) -> port
    | `Unix _ -> invalid_arg "Fake_mcp.start: listener is not TCP"
  in
  let server = Cohttp_eio.Server.make_expert ~callback () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Cohttp_eio.Server.run sock server ~on_error:(fun _ -> ()));
  { port; log }

let port t = t.port
let base_url t = Printf.sprintf "http://127.0.0.1:%d" t.port
let requests t = List.rev !(t.log)
