open Bactrian_core

let modern_version = "2026-07-28"
let latest_legacy = "2025-11-25"
let legacy_versions = [ latest_legacy; "2025-06-18"; "2025-03-26" ]

type era = Undecided | Legacy of string | Modern

type server = {
  name : string;
  version : string;
  instructions : string;
  tools : (string * Canon.t) list;
}

type t = { server : server; mutable era : era }

let create ?(era = Undecided) server = { server; era }
let era t = t.era

type task_op = Get_task | Update_task | Cancel_task
type tool_result = Complete of Canon.t | Task of Canon.t

type handlers = {
  call : tasks:bool -> string -> Canon.t option -> tool_result;
  task : task_op -> string -> Canon.t option;
}

type action =
  | Reply of Mcp_msg.message
  | Call of {
      id : Mcp_msg.id;
      name : string;
      arguments : Canon.t option;
      tasks : bool;
    }
  | Task_op of { id : Mcp_msg.id; op : task_op; task_id : string }
  | Cancel of Mcp_msg.id
  | Ignore

let obj = Canon.of_fields
let strings l = Canon.list (List.map Canon.string l)
let reply id result = Reply (Response { id; result })

let error_message ?data id code message : Mcp_msg.message =
  Error_response { id = Some id; code; message; data }

let error ?data id code message = Reply (error_message ?data id code message)
let invalid_params id message = error id (-32602) message
let method_not_found id m = error id (-32601) ("Method not found: " ^ m)
let version_key = "io.modelcontextprotocol/protocolVersion"
let capabilities_key = "io.modelcontextprotocol/clientCapabilities"

let server_info s =
  obj [ ("name", Canon.string s.name); ("version", Canon.string s.version) ]

let tasks_extension = "io.modelcontextprotocol/tasks"
let tools_capability = ("tools", Canon.empty_object)
let legacy_capabilities = ("capabilities", obj [ tools_capability ])

(* Tasks are offered in 2026-07-28 only, where a client declares extensions
   on each request. *)
let modern_capabilities =
  ( "capabilities",
    obj
      [
        tools_capability;
        ("extensions", obj [ (tasks_extension, Canon.empty_object) ]);
      ] )

let instructions s = ("instructions", Canon.string s.instructions)

(* The tool list does not change while the server runs, and holds nothing
   specific to a user. *)
let cacheable fields =
  ("ttlMs", Canon.int 3_600_000)
  :: ("cacheScope", Canon.string "public")
  :: fields

let complete fields = ("resultType", Canon.string "complete") :: fields

(* Every result of this era names the server in its _meta, as the overview
   page asks of each response. *)
let identified s result =
  Canon.set_member "_meta"
    (obj [ ("io.modelcontextprotocol/serverInfo", server_info s) ])
    result

(* [supportedVersions] are what a client may put in [_meta] for its next
   requests, so only 2026-07-28: the legacy revisions are reached through
   [initialize]. *)
let discover s =
  identified s
    (obj
       (complete
          (cacheable
             [
               ("supportedVersions", strings [ modern_version ]);
               modern_capabilities;
               instructions s;
             ])))

let initialize t id params =
  match Option.bind params (Canon.str_member "protocolVersion") with
  | None -> invalid_params id "initialize needs a protocolVersion string"
  | Some requested ->
      let version =
        if List.exists (String.equal requested) legacy_versions then requested
        else latest_legacy
      in
      t.era <- Legacy version;
      reply id
        (obj
           [
             ("protocolVersion", Canon.string version);
             legacy_capabilities;
             ("serverInfo", server_info t.server);
             instructions t.server;
           ])

let tools_list t ~modern id params =
  match Option.bind params (Canon.member "cursor") with
  | Some _ -> invalid_params id "Invalid cursor: tools/list has a single page"
  | None ->
      let tools = ("tools", Canon.list (List.map snd t.server.tools)) in
      if modern then
        reply id (identified t.server (obj (complete (cacheable [ tools ]))))
      else reply id (obj [ tools ])

(* Arguments that are not an object fail the CallToolRequest schema, which
   the tools page makes a protocol error. *)
let tools_call t ~tasks id params =
  let arguments = Option.bind params (Canon.member "arguments") in
  match Option.bind params (Canon.str_member "name") with
  | None -> invalid_params id "tools/call needs a tool name"
  | Some name when not (List.mem_assoc name t.server.tools) ->
      invalid_params id ("Unknown tool: " ^ name)
  | Some _
    when Option.is_some arguments
         && Option.is_none (Option.bind arguments Canon.to_fields_opt) ->
      invalid_params id "tools/call arguments must be an object"
  | Some name -> Call { id; name; arguments; tasks }

let task_methods =
  [
    ("tasks/get", Get_task);
    ("tasks/update", Update_task);
    ("tasks/cancel", Cancel_task);
  ]

let modern_methods =
  [ "server/discover"; "tools/list"; "tools/call" ] @ List.map fst task_methods

let missing_tasks id =
  error id (-32021) "Missing required client capability"
    ~data:
      (obj
         [
           ( "requiredCapabilities",
             obj
               [ ("extensions", obj [ (tasks_extension, Canon.empty_object) ]) ]
           );
         ])

(* The task methods of the Tasks extension, for a request that declares it. *)
let task_request id op params =
  let task_id = Option.bind params (Canon.str_member "taskId") in
  let responses =
    Option.bind
      (Option.bind params (Canon.member "inputResponses"))
      Canon.to_fields_opt
  in
  match (task_id, op) with
  | None, (Get_task | Update_task | Cancel_task) ->
      invalid_params id "tasks need a taskId string"
  | Some _, Update_task when Option.is_none responses ->
      invalid_params id "tasks/update needs an inputResponses object"
  | Some task_id, (Get_task | Update_task | Cancel_task) ->
      Task_op { id; op; task_id }

let modern t id method_ params =
  let meta key =
    Option.bind (Option.bind params (Canon.member "_meta")) (Canon.member key)
  in
  match Option.map Canon.to_string_opt (meta version_key) with
  | Some (Some v) when not (String.equal v modern_version) ->
      error id (-32022) "Unsupported protocol version"
        ~data:
          (obj
             [
               ("supported", strings (modern_version :: legacy_versions));
               ("requested", Canon.string v);
             ])
  | Some None -> invalid_params id (version_key ^ " in _meta must be a string")
  | (Some (Some _) | None) as version -> (
      if not (List.exists (String.equal method_) modern_methods) then
        method_not_found id method_
      else if Option.is_none version then
        invalid_params id ("_meta needs " ^ version_key)
      else
        match meta capabilities_key with
        | None -> invalid_params id ("_meta needs " ^ capabilities_key)
        | Some capabilities -> (
            let tasks =
              Option.is_some
                (Option.bind
                   (Canon.member "extensions" capabilities)
                   (Canon.member tasks_extension))
            in
            match (method_, List.assoc_opt method_ task_methods) with
            | "server/discover", _ -> reply id (discover t.server)
            | "tools/list", _ -> tools_list t ~modern:true id params
            | _, Some _ when not tasks -> missing_tasks id
            | _, Some op -> task_request id op params
            | _, None -> tools_call t ~tasks id params))

let legacy t id method_ params =
  match method_ with
  | "initialize" -> error id (-32600) "Invalid Request: already initialized"
  | "ping" -> reply id Canon.empty_object
  | "tools/list" -> tools_list t ~modern:false id params
  | "tools/call" -> tools_call t ~tasks:false id params
  | _ -> method_not_found id method_

let undecided t msg id method_ params =
  match (Mcp_msg.era_of_request msg, method_) with
  | Some Legacy, _ -> initialize t id params
  | Some Modern, _ ->
      (* Only a request that succeeds, or the version error that a dual-era
         client recognises as modern, fixes the era. After any other error
         such a client takes the server for a legacy one and opens with
         initialize, which must then still work. *)
      let action = modern t id method_ params in
      (match action with
      | Reply (Response _) | Call _ | Task_op _ -> t.era <- Modern
      | Reply (Error_response { code; _ }) when code = -32022 -> t.era <- Modern
      | Reply (Error_response _ | Request _ | Notification _)
      | Cancel _ | Ignore ->
          ());
      action
  | None, "ping" -> reply id Canon.empty_object
  | None, _ ->
      error id (-32600)
        ("Invalid Request: open with initialize (MCP 2025-11-25 and earlier) \
          or carry " ^ version_key ^ " in _meta (MCP 2026-07-28)")
        ~data:
          (obj [ ("supported", strings (modern_version :: legacy_versions)) ])

let request_id v =
  match (Canon.to_int_opt v, Canon.to_string_opt v) with
  | Some n, _ -> Some (Mcp_msg.Int_id n)
  | None, Some s -> Some (Mcp_msg.String_id s)
  | None, None -> None

let receive t (msg : Mcp_msg.message) =
  match msg with
  | Response _ | Error_response _ -> Ignore
  | Notification { method_ = "notifications/cancelled"; params } -> (
      match
        Option.bind (Option.bind params (Canon.member "requestId")) request_id
      with
      | Some id -> Cancel id
      | None -> Ignore)
  | Notification _ -> Ignore
  | Request { id; method_; params } -> (
      match t.era with
      | Undecided -> undecided t msg id method_ params
      | Legacy _ -> legacy t id method_ params
      | Modern -> modern t id method_ params)

let tool_reply t id result =
  let result =
    match (t.era, result) with
    | Modern, Complete r ->
        identified t.server
          (Canon.set_member "resultType" (Canon.string "complete") r)
    | Modern, Task task ->
        identified t.server
          (Canon.set_member "resultType" (Canon.string "task") task)
    (* A task goes only to a request that declared the extension, which
       only 2026-07-28 has. *)
    | (Legacy _ | Undecided), (Complete r | Task r) -> r
  in
  Mcp_msg.Response { id; result }

let task_reply t id op = function
  | None -> error_message id (-32602) "Failed to retrieve task: Task not found"
  | Some task ->
      let result =
        match op with
        | Get_task -> task
        | Update_task | Cancel_task -> Canon.empty_object
      in
      Mcp_msg.Response
        {
          id;
          result =
            identified t.server
              (Canon.set_member "resultType" (Canon.string "complete") result);
        }

let undecodable : Mcp_msg.error -> Mcp_msg.message = function
  | Invalid_json m ->
      Error_response
        { id = None; code = -32700; message = "Parse error: " ^ m; data = None }
  | Not_jsonrpc m ->
      Error_response
        {
          id = None;
          code = -32600;
          message = "Invalid Request: " ^ m;
          data = None;
        }
