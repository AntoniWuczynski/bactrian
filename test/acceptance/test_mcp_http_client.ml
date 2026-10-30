(* [bactrian serve-mcp --http] and the Tasks extension from the outside: the
   real binary, a scripted client over Streamable HTTP and over stdio, and the
   real example agents. The HTTP listener must refuse what a page in a browser
   could send, and check the headers of 2026-07-28. Over either transport, a
   job started from a request that declares Tasks is a task that tasks/get
   follows and tasks/cancel stops, and its result is the command's. *)

open Bactrian_core
open Acceptance_world
open Cli_world
open Mcp_world
module C = Mcp_client
module H = Mcp_client.Http

let with_http ?env ?(args = []) ?token name f =
  with_project name @@ fun ~parent:_ ~project ->
  let s = H.start ?env ~args ?token ~exe:(exe ()) ~dir:project () in
  Fun.protect ~finally:(fun () -> H.dispose s) @@ fun () ->
  f ~project s;
  check_exit "SIGTERM ends the server as an interrupt" 130 (H.stop s)

let status what expected (r : H.response) =
  Alcotest.(check int) (what ^ ": status " ^ r.body) expected r.status

let error_code (r : H.response) =
  match H.outcome r with Error e -> Some e.code | Ok _ -> None

let check_error what ~status:st code r =
  status what st r;
  Alcotest.(check (option int))
    (what ^ ": the error") (Some code) (error_code r)

let discover ?(headers = fun h -> h) s =
  H.rpc ~headers s "server/discover" Canon.empty_object

let drop name =
  List.filter (fun (k, _) ->
      not
        (String.equal (String.lowercase_ascii k) (String.lowercase_ascii name)))

let set name v h = (name, v) :: drop name h

(* Refusals before the message is read *)

let refusals () =
  with_http
    ~args:[ "--http-allow-origin"; "http://localhost:5173" ]
    "http-refusals"
  @@ fun ~project:_ s ->
  let port = H.port s in
  status "a modern discover" 200 (discover s);
  let no_token = discover ~headers:(drop "Authorization") s in
  status "no token" 401 no_token;
  Alcotest.(check (option string))
    "the scheme to use" (Some "Bearer")
    (H.header no_token "www-authenticate");
  status "a wrong token" 401
    (discover ~headers:(set "Authorization" "Bearer not-the-token") s);
  let foreign = discover ~headers:(set "Origin" "http://evil.example") s in
  status "a foreign origin" 403 foreign;
  (match H.message foreign with
  | Error_response { id = None; _ } -> ()
  | Error_response { id = Some _; _ } | Response _ | Request _ | Notification _
    ->
      Alcotest.fail "a JSON-RPC error without an id");
  status "a foreign origin, without the token" 403
    (discover
       ~headers:(fun h ->
         set "Origin" "http://evil.example" (drop "Authorization" h))
       s);
  status "the server's own origin" 200
    (discover
       ~headers:(set "Origin" (Printf.sprintf "http://127.0.0.1:%d" port))
       s);
  status "an allowed origin" 200
    (discover ~headers:(set "Origin" "http://localhost:5173") s);
  status "a rebound host name" 403
    (discover ~headers:(set "Host" (Printf.sprintf "evil.example:%d" port)) s);
  let get = H.send ~meth:"GET" ~port [ H.auth s ] "" in
  status "GET" 405 get;
  Alcotest.(check (option string))
    "names POST" (Some "POST") (H.header get "allow");
  status "DELETE" 405 (H.send ~meth:"DELETE" ~port [ H.auth s ] "");
  status "another path" 404
    (H.send ~path:"/other" ~port
       [ H.auth s; ("Content-Type", "application/json") ]
       "{}");
  status "not JSON" 415 (discover ~headers:(set "Content-Type" "text/plain") s);
  Alcotest.(check bool)
    "the test's token is never printed" false
    (contains (H.log s) (H.token s))

(* Without BACTRIAN_MCP_TOKEN the server makes a token and prints it. *)
let printed_token () =
  with_http ~token:None "http-printed-token" @@ fun ~project:_ s ->
  Alcotest.(check int)
    "a token of 32 random bytes" 64
    (String.length (H.token s));
  status "the printed token works" 200 (discover s)

(* The headers of 2026-07-28, and the legacy era by its header *)

let headers () =
  with_http "http-headers" @@ fun ~project:_ s ->
  let found = H.must_ok "server/discover" (discover s) in
  Alcotest.(check bool)
    "the Tasks extension is offered" true
    (Option.is_some
       (Option.bind
          (Option.bind
             (Canon.member "capabilities" found)
             (Canon.member "extensions"))
          (Canon.member C.tasks_extension)));
  let call =
    obj
      [
        ("name", Canon.string "job_status");
        ("arguments", obj [ ("job_id", Canon.string "job-0") ]);
      ]
  in
  let tool ?(headers = fun h -> h) () = H.rpc ~headers s "tools/call" call in
  status "a call with its headers" 200 (tool ());
  check_error "no MCP-Protocol-Version" ~status:400 (-32020)
    (discover ~headers:(drop "MCP-Protocol-Version") s);
  check_error "another MCP-Protocol-Version" ~status:400 (-32020)
    (discover ~headers:(set "MCP-Protocol-Version" "2025-11-25") s);
  check_error "no Mcp-Method" ~status:400 (-32020)
    (discover ~headers:(drop "Mcp-Method") s);
  check_error "another Mcp-Method" ~status:400 (-32020)
    (discover ~headers:(set "Mcp-Method" "tools/list") s);
  check_error "a call without Mcp-Name" ~status:400 (-32020)
    (tool ~headers:(drop "Mcp-Name") ());
  check_error "a call naming another tool" ~status:400 (-32020)
    (tool ~headers:(set "Mcp-Name" "job_cancel") ());
  check_error "Mcp-Name outside ASCII" ~status:400 (-32020)
    (tool ~headers:(set "Mcp-Name" "job_status\xc3\xa9") ());
  status "Mcp-Name in Base64" 200
    (tool ~headers:(set "Mcp-Name" "=?base64?am9iX3N0YXR1cw==?=") ());
  check_error "an unknown method" ~status:404 (-32601)
    (H.rpc s "resources/list" Canon.empty_object);
  let unsupported =
    H.send ~port:(H.port s)
      (set "MCP-Protocol-Version" "2099-01-01"
         (H.rpc_headers ~token:(H.token s) "tools/list" Canon.empty_object))
      (H.body "tools/list"
         (obj
            [
              ( "_meta",
                obj
                  [
                    ( "io.modelcontextprotocol/protocolVersion",
                      Canon.string "2099-01-01" );
                    ( "io.modelcontextprotocol/clientCapabilities",
                      Canon.empty_object );
                  ] );
            ]))
  in
  check_error "an unsupported version" ~status:400 (-32022) unsupported;
  check_error "a request without client capabilities" ~status:400 (-32602)
    (H.send ~port:(H.port s)
       (H.rpc_headers ~token:(H.token s) "tools/list" Canon.empty_object)
       (H.body "tools/list"
          (obj
             [
               ( "_meta",
                 obj
                   [
                     ( "io.modelcontextprotocol/protocolVersion",
                       Canon.string "2026-07-28" );
                   ] );
             ])));
  let plain = [ H.auth s; ("Content-Type", "application/json") ] in
  status "a notification" 202
    (H.send ~port:(H.port s) plain
       {|{"jsonrpc":"2.0","method":"notifications/initialized"}|});
  (* The legacy era: initialize, then the version in the header. *)
  let initialized =
    H.send ~port:(H.port s) plain
      (H.body "initialize"
         (obj
            [
              ("protocolVersion", Canon.string "2025-11-25");
              ("capabilities", Canon.empty_object);
              ( "clientInfo",
                obj
                  [
                    ("name", Canon.string "test"); ("version", Canon.string "0");
                  ] );
            ]))
  in
  Alcotest.(check (option string))
    "initialize" (Some "2025-11-25")
    (Option.bind
       (Result.to_option (H.outcome initialized))
       (Canon.str_member "protocolVersion"));
  Alcotest.(check bool)
    "no session is minted" true
    (Option.is_none (H.header initialized "mcp-session-id"));
  let legacy = ("MCP-Protocol-Version", "2025-11-25") :: plain in
  let listed =
    H.send ~port:(H.port s) legacy (H.body "tools/list" Canon.empty_object)
  in
  status "a legacy tools/list" 200 listed;
  Alcotest.(check bool)
    "lists the tools" true
    (Option.is_some
       (Option.bind
          (Result.to_option (H.outcome listed))
          (Canon.list_member "tools")));
  check_error "no Tasks for legacy clients" ~status:200 (-32601)
    (H.send ~port:(H.port s) legacy
       (H.body "tasks/get" (obj [ ("taskId", Canon.string "job-0") ])));
  check_error "an unknown version header" ~status:400 (-32600)
    (H.send ~port:(H.port s)
       (("MCP-Protocol-Version", "1999-01-01") :: plain)
       (H.body "tools/list" Canon.empty_object))

(* Both transports give the same tools and the same answers. *)
let same_as_stdio () =
  with_http "http-same" @@ fun ~project s ->
  let c = C.start ~exe:(exe ()) ~dir:project Modern in
  Fun.protect ~finally:(fun () -> C.dispose c) @@ fun () ->
  let over_http =
    H.must_ok "tools/list" (H.rpc s "tools/list" Canon.empty_object)
  in
  Alcotest.check json "tools/list"
    (field "tools" (C.tools_list c))
    (field "tools" over_http);
  let plan =
    get "plan arguments"
      (Result.to_option
         (Canon.of_string
            {|{"threshold":0.05,"baseline_rates":[0.5],"scenarios":2,"simulations":50,"max_repeats":3}|}))
  in
  let structured = function C.Structured j | C.Tool_error j -> j in
  let stdio = structured (C.call_tool c "plan" plan) in
  Alcotest.check json "plan over SSE" stdio
    (structured (H.call_tool s "plan" plan));
  Alcotest.check json "plan over JSON" stdio
    (structured (H.call_tool ~accept:"application/json" s "plan" plan));
  check_exit "the stdio server exits" 0 (C.close c)

(* Tasks *)

type transport = Stdio | Http

let transport_name = function Stdio -> "stdio" | Http -> "http"

(* What a test needs of a connection, whichever transport carries it. *)
type client = {
  call_task : string -> (string * Canon.t) list -> Canon.t;
  task : string -> string -> (Canon.t, C.error) result;
  poll : string -> Canon.t;
  call : string -> (string * Canon.t) list -> C.outcome;
  without_tasks : string -> string -> (Canon.t, C.error) result;
}

let with_client transport ?(env = key_env) ~dir f =
  match transport with
  | Stdio ->
      let c = C.start ~env ~args:trusting ~exe:(exe ()) ~dir Modern in
      Fun.protect ~finally:(fun () -> C.dispose c) @@ fun () ->
      let task_request ~tasks method_ id =
        C.request ~tasks c method_ (obj [ ("taskId", Canon.string id) ])
      in
      f
        {
          call_task = (fun name args -> C.call_task c name (obj args));
          task = task_request ~tasks:true;
          poll = C.poll_task c;
          call = (fun name args -> C.call_tool c name (obj args));
          without_tasks = task_request ~tasks:false;
        };
      check_exit "the stdio server exits at end of input" 0 (C.close c)
  | Http ->
      let s = H.start ~env ~args:trusting ~exe:(exe ()) ~dir () in
      Fun.protect ~finally:(fun () -> H.dispose s) @@ fun () ->
      f
        {
          call_task = (fun name args -> H.call_task s name (obj args));
          task = H.task s;
          poll = H.poll_task s;
          call = (fun name args -> H.call_tool s name (obj args));
          without_tasks =
            (fun method_ id ->
              H.outcome (H.rpc s method_ (obj [ ("taskId", Canon.string id) ])));
        };
      check_exit "SIGTERM ends the HTTP server" 130 (H.stop s)

let str key j = Canon.str_member key j

(* A replay that hangs, as a task: tasks/get follows it, tasks/cancel stops it
   and its agents. *)
let task_cancel transport () =
  with_recording ("task-cancel-" ^ transport_name transport) @@ fun w run ->
  with_client transport ~dir:w.dir @@ fun cl ->
  let created = cl.call_task "replay_start" (hung_arguments run) in
  let id = get "taskId" (str "taskId" created) in
  Alcotest.(check (option string))
    "working" (Some "working") (str "status" created);
  Alcotest.(check bool)
    "no time to live while it runs" true
    (Option.equal Canon.equal (Canon.member "ttlMs" created) (Some Canon.null));
  Alcotest.(check (option int))
    "a poll interval" (Some 1000)
    (Canon.int_member "pollIntervalMs" created);
  Alcotest.(check bool)
    "an ISO 8601 creation time" true
    (Option.is_some (Option.bind (str "createdAt" created) Rfc3339.epoch));
  let agent = await_hung w.dir in
  (match cl.task "tasks/get" id with
  | Ok t ->
      Alcotest.(check (option string))
        "still working" (Some "working") (str "status" t);
      Alcotest.(check (option string))
        "tasks/get is complete" (Some "complete") (str "resultType" t)
  | Error e -> Alcotest.failf "tasks/get: %s" e.message);
  (match cl.without_tasks "tasks/get" id with
  | Error e ->
      Alcotest.(check int) "tasks/get without the extension" (-32021) e.code
  | Ok _ -> Alcotest.fail "tasks/get without the extension was answered");
  (match cl.task "tasks/get" "job-0" with
  | Error e -> Alcotest.(check int) "an unknown task" (-32602) e.code
  | Ok _ -> Alcotest.fail "an unknown task was found");
  (match cl.task "tasks/cancel" id with
  | Ok ack ->
      Alcotest.(check (option string))
        "acknowledged" (Some "complete") (str "resultType" ack);
      Alcotest.(check (option string))
        "an empty acknowledgement" None (str "status" ack)
  | Error e -> Alcotest.failf "tasks/cancel: %s" e.message);
  let ended = cl.poll id in
  Alcotest.(check (option string))
    "cancelled" (Some "cancelled") (str "status" ended);
  Alcotest.(check (option string))
    "as an interrupt ends a command" (Some "interrupted by SIGINT")
    (str "statusMessage" ended);
  Alcotest.(check bool)
    "now with a time to live" true
    (Option.is_some (Canon.int_member "ttlMs" ended));
  gone agent;
  match cl.call "job_status" [ ("job_id", Canon.string id) ] with
  | Structured j ->
      Alcotest.(check string)
        "the task is the job" "cancelled" (string_field "state" j)
  | Tool_error e -> Alcotest.failf "job_status: %s" (Canon.to_string e)

(* An experiment as a task: its result is what run --json prints. *)
let task_complete transport () =
  let agent = List.hd agents in
  with_project ("task-run-" ^ transport_name transport)
  @@ fun ~parent:_ ~project ->
  with_upstream ~dir:project @@ fun up ->
  write_experiment project (agent.argv ());
  let env = (upstream_variable, upstream_url up) :: key_env in
  let served = ref Canon.null in
  ( with_client transport ~env ~dir:project @@ fun cl ->
    let created = cl.call_task "run_start" (run_args ()) in
    let task = cl.poll (get "taskId" (str "taskId" created)) in
    Alcotest.(check (option string))
      "completed" (Some "completed") (str "status" task);
    let result = field "result" task in
    Alcotest.(check (option bool))
      "not an error" (Some false)
      (Canon.bool_member "isError" result);
    let structured = field "structuredContent" result in
    (match Canon.list_member "content" result with
    | Some [ block ] ->
        Alcotest.(check (option string))
          "the text block holds the same JSON"
          (Some (Canon.to_string structured))
          (Option.map Canon.to_string
             (Option.bind (str "text" block) (fun t ->
                  Result.to_option (Canon.of_string t))))
    | Some _ | None -> Alcotest.fail "one content block");
    served := structured );
  let code, printed = cli ~dir:project ~env run_flags in
  Alcotest.(check int) "run: the exit status" 0 code;
  Alcotest.check json "the task's result equals run --json"
    (summary_normalised printed)
    (summary_normalised !served)

let per_transport test =
  List.map
    (fun t -> Alcotest.test_case (transport_name t) `Slow (test t))
    [ Stdio; Http ]

let () =
  Alcotest.run "mcp http client"
    [
      ( "http",
        [
          Alcotest.test_case "refusals" `Quick refusals;
          Alcotest.test_case "a printed token" `Quick printed_token;
          Alcotest.test_case "headers and eras" `Quick headers;
          Alcotest.test_case "the same as stdio" `Quick same_as_stdio;
        ] );
      ("a task cancelled", per_transport task_cancel);
      ("a task completed", per_transport task_complete);
    ]
