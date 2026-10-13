(* The protocol side of the MCP server, in both eras, without IO. *)

open Bactrian_core
open Bactrian_server

let canon text =
  match Canon.of_string text with
  | Ok c -> c
  | Error _ -> Alcotest.fail ("malformed JSON in the test: " ^ text)

let canon_t =
  Alcotest.testable
    (fun f c -> Format.pp_print_string f (Canon.to_string c))
    Canon.equal

let server : Mcp_dispatch.server =
  {
    name = "test";
    version = "1";
    instructions = "use it";
    tools =
      [
        ("echo", canon {|{"name":"echo"}|}); ("slow", canon {|{"name":"slow"}|});
      ];
  }

let message text =
  match Mcp_msg.decode text with
  | Ok m -> m
  | Error _ -> Alcotest.fail ("not a JSON-RPC message: " ^ text)

let meta ?(version = {|"2026-07-28"|}) () =
  Printf.sprintf
    {|"_meta":{"io.modelcontextprotocol/protocolVersion":%s,"io.modelcontextprotocol/clientCapabilities":{}}|}
    version

let request ?params id method_ =
  message
    (Printf.sprintf {|{"jsonrpc":"2.0","id":%d,"method":"%s"%s}|} id method_
       (match params with None -> "" | Some p -> ",\"params\":" ^ p))

let modern ?version ?(extra = "") id method_ =
  request ~params:(Printf.sprintf "{%s%s}" (meta ?version ()) extra) id method_

let reply = function
  | Mcp_dispatch.Reply m -> canon (Mcp_msg.encode m)
  | Call _ | Cancel _ | Ignore -> Alcotest.fail "expected a reply"

let check_reply what expected action =
  Alcotest.check canon_t what (canon expected) (reply action)

let error_code action =
  Option.bind (Canon.member "error" (reply action)) (Canon.int_member "code")

let check_code what code action =
  Alcotest.(check (option int)) what (Some code) (error_code action)

let era_t =
  Alcotest.testable
    (fun f (e : Mcp_dispatch.era) ->
      Format.pp_print_string f
        (match e with
        | Undecided -> "undecided"
        | Legacy v -> "legacy " ^ v
        | Modern -> "modern"))
    (fun a b ->
      match (a, b) with
      | Mcp_dispatch.Undecided, Mcp_dispatch.Undecided | Modern, Modern -> true
      | Legacy x, Legacy y -> String.equal x y
      | (Undecided | Legacy _ | Modern), _ -> false)

let call_of = function
  | Mcp_dispatch.Call { id; name; arguments } -> (id, name, arguments)
  | Reply _ | Cancel _ | Ignore -> Alcotest.fail "expected a tool call"

let test_modern_discover () =
  let d = Mcp_dispatch.create server in
  check_reply "discover"
    {|{"jsonrpc":"2.0","id":1,"result":{"resultType":"complete","supportedVersions":["2026-07-28"],"capabilities":{"tools":{}},"instructions":"use it","ttlMs":3600000,"cacheScope":"public","_meta":{"io.modelcontextprotocol/serverInfo":{"name":"test","version":"1"}}}}|}
    (Mcp_dispatch.receive d (modern 1 "server/discover"));
  Alcotest.check era_t "era" Modern (Mcp_dispatch.era d);
  check_reply "tools in order, cacheable"
    {|{"jsonrpc":"2.0","id":2,"result":{"resultType":"complete","tools":[{"name":"echo"},{"name":"slow"}],"ttlMs":3600000,"cacheScope":"public","_meta":{"io.modelcontextprotocol/serverInfo":{"name":"test","version":"1"}}}}|}
    (Mcp_dispatch.receive d (modern 2 "tools/list"))

let test_modern_call () =
  let d = Mcp_dispatch.create server in
  let id, name, arguments =
    call_of
      (Mcp_dispatch.receive d
         (modern 3 "tools/call" ~extra:{|,"name":"echo","arguments":{"a":1}|}))
  in
  Alcotest.(check string) "tool" "echo" name;
  Alcotest.(check (option canon_t))
    "arguments"
    (Some (canon {|{"a":1}|}))
    arguments;
  Alcotest.check canon_t "a modern result is complete"
    (canon
       {|{"jsonrpc":"2.0","id":3,"result":{"isError":false,"resultType":"complete","_meta":{"io.modelcontextprotocol/serverInfo":{"name":"test","version":"1"}}}}|})
    (canon
       (Mcp_msg.encode
          (Mcp_dispatch.tool_reply d id (canon {|{"isError":false}|}))))

let test_unsupported_version () =
  let d = Mcp_dispatch.create server in
  check_reply "UnsupportedProtocolVersionError"
    {|{"jsonrpc":"2.0","id":1,"error":{"code":-32022,"message":"Unsupported protocol version","data":{"supported":["2026-07-28","2025-11-25","2025-06-18","2025-03-26"],"requested":"2099-01-01"}}}|}
    (Mcp_dispatch.receive d (modern ~version:{|"2099-01-01"|} 1 "tools/list"));
  Alcotest.check era_t "still modern" Modern (Mcp_dispatch.era d)

let test_modern_errors () =
  let d = Mcp_dispatch.create server in
  let receive = Mcp_dispatch.receive d in
  ignore (reply (receive (modern 0 "server/discover")) : Canon.t);
  check_code "no capabilities" (-32602)
    (receive
       (request 1 "tools/list"
          ~params:
            {|{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}|}));
  check_code "a version that is not a string" (-32602)
    (receive (modern ~version:"7" 2 "tools/list"));
  check_code "no _meta on a modern connection" (-32602)
    (receive (request 3 "tools/list"));
  check_code "unknown method" (-32601) (receive (modern 4 "resources/list"));
  check_code "ping was removed" (-32601) (receive (modern 5 "ping"));
  check_code "initialize on a modern connection" (-32601)
    (receive
       (request 6 "initialize" ~params:{|{"protocolVersion":"2025-11-25"}|}));
  check_code "unknown tool" (-32602)
    (receive (modern 7 "tools/call" ~extra:{|,"name":"nope"|}));
  check_code "no tool name" (-32602) (receive (modern 8 "tools/call"));
  check_code "arguments that are not an object" (-32602)
    (receive (modern 10 "tools/call" ~extra:{|,"name":"echo","arguments":[]|}));
  check_code "a cursor never given" (-32602)
    (receive (modern 9 "tools/list" ~extra:{|,"cursor":"x"|}))

(* A modern request that fails as malformed does not fix the era: a dual-era
   client then falls back to initialize, which must still work. *)
let test_era_not_fixed_by_an_error () =
  let d = Mcp_dispatch.create server in
  let receive = Mcp_dispatch.receive d in
  check_code "no capabilities" (-32602)
    (receive
       (request 1 "server/discover"
          ~params:
            {|{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}|}));
  Alcotest.check era_t "still undecided" Undecided (Mcp_dispatch.era d);
  ignore
    (reply
       (receive
          (request 2 "initialize" ~params:{|{"protocolVersion":"2025-11-25"}|}))
      : Canon.t);
  Alcotest.check era_t "legacy" (Legacy "2025-11-25") (Mcp_dispatch.era d)

let test_legacy () =
  let d = Mcp_dispatch.create server in
  let receive = Mcp_dispatch.receive d in
  check_reply "initialize answers the requested revision"
    {|{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-06-18","capabilities":{"tools":{}},"serverInfo":{"name":"test","version":"1"},"instructions":"use it"}}|}
    (receive
       (request 1 "initialize"
          ~params:
            {|{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"c","version":"1"}}|}));
  Alcotest.check era_t "era" (Legacy "2025-06-18") (Mcp_dispatch.era d);
  (match
     receive
       (message {|{"jsonrpc":"2.0","method":"notifications/initialized"}|})
   with
  | Ignore -> ()
  | Reply _ | Call _ | Cancel _ -> Alcotest.fail "initialized is a notification");
  check_reply "ping" {|{"jsonrpc":"2.0","id":2,"result":{}}|}
    (receive (request 2 "ping"));
  check_reply "tools/list without the modern fields"
    {|{"jsonrpc":"2.0","id":3,"result":{"tools":[{"name":"echo"},{"name":"slow"}]}}|}
    (receive (request 3 "tools/list"));
  let id, _, arguments =
    call_of (receive (request 4 "tools/call" ~params:{|{"name":"slow"}|}))
  in
  Alcotest.(check (option canon_t)) "no arguments" None arguments;
  Alcotest.check canon_t "a legacy result has no resultType"
    (canon {|{"jsonrpc":"2.0","id":4,"result":{"isError":false}}|})
    (canon
       (Mcp_msg.encode
          (Mcp_dispatch.tool_reply d id (canon {|{"isError":false}|}))));
  check_code "no server/discover" (-32601)
    (receive (request 5 "server/discover"));
  check_code "a second initialize" (-32600)
    (receive
       (request 6 "initialize" ~params:{|{"protocolVersion":"2025-11-25"}|}))

let test_legacy_versions () =
  let agreed requested =
    let d = Mcp_dispatch.create server in
    reply
      (Mcp_dispatch.receive d
         (request 1 "initialize"
            ~params:(Printf.sprintf {|{"protocolVersion":"%s"}|} requested)))
    |> Canon.member "result"
    |> fun r -> Option.bind r (Canon.str_member "protocolVersion")
  in
  List.iter
    (fun (requested, expected) ->
      Alcotest.(check (option string))
        requested (Some expected) (agreed requested))
    [
      ("2025-11-25", "2025-11-25");
      ("2025-06-18", "2025-06-18");
      ("2025-03-26", "2025-03-26");
      ("2024-11-05", "2025-11-25");
      ("2026-07-28", "2025-11-25");
    ]

let test_undecided () =
  let d = Mcp_dispatch.create server in
  let receive = Mcp_dispatch.receive d in
  check_reply "a request that names no era"
    {|{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"Invalid Request: open with initialize (MCP 2025-11-25 and earlier) or carry io.modelcontextprotocol/protocolVersion in _meta (MCP 2026-07-28)","data":{"supported":["2026-07-28","2025-11-25","2025-06-18","2025-03-26"]}}}|}
    (receive (request 1 "tools/list"));
  check_reply "ping is answered" {|{"jsonrpc":"2.0","id":2,"result":{}}|}
    (receive (request 2 "ping"));
  check_code "initialize without a version" (-32602)
    (receive (request 3 "initialize" ~params:"{}"));
  Alcotest.check era_t "still undecided" Undecided (Mcp_dispatch.era d)

let test_cancel () =
  let d = Mcp_dispatch.create server in
  let cancelled params =
    Mcp_dispatch.receive d
      (message
         (Printf.sprintf
            {|{"jsonrpc":"2.0","method":"notifications/cancelled","params":%s}|}
            params))
  in
  let id_t =
    Alcotest.testable
      (fun f (id : Mcp_msg.id) ->
        match id with
        | Int_id n -> Format.pp_print_int f n
        | String_id s -> Format.pp_print_string f s)
      (fun a b ->
        match (a, b) with
        | Mcp_msg.Int_id x, Mcp_msg.Int_id y -> Int.equal x y
        | String_id x, String_id y -> String.equal x y
        | (Int_id _ | String_id _), _ -> false)
  in
  let cancel_of = function
    | Mcp_dispatch.Cancel id -> Some id
    | Reply _ | Call _ | Ignore -> None
  in
  Alcotest.(check (option id_t))
    "a number" (Some (Int_id 7))
    (cancel_of (cancelled {|{"requestId":7,"reason":"user"}|}));
  Alcotest.(check (option id_t))
    "a string" (Some (String_id "x"))
    (cancel_of (cancelled {|{"requestId":"x"}|}));
  Alcotest.(check (option id_t))
    "malformed is ignored" None
    (cancel_of (cancelled {|{"requestId":true}|}));
  match
    Mcp_dispatch.receive d (message {|{"jsonrpc":"2.0","id":1,"result":{}}|})
  with
  | Ignore -> ()
  | Reply _ | Call _ | Cancel _ -> Alcotest.fail "a response is ignored"

let test_undecodable () =
  let code text =
    match Mcp_msg.decode text with
    | Ok _ -> Alcotest.fail "decoded"
    | Error e ->
        let m = canon (Mcp_msg.encode (Mcp_dispatch.undecodable e)) in
        ( Option.bind (Canon.member "error" m) (Canon.int_member "code"),
          Option.map Canon.to_string (Canon.member "id" m) )
  in
  Alcotest.(check (pair (option int) (option string)))
    "not JSON"
    (Some (-32700), Some "null")
    (code "{");
  Alcotest.(check (pair (option int) (option string)))
    "a batch"
    (Some (-32600), Some "null")
    (code "[]")

let () =
  Alcotest.run "mcp_dispatch"
    [
      ( "modern",
        [
          Alcotest.test_case "discover and list" `Quick test_modern_discover;
          Alcotest.test_case "tools/call" `Quick test_modern_call;
          Alcotest.test_case "unsupported version" `Quick
            test_unsupported_version;
          Alcotest.test_case "errors" `Quick test_modern_errors;
          Alcotest.test_case "an error fixes no era" `Quick
            test_era_not_fixed_by_an_error;
        ] );
      ( "legacy",
        [
          Alcotest.test_case "session" `Quick test_legacy;
          Alcotest.test_case "version agreed" `Quick test_legacy_versions;
        ] );
      ( "any era",
        [
          Alcotest.test_case "before the era is known" `Quick test_undecided;
          Alcotest.test_case "cancellation" `Quick test_cancel;
          Alcotest.test_case "undecodable lines" `Quick test_undecodable;
        ] );
    ]
