(* What a run keeps about its MCP servers, and how a replay gets them back. *)

open Bactrian_core
open Bactrian_runtime
open Run_support
module J = Yojson.Safe.Util

let secret = "s3cret-token"

(* One server of each kind, with secrets where a careless writer would copy
   them. None of them is ever started: the agent does not call a tool. *)
let servers : Mcp_proxy.config list =
  [
    {
      name = "calendar";
      policy = Record;
      server =
        Stdio
          {
            argv = [ "python3"; "cal.py"; "--token=" ^ secret ];
            env = [ ("CALENDAR_KEY", secret) ];
            cwd = None;
          };
    };
    {
      name = "web-search";
      policy = Passthrough;
      server =
        Http { url = Uri.of_string ("https://x.example/mcp?k=" ^ secret) };
    };
    {
      name = "blocked";
      policy = Block;
      server = Stdio { argv = [ "true" ]; env = []; cwd = None };
    };
  ]

let summary (cs : Mcp_proxy.config list) =
  List.map
    (fun (c : Mcp_proxy.config) ->
      let transport : Mcp_proxy.server -> string = function
        | Stdio _ -> "stdio"
        | Http _ -> "http"
      in
      Printf.sprintf "%s %s %s" c.name (transport c.server)
        (Experiment.policy_name c.policy))
    cs

let recorded_with ctx mcp = ok (execute ctx { (config ()) with mcp })

let test_meta_keeps_names_kinds_and_policies =
  with_ctx "meta" @@ fun ctx ->
  let r = recorded_with ctx servers in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  let meta = read (file r "meta.json") in
  Alcotest.(check bool) "no secret in meta.json" false (contains meta secret);
  Alcotest.(check bool) "no command in meta.json" false (contains meta "cal.py");
  Alcotest.(check string)
    "mcp field"
    {|[{"name":"calendar","transport":"stdio","policy":"record"},{"name":"web-search","transport":"http","policy":"passthrough"},{"name":"blocked","transport":"stdio","policy":"block"}]|}
    (Yojson.Safe.to_string J.(member "mcp" (Yojson.Safe.from_string meta)))

let test_meta_without_servers =
  with_ctx "meta-none" @@ fun ctx ->
  let r = record ctx in
  Alcotest.(check string)
    "empty list" "[]"
    (Yojson.Safe.to_string
       J.(member "mcp" (Yojson.Safe.from_file (file r "meta.json"))))

let recorded_mcp ctx (r : Run.result) =
  Run.recorded_mcp ctx.store ~from:r.run_id

let test_recorded_servers_replay =
  with_ctx "recorded" @@ fun ctx ->
  let r = recorded_with ctx servers in
  Alcotest.(check (result strs string))
    "every server replays, a blocked one stays blocked"
    (Ok
       [
         "calendar stdio replay";
         "web-search http replay";
         "blocked stdio block";
       ])
    (Result.map summary (recorded_mcp ctx r));
  (* A path to the run directory names the recording as well. *)
  Alcotest.(check (result strs string))
    "by directory"
    (Ok
       [
         "calendar stdio replay";
         "web-search http replay";
         "blocked stdio block";
       ])
    (Result.map summary (Run.recorded_mcp ctx.store ~from:(r.run_dir ^ "/")))

let rewrite_meta (r : Run.result) f =
  let fields = J.to_assoc (Yojson.Safe.from_file (file r "meta.json")) in
  Out_channel.with_open_bin (file r "meta.json") (fun oc ->
      output_string oc (Yojson.Safe.to_string (`Assoc (f fields))))

let test_older_recordings_have_no_servers =
  with_ctx "older" @@ fun ctx ->
  let r = recorded_with ctx servers in
  rewrite_meta r (List.remove_assoc "mcp");
  Alcotest.(check (result strs string))
    "no field" (Ok [])
    (Result.map summary (recorded_mcp ctx r));
  Sys.remove (file r "meta.json");
  Alcotest.(check (result strs string))
    "no meta.json" (Ok [])
    (Result.map summary (recorded_mcp ctx r))

let test_malformed_field =
  with_ctx "malformed" @@ fun ctx ->
  let r = recorded_with ctx servers in
  let broken field =
    rewrite_meta r (fun fields ->
        ("mcp", field) :: List.remove_assoc "mcp" fields);
    match recorded_mcp ctx r with
    | Ok _ -> Alcotest.fail "expected an error"
    | Error m -> m
  in
  let entry fields = `List [ `Assoc fields ] in
  let valid =
    [
      ("name", `String "a");
      ("transport", `String "stdio");
      ("policy", `String "record");
    ]
  in
  let prefix = Printf.sprintf "recording %s: meta.json mcp: " r.run_id in
  let check label expected field =
    Alcotest.(check string) label (prefix ^ expected) (broken field)
  in
  check "not a list" "not a list" (`String "x");
  check "not an object" "a server is not an object" (`List [ `Int 1 ]);
  check "no name" "name is missing" (entry (List.remove_assoc "name" valid));
  check "name not a string" "name is not a string"
    (entry (("name", `Int 1) :: List.remove_assoc "name" valid));
  check "bad name" {|invalid server name "a/b"|}
    (entry (("name", `String "a/b") :: List.remove_assoc "name" valid));
  check "unknown transport" {|unknown transport "ws"|}
    (entry (("transport", `String "ws") :: List.remove_assoc "transport" valid));
  check "unknown policy" {|unknown policy "maybe"|}
    (entry (("policy", `String "maybe") :: List.remove_assoc "policy" valid))

(* The command line replays a recording made with servers: the agent gets the
   shim URL and the HTTP server's URL without being told anything. *)
let test_cli_replay_serves_recorded_servers =
  with_ctx "cli-mcp" @@ fun ctx ->
  let r = recorded_with ctx servers in
  let script = Filename.concat (Sys.getcwd ()) "cli-mcp-agent.py" in
  Out_channel.with_open_bin script (fun oc -> output_string oc python_agent);
  Fun.protect ~finally:(fun () -> Sys.remove script) @@ fun () ->
  let code, text =
    cli ctx
      (Printf.sprintf "replay %s --sandbox-backend %s --json -- python3 -I %s"
         r.run_id host_backend (Filename.quote script))
  in
  if code <> 0 then Alcotest.failf "exit %d: %s" code text;
  let json = Yojson.Safe.from_string text in
  let run_dir = J.(to_string (member "run_dir" json)) in
  let env =
    J.to_assoc
      (Yojson.Safe.from_file (Filename.concat run_dir "state/env.json"))
  in
  Alcotest.(check bool)
    "the shim URL is set" true
    (List.mem_assoc "BACTRIAN_MCP_URL" env);
  Alcotest.(check bool)
    "the HTTP server's URL is set" true
    (List.mem_assoc "BACTRIAN_MCP_HTTP_WEB_SEARCH" env);
  Alcotest.(check bool)
    "no URL for a stdio server" false
    (List.mem_assoc "BACTRIAN_MCP_HTTP_CALENDAR" env);
  (* The replay's own meta.json lists the servers, so it can be replayed. *)
  Alcotest.(check string)
    "the replay keeps the servers"
    {|[{"name":"calendar","transport":"stdio","policy":"replay"},{"name":"web-search","transport":"http","policy":"replay"},{"name":"blocked","transport":"stdio","policy":"block"}]|}
    (Yojson.Safe.to_string
       J.(
         member "mcp"
           (Yojson.Safe.from_file (Filename.concat run_dir "meta.json"))))

let test_cli_replay_of_a_bad_field =
  with_ctx "cli-bad" @@ fun ctx ->
  let r = recorded_with ctx servers in
  rewrite_meta r (fun fields ->
      ("mcp", `Int 3) :: List.remove_assoc "mcp" fields);
  let code, text =
    cli ctx
      (Printf.sprintf "replay %s --sandbox-backend %s -- true" r.run_id
         host_backend)
  in
  Alcotest.(check int) "usage error" 64 code;
  Alcotest.(check string)
    "message"
    (Printf.sprintf "bactrian: recording %s: meta.json mcp: not a list\n"
       r.run_id)
    text

let () =
  Alcotest.run "run mcp"
    [
      ( "meta.json",
        [
          Alcotest.test_case "keeps names, kinds and policies, never secrets"
            `Slow test_meta_keeps_names_kinds_and_policies;
          Alcotest.test_case "lists no servers for a run without any" `Slow
            test_meta_without_servers;
        ] );
      ( "replay",
        [
          Alcotest.test_case "serves every recorded server from the recording"
            `Slow test_recorded_servers_replay;
          Alcotest.test_case "older recordings have no servers" `Slow
            test_older_recordings_have_no_servers;
          Alcotest.test_case "malformed field is an error" `Slow
            test_malformed_field;
          Alcotest.test_case "command line gives the agent the servers" `Slow
            test_cli_replay_serves_recorded_servers;
          Alcotest.test_case "command line refuses a bad field" `Slow
            test_cli_replay_of_a_bad_field;
        ] );
    ]
