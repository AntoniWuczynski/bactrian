open Bactrian_core
open Bactrian_runtime
open Acceptance_world

let acceptance agent () =
  let argv = agent.argv () in
  with_world agent.name @@ fun { up; execute; dir = _; env = _ } ->
  let execute = execute ~agent:(Command argv) in
  (* (a) record *)
  let recorded = execute () in
  Alcotest.(check string) "record outcome" "passed" (outcome_name recorded);
  check_sandboxed recorded;
  Alcotest.(check int) "model exchanges" 2 recorded.findings.exchanges;
  Alcotest.(check int) "upstream requests" 2 (List.length (F.requests up));
  Alcotest.(check (list string))
    "tool calls" [ "get_weather" ]
    (List.map
       (fun j -> Yojson.Safe.Util.(to_string (member "name" j)))
       (entries "tool_call" recorded));
  Alcotest.(check int) "one reply" 1 (List.length (entries "reply" recorded));
  (* (b) strict replay *)
  let replayed = execute ~replay:(strict recorded.run_dir) () in
  Alcotest.(check string) "replay outcome" "passed" (outcome_name replayed);
  check_sandboxed replayed;
  Alcotest.(check int)
    "no divergences" 0
    (List.length replayed.findings.divergences);
  Alcotest.(check int) "upstream untouched" 2 (List.length (F.requests up));
  (match replayed.violations with
  | Collected { items = []; _ } -> ()
  | Collected { items = v :: _; _ } ->
      Alcotest.failf "unexpected sandbox violation: %s %s" v.kind v.detail
  | Not_collected m -> Alcotest.failf "violations not collected: %s" m);
  (* (c) changed system prompt *)
  Unix.putenv prompt_variable "You are a terse assistant.";
  Fun.protect ~finally:(fun () -> Unix.putenv prompt_variable "") @@ fun () ->
  let changed = execute ~replay:(strict recorded.run_dir) () in
  Alcotest.(check string) "changed outcome" "diverged" (outcome_name changed);
  Alcotest.(check int) "upstream untouched" 2 (List.length (F.requests up));
  match
    List.filter_map
      (function
        | Matcher.Missing_request { nearest; _ } -> Some nearest
        | Unconsumed _ | Reordered _ | Shape_mismatch _ -> None)
      changed.findings.divergences
  with
  | [ Some (_, changes, _) ] ->
      Alcotest.(check bool)
        "diff names the system message" true
        (List.exists
           (fun (c : Diff.change) -> String.equal c.path "messages[0].content")
           changes)
  | [ None ] -> Alcotest.fail "the missing request has no nearest exchange"
  | [] | _ :: _ :: _ -> Alcotest.fail "expected exactly one Missing_request"

(* MCP: the agent reaches a stdio server through [bactrian mcp-shim]. The
   recording is replayed with the server unavailable. *)

type era = { label : string; fake : string; protocol : string; requests : int }

let eras =
  [
    { label = "modern"; fake = "modern"; protocol = "2026-07-28"; requests = 1 };
    (* initialize, the initialized notification and tools/call *)
    { label = "legacy"; fake = "legacy"; protocol = "2025-11-25"; requests = 3 };
  ]

let weather_server argv : Mcp_proxy.config list =
  [
    {
      name = "weather";
      server = Stdio { argv; env = []; cwd = None };
      policy = Record;
    };
  ]

let member_string key j = Yojson.Safe.Util.(to_string (member key j))

let mcp_acceptance agent era () =
  let argv = agent.argv () in
  let log = Filename.temp_file "fake-mcp" ".log" in
  Fun.protect ~finally:(fun () -> Sys.remove log) @@ fun () ->
  with_world (agent.name ^ "-mcp-" ^ era.label)
  @@ fun { up; execute; dir = _; env = _ } ->
  let variant_env =
    [
      ( "AGENT_MCP_COMMAND",
        Yojson.Safe.to_string
          (`List
             (List.map
                (fun s -> `String s)
                [ executable "BACTRIAN_EXE"; "mcp-shim"; "weather" ])) );
      ("AGENT_MCP_PROTOCOL", era.protocol);
    ]
  in
  let fake_calls () = List.length (lines log) in
  let recorded =
    execute ~agent:(Command argv) ~variant_env
      ~mcp:
        (weather_server
           [ executable "FAKE_MCP_SERVER"; "--era"; era.fake; "--log"; log ])
      ()
  in
  Alcotest.(check string) "record outcome" "passed" (outcome_name recorded);
  Alcotest.(check int) "server saw" era.requests (fake_calls ());
  (* A notification is not recorded. *)
  Alcotest.(check int)
    "model and MCP exchanges"
    (2 + era.requests - (era.requests / 3))
    recorded.findings.exchanges;
  Alcotest.(check (list string))
    "tool call joined to its MCP record" [ "joined" ]
    (List.map (member_string "mcp") (entries "tool_call" recorded));
  Alcotest.(check (list string))
    "the server's answer reached the model" [ "Sunny, 21C in Paris" ]
    (List.map (member_string "content") (entries "tool_result" recorded));
  (* Strict replay, with the server unavailable. *)
  let replayed =
    execute ~agent:(Command argv) ~variant_env ~replay:(strict recorded.run_dir)
      ~mcp:(weather_server [ "/nonexistent/fake-mcp-server" ])
      ()
  in
  Alcotest.(check string) "replay outcome" "passed" (outcome_name replayed);
  Alcotest.(check int)
    "no divergences" 0
    (List.length replayed.findings.divergences);
  Alcotest.(check int) "server untouched" era.requests (fake_calls ());
  Alcotest.(check int) "upstream untouched" 2 (List.length (F.requests up));
  Alcotest.(check (list string))
    "same joins in the replay's trace" [ "joined" ]
    (List.map (member_string "mcp") (entries "tool_call" replayed))

(* The same through the command line's own pieces. [bactrian record] cannot
   reach the fake upstream (a live record needs TLS and the providers' real
   addresses), so the record goes through the library with the servers the
   record flags declare, and the replay is the real [bactrian replay], which
   gets the servers from the recording's meta.json and takes no MCP flags. *)
let mcp_flags_acceptance agent era () =
  let argv = agent.argv () in
  let log = Filename.temp_file "fake-mcp" ".log" in
  Fun.protect ~finally:(fun () -> Sys.remove log) @@ fun () ->
  with_world (agent.name ^ "-mcp-flags-" ^ era.label)
  @@ fun { up; dir; execute; env = _ } ->
  let shim_command =
    Yojson.Safe.to_string
      (`List
         (List.map
            (fun s -> `String s)
            [ executable "BACTRIAN_EXE"; "mcp-shim"; "weather" ]))
  in
  let variant_env =
    [
      ("AGENT_MCP_COMMAND", shim_command); ("AGENT_MCP_PROTOCOL", era.protocol);
    ]
  in
  let fake_command =
    Yojson.Safe.to_string
      (`List
         (List.map
            (fun s -> `String s)
            [ executable "FAKE_MCP_SERVER"; "--era"; era.fake; "--log"; log ]))
  in
  let mcp =
    match
      Mcp_flags.parse ~stdio:[ "weather=" ^ fake_command ] ~http:[] ~policy:[]
    with
    | Ok mcp -> mcp
    | Error m -> Alcotest.failf "flags: %s" m
  in
  let recorded = execute ~agent:(Command argv) ~variant_env ~mcp () in
  Alcotest.(check string) "record outcome" "passed" (outcome_name recorded);
  Alcotest.(check int) "server saw" era.requests (List.length (lines log));
  (* The replay finds no server: the fake's path is nowhere in its command. *)
  List.iter (fun (k, v) -> Unix.putenv k v) variant_env;
  let out = Filename.concat dir "replay-out.json" in
  let command =
    Printf.sprintf
      "cd %s && %s replay %s --json --sandbox %s --env AGENT_MCP_COMMAND --env \
       AGENT_MCP_PROTOCOL --env %s -- %s > %s"
      (Filename.quote dir)
      (Filename.quote (executable "BACTRIAN_EXE"))
      (Filename.quote (Filename.basename recorded.run_dir))
      sandbox_flag prompt_variable
      (String.concat " " (List.map Filename.quote argv))
      (Filename.quote out)
  in
  Alcotest.(check int) "bactrian replay exit status" 0 (Sys.command command);
  let json = Yojson.Safe.from_string (read out) in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "replay outcome" "passed"
    (to_string (member "kind" (member "outcome" json)));
  Alcotest.(check int)
    "no divergences" 0
    (List.length (to_list (member "divergences" json)));
  Alcotest.(check int) "server untouched" era.requests (List.length (lines log));
  Alcotest.(check int) "upstream untouched" 2 (List.length (F.requests up));
  let meta =
    Yojson.Safe.from_file
      (Filename.concat (to_string (member "run_dir" json)) "meta.json")
  in
  Alcotest.(check bool)
    "the replay was sandboxed" sandbox_available
    (to_bool (member "applied" (member "sandbox" meta)));
  Alcotest.(check string)
    "the replay's meta.json lists the server, without its command"
    {|[{"name":"weather","transport":"stdio","policy":"replay"}]|}
    (Yojson.Safe.to_string (member "mcp" meta))

(* Escape agents (test/sandbox): each makes one model call, then tries to reach
   a public host by HTTPS, TCP, WebSocket and DNS, to write to the real home
   directory and the project, to read a denied directory, to forge a line in
   the forwarder's violation log through /proc and to leak the real key. Every
   attempt has to fail. *)

let usable tool = Sys.command (tool ^ " --version > /dev/null 2>&1") = 0

let escapers =
  [
    {
      name = "typescript";
      argv = (fun () -> [ "node"; in_repo "test/sandbox/escape.mts" ]);
    };
    {
      name = "python";
      argv = (fun () -> [ "python3"; "-I"; in_repo "test/sandbox/escape.py" ]);
    };
  ]

let rec files_under path =
  if Sys.is_directory path then
    Array.to_list (Sys.readdir path)
    |> List.concat_map (fun f -> files_under (Filename.concat path f))
  else [ path ]

let escape_results (r : Run.result) =
  let replies =
    List.filter_map
      (fun j ->
        let open Yojson.Safe.Util in
        if String.equal (to_string (member "type" j)) "reply" then
          Some (to_string (member "content" j))
        else None)
      (trace r)
  in
  match replies with
  | [ content ] -> Yojson.Safe.Util.(to_assoc (Yojson.Safe.from_string content))
  | _ -> Alcotest.fail "expected exactly one reply"

let escapes agent () =
  let runtime =
    if String.equal agent.name "typescript" then "node" else "python3"
  in
  if not sandbox_available then begin
    print_endline "no sandbox-exec or bwrap on this host: skipped";
    Alcotest.skip ()
  end
  else if not (usable runtime) then begin
    Printf.printf "%s is not usable on this host: skipped\n" runtime;
    Alcotest.skip ()
  end;
  let home =
    match Sys.getenv_opt "HOME" with
    | Some h -> h
    | None -> Alcotest.fail "HOME"
  in
  let home_file =
    Filename.concat home
      (Printf.sprintf ".bactrian-escape-%s-%d" agent.name (Unix.getpid ()))
  in
  let previous = Sys.getenv_opt "OPENAI_API_KEY" in
  (* The key sits in the environment bactrian runs in, as it would for a
     live run, and must not be passed on. *)
  Unix.putenv "OPENAI_API_KEY" real_key;
  Fun.protect ~finally:(fun () ->
      Unix.putenv "OPENAI_API_KEY" (Option.value previous ~default:"");
      if Sys.file_exists home_file then Sys.remove home_file)
  @@ fun () ->
  with_world ~deny_read:[ "secret" ] ("escape-" ^ agent.name)
  @@ fun { up; execute; dir; env = _ } ->
  let secret_file = Filename.concat (Filename.concat dir "secret") "key" in
  Out_channel.with_open_bin secret_file (fun oc -> output_string oc "s3cret");
  let project_file = Filename.concat dir "escaped" in
  let r =
    execute
      ~agent:(Command (agent.argv ()))
      ~variant_env:
        [
          ("REAL_HOME_FILE", home_file);
          ("PROJECT_FILE", project_file);
          ("SECRET_FILE", secret_file);
        ]
      ()
  in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  check_sandboxed r;
  let results = escape_results r in
  let outcome name =
    match List.assoc_opt name results with
    | Some j -> Yojson.Safe.Util.(to_string (member "outcome" j))
    | None -> Alcotest.failf "no result for %s" name
  in
  let detail name =
    Yojson.Safe.Util.(to_string (member "detail" (List.assoc name results)))
  in
  List.iter
    (fun name -> Alcotest.(check string) (name ^ " works") "ok" (outcome name))
    [ "model"; "state_write" ];
  Alcotest.(check string)
    "the model call went through" "status 200" (detail "model");
  Alcotest.(check int)
    "upstream saw the model call" 1
    (List.length (F.requests up));
  let escapes =
    [
      "https";
      "tcp";
      "websocket";
      "dns";
      "home_write";
      "project_write";
      "deny_read";
      "forwarder_log";
    ]
  in
  List.iter
    (fun name ->
      Printf.printf "%s: %s\n" name (detail name);
      Alcotest.(check string) (name ^ " is denied") "failed" (outcome name))
    escapes;
  Alcotest.(check bool)
    "nothing in the real home" false
    (Sys.file_exists home_file);
  Alcotest.(check bool)
    "nothing in the project" false
    (Sys.file_exists project_file);
  (* The agent's own account of its environment: the placeholder, never the
     key. *)
  let dump =
    Yojson.Safe.from_file (Filename.concat r.run_dir "state/agent-env.json")
  in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "the agent holds a placeholder" true
    (String.starts_with ~prefix:"bactrian-"
       (to_string (member "OPENAI_API_KEY" (member "env" dump))));
  Alcotest.(check bool) "argv recorded" true (to_list (member "argv" dump) <> []);
  Alcotest.(check (list string))
    "no file of the world holds the real key" []
    (List.filter (fun f -> contains (read f) real_key) (files_under dir));
  match r.violations with
  | Not_collected m -> Alcotest.failf "violations not collected: %s" m
  | Collected { items = vs; coverage = Seatbelt_log; gaps } ->
      Alcotest.(check bool) "labelled a lower bound" true (gaps <> []);
      (* Seatbelt logs each denial, with the remote host shown as [*]. *)
      Denials.check vs
        [
          ("https", "network-outbound", ":443");
          ("tcp", "network-outbound", ":853");
          ("websocket", "network-outbound", ":80");
          ("dns", "network-outbound", "mDNSResponder");
          ("home write", "file-write-create", Filename.basename home_file);
          ("project write", "file-write-create", "/escaped");
          ("denied read", "file-read-data", "/secret/key");
        ];
      Alcotest.(check bool)
        "in the trace" true
        (List.mem "sandbox_violation"
           (List.map
              (fun j -> Yojson.Safe.Util.(to_string (member "type" j)))
              (trace r)))
  | Collected { items = vs; coverage = Relay_failures_only; gaps = _ } ->
      (* Linux reports only relay failures, and the model call makes none. The
         other denials show only as the agent's own errors, checked above. The
         forwarder is not dumpable, so its descriptors are refused, not
         missing. *)
      Alcotest.(check int) "no relay failures" 0 (List.length vs);
      Alcotest.(check bool)
        "the forwarder's descriptors are refused" true
        (contains
           (String.lowercase_ascii (detail "forwarder_log"))
           "permission denied")

(* HTTP mode: the agent is a server the test starts (--http 0 prints its port
   as the first line of its standard output), and stops by its recorded pid. *)

let rec reap pid =
  match Unix.waitpid [] pid with
  | _ -> ()
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> reap pid

let with_server argv f =
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  let null = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let pid =
    Unix.create_process (List.hd argv) (Array.of_list argv) null out_w
      Unix.stderr
  in
  Unix.close null;
  Unix.close out_w;
  let out = Unix.in_channel_of_descr out_r in
  Fun.protect ~finally:(fun () ->
      Unix.kill pid Sys.sigterm;
      reap pid;
      close_in out)
  @@ fun () ->
  match int_of_string_opt (input_line out) with
  | Some port -> f port
  | None -> Alcotest.fail "the server did not print its port first"
  | exception End_of_file -> Alcotest.fail "the server exited before its port"

let namespace_header = "X-Agent-Run"

let http_agent port : Run.agent =
  Http
    { url = Printf.sprintf "http://127.0.0.1:%d/agent" port; namespace_header }

(* The record, strict replay and prompt-change cycle of [acceptance]. The
   system prompt reaches the agent through the run's env, so the variant
   carries it. *)
let http_acceptance agent () =
  let argv = agent.argv () @ [ "--http"; "0" ] in
  with_server argv @@ fun port ->
  with_world (agent.name ^ "-http") @@ fun { up; execute; dir = _; env = _ } ->
  let execute = execute ~agent:(http_agent port) in
  let recorded = execute () in
  Alcotest.(check string) "record outcome" "passed" (outcome_name recorded);
  Alcotest.(check int) "model exchanges" 2 recorded.findings.exchanges;
  Alcotest.(check int) "upstream requests" 2 (List.length (F.requests up));
  Alcotest.(check (list string))
    "tool calls" [ "get_weather" ]
    (List.map (member_string "name") (entries "tool_call" recorded));
  Alcotest.(check int) "one reply" 1 (List.length (entries "reply" recorded));
  let meta =
    Yojson.Safe.from_file (Filename.concat recorded.run_dir "meta.json")
  in
  let label = Yojson.Safe.Util.member "sandbox" meta in
  Alcotest.(check bool)
    "unsandboxed" false
    Yojson.Safe.Util.(to_bool (member "applied" label));
  Alcotest.(check string)
    "because of the mode" "http mode"
    (member_string "reason" label);
  let replayed = execute ~replay:(strict recorded.run_dir) () in
  Alcotest.(check string) "replay outcome" "passed" (outcome_name replayed);
  Alcotest.(check int)
    "no divergences" 0
    (List.length replayed.findings.divergences);
  Alcotest.(check int) "upstream untouched" 2 (List.length (F.requests up));
  let changed =
    execute
      ~variant_env:[ (prompt_variable, "You are a terse assistant.") ]
      ~replay:(strict recorded.run_dir) ()
  in
  Alcotest.(check string) "changed outcome" "diverged" (outcome_name changed);
  Alcotest.(check int) "upstream untouched" 2 (List.length (F.requests up));
  match
    List.filter_map
      (function
        | Matcher.Missing_request { nearest; _ } -> Some nearest
        | Unconsumed _ | Reordered _ | Shape_mismatch _ -> None)
      changed.findings.divergences
  with
  | [ Some (_, changes, _) ] ->
      Alcotest.(check bool)
        "diff names the system message" true
        (List.exists
           (fun (c : Diff.change) -> String.equal c.path "messages[0].content")
           changes)
  | [ None ] -> Alcotest.fail "the missing request has no nearest exchange"
  | [] | _ :: _ :: _ -> Alcotest.fail "expected exactly one Missing_request"

(* Two strict replays at once against one server: each run keeps its own
   conversation, so both match the recording. *)
let http_concurrent agent () =
  let argv = agent.argv () @ [ "--http"; "0" ] in
  with_server argv @@ fun port ->
  with_world (agent.name ^ "-http-concurrent")
  @@ fun { up; execute; dir = _; env = _ } ->
  let execute = execute ~agent:(http_agent port) in
  let recorded = execute () in
  Alcotest.(check string) "record outcome" "passed" (outcome_name recorded);
  let replay () = execute ~replay:(strict recorded.run_dir) () in
  let a = ref None and b = ref None in
  Eio.Fiber.both
    (fun () -> a := Some (replay ()))
    (fun () -> b := Some (replay ()));
  let a = Option.get !a and b = Option.get !b in
  List.iter
    (fun (r : Run.result) ->
      Alcotest.(check string) "replay outcome" "passed" (outcome_name r);
      Alcotest.(check int)
        "no divergences" 0
        (List.length r.findings.divergences);
      Alcotest.(check int) "its own exchanges" 2 r.findings.exchanges)
    [ a; b ];
  Alcotest.(check bool) "distinct run IDs" true (a.run_id <> b.run_id);
  Alcotest.(check int) "upstream untouched" 2 (List.length (F.requests up))

let () =
  Alcotest.run "acceptance"
    [
      ( "example agents",
        List.map
          (fun a -> Alcotest.test_case a.name `Slow (acceptance a))
          agents );
      ( "http mode",
        List.concat_map
          (fun a ->
            [
              Alcotest.test_case (a.name ^ ", cycle") `Slow (http_acceptance a);
              Alcotest.test_case
                (a.name ^ ", concurrent replays")
                `Slow (http_concurrent a);
            ])
          agents );
      ( "mcp through mcp-shim",
        List.concat_map
          (fun a ->
            List.map
              (fun e ->
                Alcotest.test_case
                  (a.name ^ ", " ^ e.label)
                  `Slow (mcp_acceptance a e))
              eras)
          agents );
      ( "mcp flags and bactrian replay",
        List.concat_map
          (fun a ->
            List.map
              (fun e ->
                Alcotest.test_case
                  (a.name ^ ", " ^ e.label)
                  `Slow (mcp_flags_acceptance a e))
              eras)
          agents );
      ( "sandbox escapes",
        List.map (fun a -> Alcotest.test_case a.name `Slow (escapes a)) escapers
      );
    ]
