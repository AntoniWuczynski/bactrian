(* HTTP-mode agents (design §5.1): the per-run environment travels in the
   [start] message, and concurrent runs against one server stay apart. *)

open Bactrian_core
open Bactrian_runtime
open Run_support
module H = Http_agent

let namespace_header = "X-Agent-Run"

let model_request =
  read "../fixtures/protocols/openai_chat/luna_nonstream/request.json"

let http_config ?(variant_env = []) url : Run.config =
  {
    (config ()) with
    agent = Http { url; namespace_header };
    variant_env;
    sandbox = Sandbox.http_mode `Auto;
  }

let with_agent ?(start_delay = 0.) ctx f =
  Eio.Switch.run @@ fun sw ->
  f
    (H.start ~sw ~net:(Eio.Stdenv.net ctx.env) ~clock:(Eio.Stdenv.clock ctx.env)
       ~namespace_header ~start_delay ~model_request)

(* The run token is the path segment after /r/ in a base URL. *)
let token_of (s : H.session) =
  match String.split_on_char '/' (List.assoc "OPENAI_BASE_URL" s.env) with
  | _ :: _ :: _ :: "r" :: token :: _ -> token
  | _ -> Alcotest.fail "OPENAI_BASE_URL has no run token"

let sorted = List.sort String.compare

(* The agent's own process keeps these: they are not routing or identity. *)
let process_variables =
  [
    "HOME";
    "XDG_CACHE_HOME";
    "XDG_CONFIG_HOME";
    "XDG_DATA_HOME";
    "npm_config_cache";
    "TMPDIR";
    "PYTHONDONTWRITEBYTECODE";
  ]

let test_env_contract =
  with_ctx "http-env" @@ fun ctx ->
  with_agent ctx @@ fun agent ->
  let r =
    ok
      (execute ctx
         (http_config ~variant_env:[ ("AGENT_MODEL", "gpt-x") ] (H.url agent)))
  in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  Alcotest.(check int) "exchanges" 1 r.findings.exchanges;
  let s = List.hd (H.sessions agent) in
  Alcotest.check strs "the env holds the per-run variables and nothing else"
    (sorted
       ("AGENT_MODEL"
       :: List.filter
            (fun k ->
              k <> "BACTRIAN_MCP_URL" && not (List.mem k process_variables))
            Experiment.reserved_env))
    (sorted (List.map fst s.env));
  let var k = List.assoc k s.env in
  Alcotest.(check string) "run id in env" r.run_id (var "BACTRIAN_RUN_ID");
  Alcotest.(check string) "run id header" r.run_id s.run_id;
  Alcotest.(check (option string))
    "namespace header" (Some r.run_id) s.namespace;
  Alcotest.(check string)
    "state dir"
    (Filename.concat r.run_dir "state")
    (var "BACTRIAN_STATE_DIR");
  Alcotest.(check string) "variant variable" "gpt-x" (var "AGENT_MODEL");
  List.iter
    (fun (provider, _) ->
      let key = Run.key_variable provider in
      Alcotest.(check string)
        (key ^ " is a placeholder")
        ("bactrian-" ^ token_of s)
        (var key))
    Run.providers;
  Alcotest.(check bool)
    "the real key is not in the start message" false
    (contains s.start_body real_openai_key);
  Alcotest.(check (option int))
    "the model call went through the run's token" (Some 200) s.model_status;
  Alcotest.check strs "trace"
    [ "run_start"; "turn_start"; "model_call"; "tool_call"; "reply"; "done" ]
    (trace_types r);
  Alcotest.(check bool) "result label: unsandboxed" false r.sandbox.applied;
  Alcotest.(check (option string))
    "result label: reason" (Some "http mode") r.sandbox.reason;
  let meta = Yojson.Safe.from_file (file r "meta.json") in
  Alcotest.(check string)
    "meta.json label" {|{"mode":"auto","applied":false,"reason":"http mode"}|}
    (Yojson.Safe.to_string (Yojson.Safe.Util.member "sandbox" meta))

let test_concurrent_runs =
  with_ctx "http-concurrent" @@ fun ctx ->
  with_agent ~start_delay:0.3 ctx @@ fun agent ->
  let run name =
    ok (execute ~ledger_name:name ctx (http_config (H.url agent)))
  in
  let a = ref None and b = ref None in
  Eio.Fiber.both (fun () -> a := Some (run "a")) (fun () -> b := Some (run "b"));
  let a = Option.get !a and b = Option.get !b in
  Alcotest.(check string) "a" "passed" (outcome_name a);
  Alcotest.(check string) "b" "passed" (outcome_name b);
  let sessions = H.sessions agent in
  Alcotest.(check int) "two sessions" 2 (List.length sessions);
  let distinct what values =
    Alcotest.(check int)
      (what ^ " are distinct") 2
      (List.length (List.sort_uniq String.compare values))
  in
  distinct "run IDs" (List.map (fun (s : H.session) -> s.run_id) sessions);
  distinct "tokens" (List.map token_of sessions);
  distinct "namespaces"
    (List.filter_map (fun (s : H.session) -> s.namespace) sessions);
  Alcotest.check strs "each run has its own session"
    (sorted [ a.run_id; b.run_id ])
    (sorted (List.map (fun (s : H.session) -> s.run_id) sessions));
  List.iter
    (fun (r : Run.result) ->
      let s = List.find (fun (s : H.session) -> s.run_id = r.run_id) sessions in
      Alcotest.(check (option string))
        "namespace is the run's own" (Some r.run_id) s.namespace;
      Alcotest.(check string)
        "env is the run's own" r.run_id
        (List.assoc "BACTRIAN_RUN_ID" s.env);
      let cassette =
        Result.get_ok (Store.read_cassette ctx.store ~run_id:r.run_id)
      in
      Alcotest.(check int)
        "one exchange in its own cassette" 1
        (List.length cassette.exchanges);
      Alcotest.(check string)
        "the cassette is the run's" r.run_id cassette.header.run_id)
    [ a; b ]

(* A server that is not there ends the run, it does not hang it. *)
let test_unreachable =
  with_ctx "http-unreachable" @@ fun ctx ->
  let port =
    Eio.Switch.run @@ fun sw ->
    let sock =
      Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true (Eio.Stdenv.net ctx.env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    match Eio.Net.listening_addr sock with
    | `Tcp (_, port) -> port
    | `Unix _ -> Alcotest.fail "not a TCP listener"
  in
  let r =
    ok (execute ctx (http_config (Printf.sprintf "http://127.0.0.1:%d/a" port)))
  in
  Alcotest.(check bool)
    "failed with agent_http_error" true
    (String.starts_with ~prefix:"failed: agent_http_error: agent unreachable"
       (outcome_name r))

let () =
  Alcotest.run "run_http"
    [
      ( "http mode",
        [
          Alcotest.test_case "the env contract" `Slow test_env_contract;
          Alcotest.test_case "concurrent runs" `Slow test_concurrent_runs;
          Alcotest.test_case "unreachable server" `Slow test_unreachable;
        ] );
    ]
