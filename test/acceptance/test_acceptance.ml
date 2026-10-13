open Bactrian_core
open Bactrian_runtime
module P = Model_proxy
module F = Fake_upstream

let () = Mirage_crypto_rng_unix.use_default ()
let fixtures = "../fixtures/protocols/openai_chat"
let prompt_variable = "AGENT_SYSTEM_PROMPT"

let first_turn =
  "What is the weather in Paris right now? Use the get_weather tool."

(* The repository root: dune sets DUNE_SOURCEROOT in every action. *)
let repo_root =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root -> root
  | None -> Alcotest.failf "DUNE_SOURCEROOT is not set (run through dune)"

let in_repo path = Filename.concat repo_root path

(* A missing tool is a failure, never a skip. *)
let require tool =
  let cmd = Printf.sprintf "%s --version > /dev/null 2>&1" tool in
  if Sys.command cmd <> 0 then
    Alcotest.failf "%s is required for the acceptance test but is not usable"
      tool

type agent = { name : string; argv : unit -> string list }

let agents =
  [
    {
      name = "typescript";
      argv =
        (fun () ->
          require "node";
          [ "node"; in_repo "examples/agents/typescript/dist/agent.js" ]);
    };
    {
      name = "python";
      argv =
        (fun () ->
          require "uv";
          let project = in_repo "examples/agents/python" in
          (* uv run would lock the project's virtual environment, a write
             outside the run that the sandbox refuses. *)
          [
            "uv";
            "run";
            "--no-sync";
            "--project";
            project;
            Filename.concat project "agent.py";
          ]);
    };
  ]

let read path = In_channel.with_open_bin path In_channel.input_all

let reply case : F.reply =
  {
    status = 200;
    headers = [ ("content-type", "application/json") ];
    chunks = [ read (Filename.concat fixtures (case ^ "/response.raw")) ];
    ending = Complete;
  }

let caps =
  List.map
    (fun p -> (p, 100.))
    [ "openai"; "anthropic"; "gemini"; "openrouter" ]

let counter = ref 0

let random_bytes n =
  incr counter;
  String.init n (fun i -> Char.chr ((!counter + (i * 7)) land 255))

let lines path =
  String.split_on_char '\n' (read path) |> List.filter (( <> ) "")

let trace (r : Run.result) =
  List.map
    (fun l -> Yojson.Safe.from_string l)
    (lines (Filename.concat r.run_dir "trace.jsonl"))

let entries kind (r : Run.result) =
  List.filter
    (fun j -> String.equal Yojson.Safe.Util.(to_string (member "type" j)) kind)
    (trace r)

let outcome_name (r : Run.result) =
  match r.outcome with
  | Passed -> "passed"
  | Failed s -> "failed: " ^ s
  | Diverged -> "diverged"
  | Errored s -> "errored: " ^ s
  | Not_run s -> "not_run: " ^ s

let ok = function Ok v -> v | Error e -> Alcotest.failf "execute: %s" e

(* dune gives the paths of the executables relative to the test's directory. *)
let executable variable =
  match Sys.getenv_opt variable with
  | Some p when Filename.is_relative p -> Filename.concat (Sys.getcwd ()) p
  | Some p -> p
  | None -> Alcotest.failf "%s is not set (run through dune)" variable

(* Every run here is sandboxed where the host has a sandbox (design §5.3), with
   strict, so a sandbox that does not work fails the test. *)
let on_path name =
  List.exists
    (fun dir ->
      (not (String.equal dir "")) && Sys.file_exists (Filename.concat dir name))
    (String.split_on_char ':'
       (Option.value (Sys.getenv_opt "PATH") ~default:""))

let sandbox_available =
  Sys.file_exists "/usr/bin/sandbox-exec" || on_path "bwrap"

let () =
  if not sandbox_available then
    print_endline
      "no sandbox-exec or bwrap on this host: runs are not sandboxed"

let sandbox_flag = if sandbox_available then "strict" else "off"

let sandbox ~env ~dir =
  if not sandbox_available then Sandbox.off
  else
    match
      Sandbox.preflight ~env
        ~warn:(fun w -> Alcotest.failf "warning: %s" w)
        {
          mode = `Strict;
          allow_write = [];
          deny_read = [];
          bactrian_exe = executable "BACTRIAN_EXE";
          bactrian_dir = Filename.concat dir ".bactrian";
        }
    with
    | Ok t -> t
    | Error m -> Alcotest.failf "sandbox preflight: %s" m

let check_sandboxed (r : Run.result) =
  Alcotest.(check bool) "sandboxed" sandbox_available r.sandbox.applied

type world = {
  up : F.t;
  dir : string;  (** The world's directory, with its [.bactrian] store. *)
  execute :
    argv:string list ->
    ?replay:Run.replay ->
    ?mcp:Mcp_proxy.config list ->
    ?variant_env:(string * string) list ->
    unit ->
    Run.result;
}

let with_world name f =
  Eio_main.run @@ fun env ->
  let dir = Eio.Path.(Eio.Stdenv.cwd env / ("tmp-acceptance-" ^ name)) in
  Eio.Path.rmtree ~missing_ok:true dir;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let proxy =
    match
      P.start ~sw ~net ~clock:(Eio.Stdenv.clock env)
        ~process_mgr:(Eio.Stdenv.process_mgr env)
    with
    | Ok p -> p
    | Error e -> Alcotest.fail e
  in
  let up =
    match
      F.start ~sw ~net (F.Script [ reply "luna_nonstream"; reply "luna_turn2" ])
    with
    | Ok u -> u
    | Error e -> Alcotest.failf "fake upstream: %a" F.pp_error e
  in
  let store = Store.create ~root:Eio.Path.(dir / ".bactrian") in
  let sandbox =
    sandbox ~env
      ~dir:(Filename.concat (Sys.getcwd ()) (Eio.Path.native_exn dir))
  in
  let execute ~argv ?replay ?(mcp = []) ?(variant_env = []) () =
    incr counter;
    let ledger =
      Ledger.create
        ~path:Eio.Path.(dir / ("ledger-" ^ string_of_int !counter ^ ".jsonl"))
        ~caps
    in
    let config : Run.config =
      {
        agent = argv;
        cwd = ".";
        turns = (if replay = None then [ first_turn ] else []);
        scenario = "weather";
        variant = "base";
        sim_now = "";
        timezone = "UTC";
        replay;
        env_passthrough = [ prompt_variable ];
        variant_env;
        fixtures = None;
        normalise = [];
        redact = [];
        pricing = Pricing.default;
        checks = None;
        unenforced = [];
        turn_timeout = 60.;
        total_timeout = 90.;
        mcp;
        sandbox;
        faketime = true;
      }
    in
    ok
      (Run.execute ~random_bytes
         ~keys:[ (P.Openai, "real-openai-key") ]
         ~upstream:(fun _ -> Uri.of_string (F.base_url up))
         ~env ~proxy ~store ~ledger config)
  in
  f { up; dir = Filename.concat (Sys.getcwd ()) (snd dir); execute }

let strict from : Run.replay =
  { from; matcher = Strict; allow_reorder = false; timing = P.Fast }

let acceptance agent () =
  let argv = agent.argv () in
  with_world agent.name @@ fun { up; execute; dir = _ } ->
  let execute = execute ~argv in
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
  | Collected [] -> ()
  | Collected (v :: _) ->
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
  @@ fun { up; execute; dir = _ } ->
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
    execute ~argv ~variant_env
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
    execute ~argv ~variant_env ~replay:(strict recorded.run_dir)
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
  @@ fun { up; dir; execute } ->
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
  let recorded = execute ~argv ~variant_env ~mcp () in
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

let () =
  Alcotest.run "acceptance"
    [
      ( "example agents",
        List.map
          (fun a -> Alcotest.test_case a.name `Slow (acceptance a))
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
    ]
