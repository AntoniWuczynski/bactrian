(* The server's policy: without the flags it refuses whatever runs outside the
   sandbox, and each refusal names the flag that allows it. The tests run in a
   fresh directory of their own, which is the server's root. *)

open Bactrian_core
open Bactrian_server
open Mcp_support

(* Without the flags the server refuses whatever runs outside the sandbox,
   and each refusal names the flag that allows it. Checkers run inside it in
   strict mode, so only a mode that can leave them outside needs a flag. *)
let test_policy () =
  let names flag result = mentions flag (Canon.to_string result) in
  let refused_for flag what result =
    Alcotest.(check bool) (what ^ " refused") true (is_error result);
    Alcotest.(check bool) (what ^ " names " ^ flag) true (names flag result)
  in
  ( with_context @@ fun context call ->
    write "pol/s.json"
      {|{"version": 1, "id": "s", "turns": [{"user": "hi"}],
        "checkers": [{"command": ["true"]}], "golden": {"pass": []}}|};
    List.iter
      (fun (tool, args, flag) ->
        refused_for flag (tool ^ " " ^ args) (call tool (Some args)))
      [
        ( "run_start",
          {|{"experiment":"pol/s.json","sandbox":"off"}|},
          "--allow-unsandboxed" );
        ( "run_start",
          {|{"experiment":"pol/s.json","sandbox":"auto"}|},
          "--allow-unsandboxed" );
        ( "record_start",
          {|{"agent":["true"],"turns":["hi"],"sandbox":"off"}|},
          "--allow-unsandboxed" );
        ( "record_start",
          {|{"agent":["true"],"turns":["hi"],"sandbox_backend":"seatbelt"}|},
          "--allow-unsandboxed" );
        ( "replay_start",
          {|{"agent":["true"],"run":"r1","sandbox_backend":"seatbelt"}|},
          "--allow-unsandboxed" );
        ( "record_start",
          {|{"agent":["true"],"turns":["hi"],"env":["HOME"]}|},
          "--trust-project" );
        ( "record_start",
          {|{"agent":["true"],"turns":["hi"],"mcp_stdio":["x=[\"true\"]"]}|},
          "--trust-project" );
        ( "record_start",
          {|{"agent":["true"],"turns":["hi"],"mcp_http":["x=http://127.0.0.1:9/"]}|},
          "--trust-project" );
        ( "replay_start",
          {|{"agent":["true"],"run":"r1","env":["HOME"]}|},
          "--trust-project" );
        ( "check",
          {|{"scenarios":["pol/s.json"],"sandbox":"off"}|},
          "--allow-unsandboxed" );
      ];
    (match
       Bactrian_runtime.Golden.load
         ~files:(Eio.Stdenv.fs context.env)
         ~scenario_file:"pol/s.json" ()
     with
    | Ok s ->
        Alcotest.(check (result unit string))
          "checkers in strict pass" (Ok ())
          (Policy.scenario context.policy ~sandbox:`Strict s)
    | Error _ -> Alcotest.fail "pol/s.json does not load");
    write "pol2/s.json" {|{"version": 1, "id": "s", "turns": [{"user": "hi"}]}|};
    let experiment (scenarios, agent) =
      Printf.sprintf
        {|{"version": 1, "name": "x",
          "agent": {"protocol": "command", "command": ["true"]%s},
          "scenarios": [%S], "variants": {"a": {}}, "repeats": 1,
          "caps_usd": {"openai": 1}}|}
        agent scenarios
    in
    List.iter
      (fun (name, extra, flag) ->
        write (Printf.sprintf "polx/%s.json" name) (experiment extra);
        let status =
          finished context.env call
            (call "run_start"
               (Some (Printf.sprintf {|{"experiment":"polx/%s.json"}|} name)))
        in
        Alcotest.(check bool)
          (name ^ " names " ^ flag)
          true
          (mentions flag (Canon.to_string status)))
      [
        ( "passthrough",
          ("../pol2/*.json", {|, "env_passthrough": ["HOME"]|}),
          "--trust-project" );
      ];
    write "polx/seatbelt.json"
      {|{"version": 1, "name": "x",
        "agent": {"protocol": "command", "command": ["true"]},
        "scenarios": ["../pol2/*.json"], "variants": {"a": {}}, "repeats": 1,
        "caps_usd": {"openai": 1},
        "sandbox": {"mode": "strict", "backend": "seatbelt"}}|};
    let status =
      finished context.env call
        (call "run_start" (Some {|{"experiment":"polx/seatbelt.json"}|}))
    in
    Alcotest.(check bool)
      "an experiment's backend seatbelt names --allow-unsandboxed" true
      (mentions "--allow-unsandboxed" (Canon.to_string status));
    let instructions = (Mcp_server.server context.policy).instructions in
    Alcotest.(check bool)
      "the instructions state the policy" true
      (mentions "--allow-unsandboxed" instructions
      && mentions "--trust-project" instructions) );
  (* Allowed outside the sandbox, checkers in auto or off still need the
     project to be trusted. *)
  ( with_context ~policy:{ Policy.default with allow_unsandboxed = true }
  @@ fun context call ->
    List.iter
      (fun mode ->
        refused_for "--trust-project" ("check in " ^ mode)
          (call "check"
             (Some
                (Printf.sprintf {|{"scenarios":["pol/s.json"],"sandbox":%S}|}
                   mode))))
      [ "auto"; "off" ];
    write "polx/checkers.json"
      {|{"version": 1, "name": "x",
        "agent": {"protocol": "command", "command": ["true"]},
        "scenarios": ["../pol/*.json"], "variants": {"a": {}}, "repeats": 1,
        "caps_usd": {"openai": 1}}|};
    let status =
      finished context.env call
        (call "run_start"
           (Some {|{"experiment":"polx/checkers.json","sandbox":"off"}|}))
    in
    Alcotest.(check bool)
      "checkers in an experiment with sandbox off name --trust-project" true
      (mentions "--trust-project" (Canon.to_string status));
    write "pol2/s.json" {|{"version": 1, "id": "s", "turns": [{"user": "hi"}]}|};
    write "polx/real-keys.json"
      {|{"version": 1, "name": "x",
        "agent": {"protocol": "command", "command": ["true"], "real_keys": true},
        "scenarios": ["../pol2/*.json"], "variants": {"a": {}}, "repeats": 1,
        "caps_usd": {"openai": 1}}|};
    let status =
      finished context.env call
        (call "run_start" (Some {|{"experiment":"polx/real-keys.json"}|}))
    in
    Alcotest.(check bool)
      "real keys in an experiment name --trust-project" true
      (mentions "agent.real_keys" (Canon.to_string status)
      && mentions "--trust-project" (Canon.to_string status)) );
  (* Trusting the project is not enough: the job runs in strict mode, which
     real keys cannot meet, so the refusal names the other flag. *)
  ( with_context ~policy:{ Policy.default with trust_project = true }
  @@ fun context call ->
    write "pol2/s.json" {|{"version": 1, "id": "s", "turns": [{"user": "hi"}]}|};
    write "polx/real-keys.json"
      {|{"version": 1, "name": "x",
        "agent": {"protocol": "command", "command": ["true"], "real_keys": true},
        "scenarios": ["../pol2/*.json"], "variants": {"a": {}}, "repeats": 1,
        "caps_usd": {"openai": 1}}|};
    let status =
      finished context.env call
        (call "run_start" (Some {|{"experiment":"polx/real-keys.json"}|}))
    in
    Alcotest.(check bool)
      "real keys without --allow-unsandboxed name it, not --trust-project" true
      (mentions "agent.real_keys" (Canon.to_string status)
      && mentions "--allow-unsandboxed" (Canon.to_string status)
      && not (mentions "needs --trust-project" (Canon.to_string status))) );
  with_context
    ~policy:
      { Policy.default with allow_unsandboxed = true; trust_project = true }
  @@ fun context _ ->
  Alcotest.(check (result (option string) string))
    "the flag leaves the mode to the caller" (Ok (Some "off"))
    (Result.map
       (Option.map Bactrian_runtime.Sandbox.mode_name)
       (Policy.sandbox context.policy (Some `Off)));
  Alcotest.(check (result unit string))
    "trusted, env passes" (Ok ())
    (Policy.env context.policy [ "HOME" ]);
  Alcotest.(check (result unit string))
    "allowed outside the sandbox, seatbelt passes" (Ok ())
    (Policy.backend context.policy ~field:"argument sandbox_backend" `Seatbelt)

let () =
  let dir = Filename.temp_dir ~temp_dir:(Sys.getcwd ()) "mcp-policy" "" in
  Sys.chdir dir;
  Alcotest.run "mcp_policy"
    [
      ( "policy",
        [ Alcotest.test_case "the policy and its flags" `Quick test_policy ] );
    ]
