open Bactrian_core
open Bactrian_runtime
open Run_support
module P = Model_proxy
module F = Fake_upstream

(* Record *)

let test_record =
  with_ctx "record" @@ fun ctx ->
  let r = record ctx in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  Alcotest.(check int) "exchanges" 2 r.findings.exchanges;
  Alcotest.(check int) "upstream requests" 2 (List.length (F.requests ctx.up));
  let cassette =
    Result.get_ok (Store.read_cassette ctx.store ~run_id:r.run_id)
  in
  Alcotest.(check bool) "cassette complete" true cassette.complete;
  Alcotest.(check int) "cassette exchanges" 2 (List.length cassette.exchanges);
  Alcotest.check strs "trace"
    [
      "run_start";
      "turn_start";
      "model_call";
      "tool_call";
      "agent_event";
      "tool_result";
      "model_call";
      "reply";
      "done";
    ]
    (trace_types r);
  let trace = read (file r "trace.jsonl") in
  Alcotest.(check bool)
    "usage in trace" true
    (contains trace {|"gen_ai.usage.input_tokens":145|});
  let meta = Yojson.Safe.from_file (file r "meta.json") in
  let str k = Yojson.Safe.Util.(to_string (member k meta)) in
  Alcotest.(check string) "meta run id" r.run_id (str "run_id");
  Alcotest.(check string) "meta outcome" "passed" (str "outcome");
  Alcotest.(check string) "meta clock" "contract_only" (str "clock");
  Alcotest.(check string) "meta sandbox" "unsandboxed" (str "sandbox");
  Alcotest.(check string) "meta scenario" "weather" (str "scenario");
  Alcotest.(check string)
    "meta version" Version.current (str "bactrian_version");
  Alcotest.(check bool)
    "meta cost" true
    (Yojson.Safe.Util.(to_float (member "cost_usd" meta)) > 0.);
  Alcotest.(check bool) "spend in result" true (r.spend > 0.);
  Alcotest.(check bool)
    "no temporary files" false
    (Sys.file_exists (file r "meta.json.tmp"));
  Alcotest.(check bool)
    "real key not in the cassette" false
    (contains (read (file r "cassette.jsonl")) real_openai_key)

(* The environment is exactly the allowlist *)

let allowed =
  [
    "PATH";
    "HOME";
    "LANG";
    "TERM";
    "SIM_NOW";
    "TZ";
    "BACTRIAN_RUN_ID";
    "BACTRIAN_STATE_DIR";
    "OPENAI_BASE_URL";
    "ANTHROPIC_BASE_URL";
    "GOOGLE_GEMINI_BASE_URL";
    "OPENROUTER_BASE_URL";
    "OPENAI_API_KEY";
    "ANTHROPIC_API_KEY";
    "GEMINI_API_KEY";
    "OPENROUTER_API_KEY";
    "PASSED_THROUGH";
    (* Added by the Python runtime itself on macOS and for the C locale. *)
    "__CF_USER_TEXT_ENCODING";
    "LC_CTYPE";
  ]

(* MCP servers add the shim URL and one URL per HTTP server. *)
let test_mcp_environment =
  with_ctx "mcp-env" @@ fun ctx ->
  let mcp : Mcp_proxy.config list =
    [
      {
        name = "calendar";
        policy = Block;
        server = Stdio { argv = [ "true" ]; env = []; cwd = None };
      };
      {
        name = "web-search";
        policy = Block;
        server = Http { url = Uri.of_string "http://127.0.0.1:9" };
      };
    ]
  in
  let r = ok (execute ctx { (config ()) with mcp }) in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  let env =
    Yojson.Safe.Util.to_assoc (Yojson.Safe.from_file (file r "state/env.json"))
  in
  let value k =
    Option.map Yojson.Safe.Util.to_string (List.assoc_opt k env)
    |> Option.value ~default:"<unset>"
  in
  let prefix = Printf.sprintf "http://127.0.0.1:%d/r/" (P.port ctx.proxy) in
  let shim = value "BACTRIAN_MCP_URL" in
  Alcotest.(check bool)
    ("shim URL " ^ shim) true
    (String.starts_with ~prefix shim && String.ends_with ~suffix:"/mcp" shim);
  let web = value "BACTRIAN_MCP_HTTP_WEB_SEARCH" in
  Alcotest.(check bool)
    ("HTTP URL " ^ web) true
    (String.starts_with ~prefix web
    && String.ends_with ~suffix:"/mcp-http/web-search" web);
  Alcotest.(check string)
    "no URL for a stdio server" "<unset>"
    (value "BACTRIAN_MCP_HTTP_CALENDAR")

let test_environment =
  with_ctx "env" @@ fun ctx ->
  Unix.putenv "PARENT_ONLY_SECRET" "leak";
  Unix.putenv "PASSED_THROUGH" "yes";
  Unix.putenv "OPENAI_API_KEY" real_openai_key;
  let c =
    {
      (config ()) with
      env_passthrough = [ "PASSED_THROUGH"; "OPENAI_API_KEY" ];
    }
  in
  let r = ok (execute ctx c) in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  let env =
    Yojson.Safe.Util.to_assoc (Yojson.Safe.from_file (file r "state/env.json"))
  in
  let names = List.map fst env in
  Alcotest.(check bool)
    "parent secret absent" false
    (List.mem "PARENT_ONLY_SECRET" names);
  List.iter
    (fun n ->
      if not (List.mem n allowed) then Alcotest.failf "unexpected variable %s" n)
    names;
  (* The MCP variable only exists in a run that has MCP servers. *)
  List.iter
    (fun k ->
      Alcotest.(check bool) (k ^ " is set by bactrian") true (List.mem k names))
    (List.filter
       (fun k -> not (String.equal k Mcp_proxy.url_variable))
       Experiment.reserved_env);
  let value k = Yojson.Safe.Util.to_string (List.assoc k env) in
  Alcotest.(check string) "passthrough" "yes" (value "PASSED_THROUGH");
  Alcotest.(check string) "run id" r.run_id (value "BACTRIAN_RUN_ID");
  Alcotest.(check string) "TZ" "UTC" (value "TZ");
  Alcotest.(check string)
    "state dir" (file r "state")
    (value "BACTRIAN_STATE_DIR");
  List.iter
    (fun k ->
      let v = value k in
      Alcotest.(check bool)
        (k ^ " placeholder") true
        (String.starts_with ~prefix:"bactrian-" v && String.length v = 41))
    [
      "OPENAI_API_KEY";
      "ANTHROPIC_API_KEY";
      "GEMINI_API_KEY";
      "OPENROUTER_API_KEY";
    ];
  Alcotest.(check bool)
    "real key never passed" false
    (contains (read (file r "state/env.json")) real_openai_key);
  List.iter
    (fun (k, seg) ->
      Alcotest.(check bool)
        (k ^ " base url") true
        (contains (value k)
           (Printf.sprintf "/r/%s/%s"
              (String.sub (value "OPENAI_API_KEY") 9 32)
              seg)))
    [
      ("OPENAI_BASE_URL", "openai/v1");
      ("ANTHROPIC_BASE_URL", "anthropic");
      ("GOOGLE_GEMINI_BASE_URL", "gemini");
      ("OPENROUTER_BASE_URL", "openrouter/api/v1");
    ]

(* Replay *)

let strict =
  { Run.from = ""; matcher = Strict; allow_reorder = false; timing = P.Fast }

let test_replay_strict =
  with_ctx "replay" @@ fun ctx ->
  let recorded = record ctx in
  let upstream_before = List.length (F.requests ctx.up) in
  let r =
    ok
      (execute ctx
         (config ~turns:[] ~replay:{ strict with from = recorded.run_dir } ()))
  in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  Alcotest.(check int) "divergences" 0 (List.length r.findings.divergences);
  Alcotest.(check int) "served" 2 r.findings.exchanges;
  Alcotest.(check int)
    "upstream untouched" upstream_before
    (List.length (F.requests ctx.up));
  Alcotest.(check bool) "new run" true (r.run_id <> recorded.run_id);
  Alcotest.(check (float 0.)) "nothing spent" 0. r.spend;
  (* Entries are ordered by time, which differs between the runs. *)
  Alcotest.check strs "same trace kinds as the recording"
    (List.sort String.compare (trace_types recorded))
    (List.sort String.compare (trace_types r));
  Alcotest.check strs "same tool calls" (tool_names recorded) (tool_names r);
  Alcotest.(check string)
    "turns come from the recording" first_turn
    Yojson.Safe.Util.(
      to_string
        (member "content"
           (Yojson.Safe.from_string (List.nth (lines (file r "trace.jsonl")) 1))))

let test_replay_diverged =
  with_ctx "diverged" @@ fun ctx ->
  let recorded = record ctx in
  let changed =
    config
      ~agent:[ "python3"; "-I"; "-c"; python_agent; "You are terse." ]
      ~replay:{ strict with from = recorded.run_id }
      ~turns:[] ()
  in
  let r = ok (execute ctx changed) in
  Alcotest.(check string) "outcome" "diverged" (outcome_name r);
  Alcotest.(check bool)
    "has a missing request" true
    (List.exists
       (function
         | Matcher.Missing_request _ -> true
         | Unconsumed _ | Reordered _ | Shape_mismatch _ -> false)
       r.findings.divergences);
  Alcotest.(check string)
    "meta" "diverged"
    Yojson.Safe.Util.(
      to_string (member "outcome" (Yojson.Safe.from_file (file r "meta.json"))))

(* Failures *)

(* The command line replays what the library recorded. *)

let test_cli_replay =
  with_ctx "cli" @@ fun ctx ->
  let recorded = record ctx in
  let script = Filename.concat (Sys.getcwd ()) "cli-agent.py" in
  Out_channel.with_open_bin script (fun oc -> output_string oc python_agent);
  Fun.protect ~finally:(fun () -> Sys.remove script) @@ fun () ->
  let args extra =
    Printf.sprintf "replay %s --json -- python3 -I %s%s" recorded.run_id
      (Filename.quote script) extra
  in
  let code, text = cli ctx (args "") in
  if code <> 0 then Alcotest.failf "exit %d: %s" code text;
  let json = Yojson.Safe.from_string text in
  Alcotest.(check string)
    "outcome" "passed"
    Yojson.Safe.Util.(to_string (member "kind" (member "outcome" json)));
  Alcotest.(check int)
    "no divergences" 0
    Yojson.Safe.Util.(List.length (to_list (member "divergences" json)));
  let code, text = cli ctx (args " 'You are terse.'") in
  Alcotest.(check int) "diverged exit status" 1 code;
  let json = Yojson.Safe.from_string text in
  Alcotest.(check string)
    "diverged" "diverged"
    Yojson.Safe.Util.(to_string (member "kind" (member "outcome" json)));
  Alcotest.(check string)
    "missing request" "missing_request"
    Yojson.Safe.Util.(
      to_string (member "kind" (List.hd (to_list (member "divergences" json)))))

let test_agent_failure =
  with_ctx "failure" @@ fun ctx ->
  let r =
    ok
      (execute ctx
         (config ~agent:[ "python3"; "-I"; "-c"; "import sys; sys.exit(3)" ] ()))
  in
  match r.outcome with
  | Failed _ ->
      Alcotest.(check string) "kind" "failed" (Run.outcome_kind r.outcome)
  | Passed | Diverged | Errored _ | Not_run _ ->
      Alcotest.failf "expected Failed, got %s" (outcome_name r)

let test_agent_error_reply =
  with_ctx "error" @@ fun ctx ->
  let script =
    {|
import json, sys
for line in sys.stdin:
    m = json.loads(line)
    if m["type"] == "turn":
        print(json.dumps({"v": 1, "type": "error", "turn": m["turn"], "message": "nope"}), flush=True)
|}
  in
  let r =
    ok (execute ctx (config ~agent:[ "python3"; "-I"; "-c"; script ] ()))
  in
  Alcotest.(check string) "outcome" "failed: agent_error: nope" (outcome_name r);
  Alcotest.(check bool)
    "trace has the error" true
    (contains (read (file r "trace.jsonl")) {|"name":"error"|})

let test_cap_reached =
  with_ctx "cap" @@ fun ctx ->
  let r = ok (execute ~cap:0. ctx (config ())) in
  Alcotest.(check string) "kind" "not_run" (Run.outcome_kind r.outcome);
  Alcotest.(check int) "cap refusals" 1 (List.length r.findings.cap_refusals);
  Alcotest.(check int) "upstream untouched" 0 (List.length (F.requests ctx.up))

(* Timeouts and interrupts *)

let gone pid =
  match Unix.kill pid 0 with
  | () -> false
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> true

let wait_for ~what ?(limit = 15.) ?(on_timeout = ignore) pred =
  let t0 = Unix.gettimeofday () in
  let rec go () =
    match pred () with
    | Some v -> v
    | None ->
        if Unix.gettimeofday () -. t0 > limit then (
          on_timeout ();
          Alcotest.failf "timed out waiting for %s" what)
        else (
          Unix.sleepf 0.05;
          go ())
  in
  go ()

let read_pid path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> int_of_string_opt (String.trim text)
  | exception Sys_error _ -> None

(* An agent that writes its pid to the state directory, then hangs. With
   [stubborn] it also ignores SIGTERM. *)
let hung_agent ~stubborn =
  [
    "sh";
    "-c";
    Printf.sprintf
      {|echo $$ > "$BACTRIAN_STATE_DIR/pid.tmp" && mv "$BACTRIAN_STATE_DIR/pid.tmp" "$BACTRIAN_STATE_DIR/pid"; %swhile :; do sleep 1; done|}
      (if stubborn then {|trap "" TERM; |} else "");
  ]

let test_hung_agent_is_killed =
  with_ctx "hung" @@ fun ctx ->
  let turn_timeout = 1. in
  let t0 = Unix.gettimeofday () in
  let r =
    ok
      (execute ctx
         { (config ~agent:(hung_agent ~stubborn:true) ()) with turn_timeout })
  in
  let elapsed = Unix.gettimeofday () -. t0 in
  Alcotest.(check string) "outcome" "failed: timeout: turn" (outcome_name r);
  Alcotest.(check bool)
    (Printf.sprintf "gone within turn_timeout + 6 s (took %.1f s)" elapsed)
    true
    (elapsed < turn_timeout +. 6.);
  match read_pid (Filename.concat (file r "state") "pid") with
  | Some pid -> Alcotest.(check bool) "agent group gone" true (gone pid)
  | None -> Alcotest.fail "the agent never wrote its pid"

let test_interrupt_helper =
  Alcotest.test_case "a signal cancels the function and runs its cleanup" `Quick
  @@ fun () ->
  Eio_main.run @@ fun _env ->
  let cleaned = ref false in
  let result =
    Interrupt.run (fun () ->
        Fun.protect ~finally:(fun () -> cleaned := true) @@ fun () ->
        Unix.kill (Unix.getpid ()) Sys.sigterm;
        Eio.Fiber.await_cancel ())
  in
  Alcotest.(check (result unit int)) "interrupted" (Error Sys.sigterm) result;
  Alcotest.(check bool) "cleanup ran" true !cleaned;
  Alcotest.(check (result int int))
    "no signal" (Ok 3)
    (Interrupt.run (fun () -> 3))

(* SIGINT to a replaying bactrian: the agent's group is killed, the cassette is
   left partial, meta.json says interrupted and the status is 130. *)
let test_cli_interrupt =
  with_ctx "interrupt" @@ fun ctx ->
  let recorded = record ctx in
  let root = Filename.concat (Eio.Path.native_exn ctx.dir) ".bactrian" in
  let runs = Filename.concat root "runs" in
  let cwd = Filename.concat (Sys.getcwd ()) (Eio.Path.native_exn ctx.dir) in
  let agent =
    String.concat " " (List.map Filename.quote (hung_agent ~stubborn:false))
  in
  let stderr_file = Filename.concat cwd "interrupt-stderr.txt" in
  let pid =
    let devnull = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
    let err = Unix.openfile stderr_file [ Unix.O_WRONLY; Unix.O_CREAT ] 0o600 in
    Fun.protect ~finally:(fun () ->
        Unix.close devnull;
        Unix.close err)
    @@ fun () ->
    let argv =
      Array.of_list
        [
          "sh";
          "-c";
          Printf.sprintf "cd %s && exec %s replay %s -- %s" (Filename.quote cwd)
            (Filename.quote bactrian_bin)
            recorded.run_id agent;
        ]
    in
    Unix.create_process "sh" argv devnull devnull err
  in
  let new_run =
    wait_for ~what:"the replaying agent to start" (fun () ->
        Sys.readdir runs |> Array.to_list
        |> List.find_opt (fun id ->
            (not (String.equal id recorded.run_id))
            && Option.is_some
                 (read_pid (String.concat "/" [ runs; id; "state"; "pid" ]))))
  in
  let agent_pid =
    Option.get (read_pid (String.concat "/" [ runs; new_run; "state"; "pid" ]))
  in
  Unix.kill pid Sys.sigint;
  let _, status =
    wait_for ~what:"bactrian to exit"
      ~on_timeout:(fun () ->
        let text = read stderr_file in
        Unix.kill pid Sys.sigkill;
        Alcotest.failf "bactrian stderr: %s" text)
      (fun () ->
        match Unix.waitpid [ Unix.WNOHANG ] pid with
        | 0, _ -> None
        | r -> Some r)
  in
  (match status with
  | Unix.WEXITED n ->
      if n <> 130 then Alcotest.failf "exit %d: %s" n (read stderr_file)
  | Unix.WSIGNALED n | Unix.WSTOPPED n -> Alcotest.failf "signal %d" n);
  wait_for ~what:"the agent group to be gone" ~limit:6. (fun () ->
      if gone agent_pid then Some () else None);
  let run_dir = Filename.concat runs new_run in
  let meta = Yojson.Safe.from_file (Filename.concat run_dir "meta.json") in
  Alcotest.(check string)
    "outcome" "errored"
    Yojson.Safe.Util.(to_string (member "outcome" meta));
  Alcotest.(check string)
    "reason" "interrupted"
    Yojson.Safe.Util.(to_string (member "reason" meta));
  let cassette = read (Filename.concat run_dir "cassette.jsonl") in
  Alcotest.(check bool)
    "cassette is partial" false
    (contains cassette Cassette.end_line)

let replace_all ~sub ~by text =
  Re.replace_string (Re.compile (Re.str sub)) ~by text

let test_unknown_model_errors =
  with_ctx "unpriced" @@ fun ctx ->
  let script = replace_all ~sub:"gpt-6-luna" ~by:"no-such-model" python_agent in
  let r =
    ok (execute ctx (config ~agent:[ "python3"; "-I"; "-c"; script ] ()))
  in
  Alcotest.(check string)
    "outcome" "errored: no price for model no-such-model" (outcome_name r)

let events_agent turn =
  Printf.sprintf
    {|
import json, sys
for line in sys.stdin:
    m = json.loads(line)
    if m["type"] == "turn":
        print(json.dumps({"v": 1, "type": "event", "turn": %d, "name": "x", "data": {}}), flush=True)
        print(json.dumps({"v": 1, "type": "reply", "turn": m["turn"], "content": "ok"}), flush=True)
|}
    turn

let test_event_for_another_turn =
  with_ctx "event-turn" @@ fun ctx ->
  let run turn =
    outcome_name
      (ok
         (execute ctx
            (config ~agent:[ "python3"; "-I"; "-c"; events_agent turn ] ())))
  in
  Alcotest.(check string) "own turn" "passed" (run 1);
  Alcotest.(check string)
    "other turn" "failed: protocol_violation: event for turn 2, expected 1"
    (run 2)

(* An agent that never reads stdin: the turn does not fit the pipe, so the
   write blocks until the total timeout. *)
let test_blocked_write =
  with_ctx "blocked" @@ fun ctx ->
  let t0 = Unix.gettimeofday () in
  let r =
    ok
      (execute ctx
         {
           (config ~agent:[ "sleep"; "60" ]
              ~turns:[ String.make 1_000_000 'x' ]
              ())
           with
           total_timeout = 2.;
         })
  in
  Alcotest.(check string) "outcome" "failed: timeout: total" (outcome_name r);
  Alcotest.(check bool) "gone promptly" true (Unix.gettimeofday () -. t0 < 9.)

let test_unknown_recording =
  with_ctx "missing" @@ fun ctx ->
  match
    execute ctx
      (config ~turns:[] ~replay:{ strict with from = "no-such-run" } ())
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected a setup error"

(* Settings from the experiment *)

let fixtures_agent =
  {|
import os
d = os.environ["BACTRIAN_STATE_DIR"]
with open(os.path.join(d, "sub", "data.txt")) as f:
    seen = f.read()
with open(os.path.join(d, "seen.txt"), "w") as f:
    f.write("saw:" + seen)
|}
  ^ python_agent

let test_fixtures =
  with_ctx "fixtures" @@ fun ctx ->
  let src = Eio.Path.(ctx.dir / "fx") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(src / "sub");
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(src / "sub" / "data.txt")
    "hello";
  Eio.Path.save ~create:(`Or_truncate 0o755)
    Eio.Path.(src / "run.sh")
    "#!/bin/sh\n";
  let c =
    {
      (config ~agent:[ "python3"; "-I"; "-c"; fixtures_agent ] ()) with
      fixtures = Some (Filename.concat (snd ctx.dir) "fx");
    }
  in
  let r = ok (execute ctx c) in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  Alcotest.(check string)
    "the agent saw the nested fixture" "saw:hello"
    (read (file r "state/seen.txt"));
  Alcotest.(check string) "copied" "hello" (read (file r "state/sub/data.txt"));
  Alcotest.(check bool)
    "mode kept" true
    (not (Int.equal ((Unix.stat (file r "state/run.sh")).st_perm land 0o100) 0))

let test_normalise_counted =
  with_ctx "normalise" @@ fun ctx ->
  let r = ok (execute ctx { (config ()) with normalise = [ "Paris" ] }) in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  Alcotest.(check bool) "masks in findings" true (r.findings.masks_fired > 0);
  let meta = Yojson.Safe.from_file (file r "meta.json") in
  Alcotest.(check int)
    "masks in meta.json" r.findings.masks_fired
    Yojson.Safe.Util.(
      to_int (member "normalise_masks" (member "findings" meta)));
  Alcotest.(check bool)
    "bad pattern is a setup error" true
    (Result.is_error (execute ctx { (config ()) with normalise = [ "(" ] }))

let tenfold =
  let e = Option.get (Pricing.find ~model:"gpt-6-luna" ~date:"2026-10-13") in
  Pricing.with_overrides
    [
      {
        e with
        rates =
          {
            e.rates with
            input = e.rates.input *. 10.;
            cached_input = e.rates.cached_input *. 10.;
            output = e.rates.output *. 10.;
          };
      };
    ]

let test_pricing_override =
  with_ctx "pricing" @@ fun ctx ->
  let spent (r : Run.result) = r.spend in
  let base = ok (execute ctx (config ())) in
  let dear = ok (execute ctx { (config ()) with pricing = tenfold }) in
  Alcotest.(check bool)
    "the override changes the recorded spend" true
    (Float.abs (spent dear -. (10. *. spent base)) < 1e-9 *. spent dear);
  let meta = Yojson.Safe.from_file (file dear "meta.json") in
  Alcotest.(check (float 1e-12))
    "meta.json agrees" (spent dear)
    Yojson.Safe.Util.(to_float (member "cost_usd" meta))

let () =
  Alcotest.run "run"
    [
      ( "record",
        [
          Alcotest.test_case "records and derives the trace" `Slow test_record;
          Alcotest.test_case "environment is the allowlist" `Slow
            test_environment;
          Alcotest.test_case "MCP variables" `Slow test_mcp_environment;
        ] );
      ( "replay",
        [
          Alcotest.test_case "strict replay is clean" `Slow test_replay_strict;
          Alcotest.test_case "changed request diverges" `Slow
            test_replay_diverged;
          Alcotest.test_case "unknown recording" `Quick test_unknown_recording;
          Alcotest.test_case "command line replay" `Slow test_cli_replay;
        ] );
      ( "settings",
        [
          Alcotest.test_case "fixtures are copied into the state directory"
            `Slow test_fixtures;
          Alcotest.test_case "normalise masks are counted" `Slow
            test_normalise_counted;
          Alcotest.test_case "pricing override changes the spend" `Slow
            test_pricing_override;
        ] );
      ( "failures",
        [
          Alcotest.test_case "agent exits non-zero" `Slow test_agent_failure;
          Alcotest.test_case "agent reports an error" `Slow
            test_agent_error_reply;
          Alcotest.test_case "cap reached" `Slow test_cap_reached;
          Alcotest.test_case "unpriced model is an error" `Slow
            test_unknown_model_errors;
          Alcotest.test_case "event for another turn" `Slow
            test_event_for_another_turn;
          Alcotest.test_case "write blocked by a deaf agent" `Slow
            test_blocked_write;
        ] );
      ( "teardown",
        [
          Alcotest.test_case "a hung agent is gone after the turn timeout" `Slow
            test_hung_agent_is_killed;
          test_interrupt_helper;
          Alcotest.test_case "SIGINT tears the run down" `Slow
            test_cli_interrupt;
        ] );
    ]
