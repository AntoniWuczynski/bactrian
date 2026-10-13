open Bactrian_core
open Bactrian_runtime
module P = Model_proxy
module F = Fake_upstream

let () = Mirage_crypto_rng_unix.use_default ()
let fixtures_root = "../fixtures/protocols"
let real_openai_key = "real-openai-key"

let caps value =
  [
    ("openai", value);
    ("anthropic", value);
    ("gemini", value);
    ("openrouter", value);
  ]

let python_agent =
  {|
import json, os, sys, urllib.request

base = os.environ["OPENAI_BASE_URL"]
key = os.environ["OPENAI_API_KEY"]
system = sys.argv[1] if len(sys.argv) > 1 else None
TOOLS = [{"type": "function", "function": {"name": "get_weather", "description": "Get the current weather for a city.", "parameters": {"type": "object", "properties": {"city": {"type": "string", "description": "City name"}}, "required": ["city"]}}}]

def send(m):
    sys.stdout.write(json.dumps(m) + "\n")
    sys.stdout.flush()

def chat(messages):
    body = {"model": "gpt-6-luna", "messages": messages, "tools": TOOLS, "max_completion_tokens": 400, "reasoning_effort": "none"}
    req = urllib.request.Request(base + "/chat/completions", data=json.dumps(body).encode(), headers={"Content-Type": "application/json", "Authorization": "Bearer " + key})
    with urllib.request.urlopen(req) as r:
        return json.load(r)["choices"][0]["message"]

for line in sys.stdin:
    m = json.loads(line)
    if m["type"] == "start":
        with open(os.path.join(m["state_dir"], "env.json"), "w") as f:
            json.dump(dict(os.environ), f)
    elif m["type"] == "turn":
        try:
            messages = [{"role": "user", "content": m["content"]}]
            if system:
                messages.insert(0, {"role": "system", "content": system})
            msg = chat(messages)
            while msg.get("tool_calls"):
                messages.append({"role": "assistant", "content": None, "tool_calls": msg["tool_calls"]})
                for call in msg["tool_calls"]:
                    send({"v": 1, "type": "event", "turn": m["turn"], "name": "tool", "data": {"name": call["function"]["name"]}})
                    messages.append({"role": "tool", "tool_call_id": call["id"], "content": "18 degrees Celsius, cloudy"})
                msg = chat(messages)
            send({"v": 1, "type": "reply", "turn": m["turn"], "content": msg["content"]})
        except Exception as e:
            send({"v": 1, "type": "error", "turn": m["turn"], "message": str(e)})
    elif m["type"] == "end":
        break
|}

let first_turn =
  "What is the weather in Paris right now? Use the get_weather tool."

(* Context *)

type ctx = {
  env : Eio_unix.Stdenv.base;
  proxy : P.t;
  store : Store.t;
  dir : Eio.Fs.dir_ty Eio.Path.t;
  up : F.t;
}

let counter = ref 0

let random_bytes n =
  incr counter;
  String.init n (fun i -> Char.chr ((!counter + (i * 7)) land 255))

let with_ctx name f () =
  Eio_main.run @@ fun env ->
  let path = "tmp-run-" ^ name in
  let dir = Eio.Path.(Eio.Stdenv.cwd env / path) in
  Eio.Path.rmtree ~missing_ok:true dir;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let proxy =
    match
      P.start ~sw ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
    with
    | Ok p -> p
    | Error e -> Alcotest.fail e
  in
  let up =
    match F.start ~sw ~net:(Eio.Stdenv.net env) (F.Fixtures fixtures_root) with
    | Ok u -> u
    | Error e -> Alcotest.failf "fake upstream: %a" F.pp_error e
  in
  f
    {
      env;
      proxy;
      store = Store.create ~root:Eio.Path.(dir / ".bactrian");
      dir;
      up;
    }

let config ?(agent = [ "python3"; "-I"; "-c"; python_agent ]) ?replay
    ?(turns = [ first_turn ]) () : Run.config =
  {
    agent;
    cwd = ".";
    turns;
    scenario = "weather";
    variant = "base";
    sim_now = "";
    timezone = "UTC";
    replay;
    env_passthrough = [];
    turn_timeout = 20.;
    total_timeout = 30.;
  }

let execute ?random_bytes:(rb = random_bytes) ?(cap = 100.) ctx config =
  let ledger =
    Ledger.create
      ~path:Eio.Path.(ctx.dir / ("ledger-" ^ string_of_int !counter ^ ".jsonl"))
      ~caps:(caps cap)
  in
  Run.execute ~random_bytes:rb
    ~keys:[ (P.Openai, real_openai_key) ]
    ~upstream:(fun _ -> Uri.of_string (F.base_url ctx.up))
    ~env:ctx.env ~proxy:ctx.proxy ~store:ctx.store ~ledger config

let ok = function Ok v -> v | Error e -> Alcotest.failf "execute: %s" e

let outcome_name (r : Run.result) =
  match r.outcome with
  | Passed -> "passed"
  | Failed s -> "failed: " ^ s
  | Diverged -> "diverged"
  | Errored s -> "errored: " ^ s
  | Not_run s -> "not_run: " ^ s

let read path = In_channel.with_open_bin path In_channel.input_all

let lines path =
  String.split_on_char '\n' (read path) |> List.filter (( <> ) "")

let file (r : Run.result) name = Filename.concat r.run_dir name

let trace_types (r : Run.result) =
  List.map
    (fun l ->
      Yojson.Safe.Util.(to_string (member "type" (Yojson.Safe.from_string l))))
    (lines (file r "trace.jsonl"))

let tool_names (r : Run.result) =
  List.filter_map
    (fun l ->
      let j = Yojson.Safe.from_string l in
      if String.equal Yojson.Safe.Util.(to_string (member "type" j)) "tool_call"
      then Some Yojson.Safe.Util.(to_string (member "name" j))
      else None)
    (lines (file r "trace.jsonl"))

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let strs = Alcotest.(list string)
let record ctx = ok (execute ctx (config ()))

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
    "meta spend" true
    (Yojson.Safe.Util.(to_float (member "openai" (member "spend_usd" meta)))
    > 0.);
  Alcotest.(check bool) "spend in result" true (List.mem_assoc "openai" r.spend);
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
  Alcotest.(check bool) "nothing spent" true (r.spend = []);
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

let bactrian_bin = Filename.concat (Sys.getcwd ()) (Sys.getenv "BACTRIAN_BIN")

let cli ctx args =
  let out = Filename.concat (Sys.getcwd ()) "cli-out.txt" in
  let cmd =
    Printf.sprintf "cd %s && %s %s > %s 2>&1"
      (Filename.quote
         (Filename.concat (Sys.getcwd ()) (Eio.Path.native_exn ctx.dir)))
      (Filename.quote bactrian_bin)
      args (Filename.quote out)
  in
  let code = Sys.command cmd in
  let text = read out in
  Sys.remove out;
  (code, text)

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

let test_unknown_recording =
  with_ctx "missing" @@ fun ctx ->
  match
    execute ctx
      (config ~turns:[] ~replay:{ strict with from = "no-such-run" } ())
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected a setup error"

let () =
  Alcotest.run "run"
    [
      ( "record",
        [
          Alcotest.test_case "records and derives the trace" `Slow test_record;
          Alcotest.test_case "environment is the allowlist" `Slow
            test_environment;
        ] );
      ( "replay",
        [
          Alcotest.test_case "strict replay is clean" `Slow test_replay_strict;
          Alcotest.test_case "changed request diverges" `Slow
            test_replay_diverged;
          Alcotest.test_case "unknown recording" `Quick test_unknown_recording;
          Alcotest.test_case "command line replay" `Slow test_cli_replay;
        ] );
      ( "failures",
        [
          Alcotest.test_case "agent exits non-zero" `Slow test_agent_failure;
          Alcotest.test_case "agent reports an error" `Slow
            test_agent_error_reply;
          Alcotest.test_case "cap reached" `Slow test_cap_reached;
        ] );
    ]
