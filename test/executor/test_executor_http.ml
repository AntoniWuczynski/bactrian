(* Experiments whose agent is an HTTP server (design §5.1, §6). *)

open Executor_support
module H = Http_agent

let namespace_header = "X-Agent-Run"

let model_request =
  In_channel.with_open_bin
    (fixtures_root ^ "/openai_chat/luna_nonstream/request.json")
    In_channel.input_all

let experiment ?(extra = "") url =
  Printf.sprintf
    {|{"version": 1, "name": "demo",
 "agent": {"protocol": "http", "url": %S, "namespace_header": %S},
 "scenarios": ["scenarios/*.json"],
 "variants": {"a": {}, "b": {}},
 "repeats": 2, "concurrency": 3, "caps_usd": {"openai": 100}, "seed": 3%s}|}
    url namespace_header extra

let setup name f =
  with_ctx name ~scenarios:[ "s1" ] (fun ctx ->
      write ctx "scenarios/s1.json" (scenario_json ~assertions:"[]" "s1");
      f ctx)

(* Four runs, three at a time, against one server. *)
let test_experiment_runs =
  setup "http-run" @@ fun ctx ->
  Eio.Switch.run @@ fun sw ->
  let agent =
    H.start ~sw ~net:(Eio.Stdenv.net ctx.env) ~clock:(Eio.Stdenv.clock ctx.env)
      ~namespace_header ~start_delay:0.2 ~model_request
  in
  write ctx "exp.json" (experiment (H.url agent));
  let warnings = ref [] in
  (* The mode is [auto] and the executable the probe would run does not exist,
     so a preflight would have warned. *)
  let s =
    ok
      (go ~sandbox:(Some `Auto) ~warn:(fun w -> warnings := w :: !warnings) ctx)
  in
  Alcotest.(check (list string))
    "all passed"
    (List.init 4 (fun _ -> "passed"))
    (kinds s);
  Alcotest.(check bool)
    "the lack of isolation is warned about" true
    (List.exists (String.starts_with ~prefix:"agent: protocol http") !warnings);
  Alcotest.(check bool)
    "no sandbox preflight" false
    (List.exists (String.starts_with ~prefix:"sandbox") !warnings);
  let ids = run_ids s in
  Alcotest.(check (list string))
    "one session per run"
    (List.sort String.compare ids)
    (List.sort String.compare
       (List.map (fun (x : H.session) -> x.run_id) (H.sessions agent)));
  List.iter
    (fun id ->
      let meta = run_meta ctx id in
      Alcotest.(check string)
        "unsandboxed because of the mode"
        {|{"mode":"auto","applied":false,"reason":"http mode"}|}
        (Yojson.Safe.to_string (member "sandbox" meta));
      Alcotest.(check bool)
        "warning recorded in meta.json" true
        (List.exists
           (fun w ->
             String.starts_with ~prefix:"agent: protocol http"
               (Yojson.Safe.Util.to_string w))
           Yojson.Safe.Util.(to_list (member "unenforced" meta))))
    ids

(* bactrian starts the checkers, so they get the sandbox the agent cannot: the
   probe runs for them. Here it fails (the executable does not exist), and
   under [auto] that is a warning and a label in verdicts.json. *)
let test_checkers_probed =
  setup "http-checkers" @@ fun ctx ->
  write ctx "scenarios/s1.json"
    (scenario_json ~assertions:"[]"
       ~checkers:
         {|[{"command": ["python3", "-I", "-c", "import json; print(json.dumps({'pass': True}))"]}]|}
       "s1");
  Eio.Switch.run @@ fun sw ->
  let agent =
    H.start ~sw ~net:(Eio.Stdenv.net ctx.env) ~clock:(Eio.Stdenv.clock ctx.env)
      ~namespace_header ~start_delay:0. ~model_request
  in
  write ctx "exp.json" (experiment (H.url agent));
  let warnings = ref [] in
  let s =
    ok
      (go ~sandbox:(Some `Auto) ~warn:(fun w -> warnings := w :: !warnings) ctx)
  in
  Alcotest.(check (list string))
    "all passed"
    (List.init 4 (fun _ -> "passed"))
    (kinds s);
  Alcotest.(check bool)
    "the probe ran and warned" true
    (List.exists (String.starts_with ~prefix:"sandbox: auto") !warnings);
  List.iter
    (fun id ->
      let label =
        member "checker_sandbox"
          (Yojson.Safe.from_string
             (read ctx (Printf.sprintf ".bactrian/runs/%s/verdicts.json" id)))
      in
      Alcotest.(check string) "mode" "auto" (str label "mode");
      Alcotest.(check bool)
        "the checkers' own reason" true
        (String.starts_with ~prefix:"unavailable: " (str label "reason"));
      Alcotest.(check string)
        "the agent keeps its label"
        {|{"mode":"auto","applied":false,"reason":"http mode"}|}
        (Yojson.Safe.to_string (member "sandbox" (run_meta ctx id))))
    (run_ids s)

let test_strict_sandbox_refused =
  setup "http-strict" @@ fun ctx ->
  write ctx "exp.json"
    (experiment ~extra:{|, "sandbox": "strict"|} "http://127.0.0.1:9/a");
  match go ctx with
  | Error (E.Configuration [ e ]) ->
      Alcotest.(check string) "path" "sandbox" e.path;
      Alcotest.(check bool) "located in the file" true (e.line > 1)
  | Ok _ | Error (E.Configuration _ | E.Environment _ | E.Usage _) ->
      Alcotest.fail "expected one configuration error"

let test_strict_override_refused =
  setup "http-strict-flag" @@ fun ctx ->
  write ctx "exp.json" (experiment "http://127.0.0.1:9/a");
  match go ~sandbox:(Some `Strict) ctx with
  | Error (E.Configuration [ e ]) ->
      Alcotest.(check string) "path" "agent.protocol" e.path;
      Alcotest.(check bool) "located in the file" true (e.line > 1)
  | Ok _ | Error (E.Configuration _ | E.Environment _ | E.Usage _) ->
      Alcotest.fail "expected one configuration error"

let test_https_refused =
  setup "http-tls" @@ fun ctx ->
  write ctx "exp.json" (experiment "https://agent.example.com/a");
  match go ctx with
  | Error (E.Environment m) ->
      Alcotest.(check bool)
        "names the missing TLS support" true
        (String.starts_with ~prefix:"this build has no TLS support" m)
  | Ok _ | Error (E.Configuration _ | E.Usage _) ->
      Alcotest.fail "expected Environment"

let () =
  Alcotest.run "executor_http"
    [
      ( "http agent",
        [
          Alcotest.test_case "experiment runs" `Slow test_experiment_runs;
          Alcotest.test_case "checkers are probed for" `Slow
            test_checkers_probed;
          Alcotest.test_case "sandbox strict refused" `Quick
            test_strict_sandbox_refused;
          Alcotest.test_case "--sandbox strict refused" `Quick
            test_strict_override_refused;
          Alcotest.test_case "https refused" `Quick test_https_refused;
        ] );
    ]
