(* Experiments whose agent is an HTTP server (design §5.1, §6). *)

open Executor_support
module H = Http_agent

let namespace_header = "X-Agent-Run"

let model_request =
  In_channel.with_open_bin
    (fixtures_root ^ "/openai_chat/luna_nonstream/request.json")
    In_channel.input_all

let experiment ?(extra = "") ?(agent_extra = "") url =
  Printf.sprintf
    {|{"version": 1, "name": "demo",
 "agent": {"protocol": "http", "url": %S, "namespace_header": %S%s},
 "scenarios": ["scenarios/*.json"],
 "variants": {"a": {}, "b": {}},
 "repeats": 2, "concurrency": 3, "caps_usd": {"openai": 100}, "seed": 3%s}|}
    url namespace_header agent_extra extra

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
  let checker_labels = ref [] in
  (* The mode is [auto] and the executable the probe would run does not exist,
     so a preflight would have warned. *)
  let s =
    ok
      (go ~sandbox:(Some `Auto)
         ~warn:(fun w -> warnings := w :: !warnings)
         ~on_checker_sandbox:(fun l -> checker_labels := l :: !checker_labels)
         ctx)
  in
  Alcotest.(check int)
    "no checkers, no checker label" 0
    (List.length !checker_labels);
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
  let checker_labels = ref [] in
  let s =
    ok
      (go ~sandbox:(Some `Auto)
         ~warn:(fun w -> warnings := w :: !warnings)
         ~on_checker_sandbox:(fun l -> checker_labels := l :: !checker_labels)
         ctx)
  in
  Alcotest.(check (list string))
    "all passed"
    (List.init 4 (fun _ -> "passed"))
    (kinds s);
  (match !checker_labels with
  | [ { mode = `Auto; applied = false; reason = Some reason; _ } ] ->
      Alcotest.(check bool)
        "reported once, with the checkers' own reason" true
        (String.starts_with ~prefix:"unavailable: " reason)
  | [] | _ :: _ -> Alcotest.fail "expected one unapplied checker label");
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

let mentions needle m =
  Alcotest.(check bool)
    (Printf.sprintf "%S mentions %S" m needle)
    true
    (Re.execp (Re.compile (Re.str needle)) m)

let real_keys_warnings ws =
  List.filter (String.starts_with ~prefix:"agent.real_keys") ws

(* [agent.real_keys]: the agent is not sandboxed, and the keys reach it in the
   [start] message. On this machine that is the one warning. *)
let test_real_keys =
  setup "http-real-keys" @@ fun ctx ->
  Eio.Switch.run @@ fun sw ->
  let agent =
    H.start ~sw ~net:(Eio.Stdenv.net ctx.env) ~clock:(Eio.Stdenv.clock ctx.env)
      ~namespace_header ~start_delay:0. ~model_request
  in
  write ctx "exp.json"
    (experiment ~agent_extra:{|, "real_keys": true|} (H.url agent));
  let warnings = ref [] in
  let s = ok (go ~warn:(fun w -> warnings := w :: !warnings) ctx) in
  Alcotest.(check (list string))
    "all passed"
    (List.init 4 (fun _ -> "passed"))
    (kinds s);
  match real_keys_warnings !warnings with
  | [ w ] ->
      mentions "OPENAI_API_KEY" w;
      mentions "Nothing stops it" w
  | l -> Alcotest.failf "expected one warning, got %d" (List.length l)

(* An agent on another machine over plain http gets the keys in the clear: the
   runs fail to reach it, but the warning comes first. *)
let test_real_keys_cleartext =
  setup "http-real-keys-cleartext" @@ fun ctx ->
  write ctx "exp.json"
    (experiment ~agent_extra:{|, "real_keys": true|}
       "http://user:secret@agent.invalid:9/a");
  let warnings = ref [] in
  ignore (go ~warn:(fun w -> warnings := w :: !warnings) ctx);
  let unencrypted =
    List.filter
      (fun w -> Re.execp (Re.compile (Re.str "unencrypted")) w)
      (real_keys_warnings !warnings)
  in
  match unencrypted with
  | [ w ] ->
      mentions "agent.invalid" w;
      Alcotest.(check bool)
        "the credentials are not shown" false
        (Re.execp (Re.compile (Re.str "secret")) w)
  | l -> Alcotest.failf "expected one cleartext warning, got %d" (List.length l)

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
          Alcotest.test_case "real keys" `Slow test_real_keys;
          Alcotest.test_case "real keys in the clear" `Slow
            test_real_keys_cleartext;
          Alcotest.test_case "sandbox strict refused" `Quick
            test_strict_sandbox_refused;
          Alcotest.test_case "--sandbox strict refused" `Quick
            test_strict_override_refused;
          Alcotest.test_case "https refused" `Quick test_https_refused;
        ] );
    ]
