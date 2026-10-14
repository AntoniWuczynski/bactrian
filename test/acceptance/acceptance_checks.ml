(* The checks the acceptance tests run on an example agent's cycle and on the
   escape agents, in any sandbox. *)

open Bactrian_core
open Bactrian_runtime
open Acceptance_world

(* Record, strict replay and a changed system prompt: the recording replays
   with no divergence and the change diverges at the system message. *)
let cycle (w : world) argv =
  let up = w.up in
  let execute = w.execute ~agent:(Command argv) in
  (* (a) record *)
  let recorded = execute () in
  Alcotest.(check string) "record outcome" "passed" (outcome_name recorded);
  check_sandboxed w recorded;
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
  check_sandboxed w replayed;
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

(* An escape agent's attempts (see test_acceptance.ml): the model call and the
   state write work, everything else fails. *)

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

let escapes ?vm ~name argv_of =
  let home =
    match Sys.getenv_opt "HOME" with
    | Some h -> h
    | None -> Alcotest.fail "HOME"
  in
  let home_file =
    Filename.concat home
      (Printf.sprintf ".bactrian-escape-%s-%d" name (Unix.getpid ()))
  in
  let previous = Sys.getenv_opt "OPENAI_API_KEY" in
  (* The key sits in the environment bactrian runs in, as it would for a
     live run, and must not be passed on. *)
  Unix.putenv "OPENAI_API_KEY" real_key;
  Fun.protect ~finally:(fun () ->
      Unix.putenv "OPENAI_API_KEY" (Option.value previous ~default:"");
      if Sys.file_exists home_file then Sys.remove home_file)
  @@ fun () ->
  with_world ?vm ~deny_read:[ "secret" ] ("escape-" ^ name) @@ fun w ->
  let up = w.up and execute = w.execute and dir = w.dir in
  let secret_file = Filename.concat (Filename.concat dir "secret") "key" in
  Out_channel.with_open_bin secret_file (fun oc -> output_string oc "s3cret");
  let project_file = Filename.concat dir "escaped" in
  let r =
    execute
      ~agent:(Command (argv_of ~dir))
      ~variant_env:
        [
          ("REAL_HOME_FILE", home_file);
          ("PROJECT_FILE", project_file);
          ("SECRET_FILE", secret_file);
        ]
      ()
  in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  check_sandboxed w r;
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
