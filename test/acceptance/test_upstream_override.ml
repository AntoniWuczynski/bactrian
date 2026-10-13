(* BACTRIAN_UPSTREAM_<PROVIDER> from the outside: the real binary runs
   experiments with the real example agents, and the provider it reaches is a
   fake upstream in a process of its own, so nothing leaves this machine. *)

open Bactrian_core
open Acceptance_world
open Cli_world

let variable = "BACTRIAN_UPSTREAM_OPENAI"
let env_of up = (variable, upstream_url up) :: key_env
let run_flags = [ "run"; "exp.json"; "--sandbox"; "off"; "--json" ]

let runs j =
  match Canon.list_member "runs" j with
  | Some l -> l
  | None -> Alcotest.failf "no runs in %s" (Canon.to_string j)

let statuses j = List.map (string_field "status") (runs j)
let all_passed = List.init 4 (fun _ -> "passed")

let meta_of project run =
  canon_of_file
    (Filename.concat project (Printf.sprintf ".bactrian/runs/%s/meta.json" run))

let completions = List.init 8 (fun _ -> "/v1/chat/completions")

(* Four runs, two turns of the conversation each, all through the override. *)
let through_the_override agent () =
  let argv = agent.argv () in
  with_project ("override-run-" ^ agent.name) @@ fun ~parent:_ ~project ->
  with_upstream ~dir:project @@ fun up ->
  write_experiment project argv;
  let code, printed = cli ~dir:project ~env:(env_of up) run_flags in
  Alcotest.(check int) "exit status" 0 code;
  Alcotest.(check (list string))
    "every run passes" all_passed (statuses printed);
  Alcotest.(check (list string))
    "the fake upstream answered every model call" completions
    (upstream_requests up);
  List.iter
    (fun r ->
      let meta = meta_of project (string_field "run_id" r) in
      Alcotest.check json "meta.json labels the upstream"
        (obj [ ("openai", Canon.string (upstream_url up)) ])
        (field "upstreams" meta);
      Alcotest.(check bool)
        "the upstream is a plain address, with no credentials" false
        (contains (Canon.to_string (field "upstreams" meta)) "@"))
    (runs printed)

(* A run that calls nobody is not labelled with an upstream. *)
let strict_replay_has_no_label () =
  let agent = List.hd agents in
  with_project "override-replay" @@ fun ~parent:_ ~project ->
  with_upstream ~dir:project @@ fun up ->
  write_experiment project (agent.argv ());
  let _, printed = cli ~dir:project ~env:(env_of up) run_flags in
  let run = string_field "run_id" (List.hd (runs printed)) in
  let before = List.length (upstream_requests up) in
  let code, replayed =
    cli ~dir:project ~env:(env_of up)
      ([ "replay"; run; "--json"; "--sandbox"; "off"; "--" ] @ agent.argv ())
  in
  ignore (code : int);
  Alcotest.(check int)
    "a strict replay does not call the upstream" before
    (List.length (upstream_requests up));
  Alcotest.check json "and its meta.json names none" Canon.empty_object
    (field "upstreams" (meta_of project (string_field "run_id" replayed)))

(* https needs TLS, but only for a provider the run calls. *)

let https_url = "https://gateway.invalid"

let https_is_refused_without_tls () =
  if Bactrian_app.Features.tls_available then Alcotest.skip ();
  let agent = List.hd agents in
  with_project "override-https" @@ fun ~parent:_ ~project ->
  with_upstream ~dir:project @@ fun up ->
  write_experiment project (agent.argv ());
  let code, printed =
    cli ~dir:project ~env:((variable, https_url) :: key_env) run_flags
  in
  Alcotest.(check int) "69, the environment is not ready" 69 code;
  Alcotest.(check bool)
    "the message names the missing TLS and the provider" true
    (let message = string_field "error" printed in
     contains message "no TLS support" && contains message "openai");
  Alcotest.(check (list string)) "nothing was called" [] (upstream_requests up);
  (* An https upstream for a provider the experiment does not call. *)
  let code, printed =
    cli ~dir:project
      ~env:(("BACTRIAN_UPSTREAM_ANTHROPIC", https_url) :: env_of up)
      run_flags
  in
  Alcotest.(check int) "an uncalled provider's https upstream is fine" 0 code;
  Alcotest.(check (list string)) "all pass" all_passed (statuses printed);
  (* record is live too. *)
  let code, printed =
    cli ~dir:project
      ~env:(("BACTRIAN_CAP_OPENAI", "1") :: (variable, https_url) :: key_env)
      [ "record"; "--json"; "--turn"; "hi"; "--sandbox"; "off"; "--"; "true" ]
  in
  Alcotest.(check int) "record: 69" 69 code;
  Alcotest.(check bool)
    "record: names TLS" true
    (contains (string_field "error" printed) "no TLS support")

(* A bad value is a usage error that names the variable, in every command that
   can call a provider. *)

let invalid_values =
  [
    ("userinfo", "http://user:hunter2@127.0.0.1:1", "user information");
    ("a query", "http://127.0.0.1:1/v1?key=hunter2", "query or a fragment");
    ("a fragment", "http://127.0.0.1:1/v1#hunter2", "query or a fragment");
    ("a relative URL", "/v1/hunter2", "absolute http or https URL");
    ("no scheme", "127.0.0.1:11434", "absolute http or https URL");
  ]

let invalid_values_are_usage_errors () =
  let agent = List.hd agents in
  with_project "override-invalid" @@ fun ~parent:_ ~project ->
  write_experiment project (agent.argv ());
  List.iter
    (fun (what, value, reason) ->
      List.iter
        (fun (command, args) ->
          let code, printed =
            cli ~dir:project
              ~env:(("BACTRIAN_UPSTREAM_GEMINI", value) :: key_env)
              args
          in
          let label = Printf.sprintf "%s, %s" command what in
          Alcotest.(check int) (label ^ ": exit 64") 64 code;
          let message = string_field "error" printed in
          Alcotest.(check bool)
            (label ^ ": names the variable")
            true
            (String.starts_with ~prefix:"BACTRIAN_UPSTREAM_GEMINI " message);
          Alcotest.(check bool)
            (label ^ ": says why") true (contains message reason);
          Alcotest.(check bool)
            (label ^ ": repeats no part of the value")
            false
            (contains message "hunter2"))
        [
          ("run", run_flags);
          ("doctor", [ "doctor"; "--json" ]);
          ( "record",
            [
              "record";
              "--json";
              "--turn";
              "hi";
              "--sandbox";
              "off";
              "--";
              "true";
            ] );
          ( "replay",
            [
              "replay";
              "none";
              "--json";
              "--mode";
              "until-divergence";
              "--";
              "true";
            ] );
        ])
    invalid_values

(* doctor *)

let check_named name j =
  List.find_opt
    (fun c -> String.equal (string_field "name" c) name)
    (get "checks" (Canon.list_member "checks" j))

let doctor_shows_the_override () =
  let agent = List.hd agents in
  with_project "override-doctor" @@ fun ~parent:_ ~project ->
  write_experiment project (agent.argv ());
  let doctor env =
    cli ~dir:project ~env [ "doctor"; "--json"; "--experiment"; "exp.json" ]
  in
  let code, printed =
    doctor ((variable, "http://127.0.0.1:11434") :: key_env)
  in
  Alcotest.(check int) "exit status" 0 code;
  (match check_named "upstream-openai" printed with
  | Some c ->
      Alcotest.(check string) "ok" "ok" (string_field "status" c);
      Alcotest.(check string)
        "the URL" "http://127.0.0.1:11434" (string_field "message" c)
  | None -> Alcotest.fail "no upstream-openai check");
  Alcotest.(check bool)
    "a provider that is not overridden has no line" true
    (Option.is_none (check_named "upstream-anthropic" printed));
  Alcotest.(check string)
    "an http upstream does not need TLS" "skip"
    (string_field "status" (get "tls" (check_named "tls" printed)));
  (* Plain HTTP to another machine sends the key unencrypted. *)
  let _, printed = doctor ((variable, "http://192.0.2.1:11434") :: key_env) in
  Alcotest.(check string)
    "a warning" "warn"
    (string_field "status"
       (get "upstream-openai" (check_named "upstream-openai" printed)));
  if not Bactrian_app.Features.tls_available then begin
    let code, printed = doctor ((variable, https_url) :: key_env) in
    Alcotest.(check int) "an https upstream fails the tls check" 69 code;
    Alcotest.(check string)
      "tls" "fail"
      (string_field "status" (get "tls" (check_named "tls" printed)))
  end

(* Keys and prices of a local server *)

let prices =
  {|{"version": 1, "prices": [{"model": "llama3.2", "provider": "openai",
 "rates": {"input": 0, "cached_input": 0, "cache_write": 0, "output": 0},
 "max_output": 4096, "max_input": 131072}]}|}

let local_model = "llama3.2"

let zero =
  match Canon.float 0. with
  | Ok c -> c
  | Error e -> Alcotest.failf "float: %a" Canon.pp_error e

let reasons j = List.filter_map (Canon.str_member "reason") (runs j)

let key_is_still_required () =
  let agent = List.hd agents in
  with_project "override-key" @@ fun ~parent:_ ~project ->
  with_upstream ~dir:project @@ fun up ->
  write_experiment project (agent.argv ());
  let code, printed =
    cli ~dir:project ~env:[ (variable, upstream_url up) ] run_flags
  in
  Alcotest.(check int) "exit status" 69 code;
  Alcotest.(check string)
    "the missing key is named"
    "openai has a spend cap but OPENAI_API_KEY is not set"
    (string_field "error" printed);
  Alcotest.(check (list string)) "nothing was called" [] (upstream_requests up)

let unknown_model_is_refused_until_priced () =
  let agent = List.hd agents in
  with_project "override-price" @@ fun ~parent:_ ~project ->
  with_upstream ~dir:project @@ fun up ->
  write_experiment project (agent.argv ()) ~model:local_model;
  let _, printed = cli ~dir:project ~env:(env_of up) run_flags in
  Alcotest.(check (list string))
    "no run is counted as free"
    (List.init 4 (fun _ -> "errored"))
    (statuses printed);
  Alcotest.(check (list string))
    "the reason names the model"
    (List.init 4 (fun _ -> "no price for model " ^ local_model))
    (reasons printed);
  Alcotest.(check (list string))
    "the request never reached the server" [] (upstream_requests up);
  (* The same experiment with the price the user states, zero. *)
  write (Filename.concat project "prices.json") prices;
  write_experiment project (agent.argv ()) ~model:local_model
    ~extra:{|, "pricing": "prices.json"|};
  let code, printed = cli ~dir:project ~env:(env_of up) run_flags in
  Alcotest.(check int) "exit status" 0 code;
  Alcotest.(check (list string))
    "every run passes" all_passed (statuses printed);
  Alcotest.(check (list string))
    "the server answered every model call" completions (upstream_requests up);
  List.iter
    (fun r ->
      Alcotest.check json "the stated price makes the cost zero" zero
        (field "cost_usd" (meta_of project (string_field "run_id" r))))
    (runs printed)

let () =
  Alcotest.run "upstream override"
    [
      ( "an experiment through the override",
        List.map
          (fun a -> Alcotest.test_case a.name `Slow (through_the_override a))
          agents
        @ [
            Alcotest.test_case "a strict replay is unlabelled" `Slow
              strict_replay_has_no_label;
          ] );
      ( "https",
        [
          Alcotest.test_case "needs TLS only where it is called" `Slow
            https_is_refused_without_tls;
        ] );
      ( "invalid values",
        [
          Alcotest.test_case "are usage errors" `Slow
            invalid_values_are_usage_errors;
        ] );
      ( "doctor",
        [
          Alcotest.test_case "shows the override" `Slow
            doctor_shows_the_override;
        ] );
      ( "keys and prices",
        [
          Alcotest.test_case "the key is still required" `Slow
            key_is_still_required;
          Alcotest.test_case "an unknown model is refused until priced" `Slow
            unknown_model_is_refused_until_priced;
        ] );
    ]
