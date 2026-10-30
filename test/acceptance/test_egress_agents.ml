(* HTTPS egress from the outside: the real binary records the example agents,
   whose get_weather tool calls a fake HTTPS weather service through the
   egress proxy (Node fetch, Python httpx and requests), replays them strictly
   with the service never called, and reports the divergence when the agent
   asks for another URL. bactrian trusts the service's CA through
   BACTRIAN_EXTRA_CA_CERTS, and the agents trust the project CA through the
   run's bundle. Nothing leaves this machine. *)

open Bactrian_core
open Acceptance_world
open Cli_world

type client = { label : string; agent : agent; http_client : string }

let clients =
  let find name = List.find (fun a -> String.equal a.name name) agents in
  [
    { label = "node fetch"; agent = find "typescript"; http_client = "fetch" };
    { label = "python httpx"; agent = find "python"; http_client = "httpx" };
    {
      label = "python requests";
      agent = find "python";
      http_client = "requests";
    };
  ]

let sandbox = String.split_on_char ' ' sandbox_flag

let lines_of project run name =
  lines
    (Filename.concat project (Printf.sprintf ".bactrian/runs/%s/%s" run name))

let has_text project run name text =
  List.exists (fun l -> contains l text) (lines_of project run name)

let weather_calls up =
  List.filter (String.equal "/weather") (upstream_requests up)

(* The model fake, plain, and the weather service behind TLS. *)
let with_services name f =
  with_project name @@ fun ~parent:_ ~project ->
  let ca = Filename.concat project "weather-ca.pem" in
  let models = Filename.concat project "models"
  and weather = Filename.concat project "weather" in
  Sys.mkdir models 0o700;
  Sys.mkdir weather 0o700;
  with_upstream ~dir:models @@ fun models ->
  with_upstream ~tls_ca:ca ~dir:weather @@ fun weather ->
  let port = get "port" (Uri.port (Uri.of_string (upstream_url weather))) in
  f ~project ~ca ~models ~weather ~port

let env ~ca ~models ~url c =
  [
    ("BACTRIAN_UPSTREAM_OPENAI", upstream_url models);
    ("BACTRIAN_EXTRA_CA_CERTS", ca);
    ("AGENT_WEATHER_URL", url);
    ("AGENT_HTTP_CLIENT", c.http_client);
  ]

let passthrough = [ "--env"; "AGENT_WEATHER_URL"; "--env"; "AGENT_HTTP_CLIENT" ]
let outcome j = string_field "kind" (field "outcome" j)

(* A month back, as a scenario's fixed [now] or an old recording would give.
   On Linux libfaketime sets the agent's clock there, and the tunnel's
   certificates must still verify. *)
let month_ago () =
  let tm = Unix.gmtime (Unix.time () -. (30. *. 86_400.)) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

(* [host] is an address, or a name the agent's client must send as SNI and
   find in the leaf's DNS names. *)
let record_and_replay ?sim_now ?(host = "127.0.0.1") c () =
  let argv = c.agent.argv () in
  with_services
    (String.concat "-"
       ([ "egress"; c.agent.name; c.http_client; host ]
       @ match sim_now with Some _ -> [ "past" ] | None -> []))
  @@ fun ~project ~ca ~models ~weather ~port ->
  let url = Printf.sprintf "https://%s:%d/weather" host port in
  let env = env ~ca ~models ~url c in
  let live = (("BACTRIAN_CAP_OPENAI", "1") :: key_env) @ env in
  let code, recorded =
    cli ~dir:project ~env:live
      ([ "record"; "--json"; "--turn"; first_turn; "--sandbox" ]
      @ sandbox
      @ (match sim_now with Some t -> [ "--sim-now"; t ] | None -> [])
      @ [ "--egress"; Printf.sprintf "%s:%d=record" host port ]
      @ passthrough @ [ "--" ] @ argv)
  in
  Alcotest.(check (pair int string))
    "record: passed" (0, "passed")
    (code, outcome recorded);
  let run = string_field "run_id" recorded in
  Alcotest.(check int)
    "the service was called once" 1
    (List.length (weather_calls weather));
  Alcotest.(check bool)
    "the call is in the cassette" true
    (has_text project run "cassette.jsonl" {|"channel":"http"|});
  Alcotest.(check bool)
    "and in the trace" true
    (has_text project run "trace.jsonl" {|"type":"http_call"|});
  Alcotest.(check bool)
    "the reply used the forecast" true
    (has_text project run "cassette.jsonl" "18 degrees");
  let replay ?(env = env) extra =
    cli ~dir:project ~env
      ([ "replay"; run; "--json"; "--sandbox" ]
      @ sandbox @ passthrough @ extra @ [ "--" ] @ argv)
  in
  let code, replayed = replay [] in
  Alcotest.(check (pair int string))
    "strict replay: passed" (0, "passed")
    (code, outcome replayed);
  Alcotest.(check int)
    "strict replay: no divergence" 0
    (List.length (get "divergences" (Canon.list_member "divergences" replayed)));
  Alcotest.(check int)
    "strict replay: the service was not called" 1
    (List.length (weather_calls weather));
  (* Another URL is not in the recording: a replay miss on egress. *)
  let other = Printf.sprintf "https://%s:%d/weather/v2" host port in
  let code, diverged =
    replay
      ~env:
        (("AGENT_WEATHER_URL", other)
        :: List.remove_assoc "AGENT_WEATHER_URL" env)
      []
  in
  Alcotest.(check (pair int string))
    "another URL: diverged" (1, "diverged")
    (code, outcome diverged);
  Alcotest.(check bool)
    "the egress miss names the path" true
    (List.exists
       (fun d ->
         List.exists
           (fun c -> String.equal (string_field "path" c) "path")
           (get "changes" (Canon.list_member "changes" d)))
       (get "divergences" (Canon.list_member "divergences" diverged)));
  Alcotest.(check int)
    "and the service was still not called" 1
    (List.length (weather_calls weather))

(* Without --egress the host is blocked before TLS: the agent's call fails, the
   service hears nothing, and the trace shows the attempt. *)
let blocked () =
  let c = List.hd clients in
  let argv = c.agent.argv () in
  with_services "egress-blocked" @@ fun ~project ~ca ~models ~weather ~port ->
  let url = Printf.sprintf "https://127.0.0.1:%d/weather" port in
  let live =
    (("BACTRIAN_CAP_OPENAI", "1") :: key_env) @ env ~ca ~models ~url c
  in
  let code, recorded =
    cli ~dir:project ~env:live
      ([ "record"; "--json"; "--turn"; first_turn; "--sandbox" ]
      @ sandbox @ passthrough @ [ "--" ] @ argv)
  in
  Alcotest.(check (pair int string))
    "record: the agent failed" (1, "failed")
    (code, outcome recorded);
  let run = string_field "run_id" recorded in
  Alcotest.(check int)
    "the service was not called" 0
    (List.length (weather_calls weather));
  Alcotest.(check bool)
    "the blocked CONNECT is in the trace" true
    (has_text project run "trace.jsonl" {|"method":"CONNECT"|});
  Alcotest.(check bool)
    "the reason is noted" true
    (contains (Canon.to_string recorded) "egress to 127.0.0.1")

let () =
  Alcotest.run "egress agents"
    [
      ( "through the egress proxy",
        List.map
          (fun c -> Alcotest.test_case c.label `Slow (record_and_replay c))
          clients
        @ List.map
            (fun c ->
              Alcotest.test_case
                (c.label ^ ", a host name")
                `Slow
                (record_and_replay ~host:"localhost" c))
            clients
        @ [
            Alcotest.test_case "node fetch, SIM_NOW a month back" `Slow
              (record_and_replay ~sim_now:(month_ago ()) (List.hd clients));
            Alcotest.test_case "a blocked host" `Slow blocked;
          ] );
    ]
