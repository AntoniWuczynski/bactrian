(* What the egress acceptance cases share: a project with the model fake and a
   fake HTTPS weather service, and what to look for in a run's files. *)

open Acceptance_world
open Cli_world

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

let env ~ca ~models ~url ~http_client =
  [
    ("BACTRIAN_UPSTREAM_OPENAI", upstream_url models);
    ("BACTRIAN_EXTRA_CA_CERTS", ca);
    ("AGENT_WEATHER_URL", url);
    ("AGENT_HTTP_CLIENT", http_client);
  ]

let passthrough = [ "--env"; "AGENT_WEATHER_URL"; "--env"; "AGENT_HTTP_CLIENT" ]
let outcome j = string_field "kind" (field "outcome" j)
