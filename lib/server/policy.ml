open Bactrian_core
module Sandbox = Bactrian_runtime.Sandbox

type t = { allow_unsandboxed : bool; trust_project : bool; max_jobs : int }

let default = { allow_unsandboxed = false; trust_project = false; max_jobs = 4 }
let ( let* ) = Result.bind

let rec all f = function
  | [] -> Ok ()
  | x :: rest ->
      let* () = f x in
      all f rest

let unsandboxed what =
  Error (Printf.sprintf "%s, which needs serve-mcp --allow-unsandboxed" what)

let untrusted what =
  Error (Printf.sprintf "%s, which needs serve-mcp --trust-project" what)

let sandbox t (requested : Sandbox.mode option) =
  match (t.allow_unsandboxed, requested) with
  | true, _ -> Ok requested
  | false, (None | Some `Strict) -> Ok (Some `Strict)
  | false, Some ((`Auto | `Off) as m) ->
      unsandboxed
        (Printf.sprintf "argument sandbox: %s can run the agent unsandboxed"
           (Sandbox.mode_name m))

let gate t ~field what = function
  | [] -> Ok ()
  | _ :: _ when t.trust_project -> Ok ()
  | _ :: _ -> untrusted (Printf.sprintf "%s: %s" field what)

let env t names =
  gate t ~field:"argument env"
    "passes variables of the server's environment to the agent" names

let mcp t ~stdio ~http =
  let* () =
    gate t ~field:"argument mcp_stdio"
      "starts MCP servers as commands outside the sandbox" stdio
  in
  gate t ~field:"argument mcp_http" "sends the agent's tool calls to any URL"
    http

let checkers t ~field (s : Scenario.t) =
  gate t ~field "runs checker commands outside the sandbox" s.checkers

let scenario t s = checkers t ~field:"checkers" s
let inside root ~field p = Result.map ignore (Confine.path root ~field p)
let in_dir dir p = if Filename.is_relative p then Filename.concat dir p else p

let experiment t root (c : Bactrian_runtime.Executor.contents) =
  let e = c.experiment in
  let* () =
    gate t ~field:"agent.env_passthrough"
      "passes variables of the server's environment to the agent"
      e.env_passthrough
  in
  let servers = if List.mem `Mcp e.intercept then e.mcp else [] in
  let* () =
    all
      (fun (name, (s : Experiment.mcp_server)) ->
        let field = Printf.sprintf "mcp.%s" name in
        match s.transport with
        | Mcp_stdio { cwd; _ } ->
            let* () =
              gate t ~field "runs a command outside the sandbox" [ name ]
            in
            Option.fold ~none:(Ok ())
              ~some:(fun d ->
                inside root ~field:(field ^ ".cwd") (in_dir c.dir d))
              cwd
        | Mcp_http _ ->
            gate t ~field "sends the agent's tool calls to any URL" [ name ])
      servers
  in
  let* () =
    if t.trust_project then Ok ()
    else
      all
        (fun p ->
          Result.map_error
            (fun m -> m ^ " (serve-mcp --trust-project allows it)")
            (inside root ~field:"sandbox.allow_write" (in_dir c.dir p)))
        e.sandbox_allow_write
  in
  all
    (fun (file, (s : Scenario.t)) ->
      let* () = checkers t ~field:(file ^ ": checkers") s in
      match s.fixtures with
      | None -> Ok ()
      | Some f ->
          inside root
            ~field:(Printf.sprintf "%s: fixtures" file)
            (in_dir (Filename.dirname (in_dir c.dir file)) f))
    c.scenarios

let describe t =
  String.concat " "
    [
      (if t.allow_unsandboxed then
         "Jobs run their agent in the sandbox mode the experiment or the \
          sandbox argument names, auto by default."
       else
         "Every job runs its agent in the operating-system sandbox in strict \
          mode: sandbox auto or off, and HTTP-mode agents, are refused (the \
          user allows them by starting serve-mcp with --allow-unsandboxed), \
          and a job fails where the sandbox does not work.");
      (if t.trust_project then
         "Checkers, MCP servers, passed environment variables and writable \
          paths outside this directory are allowed: they run or reach outside \
          the sandbox."
       else
         "Checkers, MCP servers, passed environment variables and writable \
          paths outside this directory are refused (the user allows them by \
          starting serve-mcp with --trust-project).");
      Printf.sprintf "At most %d jobs run at once." t.max_jobs;
    ]
