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
        (Printf.sprintf "argument sandbox: %s can run outside the sandbox"
           (Sandbox.mode_name m))

(* Under Seatbelt an agent on macOS can read the launch environment of every
   process of the user that is not an Apple binary (decision 0014): a risk the
   user takes by starting the server with --allow-unsandboxed, never one a
   client or an experiment file takes for them. *)
let backend t ~field (b : Sandbox.backend) =
  match (t.allow_unsandboxed, b) with
  | true, _ | false, (`Auto | `Vm | `Bubblewrap) -> Ok ()
  | false, `Seatbelt ->
      unsandboxed
        (Printf.sprintf
           "%s: seatbelt leaves the launch environment of the user's other \
            processes readable to the agent on macOS"
           field)

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

(* Only [strict] keeps checkers inside the sandbox whatever the host: [auto]
   runs them unsandboxed where the sandbox does not work. *)
let checkers t ~(sandbox : Sandbox.mode) ~field (s : Scenario.t) =
  match sandbox with
  | `Strict -> Ok ()
  | (`Auto | `Off) as m ->
      gate t ~field
        (Printf.sprintf
           "runs checker commands that sandbox: %s can leave outside the \
            sandbox (sandbox: strict keeps them inside)"
           (Sandbox.mode_name m))
        s.checkers

let scenario t ~sandbox s = checkers t ~sandbox ~field:"checkers" s
let inside root ~field p = Result.map ignore (Confine.path root ~field p)
let in_dir dir p = if Filename.is_relative p then Filename.concat dir p else p

let experiment t root ~sandbox (c : Bactrian_runtime.Executor.contents) =
  let e = c.experiment in
  let sandbox = Option.value sandbox ~default:e.sandbox in
  let* () = backend t ~field:"sandbox.backend" e.sandbox_backend in
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
      let* () = checkers t ~sandbox ~field:(file ^ ": checkers") s in
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
      "Checkers (in check and in experiments) run in the sandbox too, with no \
       network and writes only to a temporary directory of their own.";
      (if t.trust_project then
         "MCP servers, passed environment variables, writable paths outside \
          this directory and checkers in sandbox auto or off are allowed: they \
          run or reach outside the sandbox."
       else
         "MCP servers, passed environment variables, writable paths outside \
          this directory and checkers in sandbox auto or off are refused (the \
          user allows them by starting serve-mcp with --trust-project).");
      Printf.sprintf "At most %d jobs run at once." t.max_jobs;
    ]
