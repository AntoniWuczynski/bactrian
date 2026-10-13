open Bactrian_core

type t = { max_jobs : int }

let default = { max_jobs = 4 }
let ( let* ) = Result.bind

let rec all f = function
  | [] -> Ok ()
  | x :: rest ->
      let* () = f x in
      all f rest

let inside root ~field p = Result.map ignore (Confine.path root ~field p)
let in_dir dir p = if Filename.is_relative p then Filename.concat dir p else p

(* The paths an experiment hands to the processes it starts, which no
   directory capability covers: where the agent may write, where an MCP stdio
   server runs and the fixtures copied into the agent's state directory. *)
let experiment _t root (c : Bactrian_runtime.Executor.contents) =
  let e = c.experiment in
  let* () =
    all
      (fun p -> inside root ~field:"sandbox.allow_write" (in_dir c.dir p))
      e.sandbox_allow_write
  in
  let* () =
    all
      (fun (name, (s : Experiment.mcp_server)) ->
        match s.transport with
        | Mcp_stdio { cwd = Some d; _ } ->
            inside root
              ~field:(Printf.sprintf "mcp.%s.cwd" name)
              (in_dir c.dir d)
        | Mcp_stdio { cwd = None; _ } | Mcp_http _ -> Ok ())
      (if List.mem `Mcp e.intercept then e.mcp else [])
  in
  all
    (fun (file, (s : Scenario.t)) ->
      match s.fixtures with
      | None -> Ok ()
      | Some f ->
          inside root
            ~field:(Printf.sprintf "%s: fixtures" file)
            (in_dir (Filename.dirname (in_dir c.dir file)) f))
    c.scenarios
