open Bactrian_runtime

let ( let* ) = Result.bind

type request = {
  common : Single_run.common;
  turns : string list;
  mcp_stdio : string list;
  mcp_http : string list;
  mcp_policy : string list;
}

let prepare r =
  let* () =
    match r.turns with
    | [] -> Error "record needs at least one --turn"
    | _ :: _ -> Single_run.positive_timeouts r.common
  in
  Mcp_flags.parse ~stdio:r.mcp_stdio ~http:r.mcp_http ~policy:r.mcp_policy

let validate r = Result.map ignore (prepare r)

let run ?stop ~on_progress ~bactrian_exe ~env r =
  match prepare r with
  | Error m -> Error (Command_error.Usage m)
  | Ok mcp ->
      Single_run.execute ?stop ~on_progress ~bactrian_exe ~env ~live:true
        ~sandbox:r.common.sandbox (fun _ sandbox ->
          Ok
            (Single_run.config r.common ~turns:r.turns ~replay:None ~mcp sandbox))

let exit_code = Single_run.exit_code
let to_json = Single_run.to_json
