open Bactrian_runtime

type request = { scenarios : string list }
type t = Golden.report list

let run ~env r =
  let results =
    List.map (fun scenario_file -> Golden.check ~env ~scenario_file) r.scenarios
  in
  match List.concat_map (function Ok _ -> [] | Error e -> e) results with
  | _ :: _ as errors -> Error (Command_error.Configuration errors)
  | [] -> Ok (List.filter_map Result.to_option results)

let passed = List.for_all Golden.passed
let exit_code t = if passed t then Exit_code.ok else Exit_code.failed
