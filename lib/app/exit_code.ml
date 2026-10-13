open Bactrian_runtime

let ok = 0
let failed = 1
let not_run = 2
let usage = 64
let environment = 69
let internal = 70

let of_outcome : Run.outcome -> int = function
  | Passed -> ok
  | Failed _ | Diverged -> failed
  | Not_run _ -> not_run
  | Errored _ -> internal

let of_status : Executor.status -> int = function
  | Pending | Interrupted | Done (Not_run _) -> not_run
  | Done Passed -> ok
  | Done (Failed _ | Diverged) -> failed
  | Done (Errored _) -> internal

let of_summary (s : Executor.summary) =
  let codes = List.map (fun (_, st, _) -> of_status st) s.runs in
  List.find_opt
    (fun c -> List.exists (Int.equal c) codes)
    [ internal; failed; not_run ]
  |> Option.value ~default:ok
