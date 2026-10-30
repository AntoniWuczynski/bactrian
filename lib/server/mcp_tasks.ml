open Bactrian_core
module Proc_util = Bactrian_runtime.Proc_util

let poll_interval_ms = 1000

let status_name : Jobs.state -> string = function
  | Running -> "working"
  | Succeeded | Failed -> "completed"
  | Cancelled -> "cancelled"

let progress_message (p : Jobs.progress) =
  match p.total with
  | Some total -> Printf.sprintf "%d of %d runs finished" p.done_ total
  | None -> Printf.sprintf "%d runs finished" p.done_

let ms seconds = Canon.int (Float.to_int (Float.round (seconds *. 1000.)))

let task job =
  let message =
    match Jobs.ending job with
    | Some e -> e.message
    | None -> progress_message (Jobs.progress job)
  in
  Canon.of_fields
    [
      ("taskId", Canon.string (Jobs.id job));
      ("status", Canon.string (status_name (Jobs.state job)));
      ("statusMessage", Canon.string message);
      ("createdAt", Canon.string (Proc_util.rfc3339 (Jobs.created_at job)));
      ("lastUpdatedAt", Canon.string (Proc_util.rfc3339 (Jobs.updated_at job)));
      ( "ttlMs",
        Option.fold ~none:Canon.null
          ~some:(fun at -> ms (at -. Jobs.created_at job))
          (Jobs.expires_at job) );
      ("pollIntervalMs", Canon.int poll_interval_ms);
    ]

let detailed ~result job =
  let base = task job in
  match Jobs.ending job with
  | None -> base
  | Some { state = Succeeded; body; _ } ->
      Canon.set_member "result" (result ~is_error:false body) base
  | Some { state = Failed; body; _ } ->
      Canon.set_member "result" (result ~is_error:true body) base
  | Some { state = Cancelled | Running; _ } -> base
