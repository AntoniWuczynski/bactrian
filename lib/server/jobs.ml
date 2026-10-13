open Bactrian_core
module Interrupt = Bactrian_runtime.Interrupt
module Command_error = Bactrian_app.Command_error

type state = Running | Succeeded | Failed | Cancelled

let state_name = function
  | Running -> "running"
  | Succeeded -> "succeeded"
  | Failed -> "failed"
  | Cancelled -> "cancelled"

type progress = { done_ : int; total : int option }
type outcome = (Yojson.Safe.t * int, Command_error.t) result

type finished = {
  state : state;
  exit : int;
  field : string;  (** ["result"] or ["error"]. *)
  body : Canon.t;
}

type job = {
  id : string;
  kind : string;
  progress : finished:bool -> progress;
  stop : Interrupt.trigger;
  mutable finished : finished option;
  mutable finished_at : float;
  over : unit Eio.Promise.t;
}

type t = {
  sw : Eio.Switch.t;
  now : unit -> float;
  log : string -> unit;
  jobs : (string, job) Hashtbl.t;
}

let keep_for = 3600.
let keep_at_most = 100

let create ?(now = Unix.gettimeofday) ?(log = ignore) ~sw () =
  { sw; now; log; jobs = Hashtbl.create 8 }

(* Finished jobs are forgotten an hour after they finished, and beyond the
   newest hundred. Their results stay on disk. *)
let prune t =
  let cutoff = t.now () -. keep_for in
  let finished =
    Hashtbl.fold
      (fun _ job acc -> if Option.is_some job.finished then job :: acc else acc)
      t.jobs []
    |> List.sort (fun a b -> Float.compare b.finished_at a.finished_at)
  in
  List.iteri
    (fun i job ->
      if i >= keep_at_most || job.finished_at < cutoff then
        Hashtbl.remove t.jobs job.id)
    finished

let find t id =
  prune t;
  Hashtbl.find_opt t.jobs id

let state job = match job.finished with Some f -> f.state | None -> Running

let running t =
  Hashtbl.fold
    (fun _ job n -> if Option.is_none job.finished then n + 1 else n)
    t.jobs 0

(* A rendered command output that is not canonical JSON (a non-finite number)
   is a fault of ours. *)
let canonical json =
  match Canon.of_yojson json with
  | Ok c -> c
  | Error e ->
      Canon.set_member "error"
        (Canon.string
           (Format.asprintf "unrenderable result: %a" Canon.pp_error e))
        Canon.empty_object

let finish (outcome : outcome) =
  match outcome with
  | Ok (json, exit) ->
      { state = Succeeded; exit; field = "result"; body = canonical json }
  | Error e ->
      {
        state =
          (match e with
          | Interrupted _ -> Cancelled
          | Usage _ | Environment _ | Internal _ | Configuration _ -> Failed);
        exit = Command_error.exit_code e;
        field = "error";
        body = canonical (Command_error.to_json e);
      }

let rec fresh_id t =
  let id =
    "job-" ^ Bactrian_runtime.Proc_util.hex (Mirage_crypto_rng.generate 8)
  in
  if Hashtbl.mem t.jobs id then fresh_id t else id

let start t ~kind ~progress command =
  let id = fresh_id t in
  let over, resolve = Eio.Promise.create () in
  let job =
    {
      id;
      kind;
      progress;
      stop = Interrupt.trigger ();
      finished = None;
      finished_at = 0.;
      over;
    }
  in
  prune t;
  Hashtbl.replace t.jobs id job;
  Eio.Fiber.fork ~sw:t.sw (fun () ->
      let outcome =
        match command job.stop with
        | outcome -> outcome
        | exception Eio.Io (err, _) ->
            Error
              (Command_error.Internal (Format.asprintf "%a" Eio.Exn.pp_err err))
        | exception (Eio.Cancel.Cancelled _ as ex) -> raise ex
        (* A fault of ours in one job fails that job, not the server and the
           other jobs. *)
        | exception ex ->
            t.log
              (Printf.sprintf "job %s failed: %s\n%s" id (Printexc.to_string ex)
                 (Printexc.get_backtrace ()));
            Error (Command_error.Internal (Printexc.to_string ex))
      in
      job.finished <- Some (finish outcome);
      job.finished_at <- t.now ();
      Eio.Promise.resolve resolve ());
  id

let status job =
  let p = job.progress ~finished:(Option.is_some job.finished) in
  let base =
    [
      ("job_id", Canon.string job.id);
      ("kind", Canon.string job.kind);
      ("state", Canon.string (state_name (state job)));
      ( "progress",
        Canon.empty_object
        |> Canon.set_member "done" (Canon.int p.done_)
        |> Canon.set_member "total"
             (Option.fold ~none:Canon.null ~some:Canon.int p.total) );
    ]
  in
  let fields =
    match job.finished with
    | None -> base
    | Some f -> base @ [ ("exit", Canon.int f.exit); (f.field, f.body) ]
  in
  Canon.of_fields fields

let cancel job =
  Interrupt.fire job.stop Sys.sigint;
  Eio.Promise.await job.over

let cancel_all t =
  Eio.Fiber.List.iter cancel (Hashtbl.fold (fun _ j acc -> j :: acc) t.jobs [])
