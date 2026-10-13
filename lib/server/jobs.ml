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
  over : unit Eio.Promise.t;
}

type t = { sw : Eio.Switch.t; jobs : (string, job) Hashtbl.t }

let create ~sw = { sw; jobs = Hashtbl.create 8 }
let find t id = Hashtbl.find_opt t.jobs id
let state job = match job.finished with Some f -> f.state | None -> Running

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
    { id; kind; progress; stop = Interrupt.trigger (); finished = None; over }
  in
  Hashtbl.replace t.jobs id job;
  Eio.Fiber.fork ~sw:t.sw (fun () ->
      let outcome =
        match command job.stop with
        | outcome -> outcome
        | exception Eio.Io (err, _) ->
            Error
              (Command_error.Internal (Format.asprintf "%a" Eio.Exn.pp_err err))
      in
      job.finished <- Some (finish outcome);
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
  List.fold_left
    (fun o (k, v) -> Canon.set_member k v o)
    Canon.empty_object fields

let cancel job =
  Interrupt.fire job.stop Sys.sigint;
  Eio.Promise.await job.over

let cancel_all t =
  Eio.Fiber.List.iter cancel (Hashtbl.fold (fun _ j acc -> j :: acc) t.jobs [])
