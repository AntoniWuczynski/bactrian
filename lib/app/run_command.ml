open Bactrian_runtime

type request = {
  experiment : string;
  resume : string option;
  sandbox : Sandbox.mode option;
}

type t = { summary : Executor.summary; sandbox : Sandbox.label option }

let run ?stop ?files ?vet ~on_progress ~bactrian_exe ~env (r : request) =
  Mirage_crypto_rng_unix.use_default ();
  match Upstream_env.from_environment () with
  | Error m -> Error (Command_error.Usage m)
  | Ok upstream -> (
      let announced = ref None in
      let label = ref None in
      match
        Interrupt.run ?stop (fun () ->
            Executor.run_experiment ~upstream ?sandbox:r.sandbox
              ~on_sandbox:(fun l -> label := Some l)
              ~bactrian_exe ~env ?files ?vet ~experiment_file:r.experiment
              ~resume:r.resume
              ~announce:(fun id ->
                announced := Some id;
                on_progress (Progress.Experiment_started id))
              ~warn:(fun w -> on_progress (Progress.Warning w))
              ())
      with
      | Error signal ->
          Error
            (Command_error.Interrupted { signal; experiment_id = !announced })
      | Ok (Error (Configuration errors)) ->
          Error (Command_error.Configuration errors)
      | Ok (Error (Usage m)) -> Error (Command_error.Usage m)
      | Ok (Error (Environment m)) -> Error (Command_error.Environment m)
      | Ok (Ok summary) -> Ok { summary; sandbox = !label })

let exit_code t = Exit_code.of_summary t.summary

let status_kind : Executor.status -> string = function
  | Pending -> "pending"
  | Interrupted -> "interrupted"
  | Done o -> Run.outcome_kind o

let status_reason : Executor.status -> string option = function
  | Pending | Interrupted -> None
  | Done o -> Run.outcome_reason o

let to_json t =
  `Assoc
    [
      ("experiment_id", `String t.summary.experiment_id);
      ( "sandbox",
        match t.sandbox with Some l -> Sandbox.label_json l | None -> `Null );
      ( "runs",
        `List
          (List.map
             (fun ((p : Executor.planned), status, run_id) ->
               `Assoc
                 ([
                    ("index", `Int p.index);
                    ("scenario", `String p.scenario);
                    ("variant", `String p.variant);
                    ("repeat", `Int p.repeat);
                    ("status", `String (status_kind status));
                  ]
                 @ (match status_reason status with
                   | Some r -> [ ("reason", `String r) ]
                   | None -> [])
                 @
                 match run_id with
                 | Some id -> [ ("run_id", `String id) ]
                 | None -> []))
             t.summary.runs) );
    ]
