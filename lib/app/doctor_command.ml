open Bactrian_runtime

type request = { experiment : string option }

(* Every result, or every error of the ones that failed. *)
let collect f xs =
  List.fold_left
    (fun acc x ->
      match (acc, f x) with
      | Ok found, Ok y -> Ok (y :: found)
      | Ok _, Error es | Error es, Ok _ -> Error es
      | Error es, Error more -> Error (es @ more))
    (Ok []) xs

(* Whether any scenario of the experiment has checkers, or what is wrong with
   a scenario that does not load: a run could not start either, and a broken
   scenario must not hide the checkers' sandbox line. A glob that matches
   nothing counts as no scenarios: [run] reports it, and [doctor] reads the
   scenarios for nothing else. *)
let has_checkers ~base (e : Bactrian_core.Experiment.t) =
  let files =
    List.concat_map
      (fun glob ->
        Result.value ~default:[] (Config_loader.expand_globs ~base [ glob ]))
      e.scenarios
  in
  Result.map
    (List.exists (fun (s : Bactrian_core.Scenario.t) ->
         not (List.is_empty s.checkers)))
    (collect
       (fun file -> Config_loader.load_scenario Eio.Path.(base / file))
       files)

let run ?files ~bactrian_exe ~env r =
  Mirage_crypto_rng_unix.use_default ();
  match Upstream_env.from_environment () with
  | Error m -> Error (Command_error.Usage m)
  | Ok upstream -> (
      let target =
        match r.experiment with
        | None -> Ok None
        | Some file ->
            let dir =
              match Filename.dirname file with
              | "." -> Sys.getcwd ()
              | dir when Filename.is_relative dir ->
                  Filename.concat (Sys.getcwd ()) dir
              | dir -> dir
            in
            let files = Option.value files ~default:(Eio.Stdenv.fs env) in
            Result.bind
              (Config_loader.load_experiment Eio.Path.(files / file))
              (fun e ->
                let base = Eio.Path.(files / Filename.dirname file) in
                Result.map
                  (fun checkers -> Some (Doctor.target ~dir ~checkers e))
                  (has_checkers ~base e))
      in
      match target with
      | Error errors -> Error (Command_error.Configuration errors)
      | Ok target ->
          Ok
            (Doctor.report
               (Doctor.run ~env ~bactrian_exe
                  ~tls_available:Features.tls_available ~upstream
                  ~keys:(Run.keys_from_environment ())
                  ~path:(Sys.getenv_opt "PATH") target)))

let exit_code (report : Doctor.report) =
  if report.ok then Exit_code.ok else Exit_code.environment

let to_json = Doctor.to_json
