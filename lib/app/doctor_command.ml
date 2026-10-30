open Bactrian_runtime

type request = { experiment : string option }

(* Whether any scenario of the experiment has checkers. A glob or a scenario
   that does not load counts as none: [run] reports those, [doctor] does not
   read the scenarios for anything else. *)
let has_checkers ~base (e : Bactrian_core.Experiment.t) =
  List.exists
    (fun glob ->
      match Config_loader.expand_globs ~base [ glob ] with
      | Error _ -> false
      | Ok files ->
          List.exists
            (fun file ->
              match Config_loader.load_scenario Eio.Path.(base / file) with
              | Ok s -> not (List.is_empty s.checkers)
              | Error _ -> false)
            files)
    e.scenarios

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
            Result.map
              (fun e ->
                let base = Eio.Path.(files / Filename.dirname file) in
                Some (Doctor.target ~dir ~checkers:(has_checkers ~base e) e))
              (Config_loader.load_experiment Eio.Path.(files / file))
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
