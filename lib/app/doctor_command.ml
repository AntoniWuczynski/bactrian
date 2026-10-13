open Bactrian_runtime

type request = { experiment : string option }

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
            Result.map
              (fun e -> Some (Doctor.target ~dir e))
              (Config_loader.load_experiment
                 Eio.Path.(
                   Option.value files ~default:(Eio.Stdenv.fs env) / file))
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
