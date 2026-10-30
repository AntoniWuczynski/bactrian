open Bactrian_runtime

type request = { experiment : string option }

(* Whether any scenario of the experiment has checkers, and what is wrong with
   each scenario that does not load. One that does not load may have checkers,
   so it counts as having them: a broken scenario must not hide the checkers'
   sandbox line. A glob that matches nothing counts as no scenarios: [run]
   reports it, and [doctor] reads the scenarios for nothing else. *)
let scenario_checkers ~base (e : Bactrian_core.Experiment.t) =
  let files =
    List.concat_map
      (fun glob ->
        Result.value ~default:[] (Config_loader.expand_globs ~base [ glob ]))
      e.scenarios
  in
  let loaded, errors =
    List.partition_map
      (fun file ->
        match Config_loader.load_scenario Eio.Path.(base / file) with
        | Ok s -> Left s
        | Error es -> Right es)
      files
  in
  let errors = List.concat errors in
  ( (not (List.is_empty errors))
    || List.exists
         (fun (s : Bactrian_core.Scenario.t) -> not (List.is_empty s.checkers))
         loaded,
    errors )

(* A run could not start with such a scenario, but the other checks do not
   depend on the scenarios, so this is one failing check among them. *)
let scenarios_check = function
  | [] -> []
  | _ :: _ as errors ->
      [
        {
          Doctor.name = "scenarios";
          status = Fail;
          message =
            "a scenario does not load:\n"
            ^ String.concat "\n" (List.map Config_loader.located_line errors);
        };
      ]

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
                let checkers, errors = scenario_checkers ~base e in
                Some (Doctor.target ~dir ~checkers e, errors))
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
                  ~path:(Sys.getenv_opt "PATH") (Option.map fst target)
               @ scenarios_check
                   (match target with Some (_, es) -> es | None -> []))))

let exit_code (report : Doctor.report) =
  if report.ok then Exit_code.ok else Exit_code.environment

let to_json = Doctor.to_json
