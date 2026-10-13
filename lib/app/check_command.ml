open Bactrian_runtime

type request = { scenarios : string list; sandbox : Sandbox.mode }
type t = { reports : Golden.report list; sandbox : Sandbox.label option }

let ( let* ) = Result.bind

(* The probe runs only when a checker will, so a scenario without one never
   depends on the host's sandbox. *)
let checker_sandbox ~on_progress ~bactrian_exe ~env ~mode scenarios =
  if
    List.exists
      (fun (_, (s : Bactrian_core.Scenario.t)) ->
        not (List.is_empty s.checkers))
      scenarios
  then
    Result.map Option.some
      (Result.map_error
         (fun m -> Command_error.Environment m)
         (Sandbox.preflight ~env
            ~warn:(fun w -> on_progress (Progress.Warning w))
            {
              mode;
              allow_write = [];
              deny_read = [];
              bactrian_exe;
              bactrian_dir =
                Eio.Path.native_exn Eio.Path.(Eio.Stdenv.cwd env / ".bactrian");
            }))
  else Ok None

let run ?files ?vet ~on_progress ~bactrian_exe ~env r =
  Mirage_crypto_rng_unix.use_default ();
  let files = Option.value files ~default:(Eio.Stdenv.fs env) in
  let loaded =
    List.map
      (fun scenario_file ->
        Result.map
          (fun s -> (scenario_file, s))
          (Golden.load ~files ?vet ~scenario_file ()))
      r.scenarios
  in
  match List.concat_map (function Ok _ -> [] | Error e -> e) loaded with
  | _ :: _ as errors -> Error (Command_error.Configuration errors)
  | [] ->
      let scenarios = List.filter_map Result.to_option loaded in
      let* sandbox =
        checker_sandbox ~on_progress ~bactrian_exe ~env ~mode:r.sandbox
          scenarios
      in
      Ok
        {
          reports =
            List.map
              (fun (scenario_file, scenario) ->
                Golden.check ~files ~env
                  ~sandbox:(Option.value sandbox ~default:Sandbox.off)
                  ~scenario_file scenario)
              scenarios;
          sandbox = Option.map Sandbox.label sandbox;
        }

let passed t = List.for_all Golden.passed t.reports
let exit_code t = if passed t then Exit_code.ok else Exit_code.failed
let strings l = `List (List.map (fun s -> `String s) l)

let offender_json (o : Golden.offender) =
  `Assoc
    [
      ("trace", `String o.trace);
      ( "expected",
        `String (match o.expected with Pass -> "pass" | Fail -> "fail") );
      ("reason", `String o.reason);
    ]

let to_json t =
  `Assoc
    [
      ("passed", `Bool (passed t));
      ( "sandbox",
        match t.sandbox with Some l -> Sandbox.label_json l | None -> `Null );
      ( "scenarios",
        `List
          (List.map
             (fun (r : Golden.report) ->
               `Assoc
                 [
                   ("scenario", `String r.scenario);
                   ("passed", `Bool (Golden.passed r));
                   ("offenders", `List (List.map offender_json r.offenders));
                   ("errors", strings r.errors);
                   ("warnings", strings r.warnings);
                 ])
             t.reports) );
    ]
