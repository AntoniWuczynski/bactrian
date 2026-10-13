open Bactrian_runtime

type request = { scenarios : string list }
type t = Golden.report list

let run ?files ?vet ~env r =
  let results =
    List.map
      (fun scenario_file -> Golden.check ?files ?vet ~env ~scenario_file ())
      r.scenarios
  in
  match List.concat_map (function Ok _ -> [] | Error e -> e) results with
  | _ :: _ as errors -> Error (Command_error.Configuration errors)
  | [] -> Ok (List.filter_map Result.to_option results)

let passed = List.for_all Golden.passed
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
             t) );
    ]
