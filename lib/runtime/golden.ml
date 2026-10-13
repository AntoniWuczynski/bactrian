open Bactrian_core

type expected = Pass | Fail
type offender = { trace : string; expected : expected; reason : string }

type report = {
  scenario : string;
  offenders : offender list;
  errors : string list;
  warnings : string list;
}

let ( let* ) = Result.bind
let checker_timeout = 60.

let rfc3339_date now =
  let tm = Unix.gmtime now in
  Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

let read_trace path =
  match Eio.Path.load path with
  | exception Eio.Io _ -> Error "cannot read the trace"
  | text ->
      String.split_on_char '\n' text
      |> List.filter (fun l -> not (String.equal l ""))
      |> List.fold_left
           (fun acc line ->
             let* entries = acc in
             match Trace.of_line line with
             | Ok e -> Ok (e :: entries)
             | Error e ->
                 Error (Format.asprintf "invalid trace: %a" Trace.pp_error e))
           (Ok [])
      |> Result.map List.rev

let judge ~expected items =
  let problems = Checks.problems items in
  let checker_errors =
    List.exists
      (fun ({ verdict; _ } : Checks.item) ->
        match verdict with Checker_failed _ -> true | Pass | Fail _ -> false)
      items
  in
  let failed =
    List.exists
      (fun ({ verdict; _ } : Checks.item) ->
        match verdict with Fail _ -> true | Pass | Checker_failed _ -> false)
      items
  in
  match expected with
  | Pass ->
      if List.is_empty problems then None
      else Some ("expected to pass but " ^ String.concat "; " problems)
  | Fail ->
      if checker_errors then
        Some ("a checker gave no verdict: " ^ String.concat "; " problems)
      else if failed then None
      else Some "expected to fail but every assertion and checker passed"

let passed r = List.is_empty r.offenders && List.is_empty r.errors

(* Assertions and checkers that no golden trace exercises prove nothing: with
   only [pass] traces a lax one (that always passes) goes unnoticed. *)
let coverage (scenario : Scenario.t) =
  let has_items =
    not (List.is_empty scenario.assertions && List.is_empty scenario.checkers)
  in
  match (has_items, scenario.golden_pass, scenario.golden_fail) with
  | false, _, _ -> ([], [])
  | true, [], [] ->
      ( [
          "no golden traces: add golden.pass and golden.fail traces so the \
           assertions and checkers are exercised";
        ],
        [] )
  | true, _ :: _, [] ->
      ( [],
        [
          "no golden.fail trace, so an assertion or checker that always passes \
           would go unnoticed";
        ] )
  | true, _, _ :: _ -> ([], [])

let check ~env ~scenario_file =
  let fs = Eio.Stdenv.fs env in
  let* scenario = Config_loader.load_scenario Eio.Path.(fs / scenario_file) in
  let dir = Filename.dirname scenario_file in
  let date = rfc3339_date (Eio.Time.now (Eio.Stdenv.clock env)) in
  let offender expected trace =
    let verdict =
      match read_trace Eio.Path.(fs / dir / trace) with
      | Error reason -> Some reason
      | Ok entries ->
          let date = Option.value (Checks.trace_date entries) ~default:date in
          judge ~expected
            (Checks.evaluate
               ~process_mgr:(Eio.Stdenv.process_mgr env)
               ~clock:(Eio.Stdenv.clock env) ~timeout:checker_timeout
               ~cwd:Eio.Path.(fs / dir)
               ~date ~assertions:scenario.assertions ~checkers:scenario.checkers
               entries)
    in
    Option.map (fun reason -> { trace; expected; reason }) verdict
  in
  let errors, warnings = coverage scenario in
  Ok
    {
      scenario = scenario_file;
      offenders =
        List.filter_map (offender Pass) scenario.golden_pass
        @ List.filter_map (offender Fail) scenario.golden_fail;
      errors;
      warnings;
    }
