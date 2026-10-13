open Bactrian_runtime

type request = { run : string }
type t = { run_id : string; divergences : Divergences.t list }

let internal run_id reason =
  Error
    (Command_error.Internal
       (Printf.sprintf "run %s: divergences.json: %s" run_id reason))

(* A directory names its run only when it is that run's own directory under
   .bactrian/runs: reading another run of the same name would answer a
   question that was not asked. *)
let same_dir a b =
  match (Unix.realpath a, Unix.realpath b) with
  | a, b -> String.equal a b
  | exception Unix.Unix_error _ -> false

let run ~env r =
  let run_id = Run.recording_id r.run in
  let store = Store.create ~root:Eio.Path.(Eio.Stdenv.cwd env / ".bactrian") in
  let trimmed =
    if String.ends_with ~suffix:"/" r.run then
      String.sub r.run 0 (String.length r.run - 1)
    else r.run
  in
  if
    String.contains trimmed '/'
    && not
         (same_dir (Filename.dirname trimmed)
            (Filename.concat ".bactrian" "runs"))
  then
    Error
      (Command_error.Usage
         (Printf.sprintf "%S is not a run directory under .bactrian/runs" r.run))
  else
    match Store.run_dir store ~run_id with
    | Error e ->
        Error (Command_error.Usage (Format.asprintf "%a" Store.pp_error e))
    | Ok dir when not (Eio.Path.is_directory dir) ->
        Error (Command_error.Usage (Printf.sprintf "no run %S" run_id))
    | Ok dir -> (
        match Eio.Path.load Eio.Path.(dir / "divergences.json") with
        | exception Eio.Io (Eio.Fs.E (Not_found _), _) ->
            Ok { run_id; divergences = [] }
        | exception Eio.Io _ -> internal run_id "cannot be read"
        | text -> (
            match Yojson.Safe.from_string text with
            | exception Yojson.Json_error m -> internal run_id m
            | json -> (
                match Divergences.of_file_json json with
                | Error m -> internal run_id m
                | Ok divergences -> Ok { run_id; divergences })))

let to_json t =
  `Assoc
    [
      ("run_id", `String t.run_id);
      ("divergences", `List (List.map Divergences.to_json t.divergences));
    ]
