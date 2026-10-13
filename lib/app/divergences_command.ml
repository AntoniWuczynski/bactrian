open Bactrian_runtime

type request = { run : string }
type t = { run_id : string; divergences : Divergences.t list }

let internal run_id reason =
  Error
    (Command_error.Internal
       (Printf.sprintf "run %s: divergences.json: %s" run_id reason))

let run ~env r =
  let run_id = Run.recording_id r.run in
  let store = Store.create ~root:Eio.Path.(Eio.Stdenv.cwd env / ".bactrian") in
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
