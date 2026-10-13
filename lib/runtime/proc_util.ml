let hex s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02x" (Char.code s.[i])))

let absolute ?cwd path =
  if Filename.is_relative path then
    Filename.concat (match cwd with Some d -> d | None -> Sys.getcwd ()) path
  else path

let regular_file path =
  match (Unix.stat path).st_kind with
  | S_REG -> true
  | S_DIR | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> false
  | exception Unix.Unix_error _ -> false

let executable_at path =
  if
    regular_file path
    &&
    match Unix.access path [ X_OK ] with
    | () -> true
    | exception Unix.Unix_error _ -> false
  then Some path
  else None

let find_executable ~path ~cwd name =
  if String.contains name '/' then executable_at (absolute ~cwd name)
  else
    String.split_on_char ':' (Option.value path ~default:"")
    |> List.find_map (fun dir ->
        if String.equal dir "" then None
        else executable_at (Filename.concat (absolute ~cwd dir) name))

let describe_exit : Eio.Process.exit_status -> string = function
  | `Exited n -> Printf.sprintf "exit status %d" n
  | `Signaled n -> Printf.sprintf "killed by signal %d" n
