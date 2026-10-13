let ( let* ) = Result.bind

type t = { root : string }

let create dir =
  match Unix.realpath dir with
  | root -> Ok { root }
  | exception Unix.Unix_error (e, _, _) ->
      Error (Printf.sprintf "cannot resolve %s: %s" dir (Unix.error_message e))

let root t = t.root

let is_symlink p =
  match Unix.lstat p with
  | { st_kind = S_LNK; _ } -> true
  | { st_kind = S_REG | S_DIR | S_CHR | S_BLK | S_FIFO | S_SOCK; _ } -> false
  | exception Unix.Unix_error _ -> false

(* The path with every existing prefix resolved. A missing tail is kept as
   written, except that it may not climb. *)
let rec resolve p =
  match Unix.realpath p with
  | r -> Ok r
  | exception Unix.Unix_error ((ENOENT | ENOTDIR), _, _) ->
      let parent = Filename.dirname p and base = Filename.basename p in
      if is_symlink p then Error "is a symbolic link that leads nowhere"
      else if String.equal parent p then Error "does not exist"
      else if String.equal base Filename.parent_dir_name then
        Error "climbs out of a directory that does not exist"
      else
        let* r = resolve parent in
        Ok
          (if String.equal base Filename.current_dir_name then r
           else Filename.concat r base)
  | exception Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)

let inside t p =
  String.equal p t.root
  || String.starts_with ~prefix:(Filename.concat t.root "") p

let has_nul s = String.contains s '\000'

let path t ~field p =
  let fail reason = Error (Printf.sprintf "%s: %S %s" field p reason) in
  if String.equal p "" then fail "is empty"
  else if has_nul p then fail "holds a NUL byte"
  else
    let absolute =
      if Filename.is_relative p then Filename.concat t.root p else p
    in
    match resolve absolute with
    | Error reason -> fail reason
    | Ok r when inside t r -> Ok ()
    | Ok _ -> fail "is outside the server's working directory"

let id ~field s =
  if
    String.equal s "" || String.equal s "." || String.equal s ".."
    || String.contains s '/' || has_nul s
  then
    Error
      (Printf.sprintf "%s: %S is not an ID (one name, without '/' or '..')"
         field s)
  else Ok ()

let run t ~field s =
  if String.contains s '/' then path t ~field s else id ~field s
