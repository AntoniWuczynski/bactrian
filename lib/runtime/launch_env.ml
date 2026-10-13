let marker = "BACTRIAN_ENV_FD"

(* SAFETY: [Unix.file_descr] is [int] on every Unix ([type file_descr = int] in
   the unix library), and bactrian builds only for Unix. These convert between
   the descriptor and the number written into {!marker}. *)
external int_of_fd : Unix.file_descr -> int = "%identity"

(* SAFETY: as above. The number comes from {!marker}, which only {!scrub} sets,
   and a number that is not an open descriptor fails with EBADF on use. *)
external fd_of_int : int -> Unix.file_descr = "%identity"

let names =
  [ "PATH"; "HOME"; "USER"; "LOGNAME"; "SHELL"; "LANG"; "TERM"; "TZ"; "TMPDIR" ]

let allowed name = List.mem name names || String.starts_with ~prefix:"LC_" name

let split entry =
  match String.index_opt entry '=' with
  | Some i ->
      Some
        ( String.sub entry 0 i,
          String.sub entry (i + 1) (String.length entry - i - 1) )
  | None -> None

let kept env =
  List.filter
    (fun entry ->
      match split entry with Some (name, _) -> allowed name | None -> false)
    (Array.to_list env)

let encode env =
  String.concat "" (List.map (fun entry -> entry ^ "\000") (Array.to_list env))

let decode text =
  List.filter_map
    (fun entry ->
      match split entry with
      | Some ("", _) | None -> None
      | Some pair -> Some pair)
    (String.split_on_char '\000' text)

let restored = ref false

let describe = function
  | Unix.Unix_error (e, call, "") ->
      Printf.sprintf "%s: %s" call (Unix.error_message e)
  | Unix.Unix_error (e, call, arg) ->
      Printf.sprintf "%s %s: %s" call arg (Unix.error_message e)
  | Sys_error m -> m
  | e -> Printexc.to_string e

(* The environment goes to a temporary file that is unlinked before anything is
   written to it, so no name ever leads to its contents. A pipe would block
   this process before its [exec] once the environment outgrows the pipe's
   buffer, because the reader is the image that [exec] starts. *)
let write_env env =
  let path = Filename.temp_file "bactrian-env-" "" in
  let fd =
    Fun.protect
      ~finally:(fun () -> Sys.remove path)
      (fun () -> Unix.openfile path [ O_RDWR; O_KEEPEXEC ] 0)
  in
  match
    let text = encode env in
    ignore (Unix.write_substring fd text 0 (String.length text) : int);
    ignore (Unix.lseek fd 0 SEEK_SET : int)
  with
  | () -> fd
  | exception e ->
      Unix.close fd;
      raise e

let scrub ~exe =
  let env = Unix.environment () in
  let keep = kept env in
  if !restored || List.compare_length_with keep (Array.length env) = 0 then
    Ok ()
  else
    match
      let exe = Unix.realpath (Proc_util.absolute exe) in
      let fd = write_env env in
      Unix.execve exe Sys.argv
        (Array.of_list
           (keep @ [ Printf.sprintf "%s=%d" marker (int_of_fd fd) ]))
    with
    | result -> result
    | exception ((Unix.Unix_error _ | Sys_error _) as e) ->
        Error ("cannot restart with a scrubbed environment: " ^ describe e)

let restore () =
  match Sys.getenv_opt marker with
  | None | Some "" -> Ok ()
  | Some raw -> (
      match int_of_string_opt raw with
      | Some n when n > 2 -> (
          match
            let ic = Unix.in_channel_of_descr (fd_of_int n) in
            Fun.protect
              ~finally:(fun () -> close_in_noerr ic)
              (fun () -> In_channel.input_all ic)
          with
          | exception ((Unix.Unix_error _ | Sys_error _) as e) ->
              Error
                (Printf.sprintf "cannot read the environment from %s=%d: %s"
                   marker n (describe e))
          | text ->
              List.iter
                (fun (name, value) -> Unix.putenv name value)
                (decode text);
              Unix.putenv marker "";
              restored := true;
              Ok ())
      | Some _ | None ->
          Error
            (Printf.sprintf "%s=%S is not an inherited descriptor" marker raw))

(* /dev/fd lists the descriptors open in the reading process (on Linux through
   /proc/self/fd). The one the listing itself opens is closed again by the time
   it is marked, which fails with EBADF and is skipped. *)
let close_inherited_on_exec () =
  match Sys.readdir "/dev/fd" with
  | exception Sys_error _ -> ()
  | names ->
      Array.iter
        (fun name ->
          match int_of_string_opt name with
          | Some n when n > 2 -> (
              match Unix.set_close_on_exec (fd_of_int n) with
              | () -> ()
              | exception Unix.Unix_error _ -> ())
          | Some _ | None -> ())
        names
