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

let random_hex n =
  let st = Random.State.make_self_init () in
  Proc_util.hex (String.init n (fun _ -> Char.chr (Random.State.int st 256)))

(* The environment goes to a temporary file that is created exclusively under a
   random name and unlinked before anything is written to it, so no name ever
   leads to its contents. A pipe would block this process before its [exec]
   once the environment outgrows the pipe's buffer, because the reader is the
   image that [exec] starts. *)
let rec create_unlinked attempts =
  let path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      ("bactrian-env-" ^ random_hex 8)
  in
  match Unix.openfile path [ O_RDWR; O_CREAT; O_EXCL; O_KEEPEXEC ] 0o600 with
  | fd -> (
      match Unix.unlink path with
      | () -> fd
      | exception (Unix.Unix_error _ as e) ->
          Unix.close fd;
          raise e)
  | exception Unix.Unix_error (EEXIST, _, _) when attempts > 1 ->
      create_unlinked (attempts - 1)

(* With standard input, output or error closed, the file could get descriptor
   0 to 2, which {!restore} does not accept. *)
let rec above_std fd =
  if int_of_fd fd > 2 then fd
  else
    let next = Unix.dup ~cloexec:false fd in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> above_std next)

(* The nonce comes first, so {!restore} reads only a file this process wrote. *)
let write_env ~nonce env =
  let fd = above_std (create_unlinked 5) in
  let text = nonce ^ "\000" ^ encode env in
  match
    ignore (Unix.write_substring fd text 0 (String.length text) : int);
    ignore (Unix.lseek fd 0 SEEK_SET : int)
  with
  | () -> fd
  | exception (Unix.Unix_error _ as e) ->
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
      let nonce = random_hex 16 in
      let fd = write_env ~nonce env in
      let vars =
        Array.of_list
          (keep @ [ Printf.sprintf "%s=%d:%s" marker (int_of_fd fd) nonce ])
      in
      try Unix.execve exe Sys.argv vars
      with Unix.Unix_error _ as e ->
        Unix.close fd;
        raise e
    with
    | result -> result
    | exception ((Unix.Unix_error _ | Sys_error _) as e) ->
        Error ("cannot restart with a scrubbed environment: " ^ describe e)

(* Only a regular file is read, so a stray marker that names a terminal or a
   pipe cannot block the start. *)
let read_env ~nonce n =
  let fd = fd_of_int n in
  match (Unix.fstat fd).st_kind with
  | S_DIR | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> None
  | S_REG ->
      let ic = Unix.in_channel_of_descr fd in
      let text =
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () -> In_channel.input_all ic)
      in
      let prefix = nonce ^ "\000" in
      if String.starts_with ~prefix text then
        let n = String.length prefix in
        Some (String.sub text n (String.length text - n))
      else None

let restore () =
  match Sys.getenv_opt marker with
  | None | Some "" -> Ok ()
  | Some raw -> (
      let not_ours () =
        Error
          (Printf.sprintf "%s=%S does not name the environment bactrian wrote"
             marker raw)
      in
      match String.index_opt raw ':' with
      | None -> not_ours ()
      | Some i -> (
          let nonce = String.sub raw (i + 1) (String.length raw - i - 1) in
          match int_of_string_opt (String.sub raw 0 i) with
          | Some n when n > 2 && not (String.equal nonce "") -> (
              match read_env ~nonce n with
              | exception ((Unix.Unix_error _ | Sys_error _) as e) ->
                  Error
                    (Printf.sprintf "cannot read the environment from %s=%s: %s"
                       marker raw (describe e))
              | None -> not_ours ()
              | Some text ->
                  List.iter
                    (fun (name, value) -> Unix.putenv name value)
                    (decode text);
                  Unix.putenv marker "";
                  restored := true;
                  Ok ())
          | Some _ | None -> not_ours ()))

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
