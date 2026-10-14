type command =
  | Agent of string list
  | Probe of { outside : string; inside : string; denied : string }

type entry_kind = Plain | Link_to_file of string | Other_link
type entry = { name : string; kind : entry_kind; masked_file : bool }

type input = {
  runtime : string;
  name : string;
  uid : int;
  gid : int;
  image : string;
  workdir : string;
  env_file : string;
  relay : string;
  project_root : string;
  runs_dir : string;
  state_dir : string;
  temp_dir : string;
  allow_write : string list;
  deny : (string * Sandbox_policy.kind) list;
  forwards : int list;
  command : command;
  list_dir : string -> (entry list, string) result;
}

(* Each entry beside a masked file is one more --mount of about twice its path:
   1000 of them at 300 bytes is 0.3 MiB of arguments, well under the 1 MiB
   ARG_MAX of macOS. A project root or a home directory rarely holds a few
   hundred. *)
let max_entries = 1000

type error =
  | Bad_path of { what : string; path : string }
  | Bad_argument of string
  | Bad_variable of { name : string; reason : string }
  | Bad_port of int
  | Empty_agent
  | Hidden_workdir of string
  | Mask_in_root of string
  | Crowded_dir of { file : string; dir : string; entries : int }
  | Unlistable of { dir : string; reason : string }

let describe_error = function
  | Bad_path { what; path } ->
      Printf.sprintf
        "%s %S cannot be mounted: it must be absolute and hold no comma, \
         colon, double quote or control character"
        what path
  | Bad_argument a -> Printf.sprintf "the argument %S holds a NUL byte" a
  | Bad_variable { name; reason } ->
      Printf.sprintf "the variable %S cannot be passed to the container: %s"
        name reason
  | Bad_port p -> Printf.sprintf "%d is not a port from 1 to 65535" p
  | Empty_agent -> "no agent command"
  | Hidden_workdir d ->
      Printf.sprintf
        "the agent's working directory %s is outside the project, its run and \
         sandbox.allow_write, so the virtual machine does not show it"
        d
  | Mask_in_root f ->
      Printf.sprintf
        "%s cannot be hidden in the virtual machine: it is in /, which cannot \
         be replaced by an empty directory"
        f
  | Crowded_dir { file; dir; entries } ->
      Printf.sprintf
        "%s cannot be hidden in the virtual machine: its directory %s holds %d \
         entries, more than the %d that are mounted again around it"
        file dir entries max_entries
  | Unlistable { dir; reason } ->
      Printf.sprintf
        "the directory %s, which holds a hidden file, cannot be listed: %s" dir
        reason

let relay_path = "/bactrian/relay"

(* Fixed limits, so an agent that forks without end or fills memory or /tmp
   starves neither the virtual machine's other containers nor, on Linux, the
   host. 1024 processes is far above what a Node or Python agent starts, 4 GiB
   covers a large agent's heap, and swap is set equal to memory so there is
   none. *)
let limits = [ "--pids-limit"; "1024"; "--memory"; "4g"; "--memory-swap"; "4g" ]

(* Written out, rather than left to the daemon, so they hold whatever its
   defaults are. *)
let tmp_mount = "/tmp:rw,noexec,nosuid,nodev,size=1g"
let ( let* ) = Result.bind

(* A --mount value is comma-separated, with double quotes for quoting, and a
   --tmpfs value ends at its first colon, so a path holding any of them is
   refused rather than quoted. *)
let check_path what path =
  if
    (not (Filename.is_relative path))
    && String.for_all
         (fun c ->
           not
             (Char.equal c ',' || Char.equal c ':' || Char.equal c '"'
             || Char.code c < 0x20))
         path
  then Ok ()
  else Error (Bad_path { what; path })

let check_argument a =
  if String.contains a '\000' then Error (Bad_argument a) else Ok ()

(* Mounts *)

(* A file is masked by replacing its directory ([Mask_parent]) with an empty
   read-only tmpfs and mounting every other entry of it again. A bind over the
   file itself covers one name only, and on a case-insensitive host (APFS) the
   same file is reached under another case or Unicode normalisation of its name.
   In the tmpfs a lookup is exact, so no other spelling finds anything. *)
type mount =
  | Bind of { path : string; writable : bool }
  | Mask_dir of string
  | Mask_file of string
  | Mask_parent of string

let mount_path = function
  | Bind { path; _ } | Mask_dir path | Mask_file path | Mask_parent path -> path

let depth path =
  List.length
    (List.filter
       (fun s -> not (String.equal s ""))
       (String.split_on_char '/' path))

let within ~dir path =
  String.equal dir path || String.equal dir "/"
  || Sandbox_policy.is_under ~dir path

let mount_args = function
  | Bind { path; writable } ->
      [
        "--mount";
        Printf.sprintf "type=bind,source=%s,target=%s%s" path path
          (if writable then "" else ",readonly");
      ]
  | Mask_dir path | Mask_parent path ->
      (* A tmpfs took the mode of the directory it covers (0700 for every
         directory bactrian makes) in a test on OrbStack, which stopped the
         agent reaching its own directories beneath the mask over the runs. *)
      [ "--tmpfs"; path ^ ":ro,mode=0755" ]
  | Mask_file _ -> []

(* The deepest of [mounts] at or above [path]. *)
let deepest mounts path =
  List.fold_left
    (fun best o ->
      if not (within ~dir:(mount_path o) path) then best
      else
        match best with
        | Some b when depth (mount_path b) >= depth (mount_path o) -> best
        | Some _ | None -> Some o)
    None mounts

(* Whether [mounts] show [path], and writable. *)
let shows mounts path =
  match deepest mounts path with
  | Some (Bind { writable; _ }) -> Some writable
  | Some (Mask_dir _ | Mask_file _ | Mask_parent _) | None -> None

(* The mounts of the deny-list, as single binds and masks. *)
let plain_mounts i =
  let dirs =
    List.sort_uniq String.compare
      (List.filter_map
         (fun (p, k) -> match k with `Dir -> Some p | `File | `Absent -> None)
         i.deny)
  and files =
    List.sort_uniq String.compare
      (List.filter_map
         (fun (p, k) -> match k with `File -> Some p | `Dir | `Absent -> None)
         i.deny)
  in
  (* A writable path is hidden by any directory mask over it, except that the
     run's own directories come back under the mask over every run. *)
  let hidden ~own path =
    List.exists
      (fun d -> within ~dir:d path && not (own && String.equal d i.runs_dir))
      dirs
  in
  let masked path = List.exists (String.equal path) (dirs @ files) in
  let binds =
    (if masked i.project_root then []
     else [ Bind { path = i.project_root; writable = false } ])
    @ List.filter_map
        (fun (own, path) ->
          if hidden ~own path || masked path then None
          else Some (Bind { path; writable = true }))
        (List.map (fun p -> (true, p)) [ i.state_dir; i.temp_dir ]
        @ List.map (fun p -> (false, p)) i.allow_write)
  in
  let masks =
    List.map (fun d -> Mask_dir d) dirs @ List.map (fun f -> Mask_file f) files
  in
  (* A mask counts where the deepest mount above it shows its path. *)
  let shown m =
    let p = mount_path m in
    Option.is_some
      (shows
         (List.filter
            (fun o ->
              (not (String.equal (mount_path o) p))
              && within ~dir:(mount_path o) p)
            (binds @ masks))
         p)
  in
  binds @ List.filter shown masks

let rec all f = function
  | [] -> Ok ()
  | x :: rest ->
      let* () = f x in
      all f rest

let rec map_all f = function
  | [] -> Ok []
  | x :: rest ->
      let* y = f x in
      let* ys = map_all f rest in
      Ok (y :: ys)

(* The entries of a masked file's directory, mounted again as the plain mounts
   showed them. One that is a masked file under any name (a hard link, a link
   to it) is left out, and so is a link to anything but a file the container
   shows, because a bind follows the link and would show its target here,
   unmasked. Entries that have a mount of their own keep it. *)
let entries_of i plain ~parents ~files dir =
  let* listing =
    Result.map_error (fun reason -> Unlistable { dir; reason }) (i.list_dir dir)
  in
  let file =
    Option.value ~default:dir
      (List.find_opt (fun f -> String.equal (Filename.dirname f) dir) files)
  in
  let n = List.length listing in
  if n > max_entries then Error (Crowded_dir { file; dir; entries = n })
  else
    let taken path =
      List.exists (fun m -> String.equal (mount_path m) path) plain
      || List.exists (String.equal path) parents
    in
    let* binds =
      map_all
        (fun (e : entry) ->
          let path = Filename.concat dir e.name in
          if e.masked_file || taken path then Ok None
          else
            let bind writable =
              let* () = check_path "an entry beside a hidden file" path in
              Ok (Some (Bind { path; writable }))
            in
            match (e.kind, shows plain path) with
            | Plain, Some writable -> bind writable
            | Link_to_file target, Some _ -> (
                match shows plain target with
                | Some writable -> bind writable
                | None -> Ok None)
            | (Plain | Link_to_file _ | Other_link), None | Other_link, Some _
              ->
                Ok None)
        listing
    in
    Ok (List.filter_map Fun.id binds)

let mounts i =
  let plain = plain_mounts i in
  let files =
    List.filter_map
      (function
        | Mask_file f -> Some f | Bind _ | Mask_dir _ | Mask_parent _ -> None)
      plain
  in
  let parents =
    List.sort_uniq String.compare (List.map Filename.dirname files)
  in
  let* () =
    all
      (fun f ->
        if String.equal (Filename.dirname f) "/" then Error (Mask_in_root f)
        else Ok ())
      files
  in
  let* entries = map_all (entries_of i plain ~parents ~files) parents in
  let replaced m =
    match m with
    | Mask_file _ -> true
    | Bind { path; _ } -> List.exists (String.equal path) parents
    | Mask_dir _ | Mask_parent _ -> false
  in
  Ok
    (List.stable_sort
       (fun a b ->
         match Int.compare (depth (mount_path a)) (depth (mount_path b)) with
         | 0 -> String.compare (mount_path a) (mount_path b)
         | c -> c)
       (List.filter (fun m -> not (replaced m)) plain
       @ List.map (fun d -> Mask_parent d) parents
       @ List.concat entries))

let check_port p = if p >= 1 && p <= 65535 then Ok () else Error (Bad_port p)

let argv i =
  let* () =
    all
      (fun (what, p) -> check_path what p)
      ([
         ("the working directory", i.workdir);
         ("the relay", i.relay);
         ("the project root", i.project_root);
         ("the state directory", i.state_dir);
         ("the temporary directory", i.temp_dir);
       ]
      @ List.map (fun p -> ("sandbox.allow_write", p)) i.allow_write
      @ List.map (fun (p, _) -> ("the deny-list path", p)) i.deny)
  in
  let* () = all check_port i.forwards in
  let* mounts = mounts i in
  let* () =
    match shows (plain_mounts i) i.workdir with
    | Some _ -> Ok ()
    | None -> Error (Hidden_workdir i.workdir)
  in
  let* tail =
    match i.command with
    | Agent [] -> Error Empty_agent
    | Agent agent ->
        let* () = all check_argument agent in
        Ok ("--" :: agent)
    | Probe { outside; inside; denied } ->
        let* () = all check_argument [ outside; inside; denied ] in
        Ok [ "--probe"; outside; inside; denied ]
  in
  let* () =
    all check_argument [ i.runtime; i.name; i.image; i.env_file; i.workdir ]
  in
  Ok
    ([ i.runtime; "run"; "--rm"; "-i"; "--name"; i.name ]
    @ [ "--network"; "none"; "--read-only"; "--cap-drop"; "ALL" ]
    @ [ "--security-opt"; "no-new-privileges" ]
    @ [ "--user"; Printf.sprintf "%d:%d" i.uid i.gid; "--tmpfs"; tmp_mount ]
    @ limits
    @ [ "--log-driver"; "none"; "--pull"; "never"; "--workdir"; i.workdir ]
    @ [ "--env-file"; i.env_file; "--entrypoint"; relay_path ]
    @ [
        "--mount";
        Printf.sprintf "type=bind,source=%s,target=%s,readonly" i.relay
          relay_path;
      ]
    @ List.concat_map mount_args mounts
    @ [ "--"; i.image ]
    @ List.concat_map (fun p -> [ "--forward"; string_of_int p ]) i.forwards
    @ tail)

(* docker/cli reads the file with a bufio.Scanner, whose token limit is 64 KiB,
   and drops a carriage return at the end of a line. *)
let max_line = 65535

let env_line (name, value) =
  let bad reason = Error (Bad_variable { name; reason }) in
  let line = name ^ "=" ^ value in
  if String.equal name "" then bad "the name is empty"
  else if String.starts_with ~prefix:"#" name then
    bad "a name starting with # is a comment"
  else if
    String.exists
      (fun c -> Char.equal c '=' || Char.equal c ' ' || Char.equal c '\t')
      name
  then bad "the name holds = or whitespace"
  else if String.contains line '\n' then bad "it holds a newline"
  else if String.contains line '\000' then bad "it holds a NUL byte"
  else if String.ends_with ~suffix:"\r" line then
    bad "it ends with a carriage return"
  else if not (String.is_valid_utf_8 line) then bad "it is not valid UTF-8"
  else if String.length line > max_line then bad "it is longer than 64 KiB"
  else Ok line

let env_file vars =
  let* lines =
    List.fold_left
      (fun acc v ->
        let* l = acc in
        let* line = env_line v in
        Ok (line :: l))
      (Ok []) vars
  in
  Ok (String.concat "" (List.rev_map (fun l -> l ^ "\n") lines))
