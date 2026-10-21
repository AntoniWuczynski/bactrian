type probe = {
  outside : string;
  inside : string;
  denied : string;
  masked : string;
  masked_case : string;
  root : string;
  other_run : string;
}

type command = Agent of string list | Probe of probe
type entry_kind = File | Dir | Special | Link_to_file of string | Other_link
type entry = { name : string; kind : entry_kind; masked_file : bool }

type input = {
  kind : Experiment.sandbox_runtime;
  runtime : string;
  name : string;
  labels : (string * string) list;
  uid : int;
  gid : int;
  image : string;
  workdir : string;
  env_file : string;
  relay : string;
  project_root : string;
  bactrian_dir : string;
  runs_dir : string;
  state_dir : string;
  temp_dir : string;
  allow_write : (string * Sandbox_policy.kind) list;
  read_only : string list;
  deny : (string * Sandbox_policy.kind) list;
  staging_dir : string;
  forwards : int list;
  command : command;
  list_dir : string -> (entry list, string) result;
}

type link = { name : string; source : string }

type stage = {
  dir : string;
  over : string;
  links : link list;
  mount_points : string list;
}

type invocation = { argv : string list; stages : stage list }

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
  | Root_user
  | Writable_mask_dir of { file : string; dir : string }
  | File_allow_write of string

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
  | Root_user ->
      "the agent cannot run as root (user ID 0) in container: its masks are \
       directories owned by root, which only another user cannot write to"
  | Writable_mask_dir { file; dir } ->
      Printf.sprintf
        "%s cannot be hidden in container: its directory %s is writable \
         (sandbox.allow_write or the run's own), and container shows the \
         directory of a hidden file read-only. Fix: move the file, or make \
         only a subdirectory of %s writable"
        file dir dir
  | File_allow_write path ->
      Printf.sprintf
        "sandbox.allow_write %s is a file, which container cannot mount (it \
         mounts directories only). Fix: name its directory"
        path

let relay_path = "/bactrian/relay"
let relay_dir = "/bactrian"

(* Fixed limits, so an agent that forks without end or fills memory or /tmp
   starves neither the virtual machine's other containers nor, on Linux, the
   host. 1024 processes is far above what a Node or Python agent starts, 4 GiB
   covers a large agent's heap, and swap is set equal to memory so there is
   none. *)
let limits = [ "--pids-limit"; "1024"; "--memory"; "4g"; "--memory-swap"; "4g" ]

(* container has no --pids-limit. Each container is a virtual machine of its
   own, where the agent's user is the only one but root, so a limit on that
   user's processes is a limit on the container's. It has no swap setting. *)
let container_limits = [ "--ulimit"; "nproc=1024:1024"; "--memory"; "4g" ]

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

(* A file is masked by replacing its directory ([Mask_parent]) and mounting
   every other entry of it again. A bind over the file itself covers one name
   only, and on a case-insensitive host (APFS) the same file is reached under
   another case or Unicode normalisation of its name. In the replacement a
   lookup finds only what was put there, so no other spelling finds the file.
   Docker replaces the directory with an empty read-only tmpfs and binds each
   entry back. container mounts directories only, so it binds a staging
   directory over it instead, read-only, holding a hard link of each other file
   and an empty directory for each mount beneath. *)
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

let bind_value ~source ~target ~writable =
  Printf.sprintf "type=bind,source=%s,target=%s%s" source target
    (if writable then "" else ",readonly")

let mount_args (kind : Experiment.sandbox_runtime) ~stage_of = function
  | Bind { path; writable } ->
      [ "--mount"; bind_value ~source:path ~target:path ~writable ]
  | Mask_dir path | Mask_parent path -> (
      match kind with
      (* A tmpfs took the mode of the directory it covers (0700 for every
         directory bactrian makes) in a test on OrbStack, which stopped the
         agent reaching its own directories beneath the mask over the runs. *)
      | `Docker -> [ "--tmpfs"; path ^ ":ro,mode=0755" ]
      | `Container -> (
          match stage_of path with
          | Some stage ->
              [
                "--mount"; bind_value ~source:stage ~target:path ~writable:false;
              ]
          (* container cannot make a mount point in a read-only tmpfs, so its
             directory masks are writable ones owned by root, which the agent
             (never root) cannot write to. *)
          | None -> [ "--tmpfs"; path ^ ":rw,mode=0755,size=1m" ]))
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
  (* A writable path is hidden by any directory mask over it, except that
     every writable path comes back under the mask over .bactrian and the run's
     own directories under the mask over every run. *)
  let hidden ~own path =
    List.exists
      (fun d ->
        within ~dir:d path
        && not
             (String.equal d i.bactrian_dir
             || (own && String.equal d i.runs_dir)))
      dirs
  in
  let masked path = List.exists (String.equal path) (dirs @ files) in
  let binds =
    (if masked i.project_root then []
     else [ Bind { path = i.project_root; writable = false } ])
    @ List.filter_map
        (fun (own, writable, path) ->
          if hidden ~own path || masked path then None
          else Some (Bind { path; writable }))
        (List.map (fun p -> (true, true, p)) [ i.state_dir; i.temp_dir ]
        @ List.map (fun p -> (true, false, p)) i.read_only
        @ List.map (fun (p, _) -> (false, true, p)) i.allow_write)
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

(* What goes back into a masked file's directory: mounts and, in container's
   staging directory, hard links. *)
type item = Mount of mount | Link of link

(* The entries of a masked file's directory, back as the plain mounts showed
   them. One that is a masked file under any name (a hard link, a link to it)
   is left out, and so is a link to anything but a file the container shows,
   because a bind or a hard link follows the link and would show its target
   here, unmasked. Entries that have a mount of their own keep it. Docker binds
   each entry back. container links each file and binds each directory, and
   leaves out what is neither (a socket, a device). *)
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
    let* items =
      map_all
        (fun (e : entry) ->
          let path = Filename.concat dir e.name in
          let bind writable =
            let* () = check_path "an entry beside a hidden file" path in
            Ok (Some (Mount (Bind { path; writable })))
          and link source = Ok (Some (Link { name = e.name; source })) in
          if e.masked_file || taken path then Ok None
          else
            match (i.kind, e.kind, shows plain path) with
            | `Docker, (File | Dir | Special), Some writable -> bind writable
            | `Container, File, Some _ -> link path
            | `Container, Dir, Some writable -> bind writable
            | (`Docker | `Container), Link_to_file target, Some _ -> (
                match (i.kind, shows plain target) with
                | `Docker, Some writable -> bind writable
                | `Container, Some _ -> link target
                | (`Docker | `Container), None -> Ok None)
            | `Container, Special, Some _
            | (`Docker | `Container), Other_link, Some _
            | ( (`Docker | `Container),
                (File | Dir | Special | Link_to_file _ | Other_link),
                None ) ->
                Ok None)
        listing
    in
    Ok (List.filter_map Fun.id items)

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
        let dir = Filename.dirname f in
        if String.equal dir "/" then Error (Mask_in_root f)
        else
          match (i.kind, shows plain dir) with
          | `Container, Some true -> Error (Writable_mask_dir { file = f; dir })
          | `Container, (Some false | None) | `Docker, _ -> Ok ())
      files
  in
  let* entries = map_all (entries_of i plain ~parents ~files) parents in
  let replaced m =
    match m with
    | Mask_file _ -> true
    | Bind { path; _ } -> List.exists (String.equal path) parents
    | Mask_dir _ | Mask_parent _ -> false
  in
  let all_mounts =
    List.stable_sort
      (fun a b ->
        match Int.compare (depth (mount_path a)) (depth (mount_path b)) with
        | 0 -> String.compare (mount_path a) (mount_path b)
        | c -> c)
      (List.filter (fun m -> not (replaced m)) plain
      @ List.map (fun d -> Mask_parent d) parents
      @ List.filter_map
          (function Mount m -> Some m | Link _ -> None)
          (List.concat entries))
  in
  (* In container, a staging directory for each masked file's directory: its
     links, and an empty directory for each mount right beneath it, because
     container cannot make a mount point in a read-only mount. *)
  let stages =
    match i.kind with
    | `Docker -> []
    | `Container ->
        List.mapi
          (fun n (over, items) ->
            {
              dir = Filename.concat i.staging_dir (string_of_int n);
              over;
              links =
                List.filter_map
                  (function Link l -> Some l | Mount _ -> None)
                  items;
              mount_points =
                List.filter_map
                  (fun m ->
                    let p = mount_path m in
                    if
                      String.equal (Filename.dirname p) over
                      && not (String.equal p over)
                    then Some (Filename.basename p)
                    else None)
                  all_mounts;
            })
          (List.combine parents entries)
  in
  Ok (all_mounts, stages)

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
         ("the staging directory", i.staging_dir);
       ]
      @ List.map (fun (p, _) -> ("sandbox.allow_write", p)) i.allow_write
      @ List.map (fun p -> ("a read-only path", p)) i.read_only
      @ List.map (fun (p, _) -> ("the deny-list path", p)) i.deny)
  in
  let* () =
    match i.kind with
    | `Docker -> Ok ()
    | `Container ->
        let* () = if i.uid = 0 then Error Root_user else Ok () in
        all
          (fun (p, k) ->
            match k with
            | `File -> Error (File_allow_write p)
            | `Dir | `Absent -> Ok ())
          i.allow_write
  in
  let* () = all check_port i.forwards in
  let* mounts, stages = mounts i in
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
    | Probe p ->
        let paths =
          [
            p.outside;
            p.inside;
            p.denied;
            p.masked;
            p.masked_case;
            p.root;
            p.other_run;
          ]
        in
        let* () = all check_argument paths in
        Ok ("--probe" :: paths)
  in
  let* () =
    all check_argument
      ([ i.runtime; i.name; i.image; i.env_file; i.workdir ]
      @ List.concat_map (fun (k, v) -> [ k; v ]) i.labels)
  in
  let stage_of path =
    Option.map
      (fun (s : stage) -> s.dir)
      (List.find_opt (fun (s : stage) -> String.equal s.over path) stages)
  in
  let start, flags, entrypoint, relay_mount =
    match i.kind with
    | `Docker ->
        ( [ "run"; "--rm"; "-i" ],
          [ "--security-opt"; "no-new-privileges" ]
          @ [
              "--user"; Printf.sprintf "%d:%d" i.uid i.gid; "--tmpfs"; tmp_mount;
            ]
          @ limits
          @ [ "--log-driver"; "none"; "--pull"; "never" ],
          relay_path,
          bind_value ~source:i.relay ~target:relay_path ~writable:false )
    | `Container ->
        (* The relay's directory, because container mounts directories only,
           and /tmp at mode 1777 like a host's, because container's tmpfs is
           0755 and owned by root. *)
        ( [ "create"; "--rm" ],
          [
            "--user";
            Printf.sprintf "%d:%d" i.uid i.gid;
            "--tmpfs";
            tmp_mount ^ ",mode=1777";
          ]
          @ container_limits,
          Filename.concat relay_dir (Filename.basename i.relay),
          bind_value ~source:(Filename.dirname i.relay) ~target:relay_dir
            ~writable:false )
  in
  Ok
    {
      argv =
        (i.runtime :: start) @ [ "--name"; i.name ]
        @ List.concat_map (fun (k, v) -> [ "--label"; k ^ "=" ^ v ]) i.labels
        @ [ "--network"; "none"; "--read-only"; "--cap-drop"; "ALL" ]
        @ flags @ [ "--workdir"; i.workdir ]
        @ [ "--env-file"; i.env_file; "--entrypoint"; entrypoint ]
        @ [ "--mount"; relay_mount ]
        @ List.concat_map (mount_args i.kind ~stage_of) mounts
        @ [ "--"; i.image ]
        @ List.concat_map (fun p -> [ "--forward"; string_of_int p ]) i.forwards
        @ tail;
      stages;
    }

(* docker/cli reads the file with a bufio.Scanner, whose token limit is 64 KiB,
   and drops a carriage return at the end of a line. *)
let max_line = 65535

(* container's port of that reader splits the file with Swift's
   [.newlines] set: U+000A to U+000D, U+0085, U+2028 and U+2029. *)
let line_breaks =
  [ "\n"; "\x0b"; "\x0c"; "\r"; "\u{0085}"; "\u{2028}"; "\u{2029}" ]

let holds s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

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
  else if List.exists (holds line) line_breaks then
    bad
      "it holds a line break (U+000A to U+000D, U+0085, U+2028 or U+2029), \
       which would end its line early"
  else if String.contains line '\000' then bad "it holds a NUL byte"
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
