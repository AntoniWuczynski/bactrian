type command =
  | Agent of string list
  | Probe of { outside : string; inside : string; denied : string }

type input = {
  runtime : string;
  name : string;
  uid : int;
  gid : int;
  image : string;
  workdir : string;
  env_file : string;
  relay : string;
  empty_file : string;
  project_root : string;
  runs_dir : string;
  state_dir : string;
  temp_dir : string;
  allow_write : string list;
  deny : (string * Sandbox_policy.kind) list;
  forwards : int list;
  command : command;
}

type error =
  | Bad_path of { what : string; path : string }
  | Bad_argument of string
  | Bad_variable of { name : string; reason : string }
  | Bad_port of int
  | Empty_agent
  | Hidden_workdir of string

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

let relay_path = "/bactrian/relay"
let ( let* ) = Result.bind

let rec all f = function
  | [] -> Ok ()
  | x :: rest ->
      let* () = f x in
      all f rest

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

type mount =
  | Bind of { path : string; writable : bool }
  | Mask_dir of string
  | Mask_file of string

let mount_path = function
  | Bind { path; _ } | Mask_dir path | Mask_file path -> path

let depth path =
  List.length
    (List.filter
       (fun s -> not (String.equal s ""))
       (String.split_on_char '/' path))

let within ~dir path =
  String.equal dir path || Sandbox_policy.is_under ~dir path

let mount_args ~empty_file = function
  | Bind { path; writable } ->
      [
        "--mount";
        Printf.sprintf "type=bind,source=%s,target=%s%s" path path
          (if writable then "" else ",readonly");
      ]
  | Mask_dir path ->
      (* A tmpfs took the mode of the directory it covers (0700 for every
         directory bactrian makes) in a test on OrbStack, which stopped the
         agent reaching its own directories beneath the mask over the runs. *)
      [ "--tmpfs"; path ^ ":ro,mode=0755" ]
  | Mask_file path ->
      [
        "--mount";
        Printf.sprintf "type=bind,source=%s,target=%s,readonly" empty_file path;
      ]

let mounts i =
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
    let above =
      List.filter
        (fun o -> Sandbox_policy.is_under ~dir:(mount_path o) p)
        (binds @ masks)
    in
    match
      List.fold_left
        (fun best o ->
          match best with
          | Some b when depth (mount_path b) >= depth (mount_path o) -> best
          | Some _ | None -> Some o)
        None above
    with
    | Some (Bind _) -> true
    | Some (Mask_dir _ | Mask_file _) | None -> false
  in
  List.stable_sort
    (fun a b -> Int.compare (depth (mount_path a)) (depth (mount_path b)))
    (binds @ List.filter shown masks)

let check_port p = if p >= 1 && p <= 65535 then Ok () else Error (Bad_port p)

let argv i =
  let* () =
    all
      (fun (what, p) -> check_path what p)
      ([
         ("the working directory", i.workdir);
         ("the relay", i.relay);
         ("the empty file", i.empty_file);
         ("the project root", i.project_root);
         ("the state directory", i.state_dir);
         ("the temporary directory", i.temp_dir);
       ]
      @ List.map (fun p -> ("sandbox.allow_write", p)) i.allow_write
      @ List.map (fun (p, _) -> ("the deny-list path", p)) i.deny)
  in
  let* () = all check_port i.forwards in
  let* () =
    if
      List.exists
        (fun m ->
          match m with
          | Bind { path; _ } -> within ~dir:path i.workdir
          | Mask_dir _ | Mask_file _ -> false)
        (mounts i)
    then Ok ()
    else Error (Hidden_workdir i.workdir)
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
    @ [ "--user"; Printf.sprintf "%d:%d" i.uid i.gid; "--tmpfs"; "/tmp" ]
    @ [ "--log-driver"; "none"; "--pull"; "never"; "--workdir"; i.workdir ]
    @ [ "--env-file"; i.env_file; "--entrypoint"; relay_path ]
    @ [
        "--mount";
        Printf.sprintf "type=bind,source=%s,target=%s,readonly" i.relay
          relay_path;
      ]
    @ List.concat_map (mount_args ~empty_file:i.empty_file) (mounts i)
    @ [ i.image ]
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
