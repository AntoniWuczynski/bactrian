type platform = Macos | Linux
type listener = { port : int; socket : string option }

type input = {
  platform : platform;
  home : string;
  project_root : string;
  bactrian_dir : string;
  state_dir : string;
  temp_dir : string;
  allow_write : string list;
  deny_read : string list;
  listeners : listener list;
  agent : string list;
  bactrian_exe : string;
  tag : string;
}

type error =
  | Empty_agent
  | Nul_byte of { what : string; value : string }
  | Not_absolute of { what : string; path : string }
  | Invalid_port of int
  | Duplicate_port of int
  | Missing_socket of int
  | Empty_tag

let describe_error = function
  | Empty_agent -> "the agent command is empty"
  | Empty_tag -> "the violation tag is empty"
  | Nul_byte { what; value } ->
      Printf.sprintf "%s contains a NUL byte: %S" what value
  | Not_absolute { what; path } ->
      Printf.sprintf "%s must be an absolute path, got %S" what path
  | Invalid_port p -> Printf.sprintf "listener port %d is not in 1..65535" p
  | Duplicate_port p -> Printf.sprintf "listener port %d is listed twice" p
  | Missing_socket p ->
      Printf.sprintf "the listener on port %d has no Unix socket" p

type kind = [ `Dir | `File | `Absent ]

type seatbelt = {
  profile : string;
  params : (string * string) list;
  argv : string list;
}

type output = Seatbelt of seatbelt | Bwrap of string list

let ( let* ) = Result.bind

(* ---- validation ---- *)

let check_text what value =
  if String.contains value '\000' then Error (Nul_byte { what; value })
  else Ok ()

let check_path what path =
  let* () = check_text what path in
  if String.starts_with ~prefix:"/" path then Ok ()
  else Error (Not_absolute { what; path })

let check_all check items =
  List.fold_left
    (fun acc (what, value) ->
      let* () = acc in
      check what value)
    (Ok ()) items

let check_listeners platform listeners =
  let check_one seen { port; socket } =
    if port < 1 || port > 65535 then Error (Invalid_port port)
    else if List.exists (Int.equal port) seen then Error (Duplicate_port port)
    else
      let* () =
        match (socket, platform) with
        | Some s, (Macos | Linux) -> check_path "socket" s
        | None, Macos -> Ok ()
        | None, Linux -> Error (Missing_socket port)
      in
      Ok (port :: seen)
  in
  List.fold_left
    (fun acc l ->
      let* seen = acc in
      check_one seen l)
    (Ok []) listeners
  |> Result.map ignore

let validate i =
  let labelled what = List.map (fun v -> (what, v)) in
  let* () =
    check_all check_path
      ([
         ("home", i.home);
         ("project_root", i.project_root);
         ("bactrian_dir", i.bactrian_dir);
         ("state_dir", i.state_dir);
         ("temp_dir", i.temp_dir);
       ]
      @ labelled "allow_write" i.allow_write
      @ labelled "deny_read" i.deny_read)
  in
  let* () =
    match i.platform with
    | Linux -> check_path "bactrian_exe" i.bactrian_exe
    | Macos ->
        let* () = check_text "tag" i.tag in
        if String.equal i.tag "" then Error Empty_tag else Ok ()
  in
  let* () = match i.agent with [] -> Error Empty_agent | _ :: _ -> Ok () in
  let* () = check_all check_text (labelled "agent argument" i.agent) in
  check_listeners i.platform i.listeners

(* ---- deny list ---- *)

let home_credentials platform =
  [
    ".ssh";
    ".gnupg";
    ".aws";
    ".azure";
    ".config/gcloud";
    ".kube";
    ".docker";
    ".netrc";
    ".npmrc";
    ".pypirc";
    ".git-credentials";
  ]
  @ match platform with Macos -> [ "Library/Keychains" ] | Linux -> []

let env_files =
  [
    ".env";
    ".env.local";
    ".env.development";
    ".env.development.local";
    ".env.production";
    ".env.production.local";
    ".env.staging";
    ".env.test";
    ".env.test.local";
  ]

(* [keep_own_run] marks the directory that holds every run's directory: the
   run's own state and temporary directories are made readable again after it. *)
type deny = { path : string; keep_own_run : bool }

let denies i =
  let plain path = { path; keep_own_run = false } in
  List.map
    (fun p -> plain (Filename.concat i.home p))
    (home_credentials i.platform)
  @ List.map (fun p -> plain (Filename.concat i.project_root p)) env_files
  @ [
      plain (Filename.concat i.bactrian_dir "ca");
      { path = Filename.concat i.bactrian_dir "runs"; keep_own_run = true };
    ]
  @ List.map plain i.deny_read

let deny_list i = List.map (fun d -> d.path) (denies i)

(* ---- macOS ---- *)

let numbered prefix items =
  List.mapi (fun n x -> (Printf.sprintf "%s_%d" prefix n, x)) items

let param name = Printf.sprintf "(param \"%s\")" name
let subpath name = Printf.sprintf "(subpath %s)" (param name)

let seatbelt_of i =
  let deny = numbered "DENY" (denies i) in
  let writes = numbered "WRITE" i.allow_write in
  let ports = numbered "PORT" (List.map (fun l -> l.port) i.listeners) in
  let sockets =
    numbered "SOCKET" (List.filter_map (fun l -> l.socket) i.listeners)
  in
  let own = [ subpath "STATE"; subpath "TEMP" ] in
  (* Every denial is logged with the run's tag, which is how the run's
     violations are found in the system log (design §5.3). *)
  let tagged = Printf.sprintf "(with message %s)" (param "TAG") in
  let deny_rules =
    List.concat_map
      (fun (name, d) ->
        Printf.sprintf "(deny file-read-data %s %s)" (subpath name) tagged
        ::
        (if d.keep_own_run then
           [
             Printf.sprintf "(allow file-read-data %s)" (String.concat " " own);
           ]
         else []))
      deny
  in
  let write_paths =
    own
    @ List.map (fun (name, _) -> subpath name) writes
    @ [ "(literal \"/dev/null\")"; "(literal \"/dev/dtracehelper\")" ]
  in
  let profile =
    String.concat "\n"
      ([
         "(version 1)";
         Printf.sprintf "(deny default %s)" tagged;
         (* Node and Python ask for these at start, and Python's proxy
            discovery asks configd: refused, harmlessly, and not logged, so
            they do not bury real violations. *)
         "(deny mach-lookup (global-name \"com.apple.logd\" \
          \"com.apple.system.notification_center\" \
          \"com.apple.SystemConfiguration.configd\") (with no-log))";
         "(allow process-exec)";
         "(allow process-fork)";
         "(allow signal (target same-sandbox))";
         "(allow sysctl-read)";
         "(allow mach-lookup (global-name \
          \"com.apple.system.opendirectoryd.libinfo\"))";
         "(allow ipc-posix-sem)";
         "(allow file-read*)";
       ]
      @ deny_rules
      @ [
          Printf.sprintf "(allow file-write* %s)"
            (String.concat " " write_paths);
          "(allow file-ioctl (literal \"/dev/dtracehelper\"))";
        ]
      @ List.map
          (fun (name, _) ->
            Printf.sprintf
              "(allow network-outbound (remote ip (string-append \
               \"localhost:\" %s)))"
              (param name))
          ports
      @ List.map
          (fun (name, _) ->
            Printf.sprintf
              "(allow network-outbound (remote unix-socket (path-literal %s)))"
              (param name))
          sockets)
    ^ "\n"
  in
  let params =
    [ ("TAG", i.tag); ("STATE", i.state_dir); ("TEMP", i.temp_dir) ]
    @ List.map (fun (n, d) -> (n, d.path)) deny
    @ writes
    @ List.map (fun (n, p) -> (n, string_of_int p)) ports
    @ sockets
  in
  let argv =
    [ "/usr/bin/sandbox-exec"; "-p"; profile ]
    @ List.concat_map (fun (n, v) -> [ "-D"; n ^ "=" ^ v ]) params
    @ ("--" :: i.agent)
  in
  { profile; params; argv }

(* ---- Linux ---- *)

type mount = Rw of string | Ro of string

let mount_args = function
  | Rw p -> [ "--bind"; p; p ]
  | Ro p -> [ "--ro-bind"; p; p ]

let rec trim_slashes s =
  if String.length s > 1 && String.ends_with ~suffix:"/" s then
    trim_slashes (String.sub s 0 (String.length s - 1))
  else s

let is_under ~dir path =
  String.starts_with ~prefix:(trim_slashes dir ^ "/") path

let mount_path = function Rw p | Ro p -> p

let bwrap_of ~exists i =
  let writable =
    Rw i.state_dir :: Rw i.temp_dir
    :: List.filter_map
         (fun p ->
           match exists p with `Dir | `File -> Some (Rw p) | `Absent -> None)
         i.allow_write
  in
  let sockets =
    List.filter_map (fun l -> Option.map (fun s -> Ro s) l.socket) i.listeners
  in
  let binds = writable @ sockets in
  let mask d =
    let rebinds () =
      if d.keep_own_run then
        List.concat_map mount_args
          (List.filter (fun m -> is_under ~dir:d.path (mount_path m)) binds)
      else []
    in
    match exists d.path with
    | `Dir -> [ "--tmpfs"; d.path ] @ rebinds ()
    | `File -> [ "--ro-bind"; "/dev/null"; d.path ] @ rebinds ()
    | `Absent -> []
  in
  (* A tmpfs is writable, so every directory mask is made read-only, after all
     the mounts, because bwrap creates mount points inside a mask (the run's own
     directories, a deny-list path under another). *)
  let read_only d =
    match exists d.path with
    | `Dir -> [ "--remount-ro"; d.path ]
    | `File | `Absent -> []
  in
  let pairs =
    List.filter_map
      (fun l -> Option.map (fun s -> Printf.sprintf "%d=%s" l.port s) l.socket)
      i.listeners
  in
  [ "bwrap"; "--die-with-parent"; "--new-session"; "--unshare-net" ]
  @ [ "--unshare-pid"; "--unshare-ipc"; "--ro-bind"; "/"; "/" ]
  @ [ "--dev"; "/dev"; "--proc"; "/proc"; "--tmpfs"; "/tmp" ]
  @ [ "--tmpfs"; "/dev/shm"; "--tmpfs"; "/run" ]
  (* The executable may live under one of the fresh tmpfs mounts. *)
  @ mount_args (Ro i.bactrian_exe)
  @ List.concat_map mount_args binds
  @ List.concat_map mask (denies i)
  @ List.concat_map read_only (denies i)
  @ [ "--"; i.bactrian_exe; "__sandbox-init" ]
  @ pairs @ ("--" :: i.agent)

let generate ~exists i =
  let* () = validate i in
  match i.platform with
  | Macos -> Ok (Seatbelt (seatbelt_of i))
  | Linux -> Ok (Bwrap (bwrap_of ~exists i))
