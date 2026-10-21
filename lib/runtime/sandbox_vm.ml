open Bactrian_core

type t = {
  runtime : Vm_runtime.t;
  image : string;
  image_id : string;
  relay : string;
  process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
}

let runtime_name t = Vm_runtime.name t.runtime
let image t = t.image
let image_id t = t.image_id
let ( let* ) = Result.bind

let prepare ~env requested ~image =
  let process_mgr =
    (Eio.Stdenv.process_mgr env :> Eio_unix.Process.mgr_ty Eio.Resource.t)
  and clock =
    (Eio.Stdenv.clock env :> float Eio.Time.clock_ty Eio.Resource.t)
  in
  let* runtime = Vm_runtime.detect ~process_mgr ~clock requested in
  let (_ : string list) = Vm_runtime.remove_stale ~process_mgr ~clock runtime in
  let* image_id = Vm_runtime.image_id ~process_mgr ~clock runtime image in
  let* relay = Vm_runtime.relay ~process_mgr ~clock runtime in
  Ok { runtime; image; image_id; relay; process_mgr; clock }

type layout = {
  project_root : string;
  bactrian_dir : string;
  allow_write : string list;
  read_only : string list;
  deny : (string * Sandbox_policy.kind) list;
}

let write_file ~perm path text =
  Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ]
    perm path (fun oc -> output_string oc text)

let remove_file path = try Sys.remove path with Sys_error _ -> ()

(* The agent's environment, in [dir]. *)
let write_env_file ~dir vars =
  let* text = Result.map_error Vm_args.describe_error (Vm_args.env_file vars) in
  let env_file = Filename.concat dir "agent.env" in
  match write_file ~perm:0o600 env_file text with
  | () -> Ok env_file
  | exception Sys_error m -> Error m

let identity path =
  match Unix.stat path with
  | st -> Some (st.st_dev, st.st_ino)
  | exception Unix.Unix_error _ -> None

(* The entries of a directory holding a masked file, each with what it is and
   whether it leads to one of [masked] (device and inode) under another name. *)
let list_dir ~masked dir =
  match Sys.readdir dir with
  | exception Sys_error m -> Error m
  | names ->
      Ok
        (List.map
           (fun name : Vm_args.entry ->
             let path = Filename.concat dir name in
             let kind : Vm_args.entry_kind =
               match (Unix.lstat path).st_kind with
               | S_LNK -> (
                   match (Unix.stat path).st_kind with
                   | S_REG -> (
                       match Unix.realpath path with
                       | target -> Link_to_file target
                       | exception Unix.Unix_error _ -> Other_link)
                   | S_DIR | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK ->
                       Other_link
                   | exception Unix.Unix_error _ -> Other_link)
               | S_REG -> File
               | S_DIR -> Dir
               | S_CHR | S_BLK | S_FIFO | S_SOCK -> Special
               | exception Unix.Unix_error _ -> Other_link
             in
             let identity = identity path in
             let masked_file =
               match identity with
               | Some id ->
                   List.exists
                     (fun (dev, ino) ->
                       Int.equal dev (fst id) && Int.equal ino (snd id))
                     masked
               | None -> false
             in
             { name; kind; identity; masked_file })
           (List.sort String.compare (Array.to_list names)))

(* What is at each existing allow_write path: container mounts directories
   only. *)
let allow_write_kinds paths =
  List.filter_map
    (fun p ->
      match (Unix.stat p).st_kind with
      | S_DIR -> Some (p, `Dir)
      | S_REG | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> Some (p, `File)
      | exception Unix.Unix_error _ -> None)
    paths

let staging_dir ~run_dir = Filename.concat run_dir "staging"

let invocation t layout ~run_id ~run_dir ~env_file ~state_dir ~temp_dir ~cwd
    ~forwards command =
  let masked =
    List.filter_map
      (fun (p, k) ->
        match k with `File -> identity p | `Dir | `Absent -> None)
      layout.deny
  in
  Result.map_error Vm_args.describe_error
    (Vm_args.argv
       {
         kind = Vm_runtime.kind t.runtime;
         runtime = Vm_runtime.exe t.runtime;
         name = "bactrian-" ^ run_id;
         labels = Vm_runtime.labels ~run_id;
         uid = Unix.getuid ();
         gid = Unix.getgid ();
         image =
           (match Vm_runtime.kind t.runtime with
           | `Docker -> t.image_id
           | `Container -> t.image);
         workdir = cwd;
         env_file;
         relay = t.relay;
         project_root = layout.project_root;
         bactrian_dir = layout.bactrian_dir;
         runs_dir = Filename.concat layout.bactrian_dir "runs";
         state_dir;
         temp_dir;
         allow_write = allow_write_kinds layout.allow_write;
         read_only = layout.read_only;
         deny = layout.deny;
         staging_dir = staging_dir ~run_dir;
         forwards;
         command;
         list_dir = list_dir ~masked;
       })

(* A hard link of [l.source] as [path], which must turn out to be the file the
   listing saw: the source may have been replaced since, by a symbolic link or
   another name of a masked file. The link does not follow a symbolic link, and
   anything else that was linked is removed again. *)
let link_checked ~over (l : Vm_args.link) path =
  match Unix.link ~follow:false l.source path with
  | exception Unix.Unix_error (e, _, _) ->
      Error
        (Printf.sprintf
           "%s, beside a hidden file in %s, cannot be shown in container: a \
            hard link of it in %s failed (%s), as it does for a file on \
            another volume"
           l.source over (Filename.dirname path) (Unix.error_message e))
  | () ->
      let same =
        match Unix.lstat path with
        | { st_kind = S_REG; st_dev; st_ino; _ } ->
            Int.equal st_dev (fst l.identity)
            && Int.equal st_ino (snd l.identity)
        | { st_kind = S_DIR | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK; _ } ->
            false
        | exception Unix.Unix_error _ -> false
      in
      if same then Ok ()
      else begin
        (try Unix.unlink path with Unix.Unix_error _ -> ());
        Error
          (Printf.sprintf
             "%s, beside a hidden file in %s, changed while its directory was \
              being staged, so the run stops rather than show what it is now"
             l.source over)
      end

(* container's staging directories, made under the run's directory. A hard link
   fails across volumes, and then the file cannot be shown. *)
let make_stages ~run_dir (stages : Vm_args.stage list) =
  let made (s : Vm_args.stage) =
    match
      Unix.mkdir s.dir 0o755;
      List.iter
        (fun m -> Unix.mkdir (Filename.concat s.dir m) 0o755)
        s.mount_points
    with
    | () ->
        List.fold_left
          (fun acc (l : Vm_args.link) ->
            let* () = acc in
            link_checked ~over:s.over l (Filename.concat s.dir l.name))
          (Ok ()) s.links
    | exception Unix.Unix_error (e, _, path) ->
        Error
          (Printf.sprintf "the staging directory %s cannot be made: %s (%s)"
             s.dir (Unix.error_message e) path)
  in
  match stages with
  | [] -> Ok ()
  | _ :: _ -> (
      match Unix.mkdir (staging_dir ~run_dir) 0o700 with
      | exception Unix.Unix_error (e, _, _) ->
          Error
            (Printf.sprintf "the staging directory %s cannot be made: %s"
               (staging_dir ~run_dir) (Unix.error_message e))
      | () ->
          List.fold_left
            (fun acc s ->
              let* () = acc in
              made s)
            (Ok ()) stages)

let remove t name =
  Vm_runtime.remove ~process_mgr:t.process_mgr ~clock:t.clock t.runtime name

(* The command the bridge runs. Docker's [run] is it. For container the
   staging directories are made, the container created, and its image checked
   against the one [prepare] saw, so a reference moved since cannot change what
   the agent runs in. *)
let launch t ~run_dir ~name (invocation : Vm_args.invocation) =
  match Vm_runtime.kind t.runtime with
  | `Docker -> Ok invocation.argv
  | `Container ->
      let process_mgr = t.process_mgr and clock = t.clock in
      let* () = make_stages ~run_dir invocation.stages in
      let* () =
        Vm_runtime.create ~process_mgr ~clock t.runtime invocation.argv
      in
      let* digest =
        Vm_runtime.created_image ~process_mgr ~clock t.runtime name
      in
      if String.equal digest t.image_id then
        Ok [ Vm_runtime.exe t.runtime; "start"; "-a"; "-i"; name ]
      else
        Error
          (Printf.sprintf
             "the container was created from %s, but %s was %s when it was \
              checked: the image moved in between. Fix: run again"
             digest t.image t.image_id)

(* The container and container's staging directories, once the run is over. *)
let clean_up t ~run_dir ~name =
  remove t name;
  try Proc_util.remove_tree (staging_dir ~run_dir)
  with Unix.Unix_error _ | Sys_error _ -> ()

let client_env t =
  List.filter_map
    (fun kv ->
      match String.index_opt kv '=' with
      | Some i ->
          Some
            (String.sub kv 0 i, String.sub kv (i + 1) (String.length kv - i - 1))
      | None -> None)
    (Array.to_list (Vm_runtime.client_env t.runtime))

(* The probe *)

let probe_timeout = 120.

let greet ~sw listener =
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server listener
        ~on_error:(fun (_ : exn) -> ())
        (fun flow _ -> Eio.Flow.copy_string Sandbox_probe.greeting flow))

let last_line text =
  String.split_on_char '\n' text
  |> List.filter (fun l -> not (String.equal (String.trim l) ""))
  |> List.fold_left (fun _ l -> Some l) None

let probe ~env t layout ~dir (paths : Vm_args.probe) =
  let id = Filename.basename dir in
  let name = "bactrian-" ^ id in
  let state_dir = Filename.concat dir "state"
  and temp_dir = Filename.concat dir "tmp" in
  Fun.protect ~finally:(fun () -> clean_up t ~run_dir:dir ~name) @@ fun () ->
  let* env_file = write_env_file ~dir [] in
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let listener =
    Eio.Net.listen ~sw ~backlog:8 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  greet ~sw listener;
  let port =
    match Eio.Net.listening_addr listener with `Tcp (_, p) -> p | `Unix _ -> 0
  in
  let* invocation =
    invocation t layout ~run_id:id ~run_dir:dir ~env_file ~state_dir ~temp_dir
      ~cwd:state_dir ~forwards:[ port ] (Probe paths)
  in
  let* argv = launch t ~run_dir:dir ~name invocation in
  let out = Buffer.create 256 and err = Buffer.create 256 in
  let* code =
    match
      Eio.Time.with_timeout t.clock probe_timeout (fun () ->
          Ok
            (Vm_bridge.run ~net ~clock:t.clock ~process_mgr:t.process_mgr
               ~env:(Vm_runtime.client_env t.runtime)
               ~forwards:[ port ]
               ~stdin:(Eio.Flow.string_source "")
               ~stdout:(Eio.Flow.buffer_sink out)
               ~stderr:(Eio.Flow.buffer_sink err)
               ~log:(fun (_ : string) -> ())
               argv))
    with
    | Error `Timeout ->
        Error
          (Printf.sprintf "the probe did not finish within %.0f seconds"
             probe_timeout)
    | Ok (Error e) ->
        Error
          (Vm_bridge.describe_error e
          ^
          match last_line (Buffer.contents err) with
          | Some l -> ": " ^ l
          | None -> "")
    | Ok (Ok code) -> Ok code
  in
  let* line =
    match last_line (Buffer.contents out) with
    | Some l -> Ok l
    | None ->
        Error
          (Printf.sprintf "the probe gave no report (exit status %d)%s" code
             (match last_line (Buffer.contents err) with
             | Some l -> ": " ^ l
             | None -> ""))
  in
  let* report =
    Result.map_error
      (fun m -> Printf.sprintf "the probe's report is unreadable (%s)" m)
      (Vm_probe.of_line line)
  in
  let wrong =
    (match Vm_probe.judge (Vm_runtime.kind t.runtime) report with
      | Ok () -> []
      | Error w -> w)
    @ List.filter_map
        (fun (path, what) ->
          if Sys.file_exists path then Some (what ^ " exists") else None)
        [
          (paths.outside, "the file written outside the state directory");
          (paths.other_run, "the file written into another run");
        ]
  in
  match wrong with
  | [] -> Ok ()
  | _ :: _ ->
      Error
        ("the virtual machine sandbox does not enforce its policy: "
       ^ String.concat "; " wrong)

(* One run *)

type session = { argv : string list; env : (string * string) list }

let start ~sw t layout ~bactrian_exe ~run_id ~run_dir ~state_dir ~temp_dir ~cwd
    ~port ~agent_env agent =
  let name = "bactrian-" ^ run_id in
  let* env_file =
    write_env_file ~dir:run_dir
      (List.filter (fun (k, _) -> not (String.equal k "PATH")) agent_env)
  in
  Eio.Switch.on_release sw (fun () ->
      clean_up t ~run_dir ~name;
      remove_file env_file);
  let* invocation =
    invocation t layout ~run_id ~run_dir ~env_file ~state_dir ~temp_dir ~cwd
      ~forwards:[ port ] (Agent agent)
  in
  let* runtime_argv = launch t ~run_dir ~name invocation in
  Ok
    {
      argv =
        [
          bactrian_exe;
          "__vm-bridge";
          "forward=" ^ string_of_int port;
          "container=" ^ name;
        ]
        @ (match Vm_runtime.kind t.runtime with
          | `Docker -> []
          | `Container -> [ "signals=kill" ])
        @ ("--" :: runtime_argv);
      env = client_env t;
    }
