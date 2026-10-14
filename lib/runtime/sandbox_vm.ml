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
  let* image_id = Vm_runtime.image_id ~process_mgr ~clock runtime image in
  let* relay = Vm_runtime.relay ~process_mgr ~clock runtime in
  Ok { runtime; image; image_id; relay; process_mgr; clock }

type layout = {
  project_root : string;
  bactrian_dir : string;
  allow_write : string list;
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
               | S_REG | S_DIR | S_CHR | S_BLK | S_FIFO | S_SOCK -> Plain
               | exception Unix.Unix_error _ -> Other_link
             in
             let masked_file =
               match identity path with
               | Some id ->
                   List.exists
                     (fun (dev, ino) ->
                       Int.equal dev (fst id) && Int.equal ino (snd id))
                     masked
               | None -> false
             in
             { name; kind; masked_file })
           (List.sort String.compare (Array.to_list names)))

let args t layout ~name ~env_file ~state_dir ~temp_dir ~cwd ~forwards command =
  let masked =
    List.filter_map
      (fun (p, k) ->
        match k with `File -> identity p | `Dir | `Absent -> None)
      layout.deny
  in
  Result.map_error Vm_args.describe_error
    (Vm_args.argv
       {
         runtime = Vm_runtime.exe t.runtime;
         name;
         uid = Unix.getuid ();
         gid = Unix.getgid ();
         image = t.image_id;
         workdir = cwd;
         env_file;
         relay = t.relay;
         project_root = layout.project_root;
         runs_dir = Filename.concat layout.bactrian_dir "runs";
         state_dir;
         temp_dir;
         allow_write = List.filter Sys.file_exists layout.allow_write;
         deny = layout.deny;
         forwards;
         command;
         list_dir = list_dir ~masked;
       })

let client_env t =
  List.filter_map
    (fun kv ->
      match String.index_opt kv '=' with
      | Some i ->
          Some
            (String.sub kv 0 i, String.sub kv (i + 1) (String.length kv - i - 1))
      | None -> None)
    (Array.to_list (Vm_runtime.client_env t.runtime))

let remove t name =
  Vm_runtime.remove ~process_mgr:t.process_mgr ~clock:t.clock t.runtime name

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

let probe ~env t layout ~dir ~denied =
  let id = Filename.basename dir in
  let name = "bactrian-" ^ id in
  let state_dir = Filename.concat dir "state"
  and temp_dir = Filename.concat dir "tmp"
  and outside = Filename.concat dir "outside" in
  List.iter (fun d -> Unix.mkdir d 0o700) [ state_dir; temp_dir ];
  Fun.protect ~finally:(fun () -> remove t name) @@ fun () ->
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
  let* argv =
    args t layout ~name ~env_file ~state_dir ~temp_dir ~cwd:state_dir
      ~forwards:[ port ]
      (Probe { outside; inside = Filename.concat state_dir "inside"; denied })
  in
  let out = Buffer.create 256 and err = Buffer.create 256 in
  let* code =
    match
      Eio.Time.with_timeout t.clock probe_timeout (fun () ->
          Ok
            (Vm_bridge.run ~net ~process_mgr:t.process_mgr
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
    (match Vm_probe.judge report with Ok () -> [] | Error w -> w)
    @
    if Sys.file_exists outside then
      [ "the file written outside the state directory exists" ]
    else []
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
      remove t name;
      remove_file env_file);
  let* runtime_argv =
    args t layout ~name ~env_file ~state_dir ~temp_dir ~cwd ~forwards:[ port ]
      (Agent agent)
  in
  Ok
    {
      argv =
        [
          bactrian_exe;
          "__vm-bridge";
          "forward=" ^ string_of_int port;
          "container=" ^ name;
          "--";
        ]
        @ runtime_argv;
      env = client_env t;
    }
