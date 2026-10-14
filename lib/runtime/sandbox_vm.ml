open Bactrian_core

type t = {
  runtime : Vm_runtime.t;
  image : string;
  relay : string;
  process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
}

let runtime_name t = Vm_runtime.name t.runtime
let image t = t.image
let ( let* ) = Result.bind

let prepare ~env requested ~image =
  let process_mgr =
    (Eio.Stdenv.process_mgr env :> Eio_unix.Process.mgr_ty Eio.Resource.t)
  and clock =
    (Eio.Stdenv.clock env :> float Eio.Time.clock_ty Eio.Resource.t)
  in
  let* runtime = Vm_runtime.detect ~process_mgr ~clock requested in
  let* () = Vm_runtime.image_present ~process_mgr ~clock runtime image in
  let* relay = Vm_runtime.relay ~process_mgr ~clock runtime in
  Ok { runtime; image; relay; process_mgr; clock }

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

(* The empty file mounted over the deny-list's files, and the agent's
   environment, both in [dir]. *)
let prepare_files ~dir vars =
  let* text = Result.map_error Vm_args.describe_error (Vm_args.env_file vars) in
  let env_file = Filename.concat dir "agent.env"
  and empty_file = Filename.concat dir "vm-empty" in
  match
    write_file ~perm:0o600 env_file text;
    write_file ~perm:0o400 empty_file ""
  with
  | () -> Ok (env_file, empty_file)
  | exception Sys_error m -> Error m

let args t layout ~name ~env_file ~empty_file ~state_dir ~temp_dir ~cwd
    ~forwards command =
  Result.map_error Vm_args.describe_error
    (Vm_args.argv
       {
         runtime = Vm_runtime.exe t.runtime;
         name;
         uid = Unix.getuid ();
         gid = Unix.getgid ();
         image = t.image;
         workdir = cwd;
         env_file;
         relay = t.relay;
         empty_file;
         project_root = layout.project_root;
         runs_dir = Filename.concat layout.bactrian_dir "runs";
         state_dir;
         temp_dir;
         allow_write = List.filter Sys.file_exists layout.allow_write;
         deny = layout.deny;
         forwards;
         command;
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
  let* env_file, empty_file = prepare_files ~dir [] in
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
    args t layout ~name ~env_file ~empty_file ~state_dir ~temp_dir
      ~cwd:state_dir ~forwards:[ port ]
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
  let* env_file, empty_file =
    prepare_files ~dir:run_dir
      (List.filter (fun (k, _) -> not (String.equal k "PATH")) agent_env)
  in
  Eio.Switch.on_release sw (fun () ->
      remove t name;
      remove_file env_file);
  let* runtime_argv =
    args t layout ~name ~env_file ~empty_file ~state_dir ~temp_dir ~cwd
      ~forwards:[ port ] (Agent agent)
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
