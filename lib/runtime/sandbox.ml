open Bactrian_core
module Policy = Sandbox_policy

type mode = [ `Auto | `Strict | `Off ]

let modes = [ ("auto", `Auto); ("strict", `Strict); ("off", `Off) ]

let mode_name : mode -> string = function
  | `Auto -> "auto"
  | `Strict -> "strict"
  | `Off -> "off"

type label = { mode : mode; applied : bool; reason : string option }

let label_json (l : label) =
  `Assoc
    ([ ("mode", `String (mode_name l.mode)); ("applied", `Bool l.applied) ]
    @ match l.reason with Some r -> [ ("reason", `String r) ] | None -> [])

(* How the sandbox is made here: bubblewrap carries the path it was found at. *)
type engine = Seatbelt | Bubblewrap of string

type host = {
  engine : engine;
  bactrian_exe : string;
  home : string;
  project_root : string;
  bactrian_dir : string;
  allow_write : string list;
  deny_read : string list;
}

type t = Sandboxed of { mode : mode; host : host } | Unsandboxed of label

let off =
  Unsandboxed { mode = `Off; applied = false; reason = Some "sandbox: off" }

let http_mode mode =
  Unsandboxed { mode; applied = false; reason = Some "http mode" }

let label = function
  | Sandboxed { mode; host = _ } -> { mode; applied = true; reason = None }
  | Unsandboxed l -> l

type settings = {
  mode : mode;
  allow_write : string list;
  deny_read : string list;
  bactrian_exe : string;
  bactrian_dir : string;
}

let ( let* ) = Result.bind

(* Paths *)

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

(* Symbolic links resolved as far as the path exists, because Seatbelt matches
   resolved paths. *)
let rec resolve path =
  match Unix.realpath path with
  | real -> real
  | exception Unix.Unix_error _ ->
      let parent = Filename.dirname path in
      if String.equal parent path then path
      else Filename.concat (resolve parent) (Filename.basename path)

let resolved path = resolve (absolute path)

let kind path : Policy.kind =
  match (Unix.stat path).st_kind with
  | S_DIR -> `Dir
  | S_REG | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> `File
  | exception Unix.Unix_error _ -> `Absent

let home () =
  match Sys.getenv_opt "HOME" with
  | Some h when not (String.equal h "") -> h
  | Some _ | None -> (
      match Unix.getpwuid (Unix.getuid ()) with
      | pw -> pw.pw_dir
      | exception Not_found -> "/")

let remove_tree ~env path =
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / path)

(* A Unix socket path must stay under about 100 bytes, so sockets live in a
   short directory of their own rather than in the run directory. *)
let socket_dir ~sw =
  match Filename.temp_dir ~perms:0o700 "bactrian-" "" with
  | exception Sys_error m -> Error m
  | dir ->
      let dir = resolved dir in
      Eio.Switch.on_release sw (fun () ->
          match
            Array.iter
              (fun f -> Sys.remove (Filename.concat dir f))
              (Sys.readdir dir);
            Unix.rmdir dir
          with
          | () -> ()
          | exception (Sys_error _ | Unix.Unix_error _) -> ());
      Ok dir

(* The command *)

let platform = function Seatbelt -> Policy.Macos | Bubblewrap _ -> Linux

let input_of (host : host) ~state_dir ~temp_dir ~listeners ~tag ~preload agent :
    Policy.input =
  {
    platform = platform host.engine;
    home = host.home;
    project_root = host.project_root;
    bactrian_dir = host.bactrian_dir;
    state_dir = resolved state_dir;
    temp_dir = resolved temp_dir;
    allow_write = host.allow_write;
    deny_read = host.deny_read;
    listeners;
    agent;
    bactrian_exe = host.bactrian_exe;
    preload;
    tag;
  }

let command host ~state_dir ~temp_dir ~listeners ~tag ~preload agent =
  let input =
    input_of host ~state_dir ~temp_dir ~listeners ~tag ~preload agent
  in
  match Policy.generate ~resolve:resolved ~exists:kind input with
  | Error e -> Error (Policy.describe_error e)
  | Ok (Seatbelt s) -> Ok s.argv
  | Ok (Bwrap argv) -> (
      match (host.engine, argv) with
      | Bubblewrap path, _ :: args -> Ok (path :: args)
      | Bubblewrap _, [] | Seatbelt, ([] | _ :: _) -> Ok argv)

(* The helper commands bactrian runs (bwrap --version, the end marker, log
   show) get only PATH: after the launch environment is restored, inheriting
   bactrian's would put the provider keys in their launch environment, where
   another process of the user can read them (decision 0010). *)
let path_var () =
  "PATH=" ^ Option.value (Sys.getenv_opt "PATH") ~default:"/usr/bin:/bin"

let tool_env () = [| path_var () |]

(* Linux tools *)

let on_path name =
  Option.value (Sys.getenv_opt "PATH") ~default:""
  |> String.split_on_char ':'
  |> List.find_map (fun dir ->
      let path = Filename.concat dir name in
      if String.equal dir "" then None
      else
        match Unix.access path [ X_OK ] with
        | () -> Some path
        | exception Unix.Unix_error _ -> None)

(* "bubblewrap 0.9.0" *)
let bwrap_version text =
  match String.split_on_char ' ' (String.trim text) with
  | [ "bubblewrap"; v ] -> (
      match String.split_on_char '.' v with
      | major :: minor :: _ -> (
          match (int_of_string_opt major, int_of_string_opt minor) with
          | Some a, Some b -> Some (a, b)
          | None, (Some _ | None) | Some _, None -> None)
      | [] | [ _ ] -> None)
  | [] | [ _ ] | _ :: _ :: _ -> None

let bubblewrap ~env =
  match on_path "bwrap" with
  | None ->
      Error
        "bubblewrap (bwrap) is not installed. Fix: install it, for example \
         with sudo apt install bubblewrap"
  | Some path -> (
      match
        Eio.Process.parse_out
          (Eio.Stdenv.process_mgr env)
          ~env:(tool_env ()) Eio.Buf_read.take_all [ path; "--version" ]
      with
      | exception Eio.Io _ -> Error (Printf.sprintf "%s --version failed" path)
      | text -> (
          match bwrap_version text with
          | Some (major, minor) when major > 0 || minor >= 5 ->
              Ok (Bubblewrap path)
          | Some (major, minor) ->
              Error
                (Printf.sprintf
                   "bubblewrap %d.%d is older than 0.5. Fix: install \
                    bubblewrap 0.5 or later"
                   major minor)
          | None ->
              Error
                (Printf.sprintf "cannot read bubblewrap's version from %S"
                   (String.trim text))))

let apparmor_sysctl = "/proc/sys/kernel/apparmor_restrict_unprivileged_userns"

let apparmor_restricted () =
  match In_channel.with_open_text apparmor_sysctl In_channel.input_all with
  | text -> String.equal (String.trim text) "1"
  | exception Sys_error _ -> false

let apparmor_fix bwrap =
  Printf.sprintf
    "Ubuntu's AppArmor restriction on unprivileged user namespaces \
     (kernel.apparmor_restrict_unprivileged_userns = 1) stops bubblewrap. Fix \
     it once as root, either for every program with\n\
    \  sudo sysctl -w kernel.apparmor_restrict_unprivileged_userns=0\n\
     (add it to /etc/sysctl.d/ to keep it after a reboot), or for bubblewrap \
     only with an AppArmor profile:\n\
    \  printf 'abi <abi/4.0>,\\ninclude <tunables/global>\\nprofile bwrap %s \
     flags=(unconfined) {\\n  userns,\\n}\\n' | sudo tee /etc/apparmor.d/bwrap\n\
    \  sudo systemctl reload apparmor"
    bwrap

(* The probe *)

let probe_timeout = 30.

let hex s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02x" (Char.code s.[i])))

let greet ~sw listener =
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server listener
        ~on_error:(fun (_ : exn) -> ())
        (fun flow _ -> Eio.Flow.copy_string Sandbox_probe.greeting flow))

let last_line text =
  String.split_on_char '\n' text
  |> List.filter (fun l -> not (String.equal (String.trim l) ""))
  |> List.fold_left (fun _ l -> Some l) None

let describe_status : Eio.Process.exit_status -> string = function
  | `Exited n -> Printf.sprintf "exit status %d" n
  | `Signaled n -> Printf.sprintf "signal %d" n

(* Runs [argv] and returns its standard output, or why it gave none. *)
let run_probe_command ~sw ~env ~vars argv =
  let out_r, out_w = Eio_unix.pipe sw and err_r, err_w = Eio_unix.pipe sw in
  match
    Eio.Process.spawn ~sw
      (Eio.Stdenv.process_mgr env)
      ~stdout:out_w ~stderr:err_w ~env:vars argv
  with
  | exception Eio.Io _ ->
      Error
        (Printf.sprintf "the probe could not be started (%s)"
           (String.concat " " (List.filteri (fun i _ -> i < 1) argv)))
  | proc -> (
      Eio.Resource.close out_w;
      Eio.Resource.close err_w;
      let read flow =
        Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:(1 lsl 20) flow
      in
      match
        Eio.Time.with_timeout (Eio.Stdenv.clock env) probe_timeout (fun () ->
            let out, err =
              Eio.Fiber.pair (fun () -> read out_r) (fun () -> read err_r)
            in
            Ok (out, err, Eio.Process.await proc))
      with
      | Error `Timeout ->
          Error
            (Printf.sprintf "the probe did not finish within %.0f seconds"
               probe_timeout)
      | Ok (out, err, status) -> (
          match last_line out with
          | Some line -> Ok line
          | None ->
              Error
                (Printf.sprintf "the probe gave no report (%s)%s"
                   (describe_status status)
                   (match last_line err with Some l -> ": " ^ l | None -> ""))))

let host_netns () =
  match Unix.readlink "/proc/self/ns/net" with
  | ns -> Some ns
  | exception Unix.Unix_error _ -> None

let probe ~env (host : host) =
  let id = hex (Mirage_crypto_rng.generate 6) in
  let dir = Filename.concat host.bactrian_dir ("sandbox-probe-" ^ id) in
  let state_dir = Filename.concat dir "state"
  and temp_dir = Filename.concat dir "tmp"
  and outside = Filename.concat dir "outside" in
  Fun.protect ~finally:(fun () -> remove_tree ~env dir) @@ fun () ->
  List.iter (fun d -> Unix.mkdir d 0o700) [ dir; state_dir; temp_dir ];
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let tcp =
    Eio.Net.listen ~sw ~backlog:8 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  greet ~sw tcp;
  let port =
    match Eio.Net.listening_addr tcp with `Tcp (_, p) -> p | `Unix _ -> 0
  in
  let* socket =
    match host.engine with
    | Seatbelt -> Ok None
    | Bubblewrap _ ->
        let* dir = socket_dir ~sw in
        let path = Filename.concat dir "probe.sock" in
        greet ~sw (Eio.Net.listen ~sw ~backlog:8 net (`Unix path));
        Ok (Some path)
  in
  let* argv =
    command host ~state_dir ~temp_dir
      ~listeners:[ { port; socket } ]
      ~tag:(Sandbox_log.tag ("probe-" ^ id))
      ~preload:None
      [
        host.bactrian_exe;
        "__sandbox-probe";
        "--port";
        string_of_int port;
        "--outside";
        outside;
        "--inside";
        Filename.concat (resolved state_dir) "inside";
      ]
  in
  let vars = [| path_var (); "HOME=" ^ state_dir; "TMPDIR=" ^ temp_dir |] in
  let* line = run_probe_command ~sw ~env ~vars argv in
  let* report =
    Result.map_error
      (fun m -> Printf.sprintf "the probe's report is unreadable (%s)" m)
      (Sandbox_probe.of_line line)
  in
  let wrong =
    (match
       Sandbox_probe.judge (platform host.engine) ~host_netns:(host_netns ())
         report
     with
      | Ok () -> []
      | Error wrong -> wrong)
    @
    if Sys.file_exists outside then
      [ "the file written outside the state directory exists" ]
    else []
  in
  match wrong with
  | [] -> Ok ()
  | _ :: _ ->
      Error
        ("the sandbox does not enforce its policy: " ^ String.concat "; " wrong)

let check ~env settings =
  let* engine =
    if Sys.file_exists "/usr/bin/sandbox-exec" then Ok Seatbelt
    else if Sys.file_exists "/proc/self/ns/net" then bubblewrap ~env
    else
      Error
        "this operating system has no supported sandbox (macOS sandbox-exec or \
         Linux bubblewrap)"
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700
    Eio.Path.(Eio.Stdenv.fs env / absolute settings.bactrian_dir);
  let bactrian_dir = resolved settings.bactrian_dir in
  let host =
    {
      engine;
      bactrian_exe = resolved settings.bactrian_exe;
      home = resolved (home ());
      project_root = Filename.dirname bactrian_dir;
      bactrian_dir;
      allow_write = List.map resolved settings.allow_write;
      deny_read = List.map resolved settings.deny_read;
    }
  in
  match probe ~env host with
  | Ok () -> Ok host
  | Error cause -> (
      match engine with
      | Bubblewrap path when apparmor_restricted () ->
          Error (cause ^ ". " ^ apparmor_fix path)
      | Bubblewrap _ | Seatbelt -> Error cause)

let preflight ~env ~warn settings =
  match settings.mode with
  | `Off -> Ok off
  | (`Auto | `Strict) as mode -> (
      match
        try check ~env settings
        with (Unix.Unix_error _ | Eio.Io _ | Sys_error _) as e ->
          Error (Printexc.to_string e)
      with
      | Ok host -> Ok (Sandboxed { mode; host })
      | Error cause -> (
          match mode with
          | `Auto ->
              warn
                ("sandbox: auto: runs are unsandboxed, because the sandbox is \
                  unavailable: " ^ cause);
              Ok
                (Unsandboxed
                   {
                     mode;
                     applied = false;
                     reason = Some ("unavailable: " ^ cause);
                   })
          | `Strict ->
              Error ("sandbox: strict, but the sandbox is unavailable: " ^ cause)
          ))

(* One run *)

type run = {
  run_id : string;
  run_dir : string;
  state_dir : string;
  temp_dir : string;
  port : int;
  listen_unix : string -> (unit, string) result;
  preload : string option;
}

type session = {
  argv : string list;
  env : (string * string) list;
  fds : (int * Eio_unix.Fd.t) list;
}

type violations =
  | Collected of Sandbox_log.violation list
  | Not_collected of string

type started =
  | Nothing
  | Seatbelt_log of { tag : string; since : float; marker : string }
  | Relay_log of string

let start ~sw t run agent =
  match t with
  | Unsandboxed _ -> Ok ({ argv = agent; env = []; fds = [] }, Nothing)
  | Sandboxed { host; mode = _ } -> (
      let tag = Sandbox_log.tag run.run_id in
      match host.engine with
      | Seatbelt ->
          let since = Unix.gettimeofday () in
          let* argv =
            command host ~state_dir:run.state_dir ~temp_dir:run.temp_dir
              ~listeners:[ { port = run.port; socket = None } ]
              ~tag ~preload:run.preload agent
          in
          let marker = Filename.concat run.temp_dir "sandbox-end-marker" in
          Ok ({ argv; env = []; fds = [] }, Seatbelt_log { tag; since; marker })
      | Bubblewrap _ ->
          let* dir = socket_dir ~sw in
          let socket = Filename.concat dir "model.sock" in
          let* () = run.listen_unix socket in
          let log = Filename.concat run.run_dir "sandbox.log" in
          let* fd =
            match
              Unix.openfile log [ O_WRONLY; O_CREAT; O_APPEND; O_CLOEXEC ] 0o600
            with
            | fd -> Ok (Eio_unix.Fd.of_unix ~sw ~close_unix:true fd)
            | exception Unix.Unix_error (e, _, _) ->
                Error (log ^ ": " ^ Unix.error_message e)
          in
          let* argv =
            command host ~state_dir:run.state_dir ~temp_dir:run.temp_dir
              ~listeners:[ { port = run.port; socket = Some socket } ]
              ~tag ~preload:run.preload agent
          in
          Ok
            ( {
                argv;
                env = [ (Sandbox_init.log_variable, "3") ];
                fds = [ (3, fd) ];
              },
              Relay_log log ))

let own_port = function
  | Sandboxed { host = { engine = Seatbelt; _ }; _ } -> true
  | Sandboxed { host = { engine = Bubblewrap _; _ }; _ } | Unsandboxed _ ->
      false

let mcp_warning t (servers : Mcp_proxy.config list) =
  let exposed =
    List.filter_map
      (fun (c : Mcp_proxy.config) ->
        match (c.server, c.policy) with
        | Stdio { argv = _ :: _ :: _; _ }, (Record | Passthrough)
        | Stdio { env = _ :: _; _ }, (Record | Passthrough) ->
            Some c.name
        | Stdio _, (Record | Passthrough | Replay | Block)
        | Http _, (Record | Passthrough | Replay | Block) ->
            None)
      servers
  in
  match (t, exposed) with
  | Sandboxed { host = { engine = Seatbelt; _ }; _ }, _ :: _ ->
      Some
        (Printf.sprintf
           "the sandboxed agent can read the command line and environment \
            of             the MCP stdio server%s %s, as it can any process of \
            this user that             is not an Apple binary: give them no \
            credential you would not give             the agent, or run \
            bactrian as a separate user or in a virtual             machine"
           (match exposed with [ _ ] -> "" | _ -> "s")
           (String.concat ", " exposed))
  | Sandboxed { host = { engine = Seatbelt | Bubblewrap _; _ }; _ }, _
  | Unsandboxed _, _ ->
      None

(* A deny-list mask is applied after the library is bound, so a library under
   one is hidden again. Only bubblewrap binds the library at all. *)
let preload_hidden t library =
  match t with
  | Unsandboxed _ -> false
  | Sandboxed { host = { engine = Seatbelt; _ }; _ } -> false
  | Sandboxed { host = { engine = Bubblewrap _; _ } as host; _ } ->
      let dir = host.bactrian_dir in
      let input =
        input_of host ~state_dir:dir ~temp_dir:dir ~listeners:[] ~tag:""
          ~preload:None [ "agent" ]
      in
      List.exists
        (fun denied ->
          String.equal denied library
          || String.starts_with ~prefix:(denied ^ "/") library)
        (Policy.deny_list ~resolve:resolved input)

let log_timeout = 30.

(* Denies the one write it makes, with the end tag, so the marker always
   reaches the log. *)
let marker_profile =
  "(version 1)\n\
   (deny default (with message (param \"TAG\")))\n\
   (allow process-exec)\n\
   (allow file-read*)\n"

let seatbelt_violations ~env ~tag ~since ~marker =
  let mgr = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let end_tag = Sandbox_log.end_tag tag in
  (* touch reports the denial on its standard error, which is not ours. *)
  let discard = Eio.Flow.buffer_sink (Buffer.create 256) in
  match
    Eio.Process.run mgr
      ~is_success:(fun (_ : int) -> true)
      ~stdout:discard ~stderr:discard ~env:(tool_env ())
      [
        "/usr/bin/sandbox-exec";
        "-p";
        marker_profile;
        "-D";
        "TAG=" ^ end_tag;
        "/usr/bin/touch";
        marker;
      ]
  with
  | exception Eio.Io _ -> Not_collected "the end marker could not be logged"
  | () -> (
      let argv =
        [
          "/usr/bin/log";
          "show";
          "--style";
          "ndjson";
          "--start";
          Printf.sprintf "@%d" (Float.to_int since - 1);
          "--predicate";
          Sandbox_log.predicate ~tag;
        ]
      in
      let read () =
        String.split_on_char '\n'
          (Eio.Process.parse_out mgr ~env:(tool_env ()) Eio.Buf_read.take_all
             argv)
      in
      let now () = Eio.Time.now clock in
      (* The deadline is checked between reads, and the outer bound only stops
         a [log show] that hangs. *)
      match
        Eio.Time.with_timeout clock (log_timeout +. 5.) (fun () ->
            Ok
              (Sandbox_log.settle ~tag ~read ~wait:(Eio.Time.sleep clock) ~now
                 ~deadline:(now () +. log_timeout)))
      with
      | Ok (Some vs) -> Collected vs
      | Ok None ->
          Not_collected
            (Printf.sprintf
               "the system log did not show the run's end marker within %.0f \
                seconds"
               log_timeout)
      | Error `Timeout ->
          Not_collected
            (Printf.sprintf "log show did not finish within %.0f seconds"
               (log_timeout +. 5.))
      | exception (Eio.Io _ as e) ->
          Not_collected ("log show failed: " ^ Printexc.to_string e))

let relay_violations path =
  match In_channel.with_open_bin path In_channel.input_all with
  | exception Sys_error m -> Not_collected m
  | text -> (
      let lines =
        List.filter
          (fun l -> not (String.equal l ""))
          (String.split_on_char '\n' text)
      in
      match
        List.fold_right
          (fun line acc ->
            let* rest = acc in
            let* v = Sandbox_log.of_relay_line line in
            Ok (v :: rest))
          lines (Ok [])
      with
      | Ok vs -> Collected vs
      | Error m -> Not_collected ("sandbox.log: " ^ m))

let collect ~env = function
  | Nothing -> Collected []
  | Seatbelt_log { tag; since; marker } ->
      seatbelt_violations ~env ~tag ~since ~marker
  | Relay_log path -> relay_violations path
