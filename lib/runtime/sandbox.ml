open Bactrian_core
module Policy = Sandbox_policy

type mode = [ `Auto | `Strict | `Off ]

let modes = [ ("auto", `Auto); ("strict", `Strict); ("off", `Off) ]

let mode_name : mode -> string = function
  | `Auto -> "auto"
  | `Strict -> "strict"
  | `Off -> "off"

type backend = Experiment.sandbox_backend

let backends =
  [
    ("auto", `Auto);
    ("vm", `Vm);
    ("seatbelt", `Seatbelt);
    ("bubblewrap", `Bubblewrap);
  ]

let host_backend () : [ `Seatbelt | `Bubblewrap ] =
  if Sys.file_exists "/usr/bin/sandbox-exec" then `Seatbelt else `Bubblewrap

type applied_backend =
  | Seatbelt_backend
  | Bubblewrap_backend
  | Vm_backend of { runtime : string; image : string; image_id : string }

type label = {
  mode : mode;
  applied : bool;
  reason : string option;
  backend : applied_backend option;
}

let label_json (l : label) =
  `Assoc
    ([ ("mode", `String (mode_name l.mode)); ("applied", `Bool l.applied) ]
    @ (match l.reason with Some r -> [ ("reason", `String r) ] | None -> [])
    @
    match l.backend with
    | None -> []
    | Some Seatbelt_backend -> [ ("backend", `String "seatbelt") ]
    | Some Bubblewrap_backend -> [ ("backend", `String "bubblewrap") ]
    | Some (Vm_backend { runtime; image; image_id }) ->
        [
          ("backend", `String "vm");
          ( "vm",
            `Assoc
              [
                ("runtime", `String runtime);
                ("image", `String image);
                ("image_id", `String image_id);
              ] );
        ])

(* How the sandbox is made here: bubblewrap carries the path it was found at.
   The virtual machine runs the agent only: checkers use the host's own. *)
type engine = Seatbelt | Bubblewrap of string | Vm of Sandbox_vm.t

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

let unsandboxed mode reason =
  Unsandboxed { mode; applied = false; reason = Some reason; backend = None }

let off = unsandboxed `Off "sandbox: off"
let http_mode mode = unsandboxed mode "http mode"

let label = function
  | Sandboxed { mode; host } ->
      {
        mode;
        applied = true;
        reason = None;
        backend =
          Some
            (match host.engine with
            | Seatbelt -> Seatbelt_backend
            | Bubblewrap _ -> Bubblewrap_backend
            | Vm vm ->
                Vm_backend
                  {
                    runtime = Sandbox_vm.runtime_name vm;
                    image = Sandbox_vm.image vm;
                    image_id = Sandbox_vm.image_id vm;
                  });
      }
  | Unsandboxed l -> l

let vm = function
  | Sandboxed { host = { engine = Vm _; _ }; _ } -> true
  | Sandboxed { host = { engine = Seatbelt | Bubblewrap _; _ }; _ }
  | Unsandboxed _ ->
      false

type settings = {
  mode : mode;
  backend : backend;
  image : string option;
  runtime : Experiment.sandbox_runtime option;
  allow_write : string list;
  deny_read : string list;
  bactrian_exe : string;
  bactrian_dir : string;
}

let ( let* ) = Result.bind

(* Paths *)

(* Symbolic links resolved as far as the path exists, because Seatbelt matches
   resolved paths. *)
let rec resolve path =
  match Unix.realpath path with
  | real -> real
  | exception Unix.Unix_error _ ->
      let parent = Filename.dirname path in
      if String.equal parent path then path
      else Filename.concat (resolve parent) (Filename.basename path)

let resolved path = resolve (Proc_util.absolute path)

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

let platform = function
  | Seatbelt -> Policy.Macos
  | Bubblewrap _ | Vm _ -> Linux

let input_of (host : host) ~state_dir ~temp_dir ~listeners ~tag ~preload agent :
    Policy.input =
  {
    platform = platform host.engine;
    home = host.home;
    project_root = host.project_root;
    bactrian_dir = host.bactrian_dir;
    state_dir = Option.map resolved state_dir;
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
      | Bubblewrap _, [] | (Seatbelt | Vm _), ([] | _ :: _) -> Ok argv)

(* Linux tools *)

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
  match
    Proc_util.find_executable ~path:(Sys.getenv_opt "PATH") ~cwd:(Sys.getcwd ())
      "bwrap"
  with
  | None ->
      Error
        "bubblewrap (bwrap) is not installed. Fix: install it, for example \
         with sudo apt install bubblewrap"
  | Some path -> (
      match
        Eio.Process.parse_out
          (Eio.Stdenv.process_mgr env)
          ~env:(Proc_util.tool_env ()) Eio.Buf_read.take_all
          [ path; "--version" ]
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

let greet ~sw listener =
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server listener
        ~on_error:(fun (_ : exn) -> ())
        (fun flow _ -> Eio.Flow.copy_string Sandbox_probe.greeting flow))

let last_line text =
  String.split_on_char '\n' text
  |> List.filter (fun l -> not (String.equal (String.trim l) ""))
  |> List.fold_left (fun _ l -> Some l) None

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
                   (Proc_util.describe_exit status)
                   (match last_line err with Some l -> ": " ^ l | None -> ""))))

let host_netns () =
  match Unix.readlink "/proc/self/ns/net" with
  | ns -> Some ns
  | exception Unix.Unix_error _ -> None

let probe ~env (host : host) =
  let id = Proc_util.hex (Mirage_crypto_rng.generate 6) in
  let dir = Filename.concat host.bactrian_dir ("sandbox-probe-" ^ id) in
  let state_dir = Filename.concat dir "state"
  and temp_dir = Filename.concat dir "tmp"
  and outside = Filename.concat dir "outside"
  and denied_dir = Filename.concat dir "denied" in
  let denied = Filename.concat denied_dir "secret" in
  Fun.protect ~finally:(fun () -> remove_tree ~env dir) @@ fun () ->
  List.iter
    (fun d -> Unix.mkdir d 0o700)
    [ dir; state_dir; temp_dir; denied_dir ];
  Out_channel.with_open_bin denied (fun oc -> output_string oc "secret");
  let host =
    { host with deny_read = host.deny_read @ [ resolved denied_dir ] }
  in
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
    | Seatbelt | Vm _ -> Ok None
    | Bubblewrap _ ->
        let* dir = socket_dir ~sw in
        let path = Filename.concat dir "probe.sock" in
        greet ~sw (Eio.Net.listen ~sw ~backlog:8 net (`Unix path));
        Ok (Some path)
  in
  let* argv =
    command host ~state_dir:(Some state_dir) ~temp_dir
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
        "--denied";
        resolved denied;
      ]
  in
  let vars =
    [| Proc_util.path_var (); "HOME=" ^ state_dir; "TMPDIR=" ^ temp_dir |]
  in
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

let host_engine ~env : [ `Seatbelt | `Bubblewrap ] -> (engine, string) result =
  function
  | `Seatbelt ->
      if Sys.file_exists "/usr/bin/sandbox-exec" then Ok Seatbelt
      else Error "backend seatbelt needs macOS (/usr/bin/sandbox-exec)"
  | `Bubblewrap ->
      if Sys.file_exists "/proc/self/ns/net" then bubblewrap ~env
      else Error "backend bubblewrap needs Linux"

let make_host ~env settings engine =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700
    Eio.Path.(Eio.Stdenv.fs env / Proc_util.absolute settings.bactrian_dir);
  let bactrian_dir = resolved settings.bactrian_dir in
  {
    engine;
    bactrian_exe = resolved settings.bactrian_exe;
    home = resolved (home ());
    project_root = Filename.dirname bactrian_dir;
    bactrian_dir;
    allow_write = List.map resolved settings.allow_write;
    deny_read = List.map resolved settings.deny_read;
  }

let check_host ~env settings backend =
  let* engine = host_engine ~env backend in
  let host = make_host ~env settings engine in
  match probe ~env host with
  | Ok () -> Ok host
  | Error cause -> (
      match engine with
      | Bubblewrap path when apparmor_restricted () ->
          Error (cause ^ ". " ^ apparmor_fix path)
      | Bubblewrap _ | Seatbelt | Vm _ -> Error cause)

(* The deny-list with what is at each path, as the virtual machine masks it. *)
let vm_layout ?(extra = []) ?(read_only = []) (host : host) : Sandbox_vm.layout
    =
  let input =
    input_of host ~state_dir:None ~temp_dir:host.bactrian_dir ~listeners:[]
      ~tag:"" ~preload:None [ "agent" ]
  in
  (* The home a mount might show is the host's, so its credential paths are
     the host's (the keychains on macOS). The whole of .bactrian is masked:
     the run's own directories and allow_write paths come back beneath it. *)
  let credentials =
    match host_backend () with
    | `Seatbelt -> Policy.Macos
    | `Bubblewrap -> Linux
  in
  {
    project_root = host.project_root;
    bactrian_dir = host.bactrian_dir;
    allow_write = host.allow_write;
    read_only;
    deny =
      List.map
        (fun p -> (p, kind p))
        (host.bactrian_dir
        :: Policy.deny_list ~resolve:resolved ~credentials
             { input with deny_read = input.deny_read @ extra });
  }

(* The probe runs as a run would, with its directories under .bactrian/runs,
   a masked directory and a masked file in its state directory, and another
   run beside it. Under container the masked file's directory is shown
   read-only, as container requires of every directory holding a masked file.
   Docker's stays writable, as a run's may be. *)
let check_vm ~env settings =
  let* image =
    Option.to_result settings.image
      ~none:
        "sandbox.image is not set (the image the agent runs in, holding its \
         runtime, such as node:24-alpine@sha256:...)"
  in
  let* vm = Sandbox_vm.prepare ~env settings.runtime ~image in
  let host = make_host ~env settings (Vm vm) in
  let runs = Filename.concat host.bactrian_dir "runs" in
  let (_ : string list) = Sandbox_vm.remove_stale_staging ~runs_dir:runs in
  let id = "sandbox-probe-" ^ Proc_util.hex (Mirage_crypto_rng.generate 6) in
  let dir = Filename.concat runs id
  and other = Filename.concat runs (id ^ "-other") in
  let state_dir = Filename.concat dir "state" in
  let denied_dir = Filename.concat state_dir "denied"
  and mask_dir = Filename.concat state_dir "masked" in
  let masked = Filename.concat mask_dir "secret" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700
    Eio.Path.(Eio.Stdenv.fs env / runs);
  Fun.protect ~finally:(fun () ->
      remove_tree ~env dir;
      remove_tree ~env other)
  @@ fun () ->
  List.iter
    (fun d -> Unix.mkdir d 0o700)
    [ dir; state_dir; Filename.concat dir "tmp"; denied_dir; mask_dir; other ];
  List.iter
    (fun f -> Out_channel.with_open_bin f (fun oc -> output_string oc "secret"))
    [
      Filename.concat denied_dir "secret";
      masked;
      Filename.concat mask_dir "beside";
      Filename.concat other "data";
    ];
  let* () =
    Sandbox_vm.probe ~env vm
      (vm_layout
         ~extra:[ resolved denied_dir; resolved masked ]
         ~read_only:
           (match Sandbox_vm.kind vm with
           | `Docker -> []
           | `Container -> [ resolved mask_dir ])
         host)
      ~dir
      {
        outside = Filename.concat dir "outside";
        inside = Filename.concat state_dir "inside";
        denied = Filename.concat denied_dir "secret";
        masked;
        masked_case = Filename.concat mask_dir "SECRET";
        root = "/" ^ id;
        other_run = Filename.concat other "written";
      }
  in
  Ok host

let exposure =
  "the agent can read the launch environment of every process of this user \
   that is not an Apple binary (decision record 0010)"

let preflight ?(seatbelt_hint = true) ~env ~warn settings =
  match settings.mode with
  | `Off -> Ok off
  | (`Auto | `Strict) as mode -> (
      let guard f =
        try f ()
        with (Unix.Unix_error _ | Eio.Io _ | Sys_error _) as e ->
          Error (Printexc.to_string e)
      in
      let sandboxed = function
        | Ok host -> Ok (Sandboxed { mode; host })
        | Error cause -> (
            match mode with
            | `Auto ->
                warn
                  ("sandbox: auto: running unsandboxed, because the sandbox is \
                    unavailable: " ^ cause);
                Ok (unsandboxed mode ("unavailable: " ^ cause))
            | `Strict ->
                Error
                  ("sandbox: strict, but the sandbox is unavailable: " ^ cause))
      in
      let on_host backend =
        sandboxed (guard (fun () -> check_host ~env settings backend))
      in
      (* Under auto a virtual machine that does not work gives way to the
         host's own sandbox. *)
      let in_vm ~fix =
        match guard (fun () -> check_vm ~env settings) with
        | Ok host -> Ok (Sandboxed { mode; host })
        | Error cause -> (
            match mode with
            | `Strict ->
                Error
                  ("sandbox: strict, but the virtual machine sandbox is \
                    unavailable: " ^ cause ^ fix)
            | `Auto ->
                warn
                  ("sandbox: auto: the virtual machine sandbox is unavailable ("
                 ^ cause ^ "), so the host's own sandbox is used");
                on_host (host_backend ()))
      in
      match (settings.backend, host_backend ()) with
      | ((`Seatbelt | `Bubblewrap) as b), _ -> on_host b
      | `Vm, _ -> in_vm ~fix:""
      | `Auto, `Bubblewrap -> on_host `Bubblewrap
      | `Auto, `Seatbelt -> (
          let seatbelt_fix =
            if seatbelt_hint then
              ". Or set sandbox.backend: seatbelt to accept that " ^ exposure
            else ""
          in
          match (settings.image, mode) with
          | Some _, (`Auto | `Strict) -> in_vm ~fix:seatbelt_fix
          | None, `Strict ->
              Error
                ("sandbox: strict on macOS runs the agent in a virtual \
                  machine, and sandbox.image is not set. Fix: set \
                  sandbox.image to an image that holds the agent's runtime and \
                  run OrbStack, Docker Desktop or Apple's container"
               ^ seatbelt_fix)
          | None, `Auto ->
              warn
                ("sandbox: auto: the agent runs under Seatbelt, where "
               ^ exposure
               ^ ". Fix: set sandbox.image and run OrbStack, Docker Desktop or \
                  Apple's container to run it in a virtual machine"
                ^
                if seatbelt_hint then
                  ", or set sandbox.backend: seatbelt to accept this"
                else "");
              on_host `Seatbelt))

let backend_available : backend -> (unit, string) result = function
  | `Auto | `Vm -> Ok ()
  | `Seatbelt -> (
      match host_backend () with
      | `Seatbelt -> Ok ()
      | `Bubblewrap -> Error "backend seatbelt is only available on macOS")
  | `Bubblewrap -> (
      match host_backend () with
      | `Bubblewrap -> Ok ()
      | `Seatbelt -> Error "backend bubblewrap is only available on Linux")

let checker_settings settings =
  {
    settings with
    backend =
      (match settings.backend with
      | (`Seatbelt | `Bubblewrap) as b -> b
      | `Auto | `Vm -> (host_backend () :> backend));
  }

(* One run *)

type run = {
  run_id : string;
  run_dir : string;
  state_dir : string;
  temp_dir : string;
  port : int;
  listen_unix : string -> (unit, string) result;
  preload : string option;
  cwd : string;
  agent_env : (string * string) list;
}

type session = {
  argv : string list;
  env : (string * string) list;
  replaces_env : bool;
  fds : (int * Eio_unix.Fd.t) list;
}

type coverage = Sandbox_violations.coverage =
  | Seatbelt_log
  | Relay_failures_only

let coverage_name = Sandbox_violations.coverage_name
let coverage_of_name = Sandbox_violations.coverage_of_name

type violations = Sandbox_violations.violations =
  | Collected of {
      items : Sandbox_log.violation list;
      coverage : coverage;
      gaps : string list;
    }
  | Not_collected of string

let unsandboxed_violations = Sandbox_violations.unsandboxed_violations
let violations_json = Sandbox_violations.violations_json
let describe_violations = Sandbox_violations.describe_violations

type started =
  | Nothing
  | Seatbelt_log of { tag : string; since : float; marker : string }
  | Relay_log of {
      path : string;
      writer : Eio_unix.sink_ty Eio.Resource.t;
      drained : bool Eio.Promise.or_exn;
    }

(* The forwarder's or the bridge's violation log: they get the write end of a
   pipe, not the file, so a process that reopens its descriptor through /proc
   can at most add lines, never truncate or rewrite them. *)
let relay_log ~sw run ~argv ~env ~replaces_env =
  let path = Filename.concat run.run_dir "sandbox.log" in
  let* fd =
    match
      Unix.openfile path [ O_WRONLY; O_CREAT; O_APPEND; O_CLOEXEC ] 0o600
    with
    | fd -> Ok fd
    | exception Unix.Unix_error (e, _, _) ->
        Error (path ^ ": " ^ Unix.error_message e)
  in
  let reader, writer = Eio_unix.pipe sw in
  let drained =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Fun.protect
          ~finally:(fun () -> Unix.close fd)
          (fun () -> Sandbox_violations.drain reader fd))
  in
  Ok
    ( {
        argv;
        env = env @ [ (Sandbox_init.log_variable, "3") ];
        replaces_env;
        fds = [ (3, Eio_unix.Resource.fd writer) ];
      },
      Relay_log { path; writer :> Eio_unix.sink_ty Eio.Resource.t; drained } )

let start ~sw t run agent =
  match t with
  | Unsandboxed _ ->
      Ok ({ argv = agent; env = []; replaces_env = false; fds = [] }, Nothing)
  | Sandboxed { host; mode = _ } -> (
      let tag = Sandbox_log.tag run.run_id in
      match host.engine with
      | Seatbelt ->
          let since = Unix.gettimeofday () in
          let* argv =
            command host ~state_dir:(Some run.state_dir) ~temp_dir:run.temp_dir
              ~listeners:[ { port = run.port; socket = None } ]
              ~tag ~preload:run.preload agent
          in
          let marker = Filename.concat run.temp_dir "sandbox-end-marker" in
          Ok
            ( { argv; env = []; replaces_env = false; fds = [] },
              Seatbelt_log { tag; since; marker } )
      | Bubblewrap _ ->
          let* dir = socket_dir ~sw in
          let socket = Filename.concat dir "model.sock" in
          let* () = run.listen_unix socket in
          let* argv =
            command host ~state_dir:(Some run.state_dir) ~temp_dir:run.temp_dir
              ~listeners:[ { port = run.port; socket = Some socket } ]
              ~tag ~preload:run.preload agent
          in
          relay_log ~sw run ~argv ~env:[] ~replaces_env:false
      | Vm vm ->
          let* bridge =
            Sandbox_vm.start ~sw vm (vm_layout host)
              ~bactrian_exe:host.bactrian_exe ~run_id:run.run_id
              ~run_dir:run.run_dir ~state_dir:(resolved run.state_dir)
              ~temp_dir:(resolved run.temp_dir) ~cwd:(resolved run.cwd)
              ~port:run.port ~agent_env:run.agent_env agent
          in
          relay_log ~sw run ~argv:bridge.argv ~env:bridge.env ~replaces_env:true
      )

(* A checker reaches nothing (no listener), writes only to its temporary
   directory ([allow_write] does not apply) and reads nothing of any run. *)
let checker t ~temp_dir argv =
  match t with
  | Unsandboxed _ -> Ok argv
  | Sandboxed { host = { engine = Vm _; _ }; _ } ->
      Error "checkers run in the host's own sandbox, never the virtual machine"
  | Sandboxed ({ host = { engine = Seatbelt | Bubblewrap _; _ }; _ } as s) ->
      let host = s.host in
      command
        { host with allow_write = [] }
        ~state_dir:None ~temp_dir ~listeners:[]
        ~tag:(Sandbox_log.tag "checker")
        ~preload:None argv

let strips_preload = function
  | Sandboxed { host = { engine = Bubblewrap path; _ }; _ } -> (
      match Unix.stat path with
      | st -> st.st_perm land 0o4000 <> 0
      | exception Unix.Unix_error _ -> false)
  | Sandboxed { host = { engine = Seatbelt | Vm _; _ }; _ } | Unsandboxed _ ->
      false

let own_port = function
  | Sandboxed { host = { engine = Seatbelt; _ }; _ } -> true
  | Sandboxed { host = { engine = Bubblewrap _ | Vm _; _ }; _ } | Unsandboxed _
    ->
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
           "the sandboxed agent can read the command line and environment of \
            the MCP stdio server%s %s, as it can any process of this user that \
            is not an Apple binary: give them no credential you would not give \
            the agent, or use the vm backend"
           (match exposed with [ _ ] -> "" | _ -> "s")
           (String.concat ", " exposed))
  | Sandboxed { host = { engine = Seatbelt | Bubblewrap _ | Vm _; _ }; _ }, _
  | Unsandboxed _, _ ->
      None

(* A deny-list mask is applied after the library is bound, so a library under
   one is hidden again. Only bubblewrap binds the library at all. *)
let preload_hidden t library =
  match t with
  | Unsandboxed _ -> false
  | Sandboxed { host = { engine = Seatbelt | Vm _; _ }; _ } -> false
  | Sandboxed { host = { engine = Bubblewrap _; _ } as host; _ } ->
      let dir = host.bactrian_dir in
      let input =
        input_of host ~state_dir:(Some dir) ~temp_dir:dir ~listeners:[] ~tag:""
          ~preload:None [ "agent" ]
      in
      List.exists
        (fun denied ->
          String.equal denied library
          || String.starts_with ~prefix:(denied ^ "/") library)
        (Policy.deny_list ~resolve:resolved input)

let collect ~env = function
  | Nothing -> Sandbox_violations.unsandboxed_violations
  | Seatbelt_log { tag; since; marker } ->
      Sandbox_violations.seatbelt ~env ~tag ~since ~marker
  | Relay_log { path; writer; drained } ->
      Sandbox_violations.relay ~env ~path ~writer ~drained
