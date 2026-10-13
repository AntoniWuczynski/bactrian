open Bactrian_core
module P = Sandbox_policy

let run_dir = "/work/proj/.bactrian/runs/r1"

let base : P.input =
  {
    platform = Macos;
    home = "/home/u";
    project_root = "/work/proj";
    bactrian_dir = "/work/proj/.bactrian";
    state_dir = run_dir ^ "/state";
    temp_dir = run_dir ^ "/tmp";
    allow_write = [ "/work/proj/out" ];
    deny_read = [ "/work/proj/private" ];
    listeners =
      [
        { port = 8080; socket = Some (run_dir ^ "/state/model.sock") };
        { port = 8081; socket = None };
      ];
    agent = [ "node"; "agent.js"; "--flag" ];
    bactrian_exe = "/opt/bin/bactrian";
  }

let linux : P.input =
  {
    base with
    platform = Linux;
    listeners =
      [
        { port = 8080; socket = Some (run_dir ^ "/state/model.sock") };
        { port = 8081; socket = Some "/run/u/egress.sock" };
      ];
  }

let generate ~exists i =
  match P.generate ~exists i with
  | Ok o -> o
  | Error e -> Alcotest.failf "generate: %s" (P.describe_error e)

let seatbelt i =
  match generate ~exists:(fun _ -> `Absent) i with
  | Seatbelt s -> s
  | Bwrap _ -> Alcotest.fail "expected a Seatbelt output"

let bwrap ~exists i =
  match generate ~exists i with
  | Bwrap argv -> argv
  | Seatbelt _ -> Alcotest.fail "expected a bwrap output"

(* ---- snapshots ---- *)

let deny_rule n =
  Printf.sprintf "(deny file-read-data (subpath (param \"DENY_%d\")))" n

let expected_profile =
  String.concat "\n"
    ([
       "(version 1)";
       "(deny default)";
       "(allow process-exec)";
       "(allow process-fork)";
       "(allow signal (target same-sandbox))";
       "(allow sysctl-read)";
       "(allow mach-lookup (global-name \
        \"com.apple.system.opendirectoryd.libinfo\"))";
       "(allow ipc-posix-sem)";
       "(allow file-read*)";
     ]
    @ List.init 23 deny_rule
    @ [
        "(allow file-read-data (subpath (param \"STATE\")) (subpath (param \
         \"TEMP\")))";
        deny_rule 23;
        "(allow file-write* (subpath (param \"STATE\")) (subpath (param \
         \"TEMP\")) (subpath (param \"WRITE_0\")) (literal \"/dev/null\") \
         (literal \"/dev/dtracehelper\"))";
        "(allow file-ioctl (literal \"/dev/dtracehelper\"))";
        "(allow network-outbound (remote ip (string-append \"localhost:\" \
         (param \"PORT_0\"))))";
        "(allow network-outbound (remote ip (string-append \"localhost:\" \
         (param \"PORT_1\"))))";
        "(allow network-outbound (remote unix-socket (path-literal (param \
         \"SOCKET_0\"))))";
        "";
      ])

let expected_params =
  [
    ("STATE", run_dir ^ "/state");
    ("TEMP", run_dir ^ "/tmp");
    ("DENY_0", "/home/u/.ssh");
    ("DENY_1", "/home/u/.gnupg");
    ("DENY_2", "/home/u/.aws");
    ("DENY_3", "/home/u/.azure");
    ("DENY_4", "/home/u/.config/gcloud");
    ("DENY_5", "/home/u/.kube");
    ("DENY_6", "/home/u/.docker");
    ("DENY_7", "/home/u/.netrc");
    ("DENY_8", "/home/u/.npmrc");
    ("DENY_9", "/home/u/.pypirc");
    ("DENY_10", "/home/u/.git-credentials");
    ("DENY_11", "/home/u/Library/Keychains");
    ("DENY_12", "/work/proj/.env");
    ("DENY_13", "/work/proj/.env.local");
    ("DENY_14", "/work/proj/.env.development");
    ("DENY_15", "/work/proj/.env.development.local");
    ("DENY_16", "/work/proj/.env.production");
    ("DENY_17", "/work/proj/.env.production.local");
    ("DENY_18", "/work/proj/.env.staging");
    ("DENY_19", "/work/proj/.env.test");
    ("DENY_20", "/work/proj/.env.test.local");
    ("DENY_21", "/work/proj/.bactrian/ca");
    ("DENY_22", "/work/proj/.bactrian/runs");
    ("DENY_23", "/work/proj/private");
    ("WRITE_0", "/work/proj/out");
    ("PORT_0", "8080");
    ("PORT_1", "8081");
    ("SOCKET_0", run_dir ^ "/state/model.sock");
  ]

let test_seatbelt_snapshot () =
  let s = seatbelt base in
  Alcotest.(check string) "profile" expected_profile s.profile;
  Alcotest.(check (list (pair string string))) "params" expected_params s.params;
  Alcotest.(check (list string))
    "argv"
    ([ "/usr/bin/sandbox-exec"; "-p"; expected_profile ]
    @ List.concat_map (fun (n, v) -> [ "-D"; n ^ "=" ^ v ]) expected_params
    @ [ "--"; "node"; "agent.js"; "--flag" ])
    s.argv

(* The host has some of the deny-listed paths and the allow_write directory. *)
let fixture_exists path =
  match path with
  | "/home/u/.ssh" | "/home/u/.config/gcloud" | "/work/proj/.bactrian/runs"
  | "/work/proj/private" | "/work/proj/out" ->
      `Dir
  | "/home/u/.netrc" | "/work/proj/.env" | "/work/proj/.env.local" -> `File
  | _ -> `Absent

let bind p = [ "--bind"; p; p ]
let ro_bind p = [ "--ro-bind"; p; p ]

let test_bwrap_snapshot () =
  let state = run_dir ^ "/state" and sock = run_dir ^ "/state/model.sock" in
  Alcotest.(check (list string))
    "argv"
    (List.concat
       [
         [ "bwrap"; "--die-with-parent"; "--new-session"; "--unshare-net" ];
         [ "--unshare-pid"; "--unshare-ipc"; "--ro-bind"; "/"; "/" ];
         [ "--dev"; "/dev"; "--proc"; "/proc"; "--tmpfs"; "/tmp" ];
         [ "--tmpfs"; "/dev/shm"; "--tmpfs"; "/run" ];
         bind state;
         bind (run_dir ^ "/tmp");
         bind "/work/proj/out";
         ro_bind sock;
         ro_bind "/run/u/egress.sock";
         [ "--tmpfs"; "/home/u/.ssh"; "--tmpfs"; "/home/u/.config/gcloud" ];
         [ "--ro-bind"; "/dev/null"; "/home/u/.netrc" ];
         [ "--ro-bind"; "/dev/null"; "/work/proj/.env" ];
         [ "--ro-bind"; "/dev/null"; "/work/proj/.env.local" ];
         [ "--tmpfs"; "/work/proj/.bactrian/runs" ];
         bind state;
         bind (run_dir ^ "/tmp");
         ro_bind sock;
         [ "--tmpfs"; "/work/proj/private" ];
         [ "--"; "/opt/bin/bactrian"; "__sandbox-init" ];
         [ "8080=" ^ sock; "8081=/run/u/egress.sock" ];
         [ "--"; "node"; "agent.js"; "--flag" ];
       ])
    (bwrap ~exists:fixture_exists linux)

(* ---- unit cases ---- *)

let test_absent_skipped () =
  let argv = bwrap ~exists:(fun _ -> `Absent) linux in
  let count a = List.length (List.filter (String.equal a) argv) in
  Alcotest.(check int) "only the three base tmpfs mounts" 3 (count "--tmpfs");
  Alcotest.(check int) "no file masks" 0 (count "/dev/null");
  Alcotest.(check bool)
    "allow_write left out" false
    (List.mem "/work/proj/out" argv)

let test_keychains_macos_only () =
  let has i = List.mem "/home/u/Library/Keychains" (P.deny_list i) in
  Alcotest.(check bool) "macos" true (has base);
  Alcotest.(check bool) "linux" false (has linux)

let test_deny_read_wins_in_state () =
  let i = { base with deny_read = [ run_dir ^ "/state/secret" ] } in
  let lines = String.split_on_char '\n' (seatbelt i).profile in
  let index prefix =
    let rec go n = function
      | [] -> Alcotest.failf "no line starting %S" prefix
      | l :: rest -> if String.starts_with ~prefix l then n else go (n + 1) rest
    in
    go 0 lines
  in
  Alcotest.(check bool)
    "own dirs readable again before the extra deny" true
    (index "(allow file-read-data" < index (deny_rule 23))

let contains hay needle = Re.execp (Re.compile (Re.str needle)) hay

let test_no_listeners () =
  let s = seatbelt { base with listeners = [] } in
  Alcotest.(check bool) "no network rule" false (contains s.profile "network")

let error_of i =
  match P.generate ~exists:(fun _ -> `Absent) i with
  | Ok _ -> None
  | Error e -> Some e

let check_error name expected i =
  match error_of i with
  | Some e when e = expected -> ()
  | Some e -> Alcotest.failf "%s: got %s" name (P.describe_error e)
  | None -> Alcotest.failf "%s: no error" name

let test_errors () =
  check_error "nul in path"
    (Nul_byte { what = "allow_write"; value = "/a\000b" })
    { base with allow_write = [ "/a\000b" ] };
  check_error "nul in agent"
    (Nul_byte { what = "agent argument"; value = "a\000" })
    { base with agent = [ "node"; "a\000" ] };
  check_error "relative home"
    (Not_absolute { what = "home"; path = "u" })
    { base with home = "u" };
  check_error "empty state"
    (Not_absolute { what = "state_dir"; path = "" })
    { base with state_dir = "" };
  check_error "relative deny"
    (Not_absolute { what = "deny_read"; path = "../x" })
    { base with deny_read = [ "../x" ] };
  check_error "relative socket"
    (Not_absolute { what = "socket"; path = "s" })
    { base with listeners = [ { port = 1; socket = Some "s" } ] };
  check_error "empty agent" Empty_agent { base with agent = [] };
  check_error "port 0" (Invalid_port 0)
    { base with listeners = [ { port = 0; socket = None } ] };
  check_error "port 65536" (Invalid_port 65536)
    { base with listeners = [ { port = 65536; socket = None } ] };
  check_error "duplicate port" (Duplicate_port 5)
    {
      base with
      listeners =
        [ { port = 5; socket = None }; { port = 5; socket = Some "/s" } ];
    };
  check_error "linux needs sockets" (Missing_socket 9)
    { linux with listeners = [ { port = 9; socket = None } ] };
  check_error "linux needs an absolute exe"
    (Not_absolute { what = "bactrian_exe"; path = "bactrian" })
    { linux with bactrian_exe = "bactrian" };
  Alcotest.(check bool)
    "macos ignores the exe" true
    (Option.is_none (error_of { base with bactrian_exe = "" }))

let test_trailing_slash_runs () =
  let i = { linux with bactrian_dir = "/work/proj/.bactrian/" } in
  let argv = bwrap ~exists:(fun _ -> `Dir) i in
  let rec after_runs = function
    | "--tmpfs" :: p :: rest when String.ends_with ~suffix:"/runs" p ->
        List.mem (run_dir ^ "/state") rest
    | _ :: rest -> after_runs rest
    | [] -> false
  in
  Alcotest.(check bool)
    "own state rebound after the runs mask" true (after_runs argv)

(* ---- properties ---- *)

let nasty_char =
  QCheck.Gen.oneof_list
    [
      '"';
      '(';
      ')';
      ' ';
      '\\';
      '\n';
      ';';
      '#';
      '\'';
      '\t';
      '%';
      'a';
      'b';
      '/';
      '.';
      '=';
    ]

(* The fixed prefix never occurs in a profile, so a path found in one was
   spliced in. *)
let gen_path =
  QCheck.Gen.map
    (fun cs -> "/zz9" ^ String.of_seq (List.to_seq cs))
    QCheck.Gen.(list_size (int_range 0 10) nasty_char)

let gen_input platform : P.input QCheck.Gen.t =
  let open QCheck.Gen in
  let* home = gen_path in
  let* project_root = gen_path in
  let* bactrian_dir = gen_path in
  let* under_runs = bool in
  let* state_suffix = gen_path in
  let state_dir =
    if under_runs then bactrian_dir ^ "/runs/r" ^ state_suffix
    else "/zz9s" ^ state_suffix
  in
  let* temp_dir = gen_path in
  let* allow_write = list_size (int_range 0 3) gen_path in
  let* deny_read = list_size (int_range 0 3) gen_path in
  let* sockets = list_size (int_range 0 3) gen_path in
  let listeners =
    List.mapi
      (fun k s : P.listener -> { port = 2000 + k; socket = Some s })
      sockets
  in
  return
    {
      P.platform;
      home;
      project_root;
      bactrian_dir;
      state_dir;
      temp_dir;
      allow_write;
      deny_read;
      listeners;
      agent = [ "agent"; "--x" ];
      bactrian_exe = "/opt/bin/bactrian";
    }

let print_input (i : P.input) =
  let list l = String.concat ";" (List.map (Printf.sprintf "%S") l) in
  Printf.sprintf "home=%S root=%S bdir=%S state=%S tmp=%S aw=[%s] dr=[%s]"
    i.home i.project_root i.bactrian_dir i.state_dir i.temp_dir
    (list i.allow_write) (list i.deny_read)

let arb platform = QCheck.make ~print:print_input (gen_input platform)

let map_paths f (i : P.input) : P.input =
  {
    i with
    home = f i.home;
    project_root = f i.project_root;
    bactrian_dir = f i.bactrian_dir;
    state_dir = f i.state_dir;
    temp_dir = f i.temp_dir;
    allow_write = List.map f i.allow_write;
    deny_read = List.map f i.deny_read;
    listeners =
      List.map
        (fun (l : P.listener) -> { l with socket = Option.map f l.socket })
        i.listeners;
  }

let sockets (i : P.input) =
  List.filter_map (fun (l : P.listener) -> l.socket) i.listeners

let prop_no_path_in_profile =
  QCheck.Test.make ~name:"no input path appears in the Seatbelt profile"
    ~count:500 (arb Macos) (fun i ->
      let s = seatbelt i in
      let given =
        [ i.state_dir; i.temp_dir ] @ i.allow_write @ P.deny_list i @ sockets i
      in
      List.for_all
        (fun p -> not (contains s.profile p))
        (i.home :: i.project_root :: i.bactrian_dir :: given)
      && List.for_all
           (fun p -> List.exists (fun (_, v) -> String.equal v p) s.params)
           given)

let prop_profile_ignores_path_values =
  QCheck.Test.make ~name:"the Seatbelt profile does not depend on path values"
    ~count:500 (arb Macos) (fun i ->
      String.equal (seatbelt i).profile
        (seatbelt (map_paths (fun p -> p ^ "\"))(allow file-read*") i)).profile)

let prop_argv_carries_params =
  QCheck.Test.make
    ~name:"sandbox-exec argv is the profile, the params, the agent" ~count:200
    (arb Macos) (fun i ->
      let s = seatbelt i in
      List.equal String.equal s.argv
        ([ "/usr/bin/sandbox-exec"; "-p"; s.profile ]
        @ List.concat_map (fun (n, v) -> [ "-D"; n ^ "=" ^ v ]) s.params
        @ [ "--"; "agent"; "--x" ]))

type op =
  | Ro_bind of string * string
  | Bind of string * string
  | Tmpfs of string

let parse_ops argv =
  let rec go acc = function
    | "--" :: _ | [] -> List.rev acc
    | "--ro-bind" :: a :: b :: tl -> go (Ro_bind (a, b) :: acc) tl
    | "--bind" :: a :: b :: tl -> go (Bind (a, b) :: acc) tl
    | "--tmpfs" :: a :: tl -> go (Tmpfs a :: acc) tl
    | _ :: tl -> go acc tl
  in
  go [] (List.tl argv)

let indices ops op =
  List.filter_map Fun.id
    (List.mapi (fun n o -> if o = op then Some n else None) ops)

let first ops op = match indices ops op with [] -> None | n :: _ -> Some n
let last ops op = List.fold_left (fun _ n -> Some n) None (indices ops op)

let exists_by_hash path =
  match Hashtbl.hash path mod 3 with 0 -> `Dir | 1 -> `File | _ -> `Absent

(* [a >? b] holds when both exist and [a] is later. *)
let ( >? ) a b = match (a, b) with Some a, Some b -> a > b | _ -> false

let prop_mount_order =
  QCheck.Test.make ~name:"bwrap: base mounts, rw binds, then deny masks"
    ~count:500 (arb Linux) (fun i ->
      let ops = parse_ops (bwrap ~exists:exists_by_hash i) in
      let base_end = first ops (Tmpfs "/run") in
      let writable =
        i.state_dir :: i.temp_dir
        :: List.filter (fun p -> exists_by_hash p <> `Absent) i.allow_write
      in
      let last_writable =
        List.fold_left
          (fun acc p ->
            match (acc, first ops (Bind (p, p))) with
            | Some a, Some b -> Some (max a b)
            | None, b -> b
            | a, None -> a)
          None writable
      in
      first ops (Ro_bind ("/", "/")) = Some 0
      && base_end >? first ops (Ro_bind ("/", "/"))
      && List.for_all (fun p -> first ops (Bind (p, p)) >? base_end) writable
      && List.for_all
           (fun d ->
             let tmpfs = first ops (Tmpfs d)
             and file = first ops (Ro_bind ("/dev/null", d)) in
             match exists_by_hash d with
             | `Dir -> tmpfs >? last_writable && Option.is_none file
             | `File -> file >? last_writable && Option.is_none tmpfs
             | `Absent -> Option.is_none tmpfs && Option.is_none file)
           (P.deny_list i))

let prop_own_dirs_survive_runs_mask =
  QCheck.Test.make
    ~name:"bwrap: the run's directories are bound again after the runs mask"
    ~count:500 (arb Linux) (fun i ->
      let ops = parse_ops (bwrap ~exists:(fun _ -> `Dir) i) in
      let runs = Filename.concat i.bactrian_dir "runs" in
      let mask = first ops (Tmpfs runs) in
      List.for_all
        (fun p ->
          (not (String.starts_with ~prefix:(runs ^ "/") p))
          || last ops (Bind (p, p)) >? mask)
        [ i.state_dir; i.temp_dir ])

let () =
  Alcotest.run "sandbox_policy"
    [
      ( "snapshots",
        [
          Alcotest.test_case "seatbelt" `Quick test_seatbelt_snapshot;
          Alcotest.test_case "bwrap" `Quick test_bwrap_snapshot;
        ] );
      ( "cases",
        [
          Alcotest.test_case "absent entries skipped" `Quick test_absent_skipped;
          Alcotest.test_case "keychains on macOS only" `Quick
            test_keychains_macos_only;
          Alcotest.test_case "deny_read wins inside the state dir" `Quick
            test_deny_read_wins_in_state;
          Alcotest.test_case "no listeners, no network" `Quick test_no_listeners;
          Alcotest.test_case "errors" `Quick test_errors;
          Alcotest.test_case "trailing slash" `Quick test_trailing_slash_runs;
        ] );
      ( "properties",
        List.map
          (fun t -> QCheck_alcotest.to_alcotest t)
          [
            prop_no_path_in_profile;
            prop_profile_ignores_path_values;
            prop_argv_carries_params;
            prop_mount_order;
            prop_own_dirs_survive_runs_mask;
          ] );
    ]
