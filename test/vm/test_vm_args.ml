(* The container's command line: flags, mounts, masks and the environment file. *)

open Bactrian_core

let project = "/p"

let input ?(allow_write = []) ?(deny = [])
    ?(command = Vm_args.Agent [ "node"; "agent.js" ]) ?(workdir = project) () :
    Vm_args.input =
  {
    runtime = "/usr/bin/docker";
    name = "bactrian-r1";
    uid = 501;
    gid = 20;
    image = "node:24@sha256:abc";
    workdir;
    env_file = "/p/.bactrian/runs/r1/agent.env";
    relay = "/c/relay";
    empty_file = "/p/.bactrian/runs/r1/vm-empty";
    project_root = project;
    runs_dir = "/p/.bactrian/runs";
    state_dir = "/p/.bactrian/runs/r1/state";
    temp_dir = "/p/.bactrian/runs/r1/tmp";
    allow_write;
    deny;
    forwards = [ 4242 ];
    command;
  }

let argv i =
  match Vm_args.argv i with
  | Ok a -> a
  | Error e -> Alcotest.failf "argv: %s" (Vm_args.describe_error e)

let error i =
  match Vm_args.argv i with
  | Ok _ -> Alcotest.fail "expected an error"
  | Error e -> Vm_args.describe_error e

(* The values of every --mount and of every --tmpfs but /tmp, in order. *)
let mounts i =
  let rec go = function
    | "--tmpfs" :: "/tmp:rw,noexec,nosuid,nodev,size=1g" :: rest -> go rest
    | ("--mount" | "--tmpfs") :: m :: rest -> m :: go rest
    | _ :: rest -> go rest
    | [] -> []
  in
  go (argv i)

let strings = Alcotest.(list string)

(* The default deny-list of a project at /p with a home at /h. *)
let default_deny =
  [
    ("/h/.ssh", `Dir);
    ("/h/.aws", `Absent);
    ("/p/.env", `File);
    ("/p/.env.local", `Absent);
    ("/p/.bactrian/ca", `Dir);
    ("/p/.bactrian/runs", `Dir);
  ]

let test_command_line () =
  Alcotest.check strings "the whole command"
    [
      "/usr/bin/docker";
      "run";
      "--rm";
      "-i";
      "--name";
      "bactrian-r1";
      "--network";
      "none";
      "--read-only";
      "--cap-drop";
      "ALL";
      "--security-opt";
      "no-new-privileges";
      "--user";
      "501:20";
      "--tmpfs";
      "/tmp:rw,noexec,nosuid,nodev,size=1g";
      "--pids-limit";
      "1024";
      "--memory";
      "4g";
      "--memory-swap";
      "4g";
      "--log-driver";
      "none";
      "--pull";
      "never";
      "--workdir";
      "/p";
      "--env-file";
      "/p/.bactrian/runs/r1/agent.env";
      "--entrypoint";
      "/bactrian/relay";
      "--mount";
      "type=bind,source=/c/relay,target=/bactrian/relay,readonly";
      "--mount";
      "type=bind,source=/p,target=/p,readonly";
      "--mount";
      "type=bind,source=/p/.bactrian/runs/r1/state,target=/p/.bactrian/runs/r1/state";
      "--mount";
      "type=bind,source=/p/.bactrian/runs/r1/tmp,target=/p/.bactrian/runs/r1/tmp";
      "--";
      "node:24@sha256:abc";
      "--forward";
      "4242";
      "--";
      "node";
      "agent.js";
    ]
    (argv (input ()))

let test_probe () =
  let a =
    argv
      (input
         ~command:
           (Probe { outside = "/p/o"; inside = "/p/i"; denied = "/p/d/s" })
         ())
  in
  Alcotest.check strings "the probe instead of an agent"
    [
      "node:24@sha256:abc";
      "--forward";
      "4242";
      "--probe";
      "/p/o";
      "/p/i";
      "/p/d/s";
    ]
    (List.filteri (fun i _ -> i >= List.length a - 7) a)

let test_masks () =
  Alcotest.check strings "masks where the mounts show the path, by depth"
    [
      "type=bind,source=/c/relay,target=/bactrian/relay,readonly";
      "type=bind,source=/p,target=/p,readonly";
      "type=bind,source=/p/.bactrian/runs/r1/vm-empty,target=/p/.env,readonly";
      "/p/.bactrian/ca:ro,mode=0755";
      "/p/.bactrian/runs:ro,mode=0755";
      "type=bind,source=/p/.bactrian/runs/r1/state,target=/p/.bactrian/runs/r1/state";
      "type=bind,source=/p/.bactrian/runs/r1/tmp,target=/p/.bactrian/runs/r1/tmp";
    ]
    (mounts (input ~deny:default_deny ()))

let test_writable_paths () =
  let deny =
    default_deny @ [ ("/p/secret", `Dir); ("/p/secret/inner", `Dir) ]
  in
  let m =
    mounts
      (input ~deny
         ~allow_write:
           [ "/p/out"; "/p/.bactrian/runs/r2/state"; "/p/secret/w"; "/w" ]
         ())
  in
  let has s = List.mem s m in
  Alcotest.(check bool)
    "a writable path" true
    (has "type=bind,source=/p/out,target=/p/out");
  Alcotest.(check bool)
    "outside the project too" true
    (has "type=bind,source=/w,target=/w");
  Alcotest.(check bool)
    "another run stays hidden" false
    (has
       "type=bind,source=/p/.bactrian/runs/r2/state,target=/p/.bactrian/runs/r2/state");
  Alcotest.(check bool)
    "a mask hides a writable path under it" false
    (has "type=bind,source=/p/secret/w,target=/p/secret/w");
  Alcotest.(check bool)
    "a mask under a mask is left out" false
    (has "/p/secret/inner:ro,mode=0755");
  Alcotest.(check bool)
    "home is not mounted, so not masked" false
    (List.exists (fun s -> String.starts_with ~prefix:"/h" s) m)

let test_masks_over_mounts () =
  let m =
    mounts
      (input
         ~deny:
           [
             ("/p/.bactrian/runs", `Dir);
             ("/p/.bactrian/runs/r1/state/key", `File);
             ("/w/private", `Dir);
             ("/p/out", `Dir);
           ]
         ~allow_write:[ "/w"; "/p/out" ] ())
  in
  let has s = List.mem s m in
  Alcotest.(check bool)
    "a file in the run's own state is masked" true
    (has
       "type=bind,source=/p/.bactrian/runs/r1/vm-empty,target=/p/.bactrian/runs/r1/state/key,readonly");
  Alcotest.(check bool)
    "a denied directory in a writable path is masked" true
    (has "/w/private:ro,mode=0755");
  Alcotest.(check bool)
    "a mask replaces a mount of the same path" false
    (has "type=bind,source=/p/out,target=/p/out")

let test_errors () =
  Alcotest.(check string)
    "a comma"
    {|sandbox.allow_write "/p/a,b" cannot be mounted: it must be absolute and hold no comma, colon, double quote or control character|}
    (error (input ~allow_write:[ "/p/a,b" ] ()));
  Alcotest.(check string)
    "relative"
    {|the working directory "p" cannot be mounted: it must be absolute and hold no comma, colon, double quote or control character|}
    (error (input ~workdir:"p" ()));
  Alcotest.(check string)
    "a working directory the container lacks"
    "the agent's working directory /elsewhere is outside the project, its run \
     and sandbox.allow_write, so the virtual machine does not show it"
    (error (input ~workdir:"/elsewhere" ()));
  Alcotest.(check string)
    "no agent" "no agent command"
    (error (input ~command:(Agent []) ()));
  Alcotest.(check string)
    "a NUL byte" {|the argument "a\000b" holds a NUL byte|}
    (error (input ~command:(Agent [ "a\000b" ]) ()))

let env_error vars =
  match Vm_args.env_file vars with
  | Ok _ -> Alcotest.fail "expected an error"
  | Error e -> Vm_args.describe_error e

let test_env_file () =
  Alcotest.(check (result string string))
    "one line each, as written"
    (Ok "A=1\nB= quoted \"x\" # not a comment\nEMPTY=\n")
    (Result.map_error Vm_args.describe_error
       (Vm_args.env_file
          [ ("A", "1"); ("B", " quoted \"x\" # not a comment"); ("EMPTY", "") ]));
  List.iter
    (fun (what, vars, reason) ->
      Alcotest.(check string)
        what
        (Printf.sprintf "the variable %S cannot be passed to the container: %s"
           (fst (List.hd vars))
           reason)
        (env_error vars))
    [
      ("newline", [ ("A", "x\ny") ], "it holds a newline");
      ("carriage return", [ ("A", "x\r") ], "it ends with a carriage return");
      ("NUL", [ ("A", "x\000") ], "it holds a NUL byte");
      ("UTF-8", [ ("A", "\xff") ], "it is not valid UTF-8");
      ("empty name", [ ("", "x") ], "the name is empty");
      ("comment", [ ("#A", "x") ], "a name starting with # is a comment");
      ("space", [ ("A B", "x") ], "the name holds = or whitespace");
      ("long", [ ("A", String.make 65536 'x') ], "it is longer than 64 KiB");
    ]

let () =
  Alcotest.run "vm_args"
    [
      ( "command line",
        [
          Alcotest.test_case "flags" `Quick test_command_line;
          Alcotest.test_case "probe" `Quick test_probe;
          Alcotest.test_case "errors" `Quick test_errors;
        ] );
      ( "mounts",
        [
          Alcotest.test_case "deny-list masks" `Quick test_masks;
          Alcotest.test_case "writable paths" `Quick test_writable_paths;
          Alcotest.test_case "masks over mounts" `Quick test_masks_over_mounts;
        ] );
      ("environment", [ Alcotest.test_case "env file" `Quick test_env_file ]);
    ]
