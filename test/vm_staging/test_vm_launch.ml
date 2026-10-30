(* [Sandbox_vm.launch] looks again at the entries bound again beside a hidden
   file before it commits to a command, under either runtime. The runtimes are
   scripts on PATH that answer the few questions [prepare] asks, and a relay
   file in the cache that the build would otherwise make. *)

open Bactrian_runtime
module Vm_args = Bactrian_core.Vm_args

let write ?(perm = 0o644) path text =
  Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_trunc ] perm path
    (fun oc -> output_string oc text)

let runtime_script = function
  | `Docker ->
      {|#!/bin/sh
case "$1" in
  version) echo arm64 ;;
  image) echo sha256:0123 ;;
esac
|}
  | `Container ->
      {|#!/bin/sh
case "$1" in
  system) echo '{"status":"running","host":{"architecture":"arm64"}}' ;;
  image) echo '[{"configuration":{"descriptor":{"digest":"sha256:0123"}}}]' ;;
  list) echo '[]' ;;
  create) echo 'create ran' >&2; exit 1 ;;
esac
|}

let exe_name = function `Docker -> "docker" | `Container -> "container"

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect
    ~finally:(fun () -> Unix.putenv name (Option.value previous ~default:""))
    f

(* A project with a hidden .env and a directory beside it that the command
   binds again, the fake runtime on PATH, and the relay in the cache. *)
let with_runtime kind f =
  let root = Filename.temp_dir "bactrian-launch" "" in
  Fun.protect ~finally:(fun () -> Proc_util.remove_tree root) @@ fun () ->
  let bin = Filename.concat root "bin"
  and project = Filename.concat root "project"
  and run_dir = Filename.concat root "run"
  and cache = Filename.concat root "cache" in
  List.iter (fun d -> Unix.mkdir d 0o700) [ bin; project; run_dir; cache ];
  write ~perm:0o755 (Filename.concat bin (exe_name kind)) (runtime_script kind);
  write (Filename.concat project ".env") "SECRET";
  Unix.mkdir (Filename.concat project "sub") 0o755;
  with_env "PATH" (bin ^ ":/usr/bin:/bin") @@ fun () ->
  with_env "XDG_CACHE_HOME" cache @@ fun () ->
  Eio_main.run @@ fun env ->
  let process_mgr = Eio.Stdenv.process_mgr env
  and clock = Eio.Stdenv.clock env in
  let runtime =
    match Vm_runtime.detect ~process_mgr ~clock (Some kind) with
    | Ok r -> r
    | Error m -> Alcotest.failf "no runtime: %s" m
  in
  let relay_dir = Vm_runtime.relay_cache_dir () in
  Unix.mkdir (Filename.dirname relay_dir) 0o700;
  Unix.mkdir relay_dir 0o700;
  write ~perm:0o700
    (Filename.concat relay_dir (Vm_runtime.relay_name runtime))
    "#!/bin/sh\n";
  match Sandbox_vm.prepare ~env (Some kind) ~image:"img" with
  | Error m -> Alcotest.failf "prepare: %s" m
  | Ok t -> f t ~project ~run_dir

let layout project : Sandbox_vm.layout =
  {
    project_root = project;
    bactrian_dir = Filename.concat project ".bactrian";
    allow_write = [];
    read_only = [];
    deny = [ (Filename.concat project ".env", `File) ];
  }

let invocation project : Vm_args.invocation =
  let path = Filename.concat project "sub" in
  let st = Unix.stat path in
  {
    argv = [ "docker"; "run" ];
    stages = [];
    rebound =
      [
        ( path,
          {
            name = "sub";
            kind = Dir;
            identity = Some (st.st_dev, st.st_ino);
            masked_file = false;
          } );
      ];
  }

let swap project =
  let sub = Filename.concat project "sub" in
  Unix.rmdir sub;
  Unix.symlink "/" sub

let test_swapped kind () =
  with_runtime kind @@ fun t ~project ~run_dir ->
  match
    Sandbox_vm.launch t (layout project) ~run_dir ~name:"bactrian-x"
      (let inv = invocation project in
       swap project;
       inv)
  with
  | Ok _ -> Alcotest.fail "launched with an entry swapped after the listing"
  | Error m ->
      Printf.printf "%s\n" m;
      Alcotest.(check bool)
        "stopped by the second look" true
        (Substring.contains m "changed after it was listed")

let test_unchanged () =
  with_runtime `Docker @@ fun t ~project ~run_dir ->
  let inv = invocation project in
  Alcotest.(check (result (list string) string))
    "the invocation's own command" (Ok inv.argv)
    (Sandbox_vm.launch t (layout project) ~run_dir ~name:"bactrian-x" inv)

let () =
  Alcotest.run "vm_launch"
    [
      ( "launch",
        [
          Alcotest.test_case "docker, nothing changed" `Quick test_unchanged;
          Alcotest.test_case "docker, an entry swapped" `Quick
            (test_swapped `Docker);
          Alcotest.test_case "container, an entry swapped" `Quick
            (test_swapped `Container);
        ] );
    ]
