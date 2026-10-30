(* The virtual machine's clock and allow_read across kinds of image and cache
   (decisions 0020 and 0021): an image whose glibc is too old for libfaketime,
   one that carries both C libraries' loaders, one of another architecture, a
   static agent, a cache that cannot be trusted, a file and a home-like tree as
   allow_read. Like test_vm, on docker, or container when BACTRIAN_VM_RUNTIME
   is container, and skipped without one. *)

open Bactrian_runtime
open Acceptance_world
open Vm_support

(* Debian 11 of 2026-08-24, glibc 2.31. *)
let bullseye_image =
  "debian:bullseye-slim@sha256:e5b6442dd2e9684cf5e87d8338b5968f3b348636fc0be6d7850a381e3731a2bd"

let docker_only what =
  match vm_runtime with
  | `Docker -> ()
  | `Container ->
      Printf.printf "%s needs docker: skipped\n" what;
      Alcotest.skip ()

(* The shell command that builds an image from [base] with one more step, for
   docker, of [platform] when given. *)
let build_command ?platform ~tag ~base ~step () =
  Printf.sprintf
    "printf 'FROM %%s\\nRUN %%s\\n' %s %s | docker build -q%s -t %s - > \
     /dev/null"
    (Filename.quote base) (Filename.quote step)
    (match platform with
    | Some p -> " --platform " ^ Filename.quote p
    | None -> "")
    (Filename.quote tag)

let derived ~tag ~base ~step =
  gate [ base ];
  run_or_fail (build_command ~tag ~base ~step ())

let remove_image tag = ignore (quiet ("docker rmi -f " ^ Filename.quote tag))

let with_env bindings f =
  let previous = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) bindings in
  List.iter (fun (k, v) -> Unix.putenv k v) bindings;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (k, v) -> Unix.putenv k (Option.value v ~default:""))
        previous)
    f

let write path text =
  Out_channel.with_open_bin path (fun oc -> output_string oc text)

let clock_label (r : Run.result) =
  Yojson.Safe.to_string (Bactrian_core.Clock_label.to_json r.clock)

(* The shell agent runs and answers, whatever the clock could do for it. *)
let answered ~image ~name ~argv expected =
  gate [ image ];
  with_world ~vm:image name @@ fun w ->
  let script = Filename.concat w.dir "agent.sh" in
  write script shell_agent;
  let r =
    w.execute
      ~agent:(Command (argv script))
      ~variant_env:[ ("PROBE", "echo ok") ]
      ~sim_now:sim_now_text ()
  in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  check_sandboxed w r;
  Alcotest.(check string) "the agent answered" "ok" (reply_text r);
  Alcotest.(check string) "the clock" expected (clock_label r);
  no_container r

(* glibc 2.31 cannot load the library, which needs GLIBC_2.34: the agent starts,
   contract-only, where it would otherwise not start at all. *)
let old_glibc () =
  answered ~image:bullseye_image ~name:"vm-clock-old-glibc"
    ~argv:(fun script -> [ "/bin/sh"; script ])
    {|{"mode":"contract_only","reason":"library-not-found"}|}

(* The library is the one the agent's executable is linked against. An image
   that also has the other C library's loader (Debian with musl installed) does
   not fail its glibc programs for it. *)
let mixed_libc () =
  docker_only "an image with both loaders";
  let tag = "bactrian-test-mixed-libc:1" in
  derived ~tag ~base:python_image ~step:"touch /lib/ld-musl-$(uname -m).so.1";
  Fun.protect ~finally:(fun () -> remove_image tag) @@ fun () ->
  ignore
    (clocked ~image:tag ~name:"vm-clock-mixed-libc"
       ~files:[ ("clock.py", clock_python, 0o644) ]
       (fun w -> [ "python3"; "-I"; Filename.concat w.dir "clock.py" ])
      : Yojson.Safe.t)

(* A statically linked agent takes no preload. *)
let static_agent () =
  docker_only "an image with a static busybox";
  let tag = "bactrian-test-static-busybox:1" in
  derived ~tag ~base:Vm_runtime.build_image
    ~step:"apk add --no-cache busybox-static";
  Fun.protect ~finally:(fun () -> remove_image tag) @@ fun () ->
  answered ~image:tag ~name:"vm-clock-static"
    ~argv:(fun script -> [ "/bin/busybox.static"; "sh"; script ])
    {|{"mode":"contract_only","reason":"static-binary"}|}

(* An image of the other architecture, run under emulation, is given no library,
   which is built for the runtime's own: the agent starts, contract-only, where
   the wrong library would stop it (musl) or be ignored under an enforced label
   (glibc). Skipped where the runtime cannot run the other architecture. *)
let foreign_architecture () =
  docker_only "an image of another architecture";
  let platform =
    let ic =
      Unix.open_process_in "docker version --format '{{.Server.Arch}}'"
    in
    let arch = String.trim (In_channel.input_all ic) in
    ignore (Unix.close_process_in ic : Unix.process_status);
    match arch with "arm64" -> "linux/amd64" | _ -> "linux/arm64"
  in
  List.iter
    (fun (base, libc) ->
      let tag = "bactrian-test-foreign-" ^ libc ^ ":1" in
      gate [];
      if not (quiet (build_command ~platform ~tag ~base ~step:"true" ())) then begin
        Printf.printf "the runtime cannot build a %s image: skipped\n" platform;
        Alcotest.skip ()
      end;
      Fun.protect ~finally:(fun () -> remove_image tag) @@ fun () ->
      answered ~image:tag
        ~name:("vm-clock-foreign-" ^ libc)
        ~argv:(fun script -> [ "/bin/sh"; script ])
        {|{"mode":"contract_only","reason":"library-not-found"}|})
    [ (Vm_runtime.build_image, "musl"); (python_image, "glibc") ]

(* A library cache that others could write to is refused, and the run goes on
   contract-only (vm) instead of failing. *)
let untrusted_cache () =
  gate [ node_image ];
  let relay, name =
    Eio_main.run @@ fun env ->
    let process_mgr = Eio.Stdenv.process_mgr env
    and clock = Eio.Stdenv.clock env in
    match Vm_runtime.detect ~process_mgr ~clock (Some vm_runtime) with
    | Error m -> Alcotest.failf "no runtime: %s" m
    | Ok rt -> (
        match Vm_runtime.relay ~process_mgr ~clock rt with
        | Ok relay -> (relay, rt)
        | Error m -> Alcotest.failf "no relay: %s" m)
  in
  let base = Filename.concat (Sys.getcwd ()) "tmp-vm-faketime-cache" in
  let remove () = ignore (Sys.command ("rm -rf " ^ Filename.quote base)) in
  remove ();
  Fun.protect ~finally:remove @@ fun () ->
  with_env [ ("XDG_CACHE_HOME", base) ] @@ fun () ->
  let dir = Vm_runtime.relay_cache_dir () in
  run_or_fail (Printf.sprintf "mkdir -p %s" (Filename.quote dir));
  Unix.chmod dir 0o700;
  run_or_fail
    (Printf.sprintf "cp %s %s" (Filename.quote relay) (Filename.quote dir));
  let parent = Filename.concat dir (Vm_runtime.faketime_name name) in
  let refused what needle =
    match Vm_runtime.faketime_cached name Bactrian_core.Vm_exec_probe.Musl with
    | Ok _ -> Alcotest.failf "%s: the library was used" what
    | Error m ->
        Printf.printf "%s: %s\n" what m;
        Alcotest.(check bool) what true (contains m needle)
  in
  Unix.mkdir parent 0o700;
  Unix.chmod parent 0o777;
  refused "a directory others can write to" "writable by its group or by others";
  Unix.chmod parent 0o700;
  let musl = Filename.concat parent "musl" in
  Unix.mkdir musl 0o700;
  Unix.symlink "/bin/sh" (Filename.concat musl "libfaketimeMT.so.1");
  refused "a symbolic link" "is not a regular file";
  Unix.unlink (Filename.concat musl "libfaketimeMT.so.1");
  Unix.chmod parent 0o777;
  let contract_vm = {|{"mode":"contract_only","reason":"vm"}|} in
  with_world ~vm:node_image "vm-clock-untrusted" @@ fun w ->
  let script = Filename.concat w.dir "agent.sh" in
  write script shell_agent;
  let r =
    w.execute
      ~agent:(Command [ "/bin/sh"; script ])
      ~variant_env:[ ("PROBE", "echo ok") ]
      ~sim_now:sim_now_text ()
  in
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  Alcotest.(check string) "the agent answered" "ok" (reply_text r);
  Alcotest.(check string)
    "contract-only, not a failed start" contract_vm (clock_label r);
  no_container r

(* Both libraries build one after the other from a cold cache. The archive is
   downloaded once, as root in a rootful Docker, and the second build refuses
   it unless the download handed it to the user. *)
let libraries_in_turn () =
  gate [];
  let base = Filename.concat (Sys.getcwd ()) "tmp-vm-faketime-turn" in
  let remove () = ignore (Sys.command ("rm -rf " ^ Filename.quote base)) in
  remove ();
  Fun.protect ~finally:remove @@ fun () ->
  with_env [ ("XDG_CACHE_HOME", base) ] @@ fun () ->
  Eio_main.run @@ fun env ->
  let process_mgr = Eio.Stdenv.process_mgr env
  and clock = Eio.Stdenv.clock env in
  let rt =
    match Vm_runtime.detect ~process_mgr ~clock (Some vm_runtime) with
    | Ok rt -> rt
    | Error m -> Alcotest.failf "no runtime: %s" m
  in
  List.iter
    (fun libc ->
      let name = Bactrian_core.Vm_exec_probe.libc_name libc in
      (match Vm_runtime.faketime ~process_mgr ~clock rt libc with
      | Ok _ -> ()
      | Error m -> Alcotest.failf "the %s library: %s" name m);
      Alcotest.(check (result bool string))
        (name ^ " is cached and trusted")
        (Ok true)
        (Vm_runtime.faketime_cached rt libc))
    [ Bactrian_core.Vm_exec_probe.Musl; Glibc ];
  let archive =
    Filename.concat (Vm_runtime.relay_cache_dir ()) Vm_runtime.faketime_archive
  in
  Alcotest.(check int)
    "the archive belongs to this user" (Unix.getuid ())
    (Unix.stat archive).st_uid

(* allow_read: a file (docker mounts files) is there and its neighbour is not. *)
let readable_file () =
  docker_only "a file as sandbox.allow_read";
  let dir = Filename.concat (Sys.getcwd ()) "tmp-vm-allow-read-file" in
  let remove () = ignore (Sys.command ("rm -rf " ^ Filename.quote dir)) in
  remove ();
  Fun.protect ~finally:remove @@ fun () ->
  Unix.mkdir dir 0o700;
  let notes = Filename.concat dir "notes.txt" in
  write notes "shared\n";
  write (Filename.concat dir "other.txt") "not allowed\n";
  let _, seen =
    probed ~allow_read:[ notes ] "vm-allow-read-file"
      ~files:(fun _ -> [])
      (fun _ -> [ notes; Filename.concat dir "other.txt"; "w:" ^ notes ])
  in
  Alcotest.(check (list string))
    "what the agent saw"
    [ "shared"; "ENOENT"; "EROFS" ]
    (List.map snd seen)

(* allow_read of a home-like tree: the credentials in it stay hidden. *)
let readable_home () =
  docker_only "a changed HOME";
  let real_home = Option.value (Sys.getenv_opt "HOME") ~default:"/" in
  let home = Filename.concat (Sys.getcwd ()) "tmp-vm-home" in
  let remove () = ignore (Sys.command ("rm -rf " ^ Filename.quote home)) in
  remove ();
  Fun.protect ~finally:remove @@ fun () ->
  List.iter
    (fun d -> Unix.mkdir d 0o700)
    [ home; Filename.concat home ".ssh"; Filename.concat home ".aws" ];
  List.iter
    (fun (p, text) -> write (Filename.concat home p) text)
    [
      ("notes.txt", "shared\n");
      (".ssh/id_rsa", "PRIVATE KEY\n");
      (".aws/credentials", "aws_secret_access_key = x\n");
    ];
  (* The docker client keeps finding its own configuration and the bactrian
     cache stays where it was. *)
  let kept name default = (name, Option.value (Sys.getenv_opt name) ~default) in
  with_env
    [
      ("HOME", home);
      kept "DOCKER_CONFIG" (Filename.concat real_home ".docker");
      kept "XDG_CACHE_HOME" (Filename.concat real_home ".cache");
    ]
  @@ fun () ->
  let _, seen =
    probed ~allow_read:[ home ] "vm-allow-read-home"
      ~files:(fun _ -> [])
      (fun _ ->
        List.map (Filename.concat home)
          [ "notes.txt"; ".ssh/id_rsa"; ".aws/credentials" ])
  in
  Alcotest.(check (list string))
    "what the agent saw"
    [ "shared"; "ENOENT"; "ENOENT" ]
    (List.map snd seen)

let () =
  Alcotest.run "vm images"
    [
      ( "clock",
        [
          Alcotest.test_case "glibc older than libfaketime's" `Slow old_glibc;
          Alcotest.test_case "both C libraries' loaders" `Slow mixed_libc;
          Alcotest.test_case "a static agent" `Slow static_agent;
          Alcotest.test_case "an image of another architecture" `Slow
            foreign_architecture;
          Alcotest.test_case "a library cache that is not trusted" `Slow
            untrusted_cache;
          Alcotest.test_case "both libraries, one after the other" `Slow
            libraries_in_turn;
        ] );
      ( "allow_read",
        [
          Alcotest.test_case "a file" `Slow readable_file;
          Alcotest.test_case "a home-like tree" `Slow readable_home;
        ] );
    ]
