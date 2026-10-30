(* The virtual machine's clock and allow_read across kinds of image and cache
   (decisions 0020 and 0021). Like test_vm, on docker, or container when
   BACTRIAN_VM_RUNTIME is container, and skipped without one. *)

open Bactrian_runtime
open Acceptance_world
open Vm_support

let with_env bindings f =
  let previous = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) bindings in
  List.iter (fun (k, v) -> Unix.putenv k v) bindings;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (k, v) -> Unix.putenv k (Option.value v ~default:""))
        previous)
    f

(* Both libraries build one after the other from one cold cache. The archive
   is downloaded once, as root in a rootful Docker, and the second build
   refuses it unless the download handed it to the user. *)
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

let () =
  Alcotest.run "vm images"
    [
      ( "clock",
        [
          Alcotest.test_case "both libraries, one after the other" `Slow
            libraries_in_turn;
        ] );
    ]
