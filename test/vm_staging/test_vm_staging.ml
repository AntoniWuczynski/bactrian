(* container's staging directories on the host: the hard links must be the
   files the listing saw, so a neighbour of a masked file that is swapped for a
   name of the masked file before the staging never shows that file. *)

open Bactrian_runtime
module Vm_args = Bactrian_core.Vm_args

let write path text =
  Out_channel.with_open_bin path (fun oc -> output_string oc text)

let identity path =
  let st = Unix.stat path in
  (st.st_dev, st.st_ino)

(* A project with a masked .env and a neighbour, and a run directory, in a
   fresh directory. *)
let with_project f =
  let root = Filename.temp_dir "bactrian-staging" "" in
  Fun.protect ~finally:(fun () -> Proc_util.remove_tree root) @@ fun () ->
  let project = Filename.concat root "project"
  and run_dir = Filename.concat root "run" in
  List.iter (fun d -> Unix.mkdir d 0o700) [ project; run_dir ];
  write (Filename.concat project ".env") "SECRET";
  write (Filename.concat project "notes.txt") "notes";
  f ~project ~run_dir

let stage ~project ~run_dir ~identity : Vm_args.stage =
  {
    dir = Filename.concat (Filename.concat run_dir "staging") "0";
    over = project;
    links =
      [
        {
          name = "notes.txt";
          source = Filename.concat project "notes.txt";
          identity;
        };
      ];
    mount_points = [ "sub" ];
  }

let staged ~run_dir name =
  Filename.concat (Filename.concat (Filename.concat run_dir "staging") "0") name

let test_staged () =
  with_project @@ fun ~project ~run_dir ->
  let notes = Filename.concat project "notes.txt" in
  let s = stage ~project ~run_dir ~identity:(identity notes) in
  Alcotest.(check (result unit string))
    "staged" (Ok ())
    (Sandbox_vm.make_stages ~run_dir [ s ]);
  Alcotest.(check bool)
    "the same file" true
    (identity (staged ~run_dir "notes.txt") = identity notes);
  Alcotest.(check bool)
    "a mount point" true
    (Sys.is_directory (staged ~run_dir "sub"))

(* [swap] replaces notes.txt after the listing recorded it. *)
let swapped what swap () =
  with_project @@ fun ~project ~run_dir ->
  let notes = Filename.concat project "notes.txt"
  and env = Filename.concat project ".env" in
  let listed = identity notes in
  Sys.remove notes;
  swap ~env ~notes;
  match
    Sandbox_vm.make_stages ~run_dir [ stage ~project ~run_dir ~identity:listed ]
  with
  | Ok () -> Alcotest.failf "%s: staged" what
  | Error m ->
      Printf.printf "%s: %s\n" what m;
      Alcotest.(check bool)
        "no staged notes.txt" false
        (Sys.file_exists (staged ~run_dir "notes.txt"));
      Alcotest.(check bool)
        "the masked file has no new name" true
        ((Unix.stat env).st_nlink
        = if String.equal what "a hard link" then 2 else 1)

let () =
  Alcotest.run "vm_staging"
    [
      ( "staging",
        [
          Alcotest.test_case "a neighbour" `Quick test_staged;
          Alcotest.test_case "swapped for a symbolic link" `Quick
            (swapped "a symbolic link" (fun ~env ~notes ->
                 Unix.symlink env notes));
          Alcotest.test_case "swapped for a hard link" `Quick
            (swapped "a hard link" (fun ~env ~notes -> Unix.link env notes));
        ] );
    ]
