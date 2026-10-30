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

(* A staging directory whose bactrian has gone is removed by the next
   preflight, and one of a live bactrian or of another host is kept. *)
let test_stale () =
  let runs = Filename.temp_dir "bactrian-runs" "" in
  Fun.protect ~finally:(fun () -> Proc_util.remove_tree runs) @@ fun () ->
  let dead =
    Unix.create_process "true" [| "true" |] Unix.stdin Unix.stdout Unix.stderr
  in
  ignore (Unix.waitpid [] dead : int * Unix.process_status);
  let host = Unix.gethostname () in
  let owned run text =
    let staging = Filename.concat (Filename.concat runs run) "staging" in
    Unix.mkdir (Filename.concat runs run) 0o700;
    Unix.mkdir staging 0o700;
    Unix.mkdir (Filename.concat staging "0") 0o755;
    write (Filename.concat staging "owner") text;
    staging
  in
  let gone = owned "gone" (Printf.sprintf "%d\t%s\n" dead host)
  and live = owned "live" (Printf.sprintf "%d\t%s\n" (Unix.getpid ()) host)
  and elsewhere =
    owned "elsewhere" (Printf.sprintf "%d\t%s-other\n" dead host)
  in
  Unix.mkdir (Filename.concat runs "plain") 0o700;
  Alcotest.(check (list string))
    "removed" [ gone ]
    (Sandbox_vm.remove_stale_staging ~runs_dir:runs);
  Alcotest.(check (list bool))
    "what is left" [ false; true; true ]
    (List.map Sys.file_exists [ gone; live; elsewhere ])

(* Just before the container is created, the entries bound again beside a
   hidden file are looked at once more: one swapped since it was listed stops
   the run. *)

let listed ?(kind = Vm_args.Dir) dir name : string * Vm_args.entry =
  let path = Filename.concat dir name in
  (path, { name; kind; identity = Some (identity path); masked_file = false })

let deny_of project = [ (Filename.concat project ".env", `File) ]

let rechecked what ~prepare ~swap () =
  with_project @@ fun ~project ~run_dir:_ ->
  prepare project;
  let entry = listed project "sub" in
  let deny = deny_of project in
  Alcotest.(check (result unit string))
    "unchanged" (Ok ())
    (Sandbox_vm.recheck ~deny [ entry ]);
  swap project;
  match Sandbox_vm.recheck ~deny [ entry ] with
  | Ok () -> Alcotest.failf "%s: still accepted" what
  | Error m ->
      Printf.printf "%s: %s\n" what m;
      Alcotest.(check bool)
        "names the entry" true
        (String.starts_with ~prefix:(fst entry) m)

let make_sub project = Unix.mkdir (Filename.concat project "sub") 0o755

let test_recheck_symlink () =
  rechecked "a directory swapped for a symbolic link" ~prepare:make_sub
    ~swap:(fun project ->
      let sub = Filename.concat project "sub" in
      Unix.rmdir sub;
      Unix.symlink "/" sub)
    ()

let test_recheck_replaced () =
  rechecked "a directory replaced by another" ~prepare:make_sub
    ~swap:(fun project ->
      let sub = Filename.concat project "sub" in
      Unix.rename sub (Filename.concat project "old");
      Unix.mkdir sub 0o755)
    ()

let test_recheck_vanished () =
  rechecked "a directory removed" ~prepare:make_sub
    ~swap:(fun project -> Unix.rmdir (Filename.concat project "sub"))
    ()

let test_recheck_masked_name () =
  with_project @@ fun ~project ~run_dir:_ ->
  write (Filename.concat project "plain.txt") "x";
  let entry = listed ~kind:File project "plain.txt" in
  let deny = deny_of project in
  Alcotest.(check (result unit string))
    "unchanged" (Ok ())
    (Sandbox_vm.recheck ~deny [ entry ]);
  (* The name now leads to the masked file itself. *)
  Sys.remove (fst entry);
  Unix.link (Filename.concat project ".env") (fst entry);
  match Sandbox_vm.recheck ~deny [ entry ] with
  | Ok () -> Alcotest.fail "a name of the masked file was accepted"
  | Error m -> Printf.printf "a hard link of the masked file: %s\n" m

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
          Alcotest.test_case "stale staging" `Quick test_stale;
        ] );
      ( "recheck",
        [
          Alcotest.test_case "a symbolic link" `Quick test_recheck_symlink;
          Alcotest.test_case "another directory" `Quick test_recheck_replaced;
          Alcotest.test_case "removed" `Quick test_recheck_vanished;
          Alcotest.test_case "a name of the masked file" `Quick
            test_recheck_masked_name;
        ] );
    ]
