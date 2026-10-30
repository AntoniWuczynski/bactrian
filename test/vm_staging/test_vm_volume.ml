(* container's staging links a hidden file's neighbours by hard link, which
   fails for a file on another volume than the run's directory. A real second
   volume is a small disk image, made and attached with hdiutil, so this runs
   on macOS and is skipped elsewhere. *)

open Bactrian_runtime
module Vm_args = Bactrian_core.Vm_args

let hdiutil = "/usr/bin/hdiutil"

let sh fmt =
  Printf.ksprintf
    (fun cmd ->
      if Sys.command (cmd ^ " > /dev/null 2>&1") <> 0 then
        Alcotest.failf "failed: %s" cmd)
    fmt

let write path text =
  Out_channel.with_open_bin path (fun oc -> output_string oc text)

let identity path =
  let st = Unix.stat path in
  (st.st_dev, st.st_ino)

(* A scratch directory, and the volume mounted in it. *)
let with_volume f =
  let root = Filename.temp_dir "bactrian-volume" "" in
  let mount = Filename.concat root "volume"
  and image = Filename.concat root "volume.dmg" in
  Unix.mkdir mount 0o700;
  sh "%s create -size 2m -fs HFS+ -volname bactrian-test %s" hdiutil
    (Filename.quote image);
  sh "%s attach -nobrowse -noverify -mountpoint %s %s" hdiutil
    (Filename.quote mount) (Filename.quote image);
  Fun.protect ~finally:(fun () ->
      (* A volume still in use is detached by force: the test owns it. *)
      ignore
        (Sys.command
           (Printf.sprintf "%s detach -force %s > /dev/null 2>&1" hdiutil
              (Filename.quote mount)));
      Proc_util.remove_tree root)
  @@ fun () -> f ~root ~mount

let stage ~over ~identity ~run_dir : Vm_args.stage =
  {
    dir = Filename.concat (Filename.concat run_dir "staging") "0";
    over;
    links =
      [
        {
          name = "notes.txt";
          source = Filename.concat over "notes.txt";
          identity;
        };
      ];
    mount_points = [];
  }

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let test_other_volume () =
  if not (Sys.file_exists hdiutil) then Alcotest.skip ();
  with_volume @@ fun ~root ~mount ->
  (* The project, with a hidden file and its neighbour, is on the volume. The
     run's directory is on the host's own. *)
  let project = Filename.concat mount "project" in
  Unix.mkdir project 0o700;
  write (Filename.concat project ".env") "SECRET";
  let notes = Filename.concat project "notes.txt" in
  write notes "notes";
  let run_dir = Filename.concat root "run" in
  Unix.mkdir run_dir 0o700;
  Alcotest.(check bool)
    "two volumes" true
    (fst (identity project) <> fst (identity run_dir));
  let s = stage ~over:project ~identity:(identity notes) ~run_dir in
  (match Sandbox_vm.make_stages ~run_dir [ s ] with
  | Ok () -> Alcotest.fail "a file on another volume was staged"
  | Error m ->
      Printf.printf "%s\n" m;
      Alcotest.(check bool) "names the file" true (contains m notes);
      Alcotest.(check bool) "says the link failed" true (contains m "hard link");
      Alcotest.(check bool)
        "gives the reason" true
        (contains m (Unix.error_message Unix.EXDEV)));
  Alcotest.(check bool)
    "nothing of it staged" false
    (Sys.file_exists (Filename.concat s.dir "notes.txt"))

(* The same staging works when the neighbour is on the run's own volume. *)
let test_same_volume () =
  if not (Sys.file_exists hdiutil) then Alcotest.skip ();
  with_volume @@ fun ~root:_ ~mount ->
  let project = Filename.concat mount "project"
  and run_dir = Filename.concat mount "run" in
  List.iter (fun d -> Unix.mkdir d 0o700) [ project; run_dir ];
  write (Filename.concat project ".env") "SECRET";
  let notes = Filename.concat project "notes.txt" in
  write notes "notes";
  let s = stage ~over:project ~identity:(identity notes) ~run_dir in
  Alcotest.(check (result unit string))
    "staged" (Ok ())
    (Sandbox_vm.make_stages ~run_dir [ s ]);
  Alcotest.(check bool)
    "the same file" true
    (identity (Filename.concat s.dir "notes.txt") = identity notes)

let () =
  Alcotest.run "vm_volume"
    [
      ( "second volume",
        [
          Alcotest.test_case "a neighbour on another volume" `Slow
            test_other_volume;
          Alcotest.test_case "a neighbour on the same volume" `Slow
            test_same_volume;
        ] );
    ]
