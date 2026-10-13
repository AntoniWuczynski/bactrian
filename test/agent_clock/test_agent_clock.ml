open Bactrian_core
open Bactrian_runtime
module E = Elf_fixture
module L = Clock_label

let write ?(perm = 0o755) path contents =
  Out_channel.with_open_bin path (fun oc -> output_string oc contents);
  Unix.chmod path perm

(* A fresh directory under the test's own, removed with its files. *)
let with_dir f =
  let dir = Filename.temp_dir ~temp_dir:(Sys.getcwd ()) "agent-clock" "" in
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Sys.remove (Filename.concat dir name))
        (Sys.readdir dir);
      Unix.rmdir dir)
    (fun () -> f dir)

let dynamic = E.elf [ (E.pt_interp, E.interp) ]
let static = E.elf [ (E.pt_load, "code") ]
let go = E.elf [ (E.pt_interp, E.interp); (E.pt_note, E.go_note) ]
let option_string = Alcotest.(option string)

(* Library search *)

let test_locate () =
  with_dir @@ fun dir ->
  let first = Filename.concat dir "first.so" in
  let second = Filename.concat dir "second.so" in
  let missing = Filename.concat dir "missing.so" in
  write first "";
  write second "";
  let locate = Agent_clock.locate in
  Alcotest.check option_string "first candidate that exists" (Some second)
    (locate ~candidates:[ missing; second; first ] ~override:None ());
  Alcotest.check option_string "none exists" None
    (locate ~candidates:[ missing ] ~override:None ());
  Alcotest.check option_string "the override wins" (Some first)
    (locate ~candidates:[ second ] ~override:(Some first) ());
  Alcotest.check option_string
    "an override that does not exist is not replaced by a search" None
    (locate ~candidates:[ second ] ~override:(Some missing) ());
  let link = Filename.concat dir "link.so" in
  Unix.symlink first link;
  Alcotest.check option_string
    "a symbolic link is resolved, since the sandbox binds the real path"
    (Some first)
    (locate ~candidates:[] ~override:(Some link) ());
  Alcotest.check option_string "an empty override is unset" (Some second)
    (locate ~candidates:[ second ] ~override:(Some "") ());
  Alcotest.check option_string "a directory is not a library" None
    (locate ~candidates:[ dir ] ~override:None ());
  Alcotest.(check bool)
    "the usual paths are the multi-thread build" true
    (List.for_all
       (fun p -> String.ends_with ~suffix:"/libfaketimeMT.so.1" p)
       Agent_clock.candidates)

(* The agent's executable *)

let kind_name : Elf_probe.kind -> string = function
  | Dynamic -> "dynamic"
  | Static -> "static"
  | Go -> "go"
  | Not_elf -> "not-elf"

let probe ?path ~cwd argv0 =
  kind_name (Agent_clock.probe_executable ~path ~cwd argv0)

let test_probe_executable () =
  with_dir @@ fun dir ->
  let at name = Filename.concat dir name in
  write (at "dyn") dynamic;
  write (at "stat") static;
  write (at "go") go;
  write (at "text") "just text\n";
  write ~perm:0o644 (at "plain") dynamic;
  write (at "via-env") "#!/usr/bin/env dyn\nprint(1)\n";
  write (at "via-env-static") "#!/usr/bin/env -S stat --flag\n";
  write (at "via-path") ("#!" ^ at "go" ^ "\n");
  write (at "env-missing") "#!/usr/bin/env nothing-like-it\n";
  write (at "twice") ("#!" ^ at "via-path" ^ "\n");
  write (at "no-interpreter") "#!/nonexistent/interpreter\n";
  let check name expected ?path argv0 =
    Alcotest.(check string) name expected (probe ?path ~cwd:dir argv0)
  in
  check "an absolute path" "dynamic" (at "dyn");
  check "static" "static" (at "stat");
  check "go" "go" (at "go");
  check "found on PATH" "static" ~path:dir "stat";
  check "found on the second PATH entry" "dynamic" ~path:("/nonexistent:" ^ dir)
    "dyn";
  check "not on PATH" "not-elf" ~path:"/nonexistent" "dyn";
  check "no PATH at all" "not-elf" "dyn";
  check "relative to the working directory" "dynamic" "./dyn";
  check "a PATH entry relative to the working directory" "dynamic" ~path:"."
    "dyn";
  check "not executable" "not-elf" (at "plain");
  check "not a program" "not-elf" (at "text");
  check "missing" "not-elf" (at "absent");
  check "a directory" "not-elf" dir;
  check "#!/usr/bin/env NAME" "dynamic" ~path:dir "via-env";
  check "#!/usr/bin/env with options" "static" ~path:dir "via-env-static";
  check "#! with a path" "go" (at "via-path");
  check "env finds nothing" "not-elf" ~path:dir "env-missing";
  check "a script that runs a script is not followed twice" "not-elf"
    (at "twice");
  check "a missing interpreter" "not-elf" (at "no-interpreter");
  Alcotest.(check string)
    "an empty command" "not-elf"
    (kind_name (Agent_clock.probe_executable ~path:None ~cwd:dir ""))

(* The decision *)

let label_name : L.t -> string = function
  | Enforced -> "enforced"
  | Contract_only r -> "contract-only " ^ L.reason_name r

let sim_now = "2026-03-15T12:30:00Z"

let decide ?(linux = true) ?(faketime = true) ?(sim_now = sim_now) ~dir ~lib
    ~override program =
  Agent_clock.decide ~candidates:[ lib ] ~linux ~faketime
    ~library_override:override
    ~env:[ ("PATH", dir) ]
    ~cwd:dir ~agent:[ program ] ~sim_now ()

let test_decide () =
  with_dir @@ fun dir ->
  let at name = Filename.concat dir name in
  let lib = at "libfaketimeMT.so.1" in
  write lib "";
  write (at "dyn") dynamic;
  write (at "stat") static;
  write (at "go") go;
  let check name expected t =
    Alcotest.(check string) name expected (label_name (Agent_clock.label t))
  in
  let decide ?linux ?faketime ?sim_now ?(override = None) program =
    decide ?linux ?faketime ?sim_now ~dir ~lib ~override program
  in
  check "enforced" "enforced" (decide "dyn");
  check "static" "contract-only static-binary" (decide "stat");
  check "go" "contract-only go-binary" (decide "go");
  check "unreadable" "contract-only unreadable-executable" (decide "absent");
  check "not Linux" "contract-only macos" (decide ~linux:false "dyn");
  check "faketime off" "contract-only faketime-off"
    (decide ~faketime:false "dyn");
  check "no library" "contract-only library-not-found"
    (Agent_clock.decide
       ~candidates:[ at "none" ]
       ~linux:true ~faketime:true ~library_override:None
       ~env:[ ("PATH", dir) ]
       ~cwd:dir ~agent:[ "dyn" ] ~sim_now ());
  check "an override that does not exist" "contract-only library-not-found"
    (decide ~override:(Some (at "none")) "dyn");
  check "a library the sandbox hides" "contract-only library-hidden"
    (Agent_clock.decide ~candidates:[ lib ] ~hidden:(String.equal lib)
       ~linux:true ~faketime:true ~library_override:None
       ~env:[ ("PATH", dir) ]
       ~cwd:dir ~agent:[ "dyn" ] ~sim_now ());
  check "sim_now that is not a time" "contract-only invalid-sim-now"
    (decide ~sim_now:"tomorrow" "dyn");
  check "sim_now before 1970" "contract-only invalid-sim-now"
    (decide ~sim_now:"1969-12-31T23:59:59Z" "dyn");
  check "macOS wins over everything" "contract-only macos"
    (decide ~linux:false ~faketime:false ~sim_now:"x" "absent");
  check "faketime off wins over a missing library" "contract-only faketime-off"
    (Agent_clock.decide
       ~candidates:[ at "none" ]
       ~linux:true ~faketime:false ~library_override:None
       ~env:[ ("PATH", dir) ]
       ~cwd:dir ~agent:[ "dyn" ] ~sim_now ());
  check "no command" "contract-only unreadable-executable"
    (Agent_clock.decide ~candidates:[ lib ] ~linux:true ~faketime:true
       ~library_override:None ~env:[] ~cwd:dir ~agent:[] ~sim_now ())

let test_labels () =
  Alcotest.(check string)
    "enforced" {|{"mode":"enforced"}|}
    (Yojson.Safe.to_string (L.to_json Enforced));
  Alcotest.(check string)
    "contract-only" {|{"mode":"contract_only","reason":"static-binary"}|}
    (Yojson.Safe.to_string (L.to_json (Contract_only Static_binary)));
  Alcotest.(check (list string))
    "reason names"
    [
      "macos";
      "faketime-off";
      "library-not-found";
      "library-hidden";
      "invalid-sim-now";
      "static-binary";
      "go-binary";
      "unreadable-executable";
    ]
    (List.map L.reason_name
       [
         Macos;
         Faketime_off;
         Library_not_found;
         Library_hidden;
         Invalid_sim_now;
         Static_binary;
         Go_binary;
         Unreadable_executable;
       ]);
  Alcotest.(check bool)
    "the trace records enforced" true
    (match L.to_clock Enforced with Enforced -> true | Contract_only -> false);
  Alcotest.(check bool)
    "the trace records contract-only" true
    (match L.to_clock (Contract_only Go_binary) with
    | Contract_only -> true
    | Enforced -> false)

(* The environment *)

(* The library's path is only known inside, so [extra] makes the rest of the
   environment from it. *)
let enforced_for extra =
  with_dir @@ fun dir ->
  let lib = Filename.concat dir "libfaketimeMT.so.1" in
  write lib "";
  write (Filename.concat dir "dyn") dynamic;
  let t = decide ~dir ~lib ~override:None "dyn" in
  (lib, Agent_clock.inject t (("PATH", dir) :: extra lib))

let test_inject () =
  let lib, env = enforced_for (fun _ -> [ ("HOME", "/h") ]) in
  let get k = List.assoc_opt k env in
  Alcotest.check option_string "preload" (Some lib) (get "LD_PRELOAD");
  (* 2026-03-15T12:30:00Z *)
  Alcotest.check option_string "start-at, as a Unix time" (Some "@1773577800")
    (get "FAKETIME");
  Alcotest.check option_string "the format is seconds" (Some "%s")
    (get "FAKETIME_FMT");
  Alcotest.check option_string "children share the clock" (Some "1")
    (get "FAKETIME_DONT_RESET");
  Alcotest.check option_string "monotonic time is faked with the wall clock"
    None
    (get "FAKETIME_DONT_FAKE_MONOTONIC");
  Alcotest.check option_string "the rest is untouched" (Some "/h") (get "HOME")

let values key env =
  List.filter_map
    (fun (k, v) -> if String.equal k key then Some v else None)
    env

let test_inject_merges () =
  let lib, env = enforced_for (fun _ -> [ ("LD_PRELOAD", "/opt/mine.so") ]) in
  Alcotest.(check (list string))
    "the library goes first, the user's stays"
    [ lib ^ ":/opt/mine.so" ]
    (values "LD_PRELOAD" env);
  let lib, env =
    enforced_for (fun _ -> [ ("LD_PRELOAD", "/opt/a.so /opt/b.so") ])
  in
  Alcotest.(check (list string))
    "a space-separated list stays whole"
    [ lib ^ ":/opt/a.so /opt/b.so" ]
    (values "LD_PRELOAD" env);
  let lib, env = enforced_for (fun lib -> [ ("LD_PRELOAD", lib) ]) in
  Alcotest.(check (list string))
    "already preloaded, once" [ lib ] (values "LD_PRELOAD" env);
  let lib, env =
    enforced_for (fun lib -> [ ("LD_PRELOAD", "/opt/x.so:" ^ lib) ])
  in
  Alcotest.(check (list string))
    "already preloaded after another, left alone"
    [ "/opt/x.so:" ^ lib ]
    (values "LD_PRELOAD" env);
  let _, env = enforced_for (fun _ -> [ ("FAKETIME", "+1d"); ("TZ", "UTC") ]) in
  Alcotest.(check (list string))
    "the user's FAKETIME is replaced, not repeated" [ "@1773577800" ]
    (values "FAKETIME" env);
  Alcotest.(check (list string))
    "TZ is not ours to touch" [ "UTC" ] (values "TZ" env)

let test_contract_only_leaves_env () =
  with_dir @@ fun dir ->
  write (Filename.concat dir "stat") static;
  let env = [ ("PATH", dir); ("LD_PRELOAD", "/opt/mine.so") ] in
  let t =
    decide ~dir ~lib:(Filename.concat dir "absent") ~override:None "stat"
  in
  Alcotest.(check (list (pair string string)))
    "unchanged" env (Agent_clock.inject t env)

let () =
  Alcotest.run "agent_clock"
    [
      ("library", [ Alcotest.test_case "search" `Quick test_locate ]);
      ("executable", [ Alcotest.test_case "probe" `Quick test_probe_executable ]);
      ( "label",
        [
          Alcotest.test_case "decision" `Quick test_decide;
          Alcotest.test_case "json and names" `Quick test_labels;
        ] );
      ( "environment",
        [
          Alcotest.test_case "variables" `Quick test_inject;
          Alcotest.test_case "LD_PRELOAD merging" `Quick test_inject_merges;
          Alcotest.test_case "contract-only" `Quick
            test_contract_only_leaves_env;
        ] );
    ]
