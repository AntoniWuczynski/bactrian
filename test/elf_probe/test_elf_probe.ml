open Bactrian_core
module E = Elf_fixture

let name : Elf_probe.kind -> string = function
  | Dynamic -> "Dynamic"
  | Static -> "Static"
  | Go -> "Go"
  | Not_elf -> "Not_elf"

let kind =
  Alcotest.testable
    (fun ppf k -> Format.pp_print_string ppf (name k))
    (fun a b -> String.equal (name a) (name b))

let check name expected bytes =
  Alcotest.check kind name expected (Elf_probe.probe bytes)

let test_linking () =
  check "dynamic, 64-bit" Dynamic
    (E.elf [ (E.pt_load, ""); (E.pt_interp, E.interp); (E.pt_dynamic, "d") ]);
  check "dynamic, 32-bit" Dynamic
    (E.elf ~class64:false [ (E.pt_interp, E.interp); (E.pt_note, E.gnu_note) ]);
  check "static" Static (E.elf [ (E.pt_load, "code") ]);
  check "static-pie has a dynamic section but no interpreter" Static
    (E.elf [ (E.pt_load, "code"); (E.pt_dynamic, "d") ]);
  check "no segments" Static (E.elf [])

let test_go () =
  check "Go, static" Go (E.elf [ (E.pt_note, E.go_note); (E.pt_load, "code") ]);
  check "Go, dynamically linked" Go
    (E.elf [ (E.pt_interp, E.interp); (E.pt_note, E.go_note) ]);
  check "Go, 32-bit" Go (E.elf ~class64:false [ (E.pt_note, E.go_note) ]);
  check "Go note after another note" Go
    (E.elf [ (E.pt_interp, E.interp); (E.pt_note, E.gnu_note ^ E.go_note) ]);
  check "a GNU build ID is not Go's" Dynamic
    (E.elf [ (E.pt_interp, E.interp); (E.pt_note, E.gnu_note) ]);
  check "the right name with another type is not Go's" Dynamic
    (E.elf
       [
         (E.pt_interp, E.interp);
         (E.pt_note, E.note ~name:"Go\000\000" ~kind:3 ~desc:"abcd");
       ]);
  check "a Go note outside a note segment is not seen" Dynamic
    (E.elf [ (E.pt_interp, E.interp); (E.pt_load, E.go_note) ])

let test_not_elf () =
  check "empty" Not_elf "";
  check "a script" Not_elf "#!/bin/sh\necho hi\n";
  check "magic only" Not_elf "\x7fELF";
  let big_endian = Bytes.of_string (E.elf [ (E.pt_interp, E.interp) ]) in
  Bytes.set big_endian 5 '\002';
  check "big-endian" Not_elf (Bytes.to_string big_endian);
  let wrong_class = Bytes.of_string (E.elf [ (E.pt_interp, E.interp) ]) in
  Bytes.set wrong_class 4 '\003';
  check "unknown class" Not_elf (Bytes.to_string wrong_class);
  let whole = E.elf [ (E.pt_interp, E.interp) ] in
  check "program header table cut off" Not_elf (String.sub whole 0 80);
  check "interpreter name cut off is still an interpreter" Dynamic
    (String.sub whole 0 130)

let test_hostile_notes () =
  (* Sizes that point past the data must end the walk, not loop or raise. *)
  let huge = E.note ~name:"Go\000\000" ~kind:9 ~desc:"" in
  let huge = Bytes.of_string huge in
  Bytes.set huge 0 '\255';
  Bytes.set huge 1 '\255';
  Bytes.set huge 2 '\255';
  Bytes.set huge 3 '\255';
  check "huge name size" Dynamic
    (E.elf [ (E.pt_interp, E.interp); (E.pt_note, Bytes.to_string huge) ]);
  check "note segment shorter than a note header" Dynamic
    (E.elf [ (E.pt_interp, E.interp); (E.pt_note, "abc") ])

let shebang =
  Alcotest.testable
    (fun ppf (t : Shebang.t option) ->
      match t with
      | None -> Format.pp_print_string ppf "None"
      | Some (Interpreter p) -> Format.fprintf ppf "Interpreter %s" p
      | Some (Env_program p) -> Format.fprintf ppf "Env_program %s" p)
    (fun a b ->
      match (a, b) with
      | None, None -> true
      | Some (Interpreter x), Some (Interpreter y)
      | Some (Env_program x), Some (Env_program y) ->
          String.equal x y
      | None, Some _
      | Some _, None
      | Some (Interpreter _), Some (Env_program _)
      | Some (Env_program _), Some (Interpreter _) ->
          false)

let test_shebang () =
  let parse = Shebang.parse in
  let check name expected s = Alcotest.check shebang name expected (parse s) in
  check "an interpreter" (Some (Interpreter "/bin/sh")) "#!/bin/sh\necho hi\n";
  check "an option for the interpreter" (Some (Interpreter "/bin/sh"))
    "#!/bin/sh -e\n";
  check "spaces after the marker" (Some (Interpreter "/usr/bin/python3"))
    "#!   /usr/bin/python3\n";
  check "env" (Some (Env_program "node")) "#!/usr/bin/env node\n";
  check "env with a split-string option" (Some (Env_program "node"))
    "#!/usr/bin/env -S node --no-warnings\n";
  check "env with an assignment" (Some (Env_program "python3"))
    "#!/usr/bin/env FOO=1 python3\n";
  check "env in another directory" (Some (Env_program "ruby"))
    "#!/bin/env ruby\n";
  check "a tab" (Some (Env_program "node")) "#!/usr/bin/env\tnode\n";
  check "only the first line" (Some (Interpreter "/bin/sh"))
    "#!/bin/sh\n#!/usr/bin/env node\n";
  check "env with nothing to run" None "#!/usr/bin/env\n";
  check "nothing after the marker" None "#!\n";
  check "not a script" None "echo hi\n";
  check "empty" None ""

let () =
  Alcotest.run "elf_probe"
    [
      ( "elf_probe",
        [
          Alcotest.test_case "dynamic and static" `Quick test_linking;
          Alcotest.test_case "go build id" `Quick test_go;
          Alcotest.test_case "not an elf file" `Quick test_not_elf;
          Alcotest.test_case "malformed notes" `Quick test_hostile_notes;
        ] );
      ( "shebang",
        [ Alcotest.test_case "the program it starts" `Quick test_shebang ] );
    ]
