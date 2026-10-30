(* The relay's report on an image and an executable. *)

open Bactrian_core

let test_decode () =
  let line libc found prefix =
    Printf.sprintf {|{"libc":"%s","found":%b,"prefix":"%s"}|} libc found prefix
  in
  let summary (r : Vm_exec_probe.report) =
    (Option.map Vm_exec_probe.libc_name r.libc, r.executable)
  in
  Alcotest.(check (result (pair (option string) (option string)) string))
    "found"
    (Ok (Some "musl", Some "\x7fELF"))
    (Result.map summary (Vm_exec_probe.of_line (line "musl" true "7f454c46")));
  Alcotest.(check (result (pair (option string) (option string)) string))
    "not found, no loader"
    (Ok (None, None))
    (Result.map summary (Vm_exec_probe.of_line (line "none" false "")));
  let wrong line =
    match Vm_exec_probe.of_line line with
    | Ok _ -> Alcotest.fail "expected an error"
    | Error _ -> ()
  in
  wrong (line "uclibc" true "00");
  wrong (line "glibc" true "0");
  wrong (line "glibc" true "zz");
  wrong {|{"libc":"glibc"}|};
  wrong "not json"

let test_large () =
  let n = Elf_probe.prefix_length in
  match
    Vm_exec_probe.of_line
      (Printf.sprintf {|{"libc":"glibc","found":true,"prefix":"%s"}|}
         (String.concat "" (List.init n (fun _ -> "ab"))))
  with
  | Ok { executable = Some bytes; _ } ->
      Alcotest.(check int) "a whole prefix" n (String.length bytes)
  | Ok { executable = None; _ } | Error _ -> Alcotest.fail "expected a prefix"

let () =
  Alcotest.run "vm_exec_probe"
    [
      ( "report",
        [
          Alcotest.test_case "decode" `Quick test_decode;
          Alcotest.test_case "a whole prefix" `Quick test_large;
        ] );
    ]
