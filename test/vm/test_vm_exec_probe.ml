(* The relay's report on an image and an executable. *)

open Bactrian_core
module E = Elf_fixture

let line glibc found prefix =
  Printf.sprintf {|{"glibc_2_34":%b,"found":%b,"prefix":"%s"}|} glibc found
    prefix

let test_decode () =
  let summary (r : Vm_exec_probe.report) = (r.glibc_2_34, r.executable) in
  Alcotest.(check (result (pair bool (option string)) string))
    "found"
    (Ok (true, Some "\x7fELF"))
    (Result.map summary (Vm_exec_probe.of_line (line true true "7f454c46")));
  Alcotest.(check (result (pair bool (option string)) string))
    "not found, an old glibc"
    (Ok (false, None))
    (Result.map summary (Vm_exec_probe.of_line (line false false "")));
  let wrong line =
    match Vm_exec_probe.of_line line with
    | Ok _ -> Alcotest.fail "expected an error"
    | Error _ -> ()
  in
  wrong (line true true "0");
  wrong (line true true "zz");
  wrong {|{"libc":"glibc","found":false,"prefix":""}|};
  wrong "not json"

let test_large () =
  let n = Elf_probe.prefix_length in
  match
    Vm_exec_probe.of_line
      (line true true (String.concat "" (List.init n (fun _ -> "ab"))))
  with
  | Ok { executable = Some bytes; _ } ->
      Alcotest.(check int) "a whole prefix" n (String.length bytes)
  | Ok { executable = None; _ } | Error _ -> Alcotest.fail "expected a prefix"

(* The library comes from the executable's own interpreter (decision 0021). *)
let test_libc () =
  let libc ~glibc segments =
    Option.map Vm_exec_probe.libc_name
      (Vm_exec_probe.libc ~arch:"amd64"
         { glibc_2_34 = glibc; executable = Some (E.elf segments) })
  in
  let linked interp = [ (E.pt_load, "x"); (E.pt_interp, interp ^ "\000") ] in
  let check name expected ~glibc segments =
    Alcotest.(check (option string)) name expected (libc ~glibc segments)
  in
  check "musl" (Some "musl") ~glibc:false (linked "/lib/ld-musl-aarch64.so.1");
  check "musl whatever the glibc of the image" (Some "musl") ~glibc:true
    (linked "/lib/ld-musl-x86_64.so.1");
  check "glibc, new enough" (Some "glibc") ~glibc:true
    (linked "/lib64/ld-linux-x86-64.so.2");
  check "glibc older than the floor" None ~glibc:false
    (linked "/lib/ld-linux-aarch64.so.1");
  check "another loader" None ~glibc:true (linked "/lib/ld.so.1");
  check "static" None ~glibc:true [ (E.pt_load, "x") ];
  Alcotest.(check (option string))
    "not found" None
    (Option.map Vm_exec_probe.libc_name
       (Vm_exec_probe.libc ~arch:"amd64"
          { glibc_2_34 = true; executable = None }));
  Alcotest.(check (option string))
    "a script" None
    (Option.map Vm_exec_probe.libc_name
       (Vm_exec_probe.libc ~arch:"amd64"
          { glibc_2_34 = true; executable = Some "#!/bin/sh\n" }))

(* The library is built for the runtime's architecture, so an executable of
   another one, run under emulation, has none. *)
let test_architecture () =
  let libc ~arch ~machine interp =
    Option.map Vm_exec_probe.libc_name
      (Vm_exec_probe.libc ~arch
         {
           glibc_2_34 = true;
           executable = Some (E.elf ~machine [ (E.pt_interp, interp ^ "\000") ]);
         })
  in
  let check name expected ~arch ~machine interp =
    Alcotest.(check (option string)) name expected (libc ~arch ~machine interp)
  in
  let amd64 = 62 and arm64 = 183 in
  check "arm64 on arm64" (Some "musl") ~arch:"arm64" ~machine:arm64
    "/lib/ld-musl-aarch64.so.1";
  check "amd64 on amd64" (Some "glibc") ~arch:"amd64" ~machine:amd64
    "/lib64/ld-linux-x86-64.so.2";
  check "amd64 musl on arm64" None ~arch:"arm64" ~machine:amd64
    "/lib/ld-musl-x86_64.so.1";
  check "amd64 glibc on arm64" None ~arch:"arm64" ~machine:amd64
    "/lib64/ld-linux-x86-64.so.2";
  check "arm64 on amd64" None ~arch:"amd64" ~machine:arm64
    "/lib/ld-linux-aarch64.so.1";
  check "32-bit x86 on amd64" None ~arch:"amd64" ~machine:3 "/lib/ld-linux.so.2";
  check "a runtime architecture bactrian does not know" None ~arch:"riscv64"
    ~machine:243 "/lib/ld-linux-riscv64-lp64d.so.1"

(* relay_image.c reads this many bytes of the executable, which bactrian
   classifies: they must be the same number. *)
let test_prefix_length () =
  let source =
    In_channel.with_open_bin "../../vm/relay_image.c" In_channel.input_all
  in
  let define =
    Printf.sprintf "#define EXEC_PREFIX %d\n" Elf_probe.prefix_length
  in
  let rec holds i =
    i + String.length define <= String.length source
    && (String.equal (String.sub source i (String.length define)) define
       || holds (i + 1))
  in
  Alcotest.(check bool)
    "vm/relay_image.c reads Elf_probe.prefix_length" true (holds 0)

let () =
  Alcotest.run "vm_exec_probe"
    [
      ( "report",
        [
          Alcotest.test_case "decode" `Quick test_decode;
          Alcotest.test_case "a whole prefix" `Quick test_large;
          Alcotest.test_case "the C library" `Quick test_libc;
          Alcotest.test_case "the architecture" `Quick test_architecture;
          Alcotest.test_case "the prefix length" `Quick test_prefix_length;
        ] );
    ]
