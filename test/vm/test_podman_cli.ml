(* Podman 4.9.3's output, as the Ubuntu 24.04 package printed it. *)

open Bactrian_core

let hex = String.make 64 'a'

let test_info () =
  let info = Alcotest.(result (triple string bool bool) string) in
  let read text =
    Result.map
      (fun (i : Podman_cli.info) -> (i.arch, i.rootless, i.remote))
      (Podman_cli.info text)
  in
  Alcotest.check info "rootless"
    (Ok ("arm64", true, false))
    (read "arm64 true false\n");
  Alcotest.check info "rootful"
    (Ok ("amd64", false, false))
    (read "amd64 false false");
  Alcotest.check info "remote client"
    (Ok ("arm64", true, true))
    (read "arm64 true true");
  List.iter
    (fun text ->
      Alcotest.(check bool)
        (Printf.sprintf "%S is refused" text)
        true
        (Result.is_error (read text)))
    [
      "";
      "arm64";
      "arm64 true";
      " true false";
      "arm64 yes false";
      "arm64 true yes";
      "arm64 true false extra";
      "<no value> true false";
    ]

let test_image_id () =
  let id = Alcotest.(result string string) in
  Alcotest.check id "prefixed"
    (Ok ("sha256:" ^ hex))
    (Podman_cli.image_id (hex ^ "\n"));
  List.iter
    (fun text ->
      Alcotest.(check bool)
        (Printf.sprintf "%S is refused" text)
        true
        (Result.is_error (Podman_cli.image_id text)))
    [
      "";
      "sha256:" ^ hex;
      String.make 63 'a';
      String.make 65 'a';
      String.make 64 'A';
      String.make 63 'a' ^ "g";
    ]

let test_ps_label () =
  Alcotest.(check string)
    "an index into the map" {|{{index .Labels "bactrian.pid"}}|}
    (Podman_cli.ps_label "bactrian.pid")

let () =
  Alcotest.run "podman_cli"
    [
      ( "decode",
        [
          Alcotest.test_case "podman info" `Quick test_info;
          Alcotest.test_case "image ID" `Quick test_image_id;
          Alcotest.test_case "ps label" `Quick test_ps_label;
        ] );
    ]
