open Bactrian_runtime

let strings = Alcotest.(list string)

(* The client gets a short list of variables, never the provider keys or a
   credential Podman's client could also read. *)
let test_client_variables () =
  let podman = Vm_runtime.client_variables `Podman in
  List.iter
    (fun credential ->
      List.iter
        (fun kind ->
          Alcotest.(check bool)
            (credential ^ " is not passed")
            false
            (List.mem credential (Vm_runtime.client_variables kind)))
        [ `Docker; `Container; `Podman ])
    [
      "CONTAINER_PASSPHRASE";
      "SSH_AUTH_SOCK";
      "OPENAI_API_KEY";
      "AWS_SECRET_ACCESS_KEY";
    ];
  List.iter
    (fun v ->
      Alcotest.(check bool) (v ^ " reaches podman") true (List.mem v podman))
    [ "XDG_RUNTIME_DIR"; "TMPDIR"; "XDG_DATA_HOME"; "CONTAINER_HOST" ];
  Alcotest.(check bool)
    "DOCKER_HOST is not for podman" false
    (List.mem "DOCKER_HOST" podman)

let build_script kind ~rootless =
  Vm_runtime.build_script ~kind ~rootless ~uid:501 ~gid:20

let test_build_script () =
  let chowns kind ~rootless =
    let s = build_script kind ~rootless in
    let needle = "chown 501:20 /out/relay" in
    let n = String.length needle in
    let rec go i =
      i + n <= String.length s
      && (String.equal (String.sub s i n) needle || go (i + 1))
    in
    go 0
  in
  Alcotest.(check bool) "docker" true (chowns `Docker ~rootless:false);
  Alcotest.(check bool) "container" true (chowns `Container ~rootless:false);
  Alcotest.(check bool) "rootful podman" true (chowns `Podman ~rootless:false);
  Alcotest.(check bool) "rootless podman" false (chowns `Podman ~rootless:true)

let test_detection_order () =
  let names ~macos =
    List.map
      (function
        | `Docker -> "docker" | `Podman -> "podman" | `Container -> "container")
      (Vm_runtime.detection_order ~macos)
  in
  Alcotest.check strings "Linux"
    [ "docker"; "podman"; "container" ]
    (names ~macos:false);
  Alcotest.check strings "macOS"
    [ "docker"; "container"; "podman" ]
    (names ~macos:true)

let () =
  Alcotest.run "vm_runtime"
    [
      ( "runtime",
        [
          Alcotest.test_case "client variables" `Quick test_client_variables;
          Alcotest.test_case "relay build script" `Quick test_build_script;
          Alcotest.test_case "detection order" `Quick test_detection_order;
        ] );
    ]
