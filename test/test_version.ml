let test_semver_shape () =
  let parts = String.split_on_char '.' Bactrian_core.Version.current in
  Alcotest.(check int) "three dot-separated parts" 3 (List.length parts)

let () =
  Alcotest.run "bactrian.core"
    [
      ("version", [ Alcotest.test_case "semver shape" `Quick test_semver_shape ]);
    ]
