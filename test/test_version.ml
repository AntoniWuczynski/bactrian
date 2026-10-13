let is_number s =
  (not (String.equal s ""))
  && String.for_all (function '0' .. '9' -> true | _ -> false) s

let test_semver_shape () =
  let core, _prerelease =
    match String.index_opt Bactrian_core.Version.current '-' with
    | Some i ->
        ( String.sub Bactrian_core.Version.current 0 i,
          Some
            (String.sub Bactrian_core.Version.current (i + 1)
               (String.length Bactrian_core.Version.current - i - 1)) )
    | None -> (Bactrian_core.Version.current, None)
  in
  let parts = String.split_on_char '.' core in
  Alcotest.(check int) "three dot-separated parts" 3 (List.length parts);
  Alcotest.(check bool)
    "every part is a number" true
    (List.for_all is_number parts)

let () =
  Alcotest.run "bactrian.core"
    [
      ("version", [ Alcotest.test_case "semver shape" `Quick test_semver_shape ]);
    ]
