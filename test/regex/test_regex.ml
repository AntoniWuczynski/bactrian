open Bactrian_core

let ok ?allow_empty_match s =
  match Regex.compile ?allow_empty_match s with
  | Ok re -> re
  | Error m -> Alcotest.failf "%S should compile: %s" s m

let rejects ?allow_empty_match ~mention s =
  match Regex.compile ?allow_empty_match s with
  | Ok _ -> Alcotest.failf "%S should be rejected" s
  | Error m ->
      Alcotest.(check bool)
        (Printf.sprintf "%S mentions %S in %S" s mention m)
        true
        (Re.execp (Re.compile (Re.str mention)) m)

let test_plain () =
  Alcotest.(check bool) "match" true (Re.execp (ok "wed+") "wedd");
  Alcotest.(check bool) "case" false (Re.execp (ok "wed") "WED")

let test_caseless () =
  let re = ok "(?i)wednesday" in
  Alcotest.(check bool) "upper" true (Re.execp re "on WEDNESDAY");
  Alcotest.(check bool) "lower" true (Re.execp re "wednesday")

let test_invalid () = rejects ~mention:"invalid" "(unclosed"

let test_empty () =
  rejects ~mention:"empty string" "a*";
  rejects ~mention:"empty string" "(?i)a*";
  Alcotest.(check bool)
    "allowed" true
    (Re.execp (ok ~allow_empty_match:true "a*") "")

let test_other_flags () =
  List.iter
    (fun s -> rejects ~mention:"inline flag" s)
    [ "(?s)a.b"; "(?m)^a"; "(?x)a"; "(?i-s)a"; "(?i)(?s)a" ]

let () =
  Alcotest.run "regex"
    [
      ( "compile",
        [
          Alcotest.test_case "plain" `Quick test_plain;
          Alcotest.test_case "leading (?i)" `Quick test_caseless;
          Alcotest.test_case "invalid" `Quick test_invalid;
          Alcotest.test_case "empty match" `Quick test_empty;
          Alcotest.test_case "other inline flags" `Quick test_other_flags;
        ] );
    ]
