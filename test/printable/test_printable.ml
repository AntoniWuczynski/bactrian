open Bactrian_core

let test_escape () =
  let check name expected input =
    Alcotest.(check string) name expected (Printable.escape input)
  in
  check "plain text" "/home/u/a b.txt" "/home/u/a b.txt";
  check "UTF-8 is kept" "café ✓" "café ✓";
  check "a colour code" "/e\\x1b[31mRED" "/e\027[31mRED";
  check "newline, tab and DEL" "a\\x0ab\\x09c\\x7f" "a\nb\tc\127";
  check "a C1 control in UTF-8" "x\\u009by" "x\xc2\x9by";
  check "U+00A0 is not a control" "x\xc2\xa0y" "x\xc2\xa0y"

let () =
  Alcotest.run "printable"
    [ ("escape", [ Alcotest.test_case "controls" `Quick test_escape ]) ]
