open Bactrian_core

let text f = Yojson.Safe.to_string (Json_float.to_json f)

let test_values () =
  let check name want f = Alcotest.(check string) name want (text f) in
  check "negative zero is zero" "0.0" (-0.);
  check "nan" "null" Float.nan;
  check "infinity" "null" Float.infinity;
  check "minus infinity" "null" Float.neg_infinity;
  check "12 significant digits" "0.3" (0.1 +. 0.2);
  check "small negatives keep their sign" "-1e-300" (-1e-300);
  check "a plain value" "-0.0333333333333" (-0.1 /. 3.)

let () =
  Alcotest.run "json_float"
    [ ("to_json", [ Alcotest.test_case "values" `Quick test_values ]) ]
