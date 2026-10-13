open Bactrian_core

let check name expected s =
  Alcotest.(check (option int)) name expected (Rfc3339.epoch s)

let test_valid () =
  check "the epoch" (Some 0) "1970-01-01T00:00:00Z";
  check "a date" (Some 1791884755) "2026-10-13T09:45:55Z";
  check "lower case separators" (Some 1791884755) "2026-10-13t09:45:55z";
  check "leap day" (Some 951825600) "2000-02-29T12:00:00Z";
  check "after 2038" (Some 2147483648) "2038-01-19T03:14:08Z";
  check "after a century that is not a leap year" (Some 4107542400)
    "2100-03-01T00:00:00Z";
  check "before the epoch" (Some (-1)) "1969-12-31T23:59:59Z"

let test_offsets () =
  check "east of UTC" (Some 1791884755) "2026-10-13T11:45:55+02:00";
  check "west of UTC" (Some 1791884755) "2026-10-13T04:45:55-05:00";
  check "half an hour" (Some 1791884755) "2026-10-13T15:15:55+05:30";
  check "a fraction is dropped" (Some 1791884755) "2026-10-13T09:45:55.987Z";
  check "a fraction and an offset" (Some 1791884755)
    "2026-10-13T11:45:55.5+02:00"

let test_invalid () =
  List.iter
    (fun s -> check s None s)
    [
      "";
      "yesterday";
      "2026-10-13";
      "2026-10-13T09:45:55";
      "2026-10-13 09:45:55Z";
      "2026-10-13T09:45:55ZZ";
      "2026-10-13T09:45:55+0200";
      "2026-10-13T09:45:55+24:00";
      "2026-10-13T09:45:55.Z";
      "2026-13-13T09:45:55Z";
      "2026-00-13T09:45:55Z";
      "2026-02-29T09:45:55Z";
      "2100-02-29T09:45:55Z";
      "2026-04-31T09:45:55Z";
      "2026-10-13T24:00:00Z";
      "2026-10-13T09:60:00Z";
      "2026-10-13T09:45:60Z";
      "2026-10-13T09:4x:55Z";
      "2026-1-13T09:45:55Z";
    ]

let () =
  Alcotest.run "rfc3339"
    [
      ( "epoch",
        [
          Alcotest.test_case "valid times" `Quick test_valid;
          Alcotest.test_case "offsets and fractions" `Quick test_offsets;
          Alcotest.test_case "invalid times" `Quick test_invalid;
        ] );
    ]
