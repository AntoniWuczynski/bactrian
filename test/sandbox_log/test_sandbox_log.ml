open Bactrian_core
module L = Sandbox_log

let violation =
  Alcotest.(
    option
      (of_pp (fun ppf (v : L.violation) ->
           Format.fprintf ppf "%s | %s" v.kind v.detail)))

let tag = L.tag "20261013T094555Z-ab12cd34"

let test_tag () =
  Alcotest.(check string)
    "run id" "bactrian-sandbox:20261013T094555Z-ab12cd34" tag;
  Alcotest.(check string)
    "unsafe characters" "bactrian-sandbox:a_b_c" (L.tag "a\"b c");
  Alcotest.(check string)
    "predicate"
    {|sender == "Sandbox" AND eventMessage CONTAINS "bactrian-sandbox:x"|}
    (L.predicate ~tag:(L.tag "x"))

let message body = body ^ "\n" ^ tag

let test_messages () =
  let parse = L.of_seatbelt_message ~tag in
  Alcotest.check violation "network"
    (Some { kind = "network-outbound"; detail = "remote:*:443 by node(812)" })
    (parse (message "Sandbox: node(812) deny(1) network-outbound remote:*:443"));
  Alcotest.check violation "path with spaces, process with parentheses"
    (Some
       {
         kind = "file-write-create";
         detail = "/home/u/a b.txt by Helper (GPU)(9)";
       })
    (parse
       (message
          "Sandbox: Helper (GPU)(9) deny(1) file-write-create /home/u/a b.txt"));
  Alcotest.check violation "duplicates"
    (Some { kind = "file-read-data"; detail = "/x by cat(3)" })
    (parse
       (message
          "2 duplicate reports for Sandbox: cat(3) deny(1) file-read-data /x"));
  Alcotest.check violation "operation without a target"
    (Some { kind = "process-fork"; detail = "by sh(4)" })
    (parse (message "Sandbox: sh(4) deny(1) process-fork"));
  Alcotest.check violation "another run's tag" None
    (L.of_seatbelt_message ~tag:(L.tag "other")
       (message "Sandbox: cat(3) deny(1) file-read-data /x"));
  Alcotest.check violation "the end marker is not a violation" None
    (parse ("Sandbox: touch(5) deny(1) file-write-create /m\n" ^ L.end_tag tag));
  Alcotest.check violation "untagged" None
    (parse "Sandbox: cat(3) deny(1) file-read-data /x");
  Alcotest.check violation "not a denial" None
    (parse (message "something else"))

let test_ndjson () =
  let line m = Yojson.Safe.to_string (`Assoc [ ("eventMessage", `String m) ]) in
  Alcotest.check violation "event"
    (Some { kind = "file-read-data"; detail = "/x by cat(3)" })
    (L.of_seatbelt_ndjson ~tag
       (line (message "Sandbox: cat(3) deny(1) file-read-data /x")));
  Alcotest.check violation "summary line" None
    (L.of_seatbelt_ndjson ~tag {|{"count":2,"finished":1}|});
  Alcotest.check violation "not JSON" None (L.of_seatbelt_ndjson ~tag "oops")

let test_relay () =
  (match
     L.of_relay_line
       {|{"kind":"relay_failed","port":8080,"socket":"/tmp/b/model.sock","reason":"refused"}|}
   with
  | Ok v ->
      Alcotest.(check string) "kind" "relay_failed" v.kind;
      Alcotest.(check string)
        "detail" "127.0.0.1:8080 to /tmp/b/model.sock: refused" v.detail
  | Error m -> Alcotest.failf "relay: %s" m);
  Alcotest.(check bool)
    "extra field" true
    (Result.is_error
       (L.of_relay_line
          {|{"kind":"relay_failed","port":1,"socket":"/s","reason":"r","x":1}|}));
  Alcotest.(check bool) "not JSON" true (Result.is_error (L.of_relay_line "x"))

let () =
  Alcotest.run "sandbox_log"
    [
      ( "seatbelt",
        [
          Alcotest.test_case "tag and predicate" `Quick test_tag;
          Alcotest.test_case "messages" `Quick test_messages;
          Alcotest.test_case "ndjson" `Quick test_ndjson;
        ] );
      ("linux", [ Alcotest.test_case "relay failures" `Quick test_relay ]);
    ]
