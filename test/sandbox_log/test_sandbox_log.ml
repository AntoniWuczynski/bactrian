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
    (parse (message "something else"));
  Alcotest.check violation "a process named to look like a denial"
    (Some
       { kind = "network-outbound"; detail = "remote:*:443 by a deny(1) x(77)" })
    (parse
       (message "Sandbox: a deny(1) x(77) deny(1) network-outbound remote:*:443"));
  Alcotest.check violation "an unknown operation"
    (Some { kind = "unparsed"; detail = "x(7) deny(1) made-up thing" })
    (parse (message "Sandbox: x(7) deny(1) made-up thing"))

let test_ndjson () =
  let line m = Yojson.Safe.to_string (`Assoc [ ("eventMessage", `String m) ]) in
  Alcotest.check violation "event"
    (Some { kind = "file-read-data"; detail = "/x by cat(3)" })
    (L.of_seatbelt_ndjson ~tag
       (line (message "Sandbox: cat(3) deny(1) file-read-data /x")));
  Alcotest.check violation "summary line" None
    (L.of_seatbelt_ndjson ~tag {|{"count":2,"finished":1}|});
  Alcotest.check violation "not JSON" None (L.of_seatbelt_ndjson ~tag "oops")

(* [log show] as a script of reads, on a clock that only [wait] moves. *)
let settle ?(deadline = 30.) reads =
  let line m = Yojson.Safe.to_string (`Assoc [ ("eventMessage", `String m) ]) in
  let denial d = line (message ("Sandbox: cat(3) deny(1) file-read-data " ^ d))
  and marker =
    line ("Sandbox: touch(5) deny(1) file-write-create /m\n" ^ L.end_tag tag)
  in
  let script = ref reads and clock = ref 0. and calls = ref 0 in
  let marks = ref [] in
  let read () =
    incr calls;
    match !script with
    | [] -> Alcotest.fail "read after the script ended"
    | (ds, marked) :: rest ->
        script := rest;
        List.map denial ds @ if marked then [ marker ] else []
  in
  let result =
    L.settle ~tag ~read
      ~wait:(fun s -> clock := !clock +. s)
      ~now:(fun () -> !clock)
      ~mark:(fun () -> marks := !clock :: !marks)
      ~deadline
  in
  let details = List.map (fun (v : L.violation) -> v.detail) in
  ( (match result with
    | Settled vs -> Some ("settled", details vs)
    | Unsettled vs -> Some ("unsettled", details vs)
    | No_marker -> None),
    !calls,
    !clock,
    List.rev !marks )

let test_settle () =
  let check ?(marks = []) name expected (got, calls, clock, marked) =
    Alcotest.(
      check (triple (option (pair string (list string))) int (float 0.)))
      name expected (got, calls, clock);
    Alcotest.(check (list (float 0.))) (name ^ ": marks") marks marked
  in
  check "stable at once: the marker read and one more"
    (Some ("settled", [ "/a by cat(3)" ]), 2, 1.)
    (settle [ ([ "/a" ], true); ([ "/a" ], true) ]);
  check "polls until the marker, then waits for a late denial"
    (Some ("settled", [ "/a by cat(3)"; "/b by cat(3)" ]), 4, 2.5)
    (settle
       [
         ([], false);
         ([ "/a" ], true);
         ([ "/a"; "/b" ], true);
         ([ "/a"; "/b" ], true);
       ]);
  check "no marker by the deadline" (None, 3, 1.)
    (settle ~deadline:1. [ ([], false); ([], false); ([], false) ]);
  check "still changing at the deadline: the last read"
    (Some ("unsettled", [ "/a by cat(3)"; "/b by cat(3)" ]), 2, 1.)
    (settle ~deadline:1.5 [ ([ "/a" ], true); ([ "/a"; "/b" ], true) ]);
  let missing n = List.init n (fun _ -> ([], false)) in
  check "the marker logged again every 5 seconds while missing"
    ~marks:[ 5.; 10. ]
    (Some ("settled", [ "/a by cat(3)" ]), 24, 12.)
    (settle (missing 22 @ [ ([ "/a" ], true); ([ "/a" ], true) ]));
  check "bounded by the deadline however often it is logged"
    ~marks:[ 5.; 10.; 15. ] (None, 37, 18.)
    (settle ~deadline:18. (missing 37))

let test_relay_log () =
  let good =
    {|{"kind":"relay_failed","port":8080,"socket":"/s","reason":"refused"}|}
  in
  let log = L.of_relay_log (good ^ "\nnot json\n\n" ^ good ^ "\n{") in
  Alcotest.(check int) "both good lines kept" 2 (List.length log.items);
  Alcotest.(check int) "the bad ones counted" 2 log.unreadable;
  Alcotest.(check int) "none dropped" 0 log.not_kept;
  let line n =
    Printf.sprintf
      {|{"kind":"dns","transport":"udp","name":"n%d.test","type":"A"}|} n
  in
  let flood =
    L.of_relay_log ~max_items:3
      (String.concat "\n" (List.init 5 line @ [ "x"; line 5 ]))
  in
  Alcotest.(check (list string))
    "the first ones kept"
    [ "n0.test A over udp"; "n1.test A over udp"; "n2.test A over udp" ]
    (List.map (fun (v : L.violation) -> v.detail) flood.items);
  Alcotest.(check int) "the rest counted" 3 flood.not_kept;
  Alcotest.(check int) "a bad line still counted" 1 flood.unreadable

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

let test_dns () =
  let read line =
    match L.of_relay_line line with
    | Ok v -> (v.kind, v.detail)
    | Error m -> Alcotest.failf "%s: %s" line m
  in
  Alcotest.(check (pair string string))
    "a lookup"
    ("dns", "example.com AAAA over udp")
    (read
       {|{"kind":"dns","transport":"udp","name":"example.com","type":"AAAA"}|});
  Alcotest.(check (pair string string))
    "an unreadable query"
    ("dns", "an unreadable query over tcp: 2 questions, not one")
    (read {|{"kind":"dns","transport":"tcp","error":"2 questions, not one"}|});
  List.iter
    (fun line ->
      Alcotest.(check bool) line true (Result.is_error (L.of_relay_line line)))
    [
      {|{"kind":"dns","transport":"udp","name":"a"}|};
      {|{"kind":"dns","transport":"udp","name":"a","type":"A","error":"e"}|};
      {|{"kind":"dns","transport":"udp","name":"a","type":"A","x":1}|};
      {|{"kind":"other","transport":"udp"}|};
      {|["dns"]|};
    ]

let () =
  Alcotest.run "sandbox_log"
    [
      ( "seatbelt",
        [
          Alcotest.test_case "tag and predicate" `Quick test_tag;
          Alcotest.test_case "messages" `Quick test_messages;
          Alcotest.test_case "ndjson" `Quick test_ndjson;
          Alcotest.test_case "settle" `Quick test_settle;
          Alcotest.test_case "a whole relay log" `Quick test_relay_log;
        ] );
      ( "linux",
        [
          Alcotest.test_case "relay failures" `Quick test_relay;
          Alcotest.test_case "dns lookups" `Quick test_dns;
        ] );
    ]
