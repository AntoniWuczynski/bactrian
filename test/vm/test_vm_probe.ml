(* How bactrian judges the relay's probe report. *)

open Bactrian_core

let good =
  {|{"public":"ENETUNREACH","listener":"ok","outside_write":"EROFS","inside_write":"ok","denied_read":"ENOENT","masked_read":"ENOENT","masked_case_read":"ENOENT","root_write":"EROFS","other_run_write":"ENOENT","interfaces_listed":"ok","interfaces":["lo"]}|}

let judged line =
  match Vm_probe.of_line line with
  | Error m -> Error [ "unreadable: " ^ m ]
  | Ok r -> Vm_probe.judge r

let with_fields changes =
  let r =
    match Yojson.Safe.from_string good with
    | `Assoc fields ->
        `Assoc
          (List.map
             (fun (k, v) ->
               (k, Option.value (List.assoc_opt k changes) ~default:v))
             fields)
    | _ -> Alcotest.fail "not an object"
  in
  Yojson.Safe.to_string r

let with_field field value = with_fields [ (field, value) ]
let wrong = Alcotest.(result unit (list string))

let test_judge () =
  Alcotest.check wrong "a sandbox that holds" (Ok ()) (judged good);
  Alcotest.check wrong "a masked file that reads empty" (Ok ())
    (judged (with_field "denied_read" (`String "empty")));
  Alcotest.check wrong "a route out"
    (Error
       [
         "an outbound connection to 1.1.1.1:443 timed out instead of failing \
          at once, so the container has a route out";
       ])
    (judged (with_field "public" (`String "timeout")));
  Alcotest.check wrong "a network"
    (Error
       [
         "an outbound connection to 1.1.1.1:443 succeeded";
         "the container has network interfaces other than lo: eth0";
       ])
    (judged
       (with_fields
          [
            ("public", `String "ok");
            ("interfaces", `List [ `String "lo"; `String "eth0" ]);
          ]));
  Alcotest.check wrong "the shared checks"
    (Error
       [
         "a connection to bactrian's own listener failed (ECONNREFUSED)";
         "a write outside the state directory succeeded";
         "a write inside the state directory failed (EACCES)";
         "a read of a denied file succeeded";
       ])
    (judged
       {|{"public":"ENETUNREACH","listener":"ECONNREFUSED","outside_write":"ok","inside_write":"EACCES","denied_read":"ok","masked_read":"ENOENT","masked_case_read":"ENOENT","root_write":"EROFS","other_run_write":"ENOENT","interfaces_listed":"ok","interfaces":["lo"]}|});
  Alcotest.check wrong "a masked file read under another name"
    (Error
       [ "a read of a masked file under its name in another case succeeded" ])
    (judged (with_field "masked_case_read" (`String "ok")));
  Alcotest.check wrong "an empty file over a masked file is a surprise"
    (Error [ "a read of a masked file failed with empty instead of ENOENT" ])
    (judged (with_field "masked_read" (`String "empty")));
  Alcotest.check wrong "a writable root" (Error [ "a write to / succeeded" ])
    (judged (with_field "root_write" (`String "ok")));
  Alcotest.check wrong "another run visible"
    (Error
       [
         "a write into another run's directory failed with EROFS instead of \
          ENOENT";
       ])
    (judged (with_field "other_run_write" (`String "EROFS")));
  Alcotest.check wrong "an outside write denied the wrong way"
    (Error
       [
         "a write outside the state directory failed with EACCES instead of \
          EROFS";
       ])
    (judged (with_field "outside_write" (`String "EACCES")));
  Alcotest.check wrong "interfaces that cannot be listed"
    (Error [ "the network interfaces could not be listed (ENOENT)" ])
    (judged
       (with_fields
          [ ("interfaces_listed", `String "ENOENT"); ("interfaces", `List []) ]));
  Alcotest.check wrong "no interface at all"
    (Error [ "the container has no network interface at all, not even lo" ])
    (judged (with_field "interfaces" (`List [])));
  match Vm_probe.of_line {|{"public":"ok"}|} with
  | Ok _ -> Alcotest.fail "a partial report was read"
  | Error _ -> ()

let () =
  Alcotest.run "vm_probe"
    [ ("judge", [ Alcotest.test_case "reports" `Quick test_judge ]) ]
