(* How bactrian judges the relay's probe report. *)

open Bactrian_core

let good =
  {|{"public":"ENETUNREACH","listener":"ok","outside_write":"EROFS","inside_write":"ok","denied_read":"ENOENT","interfaces":["lo"]}|}

let judged line =
  match Vm_probe.of_line line with
  | Error m -> Error [ "unreadable: " ^ m ]
  | Ok r -> Vm_probe.judge r

let with_field field value =
  let r =
    match Yojson.Safe.from_string good with
    | `Assoc fields ->
        `Assoc
          (List.map
             (fun (k, v) -> if String.equal k field then (k, value) else (k, v))
             fields)
    | _ -> Alcotest.fail "not an object"
  in
  Yojson.Safe.to_string r

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
       {|{"public":"ok","listener":"ok","outside_write":"EROFS","inside_write":"ok","denied_read":"ENOENT","interfaces":["lo","eth0"]}|});
  Alcotest.check wrong "the shared checks"
    (Error
       [
         "a connection to bactrian's own listener failed (ECONNREFUSED)";
         "a write outside the state directory succeeded";
         "a write inside the state directory failed (EACCES)";
         "a read of a denied file succeeded";
       ])
    (judged
       {|{"public":"ENETUNREACH","listener":"ECONNREFUSED","outside_write":"ok","inside_write":"EACCES","denied_read":"ok","interfaces":["lo"]}|});
  match Vm_probe.of_line {|{"public":"ok"}|} with
  | Ok _ -> Alcotest.fail "a partial report was read"
  | Error _ -> ()

let () =
  Alcotest.run "vm_probe"
    [ ("judge", [ Alcotest.test_case "reports" `Quick test_judge ]) ]
