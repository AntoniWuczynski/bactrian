(* The agent's clock (design §5 step 2): on Linux with libfaketime a Python and
   a Node agent read SIM_NOW from their wall clocks, and the run says so. On
   macOS nothing is injected and the run says that. *)

open Run_support
open Clock_support
module J = Yojson.Safe.Util

let test_agent (name, tool, argv) =
  Alcotest.test_case name `Slow
    ( with_ctx ("clock-" ^ name) @@ fun ctx ->
      skip_unless (usable tool) (tool ^ " is not usable: skipped");
      skip_unless
        ((not linux) || Option.is_some library)
        "libfaketimeMT.so.1 is not installed: skipped";
      check_clock (run_agent ctx argv) )

let test_off =
  with_ctx "clock-off" @@ fun ctx ->
  skip_unless (usable "python3") "python3 is not usable: skipped";
  let r =
    ok
      (execute ctx
         {
           (config ~agent:[ "python3"; "-I"; "-c"; python_agent ] ()) with
           sim_now;
           faketime = false;
         })
  in
  Alcotest.(check string)
    "label"
    (Printf.sprintf {|{"mode":"contract_only","reason":"%s"}|}
       (if linux then "faketime-off" else "macos"))
    (meta_clock r);
  Alcotest.(check string)
    "nothing is preloaded" ""
    J.(to_string (member "preload" (reply r)))

let () =
  Alcotest.run "run_clock"
    [
      ( "clock",
        List.map test_agent agents
        @ [ Alcotest.test_case "faketime off" `Slow test_off ] );
    ]
