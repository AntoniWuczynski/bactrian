(* The servers the record flags declare. Their errors are in the command line
   tests. *)

open Bactrian_runtime

let parse ?(stdio = []) ?(http = []) ?(policy = []) () =
  Mcp_flags.parse ~stdio ~http ~policy

let describe (c : Mcp_proxy.config) =
  let server =
    match c.server with
    | Stdio { argv; env; cwd } ->
        Printf.sprintf "stdio [%s] env=%d cwd=%s" (String.concat "," argv)
          (List.length env)
          (match cwd with None -> "default" | Some _ -> "set")
    | Http { url } -> "http " ^ Uri.to_string url
  in
  Printf.sprintf "%s: %s, %s" c.name server
    (Bactrian_core.Experiment.policy_name c.policy)

let ok = function Ok cs -> List.map describe cs | Error m -> [ "error: " ^ m ]

let check label expected result =
  Alcotest.(check (list string)) label expected (ok result)

let test_servers_in_name_order () =
  check "stdio and http"
    [
      "calendar: stdio [python3,cal.py] env=0 cwd=default, record";
      "search: http https://x.example/mcp, record";
    ]
    (parse
       ~http:[ "search=https://x.example/mcp" ]
       ~stdio:[ {|calendar=["python3","cal.py"]|} ]
       ());
  check "none" [] (parse ())

let test_policies () =
  check "block and passthrough"
    [
      "a: stdio [x] env=0 cwd=default, block";
      "b: http http://y, passthrough";
      "c: stdio [z] env=0 cwd=default, record";
    ]
    (parse
       ~stdio:[ {|c=["z"]|}; {|a=["x"]|} ]
       ~http:[ "b=http://y" ]
       ~policy:[ "a=block"; "b=passthrough"; "c=record" ]
       ())

let test_value_with_equals_sign () =
  check "the first = splits"
    [ "a: stdio [sh,-c,x=1] env=0 cwd=default, record" ]
    (parse ~stdio:[ {|a=["sh","-c","x=1"]|} ] ());
  check "a URL with a query"
    [ "a: http http://x/?k=v, record" ]
    (parse ~http:[ "a=http://x/?k=v" ] ())

let test_refusals () =
  let error = function Error m -> m | Ok _ -> "accepted" in
  Alcotest.(check string)
    "replay"
    "--mcp-policy a: replay needs a recording and a record run makes one: use \
     record, passthrough or block"
    (error (parse ~stdio:[ {|a=["x"]|} ] ~policy:[ "a=replay" ] ()));
  Alcotest.(check string)
    "the later flag is blamed for a duplicate"
    "--mcp-http a: the server a is already declared by --mcp-stdio"
    (error (parse ~stdio:[ {|a=["x"]|} ] ~http:[ "a=http://x" ] ()))

let () =
  Alcotest.run "mcp flags"
    [
      ( "parse",
        [
          Alcotest.test_case "servers come in name order" `Quick
            test_servers_in_name_order;
          Alcotest.test_case "policies" `Quick test_policies;
          Alcotest.test_case "values may hold equals signs" `Quick
            test_value_with_equals_sign;
          Alcotest.test_case "refusals" `Quick test_refusals;
        ] );
    ]
