(* The bridge between the agent driver and the relay, against fake_relay.exe:
   channel 0, a connection to a host listener, log lines, and the relay's
   failures. *)

open Bactrian_runtime

let fake = Filename.concat (Sys.getcwd ()) "fake_relay.exe"

type outcome = {
  result : (int, Vm_bridge.error) result;
  out : string;
  logs : string list;
}

let bridge ?(stdin = "") ?(forwards = []) env args =
  let out = Buffer.create 64 and logs = ref [] in
  let result =
    Vm_bridge.run ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~env:[||] ~forwards
      ~stdin:(Eio.Flow.string_source stdin)
      ~stdout:(Eio.Flow.buffer_sink out)
      ~log:(fun l -> logs := l :: !logs)
      (fake :: args)
  in
  { result; out = Buffer.contents out; logs = List.rev !logs }

let code o =
  match o.result with
  | Ok c -> c
  | Error e -> Alcotest.failf "bridge: %s" (Vm_bridge.describe_error e)

let failure o =
  match o.result with
  | Ok c -> Alcotest.failf "expected a failure, got exit %d" c
  | Error e -> Vm_bridge.describe_error e

let test_channel_zero () =
  Eio_main.run @@ fun env ->
  let o = bridge env ~stdin:"hello\nworld\n" [ "echo" ] in
  Alcotest.(check int) "the agent's status" 7 (code o);
  Alcotest.(check string) "its output" "hello\nworld\n" o.out

(* The relay's connection reaches a host listener on the same port. *)
let test_connection () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let listener =
    Eio.Net.listen ~sw ~backlog:4 ~reuse_addr:true (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with `Tcp (_, p) -> p | `Unix _ -> 0
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server listener
        ~on_error:(fun (_ : exn) -> ())
        (fun flow _ ->
          let got = Eio.Buf_read.(parse_exn take_all) ~max_size:1024 flow in
          Eio.Flow.copy_string ("pong:" ^ got) flow));
  let o = bridge env ~forwards:[ port ] [ "connect"; string_of_int port ] in
  Alcotest.(check int) "status" 0 (code o);
  Alcotest.(check string) "the host's answer" "pong:ping" o.out;
  Alcotest.(check (list string)) "no failure" [] o.logs;
  (* A port that is not forwarded is refused and logged. *)
  let o = bridge env ~forwards:[] [ "connect"; string_of_int port ] in
  Alcotest.(check int) "status" 0 (code o);
  Alcotest.(check string) "nothing came back" "" o.out;
  Alcotest.(check (list string))
    "logged in the forwarder's shape"
    [
      Printf.sprintf
        {|{"kind":"relay_failed","port":%d,"socket":"host loopback","reason":"the port is not forwarded"}|}
        port;
    ]
    o.logs

(* The relay ends the agent when bactrian falls silent, so the bridge sends a
   heartbeat from the start. *)
let test_heartbeat () =
  Eio_main.run @@ fun env ->
  let o = bridge env [ "heartbeat" ] in
  Alcotest.(check int) "status" 0 (code o);
  Alcotest.(check string) "the relay heard it" "beat" o.out

(* No more connections at once than the relay's own limit. *)
let test_channel_cap () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let listener =
    Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with `Tcp (_, p) -> p | `Unix _ -> 0
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server listener
        ~on_error:(fun (_ : exn) -> ())
        (fun _ _ -> Eio.Fiber.await_cancel ()));
  let o = bridge env ~forwards:[ port ] [ "flood"; string_of_int port; "65" ] in
  Alcotest.(check int) "status" 0 (code o);
  Alcotest.(check (list string))
    "the 65th is refused"
    [
      Printf.sprintf
        {|{"kind":"relay_failed","port":%d,"socket":"host loopback","reason":"too many open connections"}|}
        port;
    ]
    o.logs

let test_log () =
  Eio_main.run @@ fun env ->
  let o = bridge env [ "log" ] in
  Alcotest.(check int) "status" 0 (code o);
  match o.logs with
  | [ line ] -> (
      match Bactrian_core.Sandbox_log.of_relay_line line with
      | Ok v -> Alcotest.(check string) "a relay failure" "relay_failed" v.kind
      | Error m -> Alcotest.failf "unreadable: %s" m)
  | _ -> Alcotest.fail "expected one line"

let test_failures () =
  Eio_main.run @@ fun env ->
  Alcotest.(check string)
    "an unknown frame" "the relay sent a bad frame: unknown frame kind 9"
    (failure (bridge env [ "bad" ]));
  Alcotest.(check string)
    "no exit frame"
    "the container runtime ended (exit status 0) before the relay reported the \
     agent's status"
    (failure (bridge env [ "silent" ]))

let test_parse () =
  let parsed args =
    match Vm_bridge.parse args with
    | Ok c ->
        Ok
          (Printf.sprintf "%s %s%s%s %s"
             (String.concat "," (List.map string_of_int c.forwards))
             c.container
             (if c.kill_signals then " (kill)" else "")
             (Option.fold ~none:"" ~some:(fun d -> " (" ^ d ^ ")") c.staging)
             (String.concat " " c.argv))
    | Error m -> Error m
  in
  Alcotest.(check (result string string))
    "words, then the runtime" (Ok "4242 bactrian-r1 docker run -i x -- y")
    (parsed
       [
         "forward=4242";
         "container=bactrian-r1";
         "--";
         "docker";
         "run";
         "-i";
         "x";
         "--";
         "y";
       ]);
  Alcotest.(check (result string string))
    "signals sent by the bridge"
    (Ok
       "4242 bactrian-r1 (kill) (/p/.bactrian/runs/r1/staging) container start \
        -a -i bactrian-r1")
    (parsed
       [
         "forward=4242";
         "container=bactrian-r1";
         "signals=kill";
         "staging=/p/.bactrian/runs/r1/staging";
         "--";
         "container";
         "start";
         "-a";
         "-i";
         "bactrian-r1";
       ]);
  Alcotest.(check (result string string))
    "a staging directory that is not one"
    (Error {|unexpected argument "staging=/p/notes"|})
    (parsed [ "staging=/p/notes"; "container=c"; "--"; "container" ]);
  Alcotest.(check (result string string))
    "another signals word" (Error {|unexpected argument "signals=pass"|})
    (parsed [ "signals=pass"; "container=c"; "--"; "container" ]);
  Alcotest.(check (result string string))
    "a container is required" (Error "missing container=NAME")
    (parsed [ "forward=1"; "--"; "docker" ]);
  Alcotest.(check (result string string))
    "a port" (Error {|"0" is not a port|})
    (parsed [ "forward=0"; "container=c"; "--"; "docker" ]);
  Alcotest.(check (result string string))
    "an option is not a word" (Error {|unexpected argument "--forward"|})
    (parsed [ "--forward"; "1"; "container=c"; "--"; "docker" ])

let () =
  Alcotest.run "vm_bridge"
    [
      ( "bridge",
        [
          Alcotest.test_case "channel 0" `Quick test_channel_zero;
          Alcotest.test_case "connections" `Quick test_connection;
          Alcotest.test_case "log lines" `Quick test_log;
          Alcotest.test_case "heartbeat" `Quick test_heartbeat;
          Alcotest.test_case "at most 64 connections" `Quick test_channel_cap;
          Alcotest.test_case "relay failures" `Quick test_failures;
          Alcotest.test_case "arguments" `Quick test_parse;
        ] );
    ]
