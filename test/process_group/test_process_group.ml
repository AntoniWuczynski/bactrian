module Pg = Bactrian_runtime.Process_group

let spawn ~sw mgr script =
  Eio_unix.Process.spawn_unix ~sw mgr ~pgid:0 ~fds:[]
    [ "/bin/sh"; "-c"; script ]

let with_env f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  f ~sw ~mgr:(Eio.Stdenv.process_mgr env) ~clock:(Eio.Stdenv.clock env)

let exits () =
  with_env @@ fun ~sw ~mgr ~clock ->
  let proc = spawn ~sw mgr "exit 0" in
  let pgid = Eio.Process.pid proc in
  ignore (Eio.Process.await proc);
  Alcotest.(check bool) "gone" true (Pg.wait_gone ~clock ~timeout:5. pgid)

let ignores_sigterm () =
  with_env @@ fun ~sw ~mgr ~clock ->
  let proc = spawn ~sw mgr "trap '' TERM; while :; do sleep 1; done" in
  let pgid = Eio.Process.pid proc in
  Eio.Time.sleep clock 0.3;
  Alcotest.(check bool) "signalled" true (Pg.signal pgid Sys.sigterm);
  Alcotest.(check bool)
    "survives term" false
    (Pg.wait_gone ~clock ~timeout:0.3 pgid);
  Alcotest.(check bool) "kill signalled" true (Pg.signal pgid Sys.sigkill);
  ignore (Eio.Process.await proc);
  Alcotest.(check bool) "gone" true (Pg.wait_gone ~clock ~timeout:5. pgid)

let already_gone () =
  with_env @@ fun ~sw ~mgr ~clock:_ ->
  let proc = spawn ~sw mgr "exit 0" in
  let pgid = Eio.Process.pid proc in
  ignore (Eio.Process.await proc);
  Alcotest.(check bool) "term" false (Pg.signal pgid Sys.sigterm);
  Alcotest.(check bool) "probe" false (Pg.signal pgid 0)

let describe_exit status = Bactrian_runtime.Proc_util.describe_exit status

let describes_a_killed_process_by_signal_name () =
  with_env @@ fun ~sw ~mgr ~clock:_ ->
  List.iter
    (fun (signal, name) ->
      let proc = spawn ~sw mgr "sleep 60" in
      Alcotest.(check bool)
        "signalled" true
        (Pg.signal (Eio.Process.pid proc) signal);
      Alcotest.(check string)
        name ("killed by " ^ name)
        (describe_exit (Eio.Process.await proc)))
    [
      (Sys.sigkill, "SIGKILL"); (Sys.sigterm, "SIGTERM"); (Sys.sigint, "SIGINT");
    ]

let names_signals () =
  let name = Bactrian_runtime.Proc_util.signal_name in
  Alcotest.(check string) "SIGINT" "SIGINT" (name Sys.sigint);
  Alcotest.(check string) "SIGABRT" "SIGABRT" (name Sys.sigabrt);
  Alcotest.(check string) "a system number" "signal 64" (name 64);
  Alcotest.(check string) "an unknown OCaml number" "signal -99" (name (-99))

let () =
  Alcotest.run "process_group"
    [
      ( "process_group",
        [
          Alcotest.test_case "group that exits" `Quick exits;
          Alcotest.test_case "leader ignores SIGTERM" `Quick ignores_sigterm;
          Alcotest.test_case "already gone" `Quick already_gone;
          Alcotest.test_case "a killed process is named by its signal" `Quick
            describes_a_killed_process_by_signal_name;
          Alcotest.test_case "signal names" `Quick names_signals;
        ] );
    ]
