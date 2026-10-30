module S = Bactrian_runtime.Sandbox_init
module Pg = Bactrian_runtime.Process_group
module Buf_read = Eio.Buf_read

let bactrian = Filename.concat (Sys.getcwd ()) (Sys.getenv "BACTRIAN_BIN")

(* Parsing *)

let config_t =
  Alcotest.of_pp (fun ppf _ -> Format.pp_print_string ppf "<config>")

let parse_result =
  Alcotest.result config_t
    (Alcotest.of_pp (fun ppf e ->
         Format.pp_print_string ppf (S.describe_parse_error e)))

let parses () =
  let check name expected args =
    Alcotest.check parse_result name expected (S.parse args)
  in
  check "two pairs"
    (Ok
       {
         S.forwards =
           [
             { port = 8080; socket = "/a/b.sock" }; { port = 1; socket = "/c" };
           ];
         dns = None;
         agent = [ "node"; "agent.js" ];
       })
    [ "8080=/a/b.sock"; "1=/c"; "--"; "node"; "agent.js" ];
  check "no pairs"
    (Ok { forwards = []; dns = None; agent = [ "x" ] })
    [ "--"; "x" ];
  check "socket keeps later equals signs"
    (Ok
       {
         forwards = [ { port = 9; socket = "/a=b" } ];
         dns = None;
         agent = [ "x" ];
       })
    [ "9=/a=b"; "--"; "x" ];
  check "later separators belong to the agent"
    (Ok { forwards = []; dns = None; agent = [ "x"; "--"; "9=/y" ] })
    [ "--"; "x"; "--"; "9=/y" ];
  check "dns as a user"
    (Ok
       {
         forwards = [ { port = 9; socket = "/s" } ];
         dns = Some { uid = 1001; gid = 118 };
         agent = [ "x" ];
       })
    [ "--dns=1001:118"; "9=/s"; "--"; "x" ];
  check "dns as a directory user's ten-digit IDs"
    (Ok
       {
         forwards = [];
         dns = Some { uid = 1876400001; gid = 4294967294 };
         agent = [ "x" ];
       })
    [ "--dns=1876400001:4294967294"; "--"; "x" ]

let rejects () =
  let check name expected args =
    Alcotest.check parse_result name (Error expected) (S.parse args)
  in
  check "no equals sign" (S.Bad_pair "8080") [ "8080"; "--"; "x" ];
  check "empty port" (S.Bad_port "") [ "=/s"; "--"; "x" ];
  List.iter
    (fun port ->
      check ("port " ^ port) (S.Bad_port port) [ port ^ "=/s"; "--"; "x" ])
    [ "0"; "65536"; "99999"; "123456"; "-1"; "+1"; "1_0"; "0x10"; "http" ];
  check "relative socket" (S.Bad_socket "s.sock") [ "80=s.sock"; "--"; "x" ];
  check "empty socket" (S.Bad_socket "") [ "80="; "--"; "x" ];
  check "duplicate port" (S.Duplicate_port 80)
    [ "80=/a"; "81=/b"; "80=/c"; "--"; "x" ];
  check "missing separator" S.Missing_separator [ "80=/a"; "x" ];
  check "nothing at all" S.Missing_separator [];
  check "empty agent" S.Empty_agent [ "80=/a"; "--" ];
  List.iter
    (fun id ->
      check ("identity " ^ id) (S.Bad_identity id) [ "--dns=" ^ id; "--"; "x" ])
    [
      "";
      "1";
      "1:";
      ":1";
      "a:b";
      "1:2:3";
      "-1:2";
      "1:+2";
      "4294967295:1";
      "1:12345678901";
      "99999999999:1";
    ];
  check "dns only first" (S.Bad_port "--dns")
    [ "80=/a"; "--dns=1:1"; "--"; "x" ]

let parses_user () =
  let t =
    Alcotest.(
      result
        (pair
           (of_pp (fun ppf (id : S.identity) ->
                Format.fprintf ppf "%d:%d" id.uid id.gid))
           (list string))
        string)
  in
  let ok = Alcotest.check t in
  ok "the IDs and the agent"
    (Ok ({ uid = 1001; gid = 118 }, [ "x"; "--"; "y" ]))
    (S.parse_user [ "1001:118"; "--"; "x"; "--"; "y" ]);
  List.iter
    (fun args ->
      match S.parse_user args with
      | Ok _ -> Alcotest.failf "accepted %s" (String.concat " " args)
      | Error (_ : string) -> ())
    [ []; [ "1:1" ]; [ "1:1"; "--" ]; [ "1:1"; "x" ]; [ "a:b"; "--"; "x" ] ]

(* Harness *)

type ctx = { env : Eio_unix.Stdenv.base; sw : Eio.Switch.t; dir : string }

let remove_tree dir =
  Array.iter (fun f -> Sys.remove (Filename.concat dir f)) (Sys.readdir dir);
  Unix.rmdir dir

let with_ctx f =
  let dir = Filename.temp_dir "bactrian-sbx" "" in
  Fun.protect ~finally:(fun () -> remove_tree dir) @@ fun () ->
  Eio_main.run @@ fun env ->
  (* Only a guard against a hang: nothing waits for this to expire. *)
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60. @@ fun () ->
  Eio.Switch.run @@ fun sw -> f { env; sw; dir }

let path ctx name = Filename.concat ctx.dir name
let loopback = Eio.Net.Ipaddr.V4.loopback

let connect ctx port =
  Eio.Net.connect ~sw:ctx.sw (Eio.Stdenv.net ctx.env) (`Tcp (loopback, port))

let reader flow = Buf_read.of_flow ~max_size:max_int flow
let half_close flow = Eio.Flow.shutdown flow `Send

let serve_unix ctx socket handler =
  let listener =
    Eio.Net.listen ~sw:ctx.sw ~backlog:16 (Eio.Stdenv.net ctx.env)
      (`Unix socket)
  in
  Eio.Fiber.fork_daemon ~sw:ctx.sw (fun () ->
      Eio.Net.run_server listener ~on_error:raise (fun flow _ -> handler flow))

let echo ctx socket =
  serve_unix ctx socket (fun flow ->
      Eio.Flow.copy flow flow;
      half_close flow)

type forwarder = {
  ports : int list;  (** Bound ports, in the order of the sockets given. *)
  out : Buf_read.t;  (** The agent's standard output. *)
  result : (int, S.error) result Eio.Promise.t;
}

let describe = function
  | Ok code -> Printf.sprintf "exit %d" code
  | Error e -> S.describe_error e

(* Runs the forwarder in a daemon fibre of [ctx.sw], so ending the switch ends
   the run, and returns once every listener is bound. *)
let start ctx ?log ?signals ~forwards agent =
  let n = List.length forwards in
  let bound = ref [] in
  let all_bound, set_bound = Eio.Promise.create () in
  let result, set_result = Eio.Promise.create () in
  let out_r, out_w = Eio_unix.pipe ctx.sw in
  let log =
    Option.map
      (fun p ->
        Eio_unix.Fd.of_unix ~sw:ctx.sw ~close_unix:true
          (Unix.openfile p [ O_WRONLY; O_CREAT; O_APPEND; O_CLOEXEC ] 0o600))
      log
  in
  let on_listening ~requested:_ ~bound:addr =
    (match addr with
    | `Tcp (ip, port) ->
        Alcotest.(check string)
          "listens on loopback only" "127.0.0.1"
          (Format.asprintf "%a" Eio.Net.Ipaddr.pp ip);
        bound := port :: !bound
    | `Unix _ -> Alcotest.fail "unexpected unix listener");
    if List.length !bound = n then
      Eio.Promise.resolve set_bound (List.rev !bound)
  in
  if n = 0 then Eio.Promise.resolve set_bound [];
  Eio.Fiber.fork_daemon ~sw:ctx.sw (fun () ->
      Eio.Promise.resolve set_result
        (S.run ~on_listening ?log ?signals
           ~stdout:(Eio_unix.Resource.fd out_w)
           ~net:(Eio.Stdenv.net ctx.env)
           ~process_mgr:(Eio.Stdenv.process_mgr ctx.env)
           {
             forwards = List.map (fun socket -> { S.port = 0; socket }) forwards;
             dns = None;
             agent;
           });
      `Stop_daemon);
  let ports =
    Eio.Fiber.first
      (fun () -> Eio.Promise.await all_bound)
      (fun () ->
        Alcotest.failf "forwarder returned early: %s"
          (describe (Eio.Promise.await result)))
  in
  { ports; out = reader out_r; result }

let sh script = [ "/bin/sh"; "-c"; script ]

(* Prints the leader's pid once its traps are set and its helper runs, then
   waits. *)
let waiting ?(traps = "") () = sh (traps ^ "sleep 60 & echo $$; wait")
let pid_line f = int_of_string (Buf_read.line f.out)
let port f i = List.nth f.ports i

let payload n =
  String.init n (fun i -> Char.chr (((i * 7) + (i / 251)) land 255))

(* Relaying *)

let echoes_both_ways () =
  with_ctx @@ fun ctx ->
  let sock = path ctx "echo.sock" in
  echo ctx sock;
  let f = start ctx ~forwards:[ sock ] (waiting ()) in
  ignore (pid_line f : int);
  let client = connect ctx (port f 0) in
  let r = reader client in
  Eio.Flow.copy_string "hello" client;
  Alcotest.(check string) "echoed" "hello" (Buf_read.take 5 r);
  Eio.Flow.copy_string " again" client;
  half_close client;
  Alcotest.(check string)
    "rest, then end of file" " again" (Buf_read.take_all r)

let large_payload () =
  with_ctx @@ fun ctx ->
  let sock = path ctx "echo.sock" in
  echo ctx sock;
  let f = start ctx ~forwards:[ sock ] (waiting ()) in
  ignore (pid_line f : int);
  let client = connect ctx (port f 0) in
  let sent = payload (4 * 1024 * 1024) and got = ref "" in
  Eio.Fiber.both
    (fun () ->
      Eio.Flow.copy_string sent client;
      half_close client)
    (fun () -> got := Buf_read.take_all (reader client));
  Alcotest.(check int) "length" (String.length sent) (String.length !got);
  Alcotest.(check bool) "identical" true (String.equal sent !got)

(* The server answers only once it has seen the end of the client's stream. *)
let half_close_to_server () =
  with_ctx @@ fun ctx ->
  let sock = path ctx "count.sock" in
  serve_unix ctx sock (fun flow ->
      let n = String.length (Buf_read.take_all (reader flow)) in
      Eio.Flow.copy_string (Printf.sprintf "got %d" n) flow);
  let f = start ctx ~forwards:[ sock ] (waiting ()) in
  ignore (pid_line f : int);
  let client = connect ctx (port f 0) in
  Eio.Flow.copy_string (payload 100_000) client;
  half_close client;
  Alcotest.(check string)
    "reply" "got 100000"
    (Buf_read.take_all (reader client))

(* The server finishes talking first, then keeps listening. *)
let half_close_to_client () =
  with_ctx @@ fun ctx ->
  let sock = path ctx "greet.sock" in
  let late, set_late = Eio.Promise.create () in
  serve_unix ctx sock (fun flow ->
      Eio.Flow.copy_string "hello" flow;
      half_close flow;
      Eio.Promise.resolve set_late (Buf_read.take_all (reader flow)));
  let f = start ctx ~forwards:[ sock ] (waiting ()) in
  ignore (pid_line f : int);
  let client = connect ctx (port f 0) in
  let r = reader client in
  Alcotest.(check string)
    "greeting, then end of file" "hello" (Buf_read.take_all r);
  Eio.Flow.copy_string "late" client;
  half_close client;
  Alcotest.(check string) "server still hears" "late" (Eio.Promise.await late)

let several_listeners () =
  with_ctx @@ fun ctx ->
  let names = [ "alpha"; "beta"; "gamma" ] in
  let socks = List.map (fun n -> path ctx (n ^ ".sock")) names in
  List.iter2
    (fun n sock ->
      serve_unix ctx sock (fun flow ->
          Eio.Flow.copy_string n flow;
          half_close flow))
    names socks;
  let f = start ctx ~forwards:socks (waiting ()) in
  ignore (pid_line f : int);
  List.iteri
    (fun i n ->
      let client = connect ctx (port f i) in
      Alcotest.(check string) n n (Buf_read.take_all (reader client)))
    names

let parallel_connections () =
  with_ctx @@ fun ctx ->
  let sock = path ctx "echo.sock" in
  echo ctx sock;
  let f = start ctx ~forwards:[ sock ] (waiting ()) in
  ignore (pid_line f : int);
  Eio.Fiber.List.iter
    (fun i ->
      let client = connect ctx (port f 0) in
      let sent = payload (50_000 + i) and got = ref "" in
      Eio.Fiber.both
        (fun () ->
          Eio.Flow.copy_string sent client;
          half_close client)
        (fun () -> got := Buf_read.take_all (reader client));
      Alcotest.(check bool)
        (Printf.sprintf "connection %d" i)
        true (String.equal sent !got))
    (List.init 16 Fun.id)

(* Failures *)

let log_lines ctx name =
  Eio.Path.load Eio.Path.(Eio.Stdenv.fs ctx.env / path ctx name)
  |> String.split_on_char '\n'
  |> List.filter (fun l -> not (String.equal l ""))

let field name json =
  Yojson.Safe.Util.member name (Yojson.Safe.from_string json)

let unreachable_socket_is_logged () =
  with_ctx @@ fun ctx ->
  let missing = path ctx "missing.sock" and stale = path ctx "stale.sock" in
  (* A socket file nobody listens on: connecting is refused. *)
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind fd (Unix.ADDR_UNIX stale);
  Unix.close fd;
  let log = path ctx "violations.log" in
  let f = start ctx ~log ~forwards:[ missing; stale ] (waiting ()) in
  ignore (pid_line f : int);
  List.iteri
    (fun i _ ->
      let client = connect ctx (port f i) in
      (* The line is written before the connection is closed. *)
      Alcotest.(check string)
        "closed without data" ""
        (Buf_read.take_all (reader client)))
    [ missing; stale ];
  let lines = log_lines ctx "violations.log" in
  Alcotest.(check int) "one line per connection" 2 (List.length lines);
  List.iteri
    (fun i line ->
      let socket = if i = 0 then missing else stale in
      Alcotest.(check string)
        "kind" "relay_failed"
        (Yojson.Safe.Util.to_string (field "kind" line));
      Alcotest.(check int)
        "port" (port f i)
        (Yojson.Safe.Util.to_int (field "port" line));
      Alcotest.(check string)
        "socket" socket
        (Yojson.Safe.Util.to_string (field "socket" line));
      Alcotest.(check bool)
        "reason" true
        (String.length (Yojson.Safe.Util.to_string (field "reason" line)) > 0))
    lines

let failing_connection_is_alone () =
  with_ctx @@ fun ctx ->
  let good = path ctx "echo.sock" and missing = path ctx "missing.sock" in
  echo ctx good;
  let log = path ctx "violations.log" in
  let f = start ctx ~log ~forwards:[ good; missing ] (waiting ()) in
  ignore (pid_line f : int);
  let healthy = connect ctx (port f 0) in
  let r = reader healthy in
  Eio.Flow.copy_string "before" healthy;
  Alcotest.(check string) "before" "before" (Buf_read.take 6 r);
  let broken = connect ctx (port f 1) in
  Alcotest.(check string) "closed" "" (Buf_read.take_all (reader broken));
  Eio.Flow.copy_string "after" healthy;
  Alcotest.(check string) "after" "after" (Buf_read.take 5 r);
  Alcotest.(check int)
    "one violation" 1
    (List.length (log_lines ctx "violations.log"));
  let again = connect ctx (port f 0) in
  Eio.Flow.copy_string "new" again;
  Alcotest.(check string)
    "new connection" "new"
    (Buf_read.take 3 (reader again))

let no_log_writes_nothing () =
  with_ctx @@ fun ctx ->
  let f = start ctx ~forwards:[ path ctx "missing.sock" ] (waiting ()) in
  ignore (pid_line f : int);
  let client = connect ctx (port f 0) in
  Alcotest.(check string) "closed" "" (Buf_read.take_all (reader client));
  Alcotest.(check (list string))
    "directory untouched" []
    (Array.to_list (Sys.readdir ctx.dir))

(* The agent *)

let run_to_end ctx ?(forwards = []) agent =
  let f = start ctx ~forwards agent in
  (f, Eio.Promise.await f.result)

let exit_code_is_propagated () =
  with_ctx @@ fun ctx ->
  let _, result = run_to_end ctx (sh "exit 3") in
  Alcotest.(check string) "status" "exit 3" (describe result);
  let _, result = run_to_end ctx (sh "exit 0") in
  Alcotest.(check string) "status" "exit 0" (describe result)

let signal_death_is_128_plus () =
  with_ctx @@ fun ctx ->
  let _, result = run_to_end ctx (sh "kill -KILL $$") in
  Alcotest.(check string) "SIGKILL" "exit 137" (describe result);
  let _, result = run_to_end ctx (sh "kill -TERM $$") in
  Alcotest.(check string) "SIGTERM" "exit 143" (describe result)

let signal_reaches_agent signal name expected_code () =
  with_ctx @@ fun ctx ->
  let traps =
    Printf.sprintf "trap 'echo got-%s; exit %d' %s; " name expected_code name
  in
  let f = start ctx ~forwards:[] (waiting ~traps ()) in
  let pid = pid_line f in
  Alcotest.(check bool) "agent is a group leader" true (Pg.signal pid 0);
  (* Delivered to this process, which is the forwarder here. *)
  Unix.kill (Unix.getpid ()) signal;
  Alcotest.(check string) "agent saw it" ("got-" ^ name) (Buf_read.line f.out);
  Alcotest.(check string)
    "exit code"
    (Printf.sprintf "exit %d" expected_code)
    (describe (Eio.Promise.await f.result));
  Alcotest.(check bool)
    "group gone" true
    (Pg.wait_gone ~clock:(Eio.Stdenv.clock ctx.env) ~timeout:5. pid)

let sigterm_reaches_agent = signal_reaches_agent Sys.sigterm "TERM" 9
let sigint_reaches_agent = signal_reaches_agent Sys.sigint "INT" 8

(* bactrian's way to send SIGTERM through bubblewrap: a byte on the pipe. Its
   end asks for nothing. *)
let signal_pipe_terminates_agent () =
  with_ctx @@ fun ctx ->
  let r, w = Unix.pipe ~cloexec:true () in
  let signals = Eio_unix.Fd.of_unix ~sw:ctx.sw ~close_unix:true r in
  let f =
    start ctx ~signals ~forwards:[]
      (waiting ~traps:"trap 'echo got-TERM; exit 9' TERM; " ())
  in
  let pid = pid_line f in
  ignore (Unix.write_substring w "t" 0 1 : int);
  Alcotest.(check string) "agent saw SIGTERM" "got-TERM" (Buf_read.line f.out);
  Alcotest.(check string)
    "exit code" "exit 9"
    (describe (Eio.Promise.await f.result));
  Unix.close w;
  Alcotest.(check bool)
    "group gone" true
    (Pg.wait_gone ~clock:(Eio.Stdenv.clock ctx.env) ~timeout:5. pid)

let closed_signal_pipe_asks_nothing () =
  with_ctx @@ fun ctx ->
  let r, w = Unix.pipe ~cloexec:true () in
  let signals = Eio_unix.Fd.of_unix ~sw:ctx.sw ~close_unix:true r in
  let f = start ctx ~signals ~forwards:[] (waiting ()) in
  let pid = pid_line f in
  Unix.close w;
  Eio.Time.sleep (Eio.Stdenv.clock ctx.env) 0.3;
  Alcotest.(check bool) "agent still runs" true (Pg.signal pid 0);
  Unix.kill (Unix.getpid ()) Sys.sigterm;
  Alcotest.(check string)
    "ended by the real signal" "exit 143"
    (describe (Eio.Promise.await f.result))

(* DNS *)

(* Port 53 needs root on Linux, and macOS has no 127.0.0.53, so the forwarder
   stops before the agent starts. *)
let dns_needs_port_53 () =
  if Unix.geteuid () = 0 then Alcotest.skip ();
  with_ctx @@ fun ctx ->
  match
    S.run ~net:(Eio.Stdenv.net ctx.env)
      ~process_mgr:(Eio.Stdenv.process_mgr ctx.env)
      {
        forwards = [];
        dns = Some { uid = 1; gid = 1 };
        agent = sh ("touch " ^ path ctx "ran");
      }
  with
  | Error (S.Dns_failed _) ->
      Alcotest.(check bool)
        "the agent did not start" false
        (Sys.file_exists (path ctx "ran"))
  | Error ((S.Listen_failed _ | S.Spawn_failed _) as e) ->
      Alcotest.failf "failed: %s" (S.describe_error e)
  | Ok code -> Alcotest.failf "ran and exited %d" code

let signal_reaches_whole_group () =
  with_ctx @@ fun ctx ->
  (* The helper dies from the forwarded SIGTERM, not from the cleanup. *)
  let f =
    start ctx ~forwards:[]
      (sh
         "sleep 60 & helper=$!; trap 'wait $helper; echo helper-$?; exit 0' \
          TERM; echo $$; wait $helper; wait")
  in
  ignore (pid_line f : int);
  Unix.kill (Unix.getpid ()) Sys.sigterm;
  Alcotest.(check string)
    "helper killed by SIGTERM" "helper-143" (Buf_read.line f.out);
  Alcotest.(check string)
    "status" "exit 0"
    (describe (Eio.Promise.await f.result))

let environment_loses_log_variable () =
  with_ctx @@ fun ctx ->
  Unix.putenv S.log_variable "3";
  Unix.putenv S.signal_variable "4";
  Unix.putenv "BACTRIAN_SANDBOX_KEPT" "yes";
  let f, result =
    run_to_end ctx
      (sh
         "echo ${BACTRIAN_SANDBOX_LOG_FD-unset} \
          ${BACTRIAN_SANDBOX_SIGNAL_FD-unset} $BACTRIAN_SANDBOX_KEPT")
  in
  Alcotest.(check string) "status" "exit 0" (describe result);
  Alcotest.(check string) "variables" "unset unset yes" (Buf_read.line f.out)

let own_process_group () =
  with_ctx @@ fun ctx ->
  let f, result = run_to_end ctx (sh "ps -o pgid= -p $$; echo $$") in
  Alcotest.(check string) "status" "exit 0" (describe result);
  let pgid = int_of_string (String.trim (Buf_read.line f.out)) in
  let pid = int_of_string (Buf_read.line f.out) in
  Alcotest.(check int) "leads its own group" pid pgid

let listeners_close_with_the_agent () =
  with_ctx @@ fun ctx ->
  let f, result =
    run_to_end ctx ~forwards:[ path ctx "x.sock" ] (sh "exit 0")
  in
  Alcotest.(check string) "status" "exit 0" (describe result);
  match connect ctx (port f 0) with
  | _ -> Alcotest.fail "the listener is still open"
  | exception Eio.Io (Eio.Net.E (Connection_failure _), _) -> ()

let ending_the_run_kills_the_group () =
  with_ctx @@ fun ctx ->
  let pid =
    Eio.Switch.run @@ fun sw ->
    let f = start { ctx with sw } ~forwards:[] (waiting ()) in
    pid_line f
  in
  Alcotest.(check bool)
    "agent and its helper are gone" true
    (Pg.wait_gone ~clock:(Eio.Stdenv.clock ctx.env) ~timeout:5. pid)

let stdout_is_the_agents () =
  with_ctx @@ fun ctx ->
  let f, result = run_to_end ctx (sh "echo one; echo two >&2; echo three") in
  Alcotest.(check string) "status" "exit 0" (describe result);
  Alcotest.(check string) "first" "one" (Buf_read.line f.out);
  Alcotest.(check string) "second" "three" (Buf_read.line f.out)

let port_in_use () =
  with_ctx @@ fun ctx ->
  let taken =
    Eio.Net.listen ~sw:ctx.sw ~reuse_addr:true ~backlog:1
      (Eio.Stdenv.net ctx.env)
      (`Tcp (loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr taken with
    | `Tcp (_, p) -> p
    | `Unix _ -> Alcotest.fail "tcp expected"
  in
  let result =
    S.run ~net:(Eio.Stdenv.net ctx.env)
      ~process_mgr:(Eio.Stdenv.process_mgr ctx.env)
      {
        forwards = [ { port; socket = path ctx "x.sock" } ];
        dns = None;
        agent = sh "exit 0";
      }
  in
  match result with
  | Error (S.Listen_failed { port = p; _ }) ->
      Alcotest.(check int) "port" port p
  | Error ((S.Spawn_failed _ | S.Dns_failed _) as e) ->
      Alcotest.failf "failed: %s" (S.describe_error e)
  | Ok code -> Alcotest.failf "ran and exited %d" code

let missing_agent () =
  with_ctx @@ fun ctx ->
  let result =
    S.run ~net:(Eio.Stdenv.net ctx.env)
      ~process_mgr:(Eio.Stdenv.process_mgr ctx.env)
      { forwards = []; dns = None; agent = [ "/nonexistent/bactrian-agent" ] }
  in
  match result with
  | Error (S.Spawn_failed _) -> ()
  | Error (S.Listen_failed _ | S.Dns_failed _) | Ok _ ->
      Alcotest.fail "expected Spawn_failed"

(* The subcommand *)

(* [log] becomes the subcommand's descriptor 3 and [signals] its descriptor 4,
   each named by its variable. *)
let run_bactrian ?log ?signals ~name args =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let out_r, out_w = Eio_unix.pipe sw in
  let inherited =
    Option.to_list
      (Option.map
         (fun p ->
           ( 3,
             Eio_unix.Fd.of_unix ~sw ~close_unix:true
               (Unix.openfile p
                  [ O_WRONLY; O_CREAT; O_APPEND; O_CLOEXEC ]
                  0o600),
             `Blocking ))
         log)
    @ Option.to_list
        (Option.map
           (fun fd ->
             (4, Eio_unix.Fd.of_unix ~sw ~close_unix:false fd, `Blocking))
           signals)
  in
  let out = Eio_unix.Resource.fd out_w in
  let env_vars =
    Eio.Process.Env.override
      [
        (S.log_variable, Option.map (fun (_ : string) -> "3") log);
        ( S.signal_variable,
          Option.map (fun (_ : Unix.file_descr) -> "4") signals );
      ]
      (Eio.Process.environment (Eio.Stdenv.process_mgr env))
  in
  let proc =
    Eio_unix.Process.spawn_unix ~sw
      (Eio.Stdenv.process_mgr env)
      ~env:env_vars
      ~fds:
        ([
           (0, Eio_unix.Fd.stdin, `Blocking);
           (1, out, `Blocking);
           (2, out, `Blocking);
         ]
        @ inherited)
      (bactrian :: name :: args)
  in
  Eio.Resource.close out_w;
  let output = Buf_read.parse_exn Buf_read.take_all ~max_size:max_int out_r in
  (Eio.Process.await proc, output)

let subcommand ?log ?signals args =
  run_bactrian ?log ?signals ~name:"__sandbox-init" args

let status_t =
  Alcotest.of_pp (fun ppf -> function
    | `Exited n -> Format.fprintf ppf "exited %d" n
    | `Signaled n -> Format.fprintf ppf "signalled %d" n)

let subcommand_runs_the_agent () =
  let status, output =
    subcommand [ "--"; "/bin/sh"; "-c"; "echo $0 $1; exit 7"; "a b"; "--" ]
  in
  Alcotest.check status_t "status" (`Exited 7) status;
  Alcotest.(check string) "argv after the first separator" "a b --\n" output

let contains ~needle text =
  let n = String.length needle in
  let rec find i =
    i + n <= String.length text
    && (String.equal (String.sub text i n) needle || find (i + 1))
  in
  find 0

let subcommand_rejects_bad_arguments () =
  let status, output = subcommand [ "8080=/s"; "/bin/true" ] in
  Alcotest.check status_t "status" (`Exited 64) status;
  Alcotest.(check bool) "says why" true (contains ~needle:"missing --" output)

(* The agent gets /dev/null, open for writing, in place of the signal pipe's
   read end, and no variable naming it. *)
let subcommand_hides_the_signal_pipe () =
  let r, w = Unix.pipe ~cloexec:true () in
  Fun.protect ~finally:(fun () ->
      Unix.close r;
      Unix.close w)
  @@ fun () ->
  let status, output =
    subcommand ~signals:r
      [
        "--";
        "/bin/sh";
        "-c";
        "echo x >&4 && echo writable; echo ${BACTRIAN_SANDBOX_SIGNAL_FD-unset}";
      ]
  in
  Alcotest.check status_t "status" (`Exited 0) status;
  Alcotest.(check string) "descriptor 4" "writable\nunset\n" output

let sandbox_user args = run_bactrian ~name:"__sandbox-user" args

let sandbox_user_rejects_bad_arguments () =
  List.iter
    (fun args ->
      let status, output = sandbox_user args in
      Alcotest.check status_t (String.concat " " args) (`Exited 64) status;
      Alcotest.(check bool)
        "says why" true
        (contains ~needle:"bactrian __sandbox-user: " output))
    [ [ "1:1"; "/bin/true" ]; [ "x:1"; "--"; "/bin/true" ] ]

(* Outside a sandbox's user namespace no user may map root of the one it
   leaves, and macOS has no user namespaces, so the agent never starts. *)
let sandbox_user_needs_a_namespace () =
  if Unix.geteuid () = 0 then Alcotest.skip ();
  let dir = Filename.temp_dir "bactrian-sbx" "" in
  Fun.protect ~finally:(fun () -> remove_tree dir) @@ fun () ->
  let ran = Filename.concat dir "ran" in
  let status, output = sandbox_user [ "1:1"; "--"; "/usr/bin/touch"; ran ] in
  Alcotest.check status_t "status" (`Exited 69) status;
  Alcotest.(check bool)
    "says why" true
    (contains ~needle:"bactrian __sandbox-user: " output);
  Alcotest.(check bool) "the agent did not start" false (Sys.file_exists ran)

(* The agent cannot add lines to the violation log: its descriptor 3 is
   /dev/null, while a relay failure still reaches the log. *)
let subcommand_hides_the_log () =
  let dir = Filename.temp_dir "bactrian-sbx" "" in
  Fun.protect ~finally:(fun () -> remove_tree dir) @@ fun () ->
  let log = Filename.concat dir "violations.log" in
  let missing = Filename.concat dir "missing.sock" in
  let status, output =
    subcommand ~log
      [
        "47913=" ^ missing;
        "--";
        "/bin/sh";
        "-c";
        "echo forged >&3; echo $?; python3 -c 'import socket; \
         socket.create_connection((\"127.0.0.1\", 47913)).recv(1)'";
      ]
  in
  Alcotest.check status_t "status" (`Exited 0) status;
  Alcotest.(check string) "the agent's write went nowhere" "0\n" output;
  let lines =
    In_channel.with_open_bin log In_channel.input_all
    |> String.split_on_char '\n'
    |> List.filter (fun l -> not (String.equal l ""))
  in
  Alcotest.(check int) "one line" 1 (List.length lines);
  Alcotest.(check string)
    "the forwarder's line" "relay_failed"
    (Yojson.Safe.Util.to_string (field "kind" (List.hd lines)))

let () =
  Alcotest.run "sandbox_init"
    [
      ( "parse",
        [
          Alcotest.test_case "accepts" `Quick parses;
          Alcotest.test_case "rejects" `Quick rejects;
          Alcotest.test_case "__sandbox-user" `Quick parses_user;
        ] );
      ( "relay",
        [
          Alcotest.test_case "bytes both ways" `Quick echoes_both_ways;
          Alcotest.test_case "large payload" `Quick large_payload;
          Alcotest.test_case "half-close to server" `Quick half_close_to_server;
          Alcotest.test_case "half-close to client" `Quick half_close_to_client;
          Alcotest.test_case "several listeners" `Quick several_listeners;
          Alcotest.test_case "parallel connections" `Quick parallel_connections;
        ] );
      ( "failures",
        [
          Alcotest.test_case "unreachable socket is logged" `Quick
            unreachable_socket_is_logged;
          Alcotest.test_case "a failing connection is alone" `Quick
            failing_connection_is_alone;
          Alcotest.test_case "no log, no file" `Quick no_log_writes_nothing;
          Alcotest.test_case "the agent cannot write the log" `Quick
            subcommand_hides_the_log;
          Alcotest.test_case "port in use" `Quick port_in_use;
          Alcotest.test_case "agent not found" `Quick missing_agent;
        ] );
      ( "agent",
        [
          Alcotest.test_case "exit code" `Quick exit_code_is_propagated;
          Alcotest.test_case "signal death" `Quick signal_death_is_128_plus;
          Alcotest.test_case "SIGTERM is forwarded" `Quick sigterm_reaches_agent;
          Alcotest.test_case "SIGINT is forwarded" `Quick sigint_reaches_agent;
          Alcotest.test_case "SIGTERM through the signal pipe" `Quick
            signal_pipe_terminates_agent;
          Alcotest.test_case "the signal pipe's end asks nothing" `Quick
            closed_signal_pipe_asks_nothing;
          Alcotest.test_case "signal reaches the group" `Quick
            signal_reaches_whole_group;
          Alcotest.test_case "environment" `Quick environment_loses_log_variable;
          Alcotest.test_case "own process group" `Quick own_process_group;
          Alcotest.test_case "stdout is inherited" `Quick stdout_is_the_agents;
          Alcotest.test_case "listeners close with the agent" `Quick
            listeners_close_with_the_agent;
          Alcotest.test_case "ending the run kills the group" `Quick
            ending_the_run_kills_the_group;
        ] );
      ("dns", [ Alcotest.test_case "port 53 refused" `Quick dns_needs_port_53 ]);
      ( "subcommand",
        [
          Alcotest.test_case "runs the agent" `Quick subcommand_runs_the_agent;
          Alcotest.test_case "rejects bad arguments" `Quick
            subcommand_rejects_bad_arguments;
          Alcotest.test_case "the agent cannot read the signal pipe" `Quick
            subcommand_hides_the_signal_pipe;
          Alcotest.test_case "__sandbox-user rejects bad arguments" `Quick
            sandbox_user_rejects_bad_arguments;
          Alcotest.test_case "__sandbox-user needs a user namespace" `Quick
            sandbox_user_needs_a_namespace;
        ] );
    ]
