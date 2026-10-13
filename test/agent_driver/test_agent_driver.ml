open Bactrian_core
module D = Bactrian_runtime.Agent_driver

let () = Mirage_crypto_rng_unix.use_default ()
let path_env = ("PATH", Sys.getenv "PATH")

let with_env fn =
  Eio_main.run @@ fun env ->
  let dir = Filename.temp_dir "agent_driver" "" in
  Fun.protect
    ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / dir))
    (fun () -> fn env dir)

let run_agent ?(env_vars = []) ?(turn_timeout = 5.) ?(total_timeout = 30.)
    ?(exit_grace = 0.3) ?(kill_grace = 0.3) ?(use_cwd = false) env dir argv f =
  let fs = Eio.Stdenv.fs env in
  let config : D.config =
    {
      argv;
      env = path_env :: env_vars;
      cwd = (if use_cwd then Some Eio.Path.(fs / dir) else None);
      stderr_path = Eio.Path.(fs / dir / "stderr.txt");
      turn_timeout;
      total_timeout;
      exit_grace;
      kill_grace;
    }
  in
  D.with_agent
    ~process_mgr:(Eio.Stdenv.process_mgr env)
    ~clock:(Eio.Stdenv.clock env) config f

let start =
  Agent_protocol.Start
    {
      run_id = "r";
      scenario = "s";
      variant = "v";
      sim_now = "2026-01-01T00:00:00Z";
      state_dir = "/tmp";
      env = None;
    }

let pp_error ppf (e : D.error) =
  match e with
  | Spawn_failed s -> Format.fprintf ppf "Spawn_failed %S" s
  | Write_failed s -> Format.fprintf ppf "Write_failed %S" s
  | Protocol_violation { line; error = _ } ->
      Format.fprintf ppf "Protocol_violation %S" line
  | Line_too_long -> Format.pp_print_string ppf "Line_too_long"
  | Turn_timeout -> Format.pp_print_string ppf "Turn_timeout"
  | Total_timeout -> Format.pp_print_string ppf "Total_timeout"
  | Agent_exited None -> Format.pp_print_string ppf "Agent_exited"
  | Agent_exited (Some (`Exited n)) ->
      Format.fprintf ppf "Agent_exited (Exited %d)" n
  | Agent_exited (Some (`Signaled n)) ->
      Format.fprintf ppf "Agent_exited (Signaled %d)" n
  | Http_error s -> Format.fprintf ppf "Http_error %S" s
  | Http_status { status; body } ->
      Format.fprintf ppf "Http_status %d %S" status body
  | No_message -> Format.pp_print_string ppf "No_message"

let show_msg : Agent_protocol.from_agent -> string = function
  | Reply { turn; content } -> Printf.sprintf "Reply %d %S" turn content
  | Event { turn; name; data = _ } -> Printf.sprintf "Event %d %s" turn name
  | Agent_error { turn; message } ->
      Printf.sprintf "Agent_error %d %S" turn message

let show r =
  match r with
  | Ok msg -> show_msg msg
  | Error e -> Format.asprintf "%a" pp_error e

let fail_error what e = Alcotest.failf "%s: %a" what pp_error e
let ok what = function Ok v -> v | Error e -> fail_error what e

let alive pid =
  match Unix.kill pid 0 with
  | () -> true
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false

let gone_within clock pid seconds =
  let rec poll n =
    if not (alive pid) then true
    else if n = 0 then false
    else (
      Eio.Time.sleep clock 0.05;
      poll (n - 1))
  in
  poll (int_of_float (seconds /. 0.05))

let python_agent =
  {|
import sys, json
for line in sys.stdin:
    m = json.loads(line)
    if m["type"] == "turn":
        print(json.dumps({"v": 1, "type": "reply", "turn": m["turn"], "content": "echo " + m["content"]}), flush=True)
    elif m["type"] == "end":
        break
|}

let test_two_turns () =
  with_env @@ fun env dir ->
  let turn t (d : D.t) content =
    ok "send" (D.send d (Agent_protocol.Turn { turn = t; content }));
    ok "recv" (D.recv d)
  in
  let (r1, r2), status =
    ok "run"
      (run_agent env dir [ "python3"; "-I"; "-c"; python_agent ] (fun d ->
           ok "send" (D.send d start);
           let r1 = turn 1 d "hello" in
           let r2 = turn 2 d "again" in
           ok "send" (D.send d Agent_protocol.End);
           (r1, r2)))
  in
  Alcotest.(check string) "turn 1" {|Reply 1 "echo hello"|} (show_msg r1);
  Alcotest.(check string) "turn 2" {|Reply 2 "echo again"|} (show_msg r2);
  Alcotest.(check bool) "clean exit" true (status = `Exited 0)

let test_garbage () =
  with_env @@ fun env dir ->
  let r, _ =
    ok "run"
      (run_agent env dir
         [ "sh"; "-c"; "echo this is not json; sleep 300" ]
         D.recv)
  in
  Alcotest.(check string)
    "offending line" {|Protocol_violation "this is not json"|} (show r)

let test_early_exit () =
  with_env @@ fun env dir ->
  let r, _ = ok "run" (run_agent env dir [ "sh"; "-c"; "exit 3" ] D.recv) in
  Alcotest.(check string) "exit seen" "Agent_exited (Exited 3)" (show r)

let test_timeouts () =
  with_env @@ fun env dir ->
  let silent = [ "sh"; "-c"; "sleep 300" ] in
  let r, _ = ok "run" (run_agent ~turn_timeout:0.3 env dir silent D.recv) in
  Alcotest.(check string) "turn" "Turn_timeout" (show r);
  let r, _ =
    ok "run"
      (run_agent ~turn_timeout:10. ~total_timeout:0.3 env dir silent D.recv)
  in
  Alcotest.(check string) "total" "Total_timeout" (show r)

(* The agent ignores TERM and end, and leaves a TERM-ignoring child behind, so only
   SIGKILL to the whole group removes both. *)
let stubborn =
  {|trap '' TERM; sleep 300 & echo $! > "$DIR/child.pid"; echo $$ > "$DIR/leader.pid"; while :; do sleep 1; done|}

let read_pid dir name =
  In_channel.with_open_text (Filename.concat dir name) (fun ic ->
      int_of_string (String.trim (input_line ic)))

let test_kills_group () =
  with_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let pids, _ =
    ok "run"
      (run_agent
         ~env_vars:[ ("DIR", dir) ]
         env dir [ "sh"; "-c"; stubborn ]
         (fun d ->
           let rec wait n =
             if
               Sys.file_exists (Filename.concat dir "leader.pid")
               && Sys.file_exists (Filename.concat dir "child.pid")
             then ()
             else if n = 0 then Alcotest.fail "agent never started"
             else (
               Eio.Time.sleep clock 0.05;
               wait (n - 1))
           in
           wait 100;
           Eio.Time.sleep clock 0.1;
           ok "send" (D.send d Agent_protocol.End);
           (Option.get (D.pid d), read_pid dir "child.pid")))
  in
  let leader, child = (fst pids, snd pids) in
  Alcotest.(check bool) "leader is gone" false (alive leader);
  Alcotest.(check bool) "child is gone" true (gone_within clock child 2.)

let test_stderr_env_cwd () =
  with_env @@ fun env dir ->
  let _ =
    ok "run"
      (run_agent
         ~env_vars:[ ("FOO", "bar") ]
         ~use_cwd:true env dir
         [ "sh"; "-c"; "echo \"err $FOO [$HOME]\" >&2; echo x > marker" ]
         (fun d -> ignore (D.recv d)))
  in
  let stderr_text =
    In_channel.with_open_text
      (Filename.concat dir "stderr.txt")
      In_channel.input_all
  in
  Alcotest.(check string)
    "stderr captured, env not inherited" "err bar []\n" stderr_text;
  Alcotest.(check bool)
    "cwd honoured" true
    (Sys.file_exists (Filename.concat dir "marker"))

let test_spawn_failure () =
  with_env @@ fun env dir ->
  let r = run_agent env dir [ "/nonexistent/agent" ] (fun _ -> ()) in
  let prefix =
    match r with
    | Ok _ -> "Ok"
    | Error e -> String.sub (Format.asprintf "%a" pp_error e) 0 13
  in
  Alcotest.(check string) "spawn error" "Spawn_failed " prefix

(* HTTP mode, against a scripted server *)

module F = Fake_upstream

let scripted_reply ?(status = 200) body : F.reply =
  {
    status;
    headers = [ ("content-type", "application/json") ];
    chunks = [ body ];
    ending = Complete;
  }

let http_config ?(turn_timeout = 5.) ?(total_timeout = 30.) url : D.http_config
    =
  {
    url = Uri.of_string url;
    namespace_header = "X-Agent-Run";
    run_id = "run-1";
    turn_timeout;
    total_timeout;
  }

(* Runs [f] against a server that answers with [replies], in order. *)
let with_script ?turn_timeout ?total_timeout replies f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let server =
    match F.start ~sw ~net (F.Script replies) with
    | Ok s -> s
    | Error e -> Alcotest.failf "fake server: %a" F.pp_error e
  in
  D.with_http_agent ~net ~clock:(Eio.Stdenv.clock env)
    (http_config ?turn_timeout ?total_timeout (F.base_url server ^ "/chat"))
    (f server)

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

(* The outcome of one POST, as text. *)
let sent d =
  match D.send d Agent_protocol.End with
  | Ok () -> "ok"
  | Error e -> Format.asprintf "%a" pp_error e

let reply_json turn content =
  Agent_protocol.(encode_array [ Reply { turn; content } ])

let test_http_two_steps () =
  let event =
    Agent_protocol.Event { turn = 1; name = "tool"; data = Canon.empty_object }
  in
  with_script
    [
      scripted_reply "[]";
      scripted_reply
        Agent_protocol.(
          encode_array [ event; Reply { turn = 1; content = "hi" } ]);
      scripted_reply "[]";
    ]
    (fun server d ->
      ok "start"
        (D.send d
           (Agent_protocol.Start
              {
                run_id = "run-1";
                scenario = "s";
                variant = "v";
                sim_now = "2026-01-01T00:00:00Z";
                state_dir = "/tmp";
                env = Some [ ("K", "v") ];
              }));
      ok "turn" (D.send d (Agent_protocol.Turn { turn = 1; content = "q" }));
      Alcotest.(check string)
        "event first" "Event 1 tool"
        (show_msg (ok "recv" (D.recv d)));
      Alcotest.(check string)
        "then the reply" {|Reply 1 "hi"|}
        (show_msg (ok "recv" (D.recv d)));
      Alcotest.(check string) "nothing more" "No_message" (show (D.recv d));
      ok "end" (D.send d Agent_protocol.End);
      let requests = F.requests server in
      Alcotest.(check (list string))
        "one POST per message" [ "POST"; "POST"; "POST" ]
        (List.map (fun (r : F.request) -> r.meth) requests);
      List.iter
        (fun (r : F.request) ->
          let header k = List.assoc_opt k r.headers in
          Alcotest.(check string) "path" "/chat" r.path;
          Alcotest.(check (option string))
            "content type" (Some "application/json") (header "content-type");
          Alcotest.(check (option string))
            "run header" (Some "run-1") (header "x-bactrian-run");
          Alcotest.(check (option string))
            "namespace header" (Some "run-1") (header "x-agent-run"))
        requests;
      Alcotest.(check bool)
        "the start message carries the env" true
        (contains (List.hd requests).body {|"env":{"K":"v"}|}))
  |> ignore

let test_http_status () =
  let body = String.make 3000 'x' in
  with_script
    [ scripted_reply ~status:500 body ]
    (fun _ d ->
      Alcotest.(check string)
        "status and the first 2 KiB of the body"
        (Printf.sprintf "Http_status 500 %S" (String.sub body 0 2048))
        (sent d))

let test_http_bad_bodies () =
  List.iter
    (fun body ->
      with_script
        [ scripted_reply body ]
        (fun _ d ->
          Alcotest.(check string)
            "the body is quoted"
            (Printf.sprintf "Protocol_violation %S" body)
            (sent d)))
    [
      "not json";
      {|{"v":1,"type":"reply","turn":1,"content":"x"}|};
      {|[{"v":1,"type":"bogus"}]|};
      reply_json 1 "ok" ^ " trailing";
    ]

(* A server that accepts connections and never answers. *)
let with_silent_server f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let sock =
    Eio.Net.listen ~sw ~backlog:4 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  match Eio.Net.listening_addr sock with
  | `Tcp (_, port) -> f env (Printf.sprintf "http://127.0.0.1:%d/a" port)
  | `Unix _ -> Alcotest.fail "not a TCP listener"

let test_http_timeouts () =
  with_silent_server @@ fun env url ->
  let send ?turn_timeout ?total_timeout () =
    D.with_http_agent ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
      (http_config ?turn_timeout ?total_timeout url)
      sent
  in
  Alcotest.(check string) "turn" "Turn_timeout" (send ~turn_timeout:0.3 ());
  Alcotest.(check string)
    "total" "Total_timeout"
    (send ~turn_timeout:10. ~total_timeout:0.3 ())

let test_http_unreachable_and_https () =
  Eio_main.run @@ fun env ->
  let port =
    Eio.Switch.run @@ fun sw ->
    match
      Eio.Net.listening_addr
        (Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true (Eio.Stdenv.net env)
           (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)))
    with
    | `Tcp (_, port) -> port
    | `Unix _ -> Alcotest.fail "not a TCP listener"
  in
  let send url =
    D.with_http_agent ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
      (http_config url) sent
  in
  Alcotest.(check bool)
    "unreachable" true
    (String.starts_with ~prefix:{|Http_error "agent unreachable|}
       (send (Printf.sprintf "http://127.0.0.1:%d/a" port)));
  Alcotest.(check string)
    "https needs a connector"
    {|Http_error "agent unreachable: https needs a TLS connector"|}
    (send "https://127.0.0.1:1/a")

let () =
  Alcotest.run "agent_driver"
    [
      ( "driver",
        [
          Alcotest.test_case "two turns" `Quick test_two_turns;
          Alcotest.test_case "garbage output" `Quick test_garbage;
          Alcotest.test_case "early exit" `Quick test_early_exit;
          Alcotest.test_case "timeouts" `Quick test_timeouts;
          Alcotest.test_case "kills the process group" `Quick test_kills_group;
          Alcotest.test_case "stderr, env, cwd" `Quick test_stderr_env_cwd;
          Alcotest.test_case "spawn failure" `Quick test_spawn_failure;
        ] );
      ( "http driver",
        [
          Alcotest.test_case "two steps" `Quick test_http_two_steps;
          Alcotest.test_case "non-2xx status" `Quick test_http_status;
          Alcotest.test_case "malformed bodies" `Quick test_http_bad_bodies;
          Alcotest.test_case "timeouts" `Quick test_http_timeouts;
          Alcotest.test_case "unreachable and https" `Quick
            test_http_unreachable_and_https;
        ] );
    ]
