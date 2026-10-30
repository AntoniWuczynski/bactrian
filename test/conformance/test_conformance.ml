(* The conformance transcripts of examples/transcripts, replayed against the
   agent driver (design §5.1). The driver sends every ">" message of a
   transcript and must get the "<" messages back. The agent side is the
   transcript itself: a process that compares each line it reads with the
   transcript byte for byte (command mode), or a server that does the same with
   each request body and also checks the headers (HTTP mode). So the lines are
   the bytes the driver writes for these messages, not the conversation a run
   builds from a scenario. *)

open Bactrian_core
module D = Bactrian_runtime.Agent_driver

let transcript name =
  match
    Transcript.load ("../../examples/transcripts/" ^ name ^ ".transcript")
  with
  | Ok exchanges -> exchanges
  | Error m -> Alcotest.failf "%s: %s" name m

let to_agent text =
  match Agent_protocol.decode_to_agent text with
  | Ok m -> m
  | Error e -> Alcotest.failf "%a: %s" Agent_protocol.pp_error e text

let from_agent text =
  match Agent_protocol.decode text with
  | Ok m -> Agent_protocol.encode_from_agent m
  | Error e -> Alcotest.failf "%a: %s" Agent_protocol.pp_error e text

let describe : D.error -> string = function
  | Spawn_failed s -> "spawn failed: " ^ s
  | Write_failed s -> "write failed: " ^ s
  | Protocol_violation { line; _ } -> "protocol violation: " ^ line
  | Line_too_long -> "line too long"
  | Turn_timeout -> "turn timeout"
  | Total_timeout -> "total timeout"
  | Agent_exited None -> "agent still running at end of output"
  | Agent_exited (Some (`Exited n)) -> Printf.sprintf "agent exited %d" n
  | Agent_exited (Some (`Signaled n)) -> Printf.sprintf "agent signalled %d" n
  | Http_error s -> "http error: " ^ s
  | Http_status { status; body } -> Printf.sprintf "http %d: %s" status body
  | No_message -> "no message"

let get what = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%s: %s" what (describe e)

(* The driver's side of one exchange: send the request, then read the replies
   the transcript lists, and no others. *)
let exchange driver (e : Transcript.exchange) =
  get "send" (D.send driver (to_agent e.request));
  List.iter
    (fun reply ->
      match D.recv driver with
      | Ok got ->
          Alcotest.(check string)
            "reply" (from_agent reply)
            (Agent_protocol.encode_from_agent got)
      | Error e -> Alcotest.failf "recv: %s" (describe e))
    e.replies

let test_messages_are_decodable () =
  List.iter
    (fun name ->
      List.iter
        (fun (e : Transcript.exchange) ->
          ignore (to_agent e.request : Agent_protocol.to_agent);
          List.iter (fun r -> ignore (from_agent r : string)) e.replies)
        (transcript name))
    [ "command"; "http" ]

(* Command mode *)

let with_dir fn =
  Eio_main.run @@ fun env ->
  let dir = Filename.temp_dir "conformance" "" in
  Fun.protect
    ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / dir))
    (fun () -> fn env dir)

let agent_exe = "../support/transcript/transcript_agent.exe"

let run_command env dir f =
  let fs = Eio.Stdenv.fs env in
  let config : D.config =
    {
      argv = [ agent_exe; "../../examples/transcripts/command.transcript" ];
      env = [];
      cwd = None;
      stderr_path = Eio.Path.(fs / dir / "stderr.txt");
      turn_timeout = 10.;
      total_timeout = 30.;
      exit_grace = 2.;
      kill_grace = 1.;
    }
  in
  D.with_agent
    ~process_mgr:(Eio.Stdenv.process_mgr env)
    ~clock:(Eio.Stdenv.clock env) config f

let stderr_text dir =
  In_channel.with_open_bin (dir ^ "/stderr.txt") In_channel.input_all

let test_command () =
  with_dir @@ fun env dir ->
  match
    run_command env dir (fun driver ->
        List.iter (exchange driver) (transcript "command"))
  with
  | Ok ((), `Exited 0) -> ()
  | Ok ((), _) ->
      Alcotest.failf "the agent did not exit 0: %s" (stderr_text dir)
  | Error e -> Alcotest.failf "spawn: %s" (describe e)

(* A driver that sent anything else would not pass: the agent of the transcript
   refuses it and exits, which is what makes the test above mean something. *)
let test_command_detects_a_difference () =
  with_dir @@ fun env dir ->
  let start =
    match transcript "command" with
    | start :: _ -> start
    | [] -> Alcotest.fail "empty transcript"
  in
  let wrong =
    Agent_protocol.Turn { turn = 7; content = "not in the transcript" }
  in
  match
    run_command env dir (fun driver ->
        exchange driver start;
        get "send" (D.send driver wrong);
        D.recv driver)
  with
  | Ok (Error e, _) ->
      Alcotest.(check bool)
        "the agent exited" true
        (String.starts_with ~prefix:"agent exited" (describe e));
      Alcotest.(check bool)
        "and said what it expected" true
        (String.starts_with ~prefix:"expected: " (stderr_text dir))
  | Ok (Ok _, _) ->
      Alcotest.fail "the agent answered a message not in the transcript"
  | Error e -> Alcotest.failf "spawn: %s" (describe e)

(* HTTP mode *)

let run_id = "run-1"
let namespace_header = "X-Run-Namespace"

type served = {
  mutable pending : Transcript.exchange list;
  mutable problems : string list;
}

let respond ?(status = `OK) body =
  Cohttp_eio.Server.respond_string ~status ~body ()

let header request name = Http.Header.get (Http.Request.headers request) name

(* The server answers each POST with the next exchange, and notes anything that
   differs instead of failing inside the handler. *)
let handler served _conn request body =
  let body =
    match Eio.Buf_read.parse ~max_size:max_int Eio.Buf_read.take_all body with
    | Ok s -> s
    | Error (`Msg _) -> ""
  in
  let problem fmt =
    Printf.ksprintf
      (fun m ->
        served.problems <- m :: served.problems;
        respond ~status:`Bad_request "[]")
      fmt
  in
  let expect_header name expected =
    match header request name with
    | Some v when String.equal v expected -> None
    | Some v -> Some (Printf.sprintf "%s is %S, expected %S" name v expected)
    | None -> Some (Printf.sprintf "%s is missing" name)
  in
  let header_problems =
    List.filter_map Fun.id
      [
        (if
           String.equal
             (Http.Method.to_string (Http.Request.meth request))
             "POST"
         then None
         else Some "the method is not POST");
        expect_header "content-type" "application/json";
        expect_header "x-bactrian-run" run_id;
        expect_header namespace_header run_id;
      ]
  in
  match (header_problems, served.pending) with
  | _ :: _, _ -> problem "%s" (String.concat "; " header_problems)
  | [], [] -> problem "a POST after the transcript: %s" body
  | [], e :: rest ->
      if String.equal body e.request then begin
        served.pending <- rest;
        respond ("[" ^ String.concat "," e.replies ^ "]")
      end
      else problem "expected: %s\ngot: %s" e.request body

(* Serves the transcript on a port of its own while [f] runs. *)
let with_transcript_server f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env and clock = Eio.Stdenv.clock env in
  let exchanges = transcript "http" in
  let served = { pending = exchanges; problems = [] } in
  let sock =
    Eio.Net.listen ~sw ~backlog:4 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr sock with `Tcp (_, p) -> p | `Unix _ -> 0
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Cohttp_eio.Server.run sock
        (Cohttp_eio.Server.make ~callback:(handler served) ())
        ~on_error:(fun exn ->
          prerr_endline ("transcript server: " ^ Printexc.to_string exn)));
  let config run_id : D.http_config =
    {
      url = Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/agent" port);
      namespace_header;
      run_id;
      turn_timeout = 10.;
      total_timeout = 30.;
    }
  in
  f ~net ~clock ~config ~exchanges served

let test_http () =
  with_transcript_server @@ fun ~net ~clock ~config ~exchanges served ->
  (* The server saw what differed, which says more than the driver's 400. *)
  Fun.protect
    ~finally:(fun () -> List.iter prerr_endline (List.rev served.problems))
    (fun () ->
      D.with_http_agent ~net ~clock (config run_id) (fun driver ->
          List.iter (exchange driver) exchanges));
  Alcotest.(check (list string))
    "nothing differed" [] (List.rev served.problems);
  Alcotest.(check int)
    "every exchange was served" 0
    (List.length served.pending)

(* A run ID that is not the one in the transcript's headers is refused. *)
let test_http_detects_a_difference () =
  with_transcript_server @@ fun ~net ~clock ~config ~exchanges served ->
  let first = List.hd exchanges in
  let sent =
    D.with_http_agent ~net ~clock (config "another-run") (fun driver ->
        D.send driver (to_agent first.request))
  in
  Alcotest.(check string)
    "refused with 400" "http 400: []"
    (match sent with Ok () -> "accepted" | Error e -> describe e);
  Alcotest.(check bool)
    "the header is named" true
    (List.exists
       (String.starts_with ~prefix:"x-bactrian-run is")
       served.problems)

let () =
  Mirage_crypto_rng_unix.use_default ();
  Alcotest.run "conformance"
    [
      ( "transcripts",
        [
          Alcotest.test_case "messages decode" `Quick
            test_messages_are_decodable;
          Alcotest.test_case "command mode" `Quick test_command;
          Alcotest.test_case "command mode detects a difference" `Quick
            test_command_detects_a_difference;
          Alcotest.test_case "http mode" `Quick test_http;
          Alcotest.test_case "http mode detects a difference" `Quick
            test_http_detects_a_difference;
        ] );
    ]
