(* The MCP server's transport, job table, root confinement and tools. The
   tests run in a fresh directory of their own, which is the server's root. *)

open Bactrian_core
open Bactrian_app
open Bactrian_server
module Interrupt = Bactrian_runtime.Interrupt
module Divergences = Bactrian_runtime.Divergences

let canon text =
  match Canon.of_string text with
  | Ok c -> c
  | Error _ -> Alcotest.fail ("malformed JSON: " ^ text)

let canon_t =
  Alcotest.testable
    (fun f c -> Format.pp_print_string f (Canon.to_string c))
    Canon.equal

let of_yojson json =
  match Canon.of_yojson json with
  | Ok c -> c
  | Error _ -> Alcotest.fail "not canonical JSON"

let path keys c =
  List.fold_left (fun acc k -> Option.bind acc (Canon.member k)) (Some c) keys

let meta =
  {|"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}|}

let call_line id name =
  Printf.sprintf
    {|{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{%s,"name":"%s"}}|}
    id meta name

let cancel_line id =
  Printf.sprintf
    {|{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":%d}}|}
    id

(* Transport *)

let server : Mcp_dispatch.server =
  {
    name = "test";
    version = "1";
    instructions = "";
    tools = [ ("echo", canon "{}"); ("slow", canon "{}") ];
  }

let echo_or_hang name _ =
  match name with
  | "slow" -> Eio.Fiber.await_cancel ()
  | _ -> canon {|{"isError":false}|}

(* Every line written is a response; the ids of the results and errors. *)
let replies text =
  String.split_on_char '\n' text
  |> List.filter (fun l -> not (String.equal l ""))
  |> List.map (fun line ->
      let fail () =
        Alcotest.fail ("not a response to one of the test's ids: " ^ line)
      in
      let number : Mcp_msg.id -> int = function
        | Int_id n -> n
        | String_id _ -> fail ()
      in
      match Mcp_msg.decode line with
      | Ok (Response { id; _ }) -> `Result (number id)
      | Ok (Error_response { id = Some id; code; _ }) -> `Error (number id, code)
      | Ok (Error_response { id = None; code; _ }) -> `Error (0, code)
      | Ok (Request _ | Notification _) | Error _ -> fail ())

let reply_t =
  Alcotest.testable
    (fun f r ->
      match r with
      | `Result n -> Format.fprintf f "result %d" n
      | `Error (n, code) -> Format.fprintf f "error %d %d" n code)
    ( = )

let test_transport_eof () =
  Eio_main.run @@ fun _env ->
  let input =
    Eio.Flow.string_source
      (String.concat "\n"
         [
           Printf.sprintf
             {|{"jsonrpc":"2.0","id":1,"method":"server/discover","params":{%s}}|}
             meta;
           call_line 2 "echo";
           call_line 3 "slow";
           call_line 3 "slow";
           "";
           cancel_line 3;
           call_line 3 "slow";
           call_line 4 "echo";
           "not json";
         ]
      ^ "\n")
  in
  let out = Buffer.create 256 in
  let ended =
    Mcp_server.serve ~server ~call:echo_or_hang ~stop:(Interrupt.trigger ())
      ~log:ignore ~input ~output:(Eio.Flow.buffer_sink out)
  in
  Alcotest.(check bool)
    "ended by the input" true
    (match ended with Input_closed -> true | Stopped _ -> false);
  (* The parse error has no id, so it is checked on its own. *)
  let lines = String.split_on_char '\n' (Buffer.contents out) in
  Alcotest.(check bool)
    "a parse error" true
    (List.exists
       (fun l -> String.starts_with ~prefix:{|{"error":{"code":-32700|} l)
       lines);
  Alcotest.(check (list reply_t))
    "the cancelled calls get no reply, a duplicate id an error without it"
    [ `Error (0, -32600); `Result 1; `Result 2; `Result 4 ]
    (List.sort compare
    @@ replies
         (String.concat "\n"
            (List.filter
               (fun l ->
                 not (String.starts_with ~prefix:{|{"error":{"code":-32700|} l))
               lines)))

(* A line past the limit is answered and skipped, and the session goes on. *)
let test_transport_long_line () =
  Eio_main.run @@ fun _env ->
  let input =
    Eio.Flow.string_source
      (String.make ((16 * 1024 * 1024) + 10) 'x'
      ^ "\n" ^ call_line 1 "echo" ^ "\n")
  in
  let out = Buffer.create 256 in
  ignore
    (Mcp_server.serve ~server ~call:echo_or_hang ~stop:(Interrupt.trigger ())
       ~log:ignore ~input ~output:(Eio.Flow.buffer_sink out)
      : Mcp_server.ended);
  Alcotest.(check (list reply_t))
    "an error without an id, then the next request's reply"
    [ `Error (0, -32600); `Result 1 ]
    (replies (Buffer.contents out))

let test_transport_stop () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let out = Buffer.create 256 in
  let stop = Interrupt.trigger () in
  let ended =
    Eio.Fiber.pair
      (fun () ->
        Mcp_server.serve ~server ~call:echo_or_hang ~stop ~log:ignore ~input:r
          ~output:(Eio.Flow.buffer_sink out))
      (fun () ->
        Eio.Flow.copy_string
          (call_line 1 "slow" ^ "\n" ^ call_line 2 "echo" ^ "\n")
          w;
        while not (String.length (Buffer.contents out) > 0) do
          Eio.Fiber.yield ()
        done;
        Interrupt.fire stop Sys.sigterm)
    |> fst
  in
  Alcotest.(check bool)
    "stopped by the signal" true
    (match ended with Stopped s -> s = Sys.sigterm | Input_closed -> false);
  Alcotest.(check (list reply_t))
    "the call in flight was dropped"
    [ `Result 2 ]
    (replies (Buffer.contents out))

(* Jobs *)

let job_state jobs id =
  match Jobs.find jobs id with
  | Some job -> Jobs.state job
  | None -> Alcotest.fail ("no job " ^ id)

let job_status jobs id =
  match Jobs.find jobs id with
  | Some job -> Jobs.status job
  | None -> Alcotest.fail ("no job " ^ id)

let state_t =
  Alcotest.testable
    (fun f s -> Format.pp_print_string f (Jobs.state_name s))
    (fun a b -> String.equal (Jobs.state_name a) (Jobs.state_name b))

let one ~finished = { Jobs.done_ = (if finished then 1 else 0); total = Some 1 }

(* A command that runs until stopped, as Run_command does on SIGINT. *)
let until_stopped ~during stop =
  match
    Interrupt.run ~stop (fun () ->
        during ();
        Eio.Fiber.await_cancel ())
  with
  | Error signal ->
      Error (Command_error.Interrupted { signal; experiment_id = Some "exp-1" })
  | Ok () -> Ok (`Null, 0)

let test_jobs () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  Eio.Switch.run @@ fun sw ->
  let jobs = Jobs.create ~sw () in
  let ok =
    Jobs.start jobs ~kind:"run" ~progress:one (fun _ ->
        Ok (`Assoc [ ("a", `Int 1) ], 1))
  in
  let failed =
    Jobs.start jobs ~kind:"record" ~progress:one (fun _ ->
        Error (Command_error.Usage "bad"))
  in
  let broken =
    Jobs.start jobs ~kind:"replay" ~progress:one (fun _ ->
        ignore
          (Eio.Path.load Eio.Path.(Eio.Stdenv.cwd env / "no-such-file")
            : string);
        Ok (`Null, 0))
  in
  let running =
    Jobs.start jobs ~kind:"run" ~progress:one (until_stopped ~during:ignore)
  in
  Alcotest.(check bool)
    "distinct IDs" true
    (List.length (List.sort_uniq String.compare [ ok; failed; broken; running ])
    = 4);
  Alcotest.(check bool)
    "IDs name jobs" true
    (String.starts_with ~prefix:"job-" ok);
  Alcotest.check canon_t "succeeded"
    (canon
       (Printf.sprintf
          {|{"job_id":"%s","kind":"run","state":"succeeded","progress":{"done":1,"total":1},"exit":1,"result":{"a":1}}|}
          ok))
    (job_status jobs ok);
  Alcotest.check canon_t "failed"
    (canon
       (Printf.sprintf
          {|{"job_id":"%s","kind":"record","state":"failed","progress":{"done":1,"total":1},"exit":64,"error":{"error":"bad","exit":64}}|}
          failed))
    (job_status jobs failed);
  (* It ignores its trigger, so cancelling it only waits for its end. *)
  Option.iter Jobs.cancel (Jobs.find jobs broken);
  Alcotest.check state_t "an IO error fails the job" Failed
    (job_state jobs broken);
  Alcotest.(check (option int))
    "as an internal error" (Some 70)
    (Option.bind (path [ "exit" ] (job_status jobs broken)) Canon.to_int_opt);
  Alcotest.check canon_t "running"
    (canon
       (Printf.sprintf
          {|{"job_id":"%s","kind":"run","state":"running","progress":{"done":0,"total":1}}|}
          running))
    (job_status jobs running);
  Option.iter Jobs.cancel (Jobs.find jobs running);
  Alcotest.check canon_t "cancelled, with the ID to resume"
    (canon
       (Printf.sprintf
          {|{"job_id":"%s","kind":"run","state":"cancelled","progress":{"done":1,"total":1},"exit":130,"error":{"error":"interrupted by SIGINT","exit":130,"experiment_id":"exp-1"}}|}
          running))
    (job_status jobs running);
  Option.iter Jobs.cancel (Jobs.find jobs ok);
  Alcotest.check state_t "cancelling a finished job changes nothing" Succeeded
    (job_state jobs ok);
  Alcotest.(check bool)
    "unknown job" true
    (Option.is_none (Jobs.find jobs "job-0"))

(* A job that raises fails alone, and finished jobs expire. *)
let test_jobs_lifetime () =
  Eio_main.run @@ fun _env ->
  Mirage_crypto_rng_unix.use_default ();
  Eio.Switch.run @@ fun sw ->
  let clock = ref 0. in
  let logged = ref [] in
  let jobs =
    Jobs.create
      ~now:(fun () -> !clock)
      ~log:(fun m -> logged := m :: !logged)
      ~sw ()
  in
  let finished () =
    Jobs.start jobs ~kind:"run" ~progress:one (fun _ -> Ok (`Null, 0))
  in
  let broken =
    Jobs.start jobs ~kind:"run" ~progress:one (fun _ -> raise Not_found)
  in
  Alcotest.check state_t "a job that raises fails" Failed
    (job_state jobs broken);
  Alcotest.(check bool) "and is logged" true (List.length !logged = 1);
  let old = finished () in
  clock := 1800.;
  let recent = finished () in
  clock := 3601.;
  Alcotest.(check bool)
    "an hour after it ended, a job is gone" true
    (Option.is_none (Jobs.find jobs old));
  Alcotest.(check bool)
    "a newer one is still there" true
    (Option.is_some (Jobs.find jobs recent));
  let many = List.init 100 (fun _ -> finished ()) in
  Alcotest.(check bool)
    "past a hundred, the oldest goes" true
    (Option.is_none (Jobs.find jobs recent));
  Alcotest.(check bool)
    "the newest hundred stay" true
    (List.for_all (fun id -> Option.is_some (Jobs.find jobs id)) many)

let test_jobs_cancel_all () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  let pid = ref None in
  Eio.Switch.run @@ fun sw ->
  let jobs = Jobs.create ~sw () in
  let agent =
    Jobs.start jobs ~kind:"run" ~progress:one (fun stop ->
        Eio.Switch.run @@ fun psw ->
        let child =
          Eio.Process.spawn ~sw:psw
            (Eio.Stdenv.process_mgr env)
            [ "sleep"; "60" ]
        in
        until_stopped
          ~during:(fun () -> pid := Some (Eio.Process.pid child))
          stop)
  in
  let other =
    Jobs.start jobs ~kind:"run" ~progress:one (until_stopped ~during:ignore)
  in
  Jobs.cancel_all jobs;
  Alcotest.check state_t "first" Cancelled (job_state jobs agent);
  Alcotest.check state_t "second" Cancelled (job_state jobs other);
  match !pid with
  | None -> Alcotest.fail "the child never started"
  | Some pid ->
      Alcotest.(check bool)
        "its child is gone" true
        (match Unix.kill pid 0 with
        | () -> false
        | exception Unix.Unix_error (ESRCH, _, _) -> true)

(* Root confinement *)

let root () =
  match Confine.create (Sys.getcwd ()) with
  | Ok r -> r
  | Error m -> Alcotest.fail m

let accepted r p = Result.is_ok (Confine.path r ~field:"f" p)

let test_confine () =
  let r = root () in
  Out_channel.with_open_text "inside.yaml" (fun oc -> output_string oc "x");
  Sys.mkdir "sub" 0o755;
  Unix.symlink "/etc" "out-link";
  Unix.symlink "inside.yaml" "in-link";
  Unix.symlink "/no/such/target" "dangling";
  List.iter
    (fun (p, expected) -> Alcotest.(check bool) p expected (accepted r p))
    [
      ("inside.yaml", true);
      ("./sub/new.html", true);
      ("sub/../inside.yaml", true);
      (Filename.concat (Confine.root r) "inside.yaml", true);
      ("in-link", true);
      ("new/dir/file", true);
      ("..", false);
      ("../x", false);
      ("/etc/passwd", false);
      ("out-link/passwd", false);
      ("out-link", false);
      ("dangling", false);
      ("new/../../x", false);
      ("", false);
      ("a\000b", false);
    ];
  List.iter
    (fun (id, expected) ->
      Alcotest.(check bool)
        id expected
        (Result.is_ok (Confine.id ~field:"f" id)))
    [
      ("run-1", true); ("", false); (".", false); ("..", false); ("a/b", false);
    ];
  Alcotest.(check (result string string))
    "given back relative to the root" (Ok "inside.yaml")
    (Confine.path r ~field:"f" (Filename.concat (Confine.root r) "inside.yaml"));
  Alcotest.(check (result string string))
    "the root itself" (Ok ".")
    (Confine.path r ~field:"f" "sub/..");
  Alcotest.(check bool)
    "a run directory inside" true
    (Result.is_ok (Confine.run r ~field:"run" ".bactrian/runs/r1"));
  Alcotest.(check bool)
    "a run directory outside" false
    (Result.is_ok (Confine.run r ~field:"run" "/tmp/r1"));
  match Confine.path r ~field:"output" "/etc/x" with
  | Ok _ -> Alcotest.fail "accepted"
  | Error m ->
      Alcotest.(check bool)
        "the error names the field" true
        (String.starts_with ~prefix:"output: " m)

(* Tools, called as the server calls them *)

let with_context ?(policy = Policy.default) f =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  Eio.Switch.run @@ fun sw ->
  let context : Mcp_tools.context =
    {
      env;
      root = root ();
      policy;
      jobs = Jobs.create ~sw ();
      bactrian_exe = Sys.executable_name;
      on_progress = ignore;
    }
  in
  f context (fun name args ->
      Mcp_tools.call context name (Option.map canon args))

let with_tools ?policy f =
  with_context ?policy (fun context call -> f context.Mcp_tools.env call)

let mentions needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || at (i + 1))
  in
  at 0

let is_error result =
  Option.value ~default:false
    (Option.bind (Canon.member "isError" result) Canon.to_bool_opt)

let error_text result =
  Option.bind (path [ "structuredContent"; "error" ] result) Canon.to_string_opt

let check_refused what expected result =
  Alcotest.(check bool) (what ^ " is a tool error") true (is_error result);
  Alcotest.(check (option string)) what (Some expected) (error_text result)

let test_definitions () =
  Alcotest.(check (list string))
    "one tool per command, in a fixed order"
    [
      "run_start";
      "record_start";
      "replay_start";
      "job_status";
      "job_cancel";
      "compare";
      "plan";
      "report";
      "check";
      "doctor";
      "divergences";
    ]
    (List.map fst Mcp_tools.definitions);
  List.iter
    (fun (name, d) ->
      Alcotest.(check (option bool))
        (name ^ " closes its schema")
        (Some false)
        (Option.bind
           (path [ "inputSchema"; "additionalProperties" ] d)
           Canon.to_bool_opt);
      List.iter
        (fun hint ->
          Alcotest.(check bool)
            (name ^ " " ^ hint)
            true
            (Option.is_some (path [ "annotations"; hint ] d)))
        [ "readOnlyHint"; "destructiveHint"; "idempotentHint"; "openWorldHint" ])
    Mcp_tools.definitions;
  Alcotest.(check (option canon_t))
    "check runs commands, so it is neither read-only nor closed-world"
    (Some
       (canon
          {|{"readOnlyHint":false,"destructiveHint":true,"idempotentHint":false,"openWorldHint":true}|}))
    (path [ "annotations" ] (List.assoc "check" Mcp_tools.definitions));
  Alcotest.(check (option canon_t))
    "required arguments"
    (Some (canon {|["experiment"]|}))
    (path
       [ "inputSchema"; "required" ]
       (List.assoc "run_start" Mcp_tools.definitions))

let test_arguments () =
  with_tools @@ fun _env call ->
  List.iter
    (fun (args, expected) ->
      check_refused args expected (call "plan" (Some args)))
    [
      ("{}", "missing argument threshold");
      ({|{"threshold":0.1,"zzz":1}|}, "unknown argument zzz");
      ({|{"threshold":"x"}|}, "argument threshold: must be a number");
      ({|{"threshold":0.1,"effect":"x"}|}, "argument effect: must be a number");
      ({|{"threshold":0.1,"seed":1.5}|}, "argument seed: must be an integer");
      ( {|{"threshold":0.1,"population":"all"}|},
        "argument population: must be one of suite, scenarios" );
      ( {|{"threshold":0.1,"baseline_rates":["x"]}|},
        "argument baseline_rates: must be an array of numbers" );
      ( {|{"threshold":0.1,"baseline_rates":[0.5],"scenarios":4611686018427387903}|},
        "--scenarios must be at most 10000" );
      ( {|{"threshold":0.1,"baseline_rates":[0.5],"scenarios":20,"simulations":20000,"max_repeats":2000}|},
        "scenarios x --simulations x --max-repeats is 800000000, more than the \
         100000000 a plan may simulate: lower --simulations or --max-repeats" );
      ("[]", "arguments: must be an object");
    ];
  check_refused "no arguments" "missing argument experiment"
    (call "run_start" None);
  check_refused "an empty agent"
    "argument agent: must be an array of at least one string"
    (call "record_start" (Some {|{"agent":[],"turns":["hi"]}|}));
  check_refused "a run ID that climbs"
    {|run: ".." is not an ID (one name, without '/' or '..')|}
    (call "divergences" (Some {|{"run":".."}|}));
  check_refused "a scenario outside"
    {|scenarios: "/etc/hosts" is outside the server's working directory|}
    (call "check" (Some {|{"scenarios":["/etc/hosts"]}|}));
  check_refused "a report outside"
    {|output: "../r.html" is outside the server's working directory|}
    (call "report" (Some {|{"experiment_id":"e","output":"../r.html"}|}));
  check_refused "an agent cwd outside"
    {|cwd: "/" is outside the server's working directory|}
    (call "replay_start" (Some {|{"run":"r","agent":["true"],"cwd":"/"}|}));
  check_refused "an unknown job"
    {|argument job_id: no job "job-0" (a finished job is kept for an hour, and only the newest 100)|}
    (call "job_status" (Some {|{"job_id":"job-0"}|}))

(* The tool's JSON is the command's [--json] output, as text and as a value. *)
let check_same what expected result =
  Alcotest.(check bool) (what ^ " succeeded") false (is_error result);
  Alcotest.(check (option canon_t))
    (what ^ " value")
    (Some (of_yojson expected))
    (path [ "structuredContent" ] result);
  Alcotest.(check (option string))
    (what ^ " text")
    (Some (Yojson.Safe.to_string expected))
    (Option.bind
       (Option.bind
          (Option.bind (Canon.member "content" result) Canon.to_list_opt)
          (function [ item ] -> Some item | [] | _ :: _ :: _ -> None))
       (Canon.str_member "text"))

let test_same_as_commands () =
  with_tools @@ fun env call ->
  let request : Plan_command.request =
    {
      rates = [ 0.8 ];
      scenarios = None;
      from = None;
      variant = None;
      effect_ = None;
      threshold = 0.1;
      population = `Suite;
      between_sd = None;
      gate_metrics = Plan_command.default_gate_metrics;
      power = Plan_command.default_power;
      confidence = Plan_command.default_confidence;
      max_repeats = Plan_command.default_max_repeats;
      simulations = 20;
      seed = Plan_command.default_seed;
    }
  in
  (match Plan_command.run ~env request with
  | Error e -> Alcotest.fail (Command_error.message e)
  | Ok plan ->
      check_same "plan"
        (Plan_command.to_json plan)
        (call "plan"
           (Some {|{"threshold":0.1,"baseline_rates":[0.8],"simulations":20}|})));
  let divergence : Divergences.t =
    {
      kind = Missing_request;
      seq = Some 3;
      similarity = Some 0.5;
      changes = [];
    }
  in
  let dir = Filename.concat ".bactrian" (Filename.concat "runs" "r1") in
  List.iter
    (fun d -> if not (Sys.file_exists d) then Sys.mkdir d 0o755)
    [ ".bactrian"; Filename.dirname dir; dir ];
  Out_channel.with_open_text (Filename.concat dir "divergences.json") (fun oc ->
      output_string oc
        (Yojson.Safe.to_string (Divergences.file_json [ divergence ])));
  check_same "divergences"
    (Divergences_command.to_json
       { run_id = "r1"; divergences = [ divergence ] })
    (call "divergences" (Some {|{"run":"r1"}|}));
  check_same "divergences of a run directory"
    (Divergences_command.to_json
       { run_id = "r1"; divergences = [ divergence ] })
    (call "divergences" (Some {|{"run":".bactrian/runs/r1/"}|}));
  check_refused "no such run" {|no run "r2"|}
    (call "divergences" (Some {|{"run":"r2"}|}));
  Sys.mkdir "elsewhere" 0o755;
  Sys.mkdir "elsewhere/r1" 0o755;
  check_refused "a directory that is not the run's"
    {|"elsewhere/r1" is not a run directory under .bactrian/runs|}
    (call "divergences" (Some {|{"run":"elsewhere/r1"}|}))

(* A plan that takes minutes runs off the event loop: the server answers
   other requests meanwhile, a cancellation stops it, and the end of input
   ends the server with another one in flight. The timeouts only bound a
   failure. *)
let test_plan_off_loop () =
  with_context @@ fun context _ ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock context.env in
  let r, w = Eio_unix.pipe sw in
  let out = Buffer.create 256 in
  let send line = Eio.Flow.copy_string (line ^ "\n") w in
  let replied id = List.mem (`Result id) (replies (Buffer.contents out)) in
  let await_reply id =
    Eio.Time.with_timeout_exn clock 30. (fun () ->
        while not (replied id) do
          Eio.Time.sleep clock 0.01
        done)
  in
  let plan id =
    Printf.sprintf
      {|{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{%s,"name":"plan","arguments":{"threshold":0.05,"baseline_rates":[0.5],"scenarios":20,"simulations":20000,"max_repeats":200}}}|}
      id meta
  in
  let list id =
    Printf.sprintf
      {|{"jsonrpc":"2.0","id":%d,"method":"tools/list","params":{%s}}|} id meta
  in
  let ended =
    Eio.Fiber.pair
      (fun () ->
        Mcp_server.serve
          ~server:{ server with tools = Mcp_tools.definitions }
          ~call:(Mcp_tools.call context) ~stop:(Interrupt.trigger ())
          ~log:ignore ~input:r ~output:(Eio.Flow.buffer_sink out))
      (fun () ->
        send (plan 1);
        send (list 2);
        await_reply 2;
        send (cancel_line 1);
        send (plan 3);
        send (list 4);
        await_reply 4;
        Eio.Flow.close w)
    |> fst
  in
  Alcotest.(check bool)
    "ended by the input" true
    (match ended with Input_closed -> true | Stopped _ -> false);
  Alcotest.(check (list reply_t))
    "only the lists were answered"
    [ `Result 2; `Result 4 ]
    (List.sort compare (replies (Buffer.contents out)))

let write file text =
  let rec mkdirs d =
    if not (Sys.file_exists d) then (
      mkdirs (Filename.dirname d);
      Sys.mkdir d 0o755)
  in
  mkdirs (Filename.dirname file);
  Out_channel.with_open_text file (fun oc -> output_string oc text)

let structured result =
  Option.value ~default:Canon.null (Canon.member "structuredContent" result)

(* Polls a job until it has finished. *)
let finished env call result =
  match
    Option.bind (path [ "job_id" ] (structured result)) Canon.to_string_opt
  with
  | None -> Alcotest.fail ("no job: " ^ Canon.to_string result)
  | Some id ->
      let rec poll () =
        let status =
          structured
            (call "job_status" (Some (Printf.sprintf {|{"job_id":%S}|} id)))
        in
        match Option.bind (path [ "state" ] status) Canon.to_string_opt with
        | Some "running" ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
            poll ()
        | Some _ | None -> status
      in
      poll ()

(* What experiment and scenario files name stays inside the root too, and an
   error never quotes a file it could not use. *)
let test_named_paths () =
  with_tools @@ fun env call ->
  let secret = "sk-proj-SENTINEL" in
  let outside =
    Filename.concat (Filename.dirname (Sys.getcwd ())) "outside-secret.env"
  in
  write outside ("OPENAI_API_KEY=" ^ secret ^ "\n");
  Fun.protect ~finally:(fun () -> Sys.remove outside) @@ fun () ->
  write "inside.env" ("OPENAI_API_KEY=" ^ secret ^ "\n");
  write "leak/s.json"
    (Printf.sprintf
       {|{"version": 1, "id": "s", "turns": [{"user": "hi"}],
          "assertions": [{"max_steps": 2}],
          "golden": {"pass": [%S, "../../outside-secret.env", "../inside.env"],
                     "fail": ["../inside.env"]}}|}
       outside);
  let result = call "check" (Some {|{"scenarios":["leak/s.json"]}|}) in
  let text = Canon.to_string result in
  Alcotest.(check bool) "no secret in the result" false (mentions secret text);
  Alcotest.(check bool)
    "files outside cannot be read" true
    (mentions "cannot read the trace" text);
  Alcotest.(check bool)
    "a file inside is named by line, not quoted" true
    (mentions "line 1 is not JSON" text);
  let experiment ?(extra = "") scenarios =
    Printf.sprintf
      {|{"version": 1, "name": "x", "agent": {"protocol": "command", "command": ["true"]},
         "scenarios": [%S], "variants": {"a": {}}, "repeats": 1,
         "caps_usd": {"openai": 1}%s}|}
      scenarios extra
  in
  write "exp/scen/s.json"
    {|{"version": 1, "id": "s", "turns": [{"user": "hi"}]}|};
  let refused name expected =
    let status =
      finished env call
        (call "run_start"
           (Some (Printf.sprintf {|{"experiment":"exp/%s.json"}|} name)))
    in
    let text = Canon.to_string status in
    Alcotest.(check bool)
      (name ^ " failed") true
      (mentions {|"state":"failed"|} text);
    Alcotest.(check bool)
      (Printf.sprintf "%s: %s in %s" name expected text)
      true (mentions expected text);
    Alcotest.(check bool)
      (name ^ " quotes no secret")
      false (mentions secret text)
  in
  write "exp/glob.json" (experiment "../../*.env");
  refused "glob" "scenarios[0]";
  write "exp/pricing.json"
    (experiment ~extra:{|, "pricing": "../../outside-secret.env"|} "scen/*.json");
  refused "pricing" "pricing";
  write "exp/write.json"
    (experiment
       ~extra:{|, "sandbox": {"mode": "strict", "allow_write": ["/tmp"]}|}
       "scen/*.json");
  refused "write" "sandbox.allow_write";
  write "exp/s2/s.json"
    {|{"version": 1, "id": "s2", "turns": [{"user": "hi"}], "fixtures": "../../../"}|};
  write "exp/fixtures.json" (experiment "s2/*.json");
  refused "fixtures" {|"path":"fixtures"|};
  check_refused "a report over bactrian's state"
    {|output: ".bactrian/ledger.jsonl" is inside .bactrian, which bactrian or git owns|}
    (call "report"
       (Some {|{"experiment_id":"x","output":".bactrian/ledger.jsonl"}|}))

(* Without the flags the server refuses whatever runs outside the sandbox,
   and each refusal names the flag that allows it. Checkers run inside it in
   strict mode, so only a mode that can leave them outside needs a flag. *)
let test_policy () =
  let names flag result = mentions flag (Canon.to_string result) in
  let refused_for flag what result =
    Alcotest.(check bool) (what ^ " refused") true (is_error result);
    Alcotest.(check bool) (what ^ " names " ^ flag) true (names flag result)
  in
  ( with_context @@ fun context call ->
    write "pol/s.json"
      {|{"version": 1, "id": "s", "turns": [{"user": "hi"}],
        "checkers": [{"command": ["true"]}], "golden": {"pass": []}}|};
    List.iter
      (fun (tool, args, flag) ->
        refused_for flag (tool ^ " " ^ args) (call tool (Some args)))
      [
        ( "run_start",
          {|{"experiment":"pol/s.json","sandbox":"off"}|},
          "--allow-unsandboxed" );
        ( "run_start",
          {|{"experiment":"pol/s.json","sandbox":"auto"}|},
          "--allow-unsandboxed" );
        ( "record_start",
          {|{"agent":["true"],"turns":["hi"],"sandbox":"off"}|},
          "--allow-unsandboxed" );
        ( "record_start",
          {|{"agent":["true"],"turns":["hi"],"sandbox_backend":"seatbelt"}|},
          "--allow-unsandboxed" );
        ( "replay_start",
          {|{"agent":["true"],"run":"r1","sandbox_backend":"seatbelt"}|},
          "--allow-unsandboxed" );
        ( "record_start",
          {|{"agent":["true"],"turns":["hi"],"env":["HOME"]}|},
          "--trust-project" );
        ( "record_start",
          {|{"agent":["true"],"turns":["hi"],"mcp_stdio":["x=[\"true\"]"]}|},
          "--trust-project" );
        ( "record_start",
          {|{"agent":["true"],"turns":["hi"],"mcp_http":["x=http://127.0.0.1:9/"]}|},
          "--trust-project" );
        ( "replay_start",
          {|{"agent":["true"],"run":"r1","env":["HOME"]}|},
          "--trust-project" );
        ( "check",
          {|{"scenarios":["pol/s.json"],"sandbox":"off"}|},
          "--allow-unsandboxed" );
      ];
    (match
       Bactrian_runtime.Golden.load
         ~files:(Eio.Stdenv.fs context.env)
         ~scenario_file:"pol/s.json" ()
     with
    | Ok s ->
        Alcotest.(check (result unit string))
          "checkers in strict pass" (Ok ())
          (Policy.scenario context.policy ~sandbox:`Strict s)
    | Error _ -> Alcotest.fail "pol/s.json does not load");
    write "pol2/s.json" {|{"version": 1, "id": "s", "turns": [{"user": "hi"}]}|};
    let experiment (scenarios, agent) =
      Printf.sprintf
        {|{"version": 1, "name": "x",
          "agent": {"protocol": "command", "command": ["true"]%s},
          "scenarios": [%S], "variants": {"a": {}}, "repeats": 1,
          "caps_usd": {"openai": 1}}|}
        agent scenarios
    in
    List.iter
      (fun (name, extra, flag) ->
        write (Printf.sprintf "polx/%s.json" name) (experiment extra);
        let status =
          finished context.env call
            (call "run_start"
               (Some (Printf.sprintf {|{"experiment":"polx/%s.json"}|} name)))
        in
        Alcotest.(check bool)
          (name ^ " names " ^ flag)
          true
          (mentions flag (Canon.to_string status)))
      [
        ( "passthrough",
          ("../pol2/*.json", {|, "env_passthrough": ["HOME"]|}),
          "--trust-project" );
      ];
    write "polx/seatbelt.json"
      {|{"version": 1, "name": "x",
        "agent": {"protocol": "command", "command": ["true"]},
        "scenarios": ["../pol2/*.json"], "variants": {"a": {}}, "repeats": 1,
        "caps_usd": {"openai": 1},
        "sandbox": {"mode": "strict", "backend": "seatbelt"}}|};
    let status =
      finished context.env call
        (call "run_start" (Some {|{"experiment":"polx/seatbelt.json"}|}))
    in
    Alcotest.(check bool)
      "an experiment's backend seatbelt names --allow-unsandboxed" true
      (mentions "--allow-unsandboxed" (Canon.to_string status));
    let instructions = (Mcp_server.server context.policy).instructions in
    Alcotest.(check bool)
      "the instructions state the policy" true
      (mentions "--allow-unsandboxed" instructions
      && mentions "--trust-project" instructions) );
  (* Allowed outside the sandbox, checkers in auto or off still need the
     project to be trusted. *)
  ( with_context ~policy:{ Policy.default with allow_unsandboxed = true }
  @@ fun context call ->
    List.iter
      (fun mode ->
        refused_for "--trust-project" ("check in " ^ mode)
          (call "check"
             (Some
                (Printf.sprintf {|{"scenarios":["pol/s.json"],"sandbox":%S}|}
                   mode))))
      [ "auto"; "off" ];
    write "polx/checkers.json"
      {|{"version": 1, "name": "x",
        "agent": {"protocol": "command", "command": ["true"]},
        "scenarios": ["../pol/*.json"], "variants": {"a": {}}, "repeats": 1,
        "caps_usd": {"openai": 1}}|};
    let status =
      finished context.env call
        (call "run_start"
           (Some {|{"experiment":"polx/checkers.json","sandbox":"off"}|}))
    in
    Alcotest.(check bool)
      "checkers in an experiment with sandbox off name --trust-project" true
      (mentions "--trust-project" (Canon.to_string status)) );
  with_context
    ~policy:
      { Policy.default with allow_unsandboxed = true; trust_project = true }
  @@ fun context _ ->
  Alcotest.(check (result (option string) string))
    "the flag leaves the mode to the caller" (Ok (Some "off"))
    (Result.map
       (Option.map Bactrian_runtime.Sandbox.mode_name)
       (Policy.sandbox context.policy (Some `Off)));
  Alcotest.(check (result unit string))
    "trusted, env passes" (Ok ())
    (Policy.env context.policy [ "HOME" ]);
  Alcotest.(check (result unit string))
    "allowed outside the sandbox, seatbelt passes" (Ok ())
    (Policy.backend context.policy ~field:"argument sandbox_backend" `Seatbelt)

(* A job beyond the limit is refused, and names the flag that raises it. *)
let test_job_limit () =
  with_context ~policy:{ Policy.default with max_jobs = 1 }
  @@ fun context call ->
  Out_channel.with_open_text "limit.yaml" (fun oc -> output_string oc "x");
  let first =
    Jobs.start context.jobs ~kind:"run" ~progress:one
      (until_stopped ~during:ignore)
  in
  let result = call "run_start" (Some {|{"experiment":"limit.yaml"}|}) in
  Alcotest.(check bool) "refused" true (is_error result);
  Alcotest.(check bool)
    "names --max-jobs" true
    (match error_text result with
    | Some m -> mentions "--max-jobs" m
    | None -> false);
  Option.iter Jobs.cancel (Jobs.find context.jobs first)

let () =
  let dir = Filename.temp_dir ~temp_dir:(Sys.getcwd ()) "mcp-server" "" in
  Sys.chdir dir;
  Alcotest.run "mcp_server"
    [
      ( "transport",
        [
          Alcotest.test_case "until the end of input" `Quick test_transport_eof;
          Alcotest.test_case "until stopped" `Quick test_transport_stop;
          Alcotest.test_case "a line too long" `Quick test_transport_long_line;
        ] );
      ( "jobs",
        [
          Alcotest.test_case "states" `Quick test_jobs;
          Alcotest.test_case "failure and expiry" `Quick test_jobs_lifetime;
          Alcotest.test_case "cancel all, children included" `Quick
            test_jobs_cancel_all;
        ] );
      ("root", [ Alcotest.test_case "confinement" `Quick test_confine ]);
      ( "tools",
        [
          Alcotest.test_case "definitions" `Quick test_definitions;
          Alcotest.test_case "argument errors" `Quick test_arguments;
          Alcotest.test_case "the commands' JSON" `Quick test_same_as_commands;
          Alcotest.test_case "the job limit" `Quick test_job_limit;
          Alcotest.test_case "paths that files name" `Quick test_named_paths;
          Alcotest.test_case "the policy and its flags" `Quick test_policy;
          Alcotest.test_case "a long plan off the event loop" `Quick
            test_plan_off_loop;
        ] );
    ]
