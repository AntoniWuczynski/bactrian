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
      | Ok (Request _ | Notification _ | Error_response { id = None; _ })
      | Error _ ->
          fail ())

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
    "the cancelled calls get no reply, a duplicate id an error"
    [ `Error (3, -32600); `Result 1; `Result 2; `Result 4 ]
    (List.sort compare
    @@ replies
         (String.concat "\n"
            (List.filter
               (fun l ->
                 not (String.starts_with ~prefix:{|{"error":{"code":-32700|} l))
               lines)))

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
  let jobs = Jobs.create ~sw in
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

let test_jobs_cancel_all () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  let pid = ref None in
  Eio.Switch.run @@ fun sw ->
  let jobs = Jobs.create ~sw in
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
  Alcotest.(check bool)
    "a run directory inside" true
    (Result.is_ok (Confine.run r ~field:"run" ".bactrian/runs/r1"));
  Alcotest.(check bool)
    "a run directory outside" false
    (Result.is_ok (Confine.run r ~field:"run" "/tmp/r1"));
  match Confine.path r ~field:"output" "/etc/x" with
  | Ok () -> Alcotest.fail "accepted"
  | Error m ->
      Alcotest.(check bool)
        "the error names the field" true
        (String.starts_with ~prefix:"output: " m)

(* Tools, called as the server calls them *)

let with_tools f =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  Eio.Switch.run @@ fun sw ->
  let context : Mcp_tools.context =
    {
      env;
      root = root ();
      jobs = Jobs.create ~sw;
      bactrian_exe = Sys.executable_name;
      on_progress = ignore;
    }
  in
  f env (fun name args -> Mcp_tools.call context name (Option.map canon args))

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
    {|argument job_id: no job "job-0" (jobs last until the server exits)|}
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
    (call "divergences" (Some {|{"run":"r2"}|}))

let () =
  let dir = Filename.temp_dir ~temp_dir:(Sys.getcwd ()) "mcp-server" "" in
  Sys.chdir dir;
  Alcotest.run "mcp_server"
    [
      ( "transport",
        [
          Alcotest.test_case "until the end of input" `Quick test_transport_eof;
          Alcotest.test_case "until stopped" `Quick test_transport_stop;
        ] );
      ( "jobs",
        [
          Alcotest.test_case "states" `Quick test_jobs;
          Alcotest.test_case "cancel all, children included" `Quick
            test_jobs_cancel_all;
        ] );
      ("root", [ Alcotest.test_case "confinement" `Quick test_confine ]);
      ( "tools",
        [
          Alcotest.test_case "definitions" `Quick test_definitions;
          Alcotest.test_case "argument errors" `Quick test_arguments;
          Alcotest.test_case "the commands' JSON" `Quick test_same_as_commands;
        ] );
    ]
