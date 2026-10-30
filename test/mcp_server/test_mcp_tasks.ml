(* The Tasks extension in the MCP server: jobs as tasks, the tools' task
   methods and their transport. *)

open Bactrian_core
open Bactrian_app
open Bactrian_server
open Server_support
module Interrupt = Bactrian_runtime.Interrupt

(* Task methods are answered in order, and a call from a client with Tasks may
   get a task. *)
let test_transport_tasks () =
  Eio_main.run @@ fun _env ->
  let tasks_meta =
    {|"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{"extensions":{"io.modelcontextprotocol/tasks":{}}}}|}
  in
  let line id method_ extra =
    Printf.sprintf {|{"jsonrpc":"2.0","id":%d,"method":"%s","params":{%s%s}}|}
      id method_ tasks_meta extra
  in
  let handlers : Mcp_dispatch.handlers =
    {
      call =
        (fun ~tasks _ _ ->
          if tasks then Task (canon {|{"taskId":"t1","status":"working"}|})
          else Complete (canon {|{"isError":false}|}));
      task =
        (fun _ id ->
          if String.equal id "t1" then
            Some (canon {|{"taskId":"t1","status":"working"}|})
          else None);
    }
  in
  let input =
    Eio.Flow.string_source
      (String.concat "\n"
         [
           line 1 "tasks/get" {|,"taskId":"t1"|};
           line 2 "tasks/cancel" {|,"taskId":"t2"|};
           line 3 "tools/call" {|,"name":"echo"|};
           call_line 4 "echo";
         ]
      ^ "\n")
  in
  let out = Buffer.create 256 in
  ignore
    (Mcp_server.serve ~server ~handlers ~stop:(Interrupt.trigger ()) ~log:ignore
       ~input ~output:(Eio.Flow.buffer_sink out)
      : Mcp_server.ended);
  Alcotest.(check (list reply_t))
    "the task, an unknown task, and both calls"
    [ `Error (2, -32602); `Result 1; `Result 3; `Result 4 ]
    (List.sort compare (replies (Buffer.contents out)));
  let result_type id =
    List.find_map
      (fun l ->
        match Canon.of_string l with
        | Ok c when Option.equal Int.equal (Canon.int_member "id" c) (Some id)
          ->
            Option.bind (Canon.member "result" c)
              (Canon.str_member "resultType")
        | Ok _ | Error _ -> None)
      (String.split_on_char '\n' (Buffer.contents out))
  in
  Alcotest.(check (option string)) "a task" (Some "task") (result_type 3);
  Alcotest.(check (option string))
    "without Tasks, a result" (Some "complete") (result_type 4)

(* Jobs as tasks of the Tasks extension, by a clock the test sets. *)
let test_jobs_as_tasks () =
  Eio_main.run @@ fun _env ->
  Mirage_crypto_rng_unix.use_default ();
  Eio.Switch.run @@ fun sw ->
  let clock = ref 86400. in
  let jobs = Jobs.create ~now:(fun () -> !clock) ~sw () in
  let started command = Jobs.start jobs ~kind:"run" ~progress:one command in
  let result ~is_error body =
    Canon.of_fields [ ("isError", Canon.bool is_error); ("body", body) ]
  in
  let task job = Mcp_tasks.detailed ~result job in
  let expect what fields job =
    Alcotest.check canon_t what
      (canon
         (Printf.sprintf
            {|{"taskId":"%s","createdAt":"1970-01-02T00:00:00Z","pollIntervalMs":1000,%s}|}
            (Jobs.id job) fields))
      (task job)
  in
  let running = started (until_stopped ~during:ignore) in
  expect "running"
    {|"status":"working","statusMessage":"0 of 1 runs finished","lastUpdatedAt":"1970-01-02T00:00:00Z","ttlMs":null|}
    running;
  Alcotest.check canon_t "a new task has no result" (Mcp_tasks.task running)
    (task running);
  clock := 86410.;
  Jobs.cancel running;
  expect "cancelled"
    {|"status":"cancelled","statusMessage":"interrupted by SIGINT","lastUpdatedAt":"1970-01-02T00:00:10Z","ttlMs":3610000|}
    running;
  clock := 86400.;
  expect "succeeded"
    {|"status":"completed","statusMessage":"finished with exit status 1","lastUpdatedAt":"1970-01-02T00:00:00Z","ttlMs":3600000,"result":{"isError":false,"body":{"a":1}}|}
    (started (fun _ -> Ok (`Assoc [ ("a", `Int 1) ], 1)));
  expect "failed: completed, with the tool error"
    {|"status":"completed","statusMessage":"bad","lastUpdatedAt":"1970-01-02T00:00:00Z","ttlMs":3600000,"result":{"isError":true,"body":{"error":"bad","exit":64}}|}
    (started (fun _ -> Error (Command_error.Usage "bad")))

(* With Tasks, a job tool gives its job as a task, which the task methods
   then read and cancel. *)
let test_tools_tasks () =
  with_context @@ fun context _ ->
  Out_channel.with_open_text "tasks.yaml" (fun oc -> output_string oc "x");
  let status task =
    Option.bind (Canon.member "status" task) Canon.to_string_opt
  in
  let id =
    match
      Mcp_tools.call context ~tasks:true "run_start"
        (Some (canon {|{"experiment":"tasks.yaml"}|}))
    with
    | Task t -> (
        match Canon.str_member "taskId" t with
        | Some id -> id
        | None -> Alcotest.fail "a task without an ID")
    | Complete r -> Alcotest.fail ("not a task: " ^ Canon.to_string r)
  in
  Option.iter Jobs.cancel (Jobs.find context.jobs id);
  (match Mcp_tools.task context Get_task id with
  | Some t ->
      Alcotest.(check bool)
        "a finished task" true
        (List.mem (status t) [ Some "completed"; Some "cancelled" ])
  | None -> Alcotest.fail "the task is gone");
  Alcotest.(check bool)
    "an unknown task" true
    (Option.is_none (Mcp_tools.task context Get_task "job-0"));
  let running =
    start context.jobs ~kind:"run" ~progress:one (until_stopped ~during:ignore)
  in
  Alcotest.(check (option string))
    "tasks/cancel acknowledges a running task" (Some "working")
    (Option.bind (Mcp_tools.task context Cancel_task running) status);
  Option.iter Jobs.cancel (Jobs.find context.jobs running);
  Alcotest.(check (option string))
    "which then stops" (Some "cancelled")
    (Option.bind (Mcp_tools.task context Get_task running) status);
  match
    Mcp_tools.call context ~tasks:true "job_status"
      (Some (canon {|{"job_id":"job-0"}|}))
  with
  | Complete _ -> ()
  | Task _ -> Alcotest.fail "only the job tools give tasks"

let () =
  Alcotest.run "mcp_tasks"
    [
      ( "tasks",
        [
          Alcotest.test_case "jobs as tasks" `Quick test_jobs_as_tasks;
          Alcotest.test_case "the tools' task methods" `Quick test_tools_tasks;
          Alcotest.test_case "over stdio" `Quick test_transport_tasks;
        ] );
    ]
