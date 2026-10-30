(* Helpers shared by the MCP server's tests. *)

open Bactrian_core
open Bactrian_app
open Bactrian_server
module Interrupt = Bactrian_runtime.Interrupt

let canon text =
  match Canon.of_string text with
  | Ok c -> c
  | Error _ -> Alcotest.fail ("malformed JSON: " ^ text)

let canon_t =
  Alcotest.testable
    (fun f c -> Format.pp_print_string f (Canon.to_string c))
    Canon.equal

let path keys c =
  List.fold_left (fun acc k -> Option.bind acc (Canon.member k)) (Some c) keys

let meta =
  {|"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}|}

let call_line id name =
  Printf.sprintf
    {|{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{%s,"name":"%s"}}|}
    id meta name

let server : Mcp_dispatch.server =
  {
    name = "test";
    version = "1";
    instructions = "";
    tools = [ ("echo", canon "{}"); ("slow", canon "{}") ];
  }

type reply = [ `Result of int | `Error of int * int ]

(* Every line written is a response; the ids of the results and errors. *)
let replies text : reply list =
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

let reply_t : reply Alcotest.testable =
  Alcotest.testable
    (fun f (r : reply) ->
      match r with
      | `Result n -> Format.fprintf f "result %d" n
      | `Error (n, code) -> Format.fprintf f "error %d %d" n code)
    ( = )

let one ~finished = { Jobs.done_ = (if finished then 1 else 0); total = Some 1 }

(* The ID of a new job. *)
let start jobs ~kind ~progress command =
  Jobs.id (Jobs.start jobs ~kind ~progress command)

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

let root () =
  match Confine.create (Sys.getcwd ()) with
  | Ok r -> r
  | Error m -> Alcotest.fail m

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
      match
        Mcp_tools.call context ~tasks:false name (Option.map canon args)
      with
      | Complete r -> r
      | Task _ -> Alcotest.fail (name ^ " gave a task to a client without Tasks"))
