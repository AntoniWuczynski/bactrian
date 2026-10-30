(* Helpers shared by the MCP server's tests. *)

open Bactrian_core
open Bactrian_server

let canon text =
  match Canon.of_string text with
  | Ok c -> c
  | Error _ -> Alcotest.fail ("malformed JSON: " ^ text)

let path keys c =
  List.fold_left (fun acc k -> Option.bind acc (Canon.member k)) (Some c) keys

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
