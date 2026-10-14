open Bactrian_core
open Acceptance_world

let exe () = executable "BACTRIAN_EXE"

(* JSON *)

let obj fields =
  List.fold_left
    (fun o (k, v) -> Canon.set_member k v o)
    Canon.empty_object fields

let strings l = Canon.list (List.map Canon.string l)

let json =
  Alcotest.testable
    (fun ppf c -> Format.pp_print_string ppf (Canon.to_string c))
    Canon.equal

let canon_of_file path =
  match Canon.of_string (read path) with
  | Ok c -> c
  | Error e -> Alcotest.failf "%s: %a" path Canon.pp_error e

let get what = function Some v -> v | None -> Alcotest.failf "missing %s" what
let field key c = get key (Canon.member key c)
let string_field key c = get key (Canon.str_member key c)

(* Directories *)

let remove_tree dir =
  ignore (Sys.command ("rm -rf " ^ Filename.quote dir) : int)

let write path text =
  ignore (Sys.command ("mkdir -p " ^ Filename.quote (Filename.dirname path)));
  Out_channel.with_open_bin path (fun oc -> output_string oc text)

let with_project name f =
  let parent = Filename.concat (Sys.getcwd ()) ("tmp-mcp-" ^ name) in
  remove_tree parent;
  let project = Filename.concat parent "project" in
  ignore (Sys.command ("mkdir -p " ^ Filename.quote project));
  Fun.protect
    ~finally:(fun () -> remove_tree parent)
    (fun () -> f ~parent ~project)

(* The command line *)

let key_env = [ ("OPENAI_API_KEY", real_key) ]

let cli ~dir ~env args =
  let out = Filename.temp_file "cli" ".json" in
  Fun.protect ~finally:(fun () -> Sys.remove out) @@ fun () ->
  let assignments =
    String.concat " "
      (List.map (fun (k, v) -> Filename.quote (k ^ "=" ^ v)) env)
  in
  let command =
    Printf.sprintf "cd %s && env %s %s %s > %s 2> /dev/null"
      (Filename.quote dir) assignments
      (Filename.quote (exe ()))
      (String.concat " " (List.map Filename.quote args))
      (Filename.quote out)
  in
  let status = Sys.command command in
  (status, canon_of_file out)

(* Processes *)

let alive pid =
  match Unix.kill pid 0 with
  | () -> true
  | exception Unix.Unix_error (ESRCH, _, _) -> false

let rec wait_until ~what ~seconds condition =
  if condition () then ()
  else if seconds <= 0. then Alcotest.failf "timed out waiting for %s" what
  else begin
    Unix.sleepf 0.05;
    wait_until ~what ~seconds:(seconds -. 0.05) condition
  end

let read_pid path =
  match int_of_string_opt (String.trim (read path)) with
  | Some pid -> pid
  | None -> Alcotest.failf "%s holds no process ID" path

let wait_gone what pid =
  wait_until ~what:(what ^ " to exit") ~seconds:5. (fun () -> not (alive pid))

(* The example experiment: two variants of the weather conversation. *)

let scenario_text =
  Printf.sprintf
    {|{"version": 1, "id": "weather", "timezone": "UTC",
 "turns": [{"user": %S}],
 "assertions": [{"tool_sequence": {"mode": "exact", "tools": ["get_weather"]}}, {"max_steps": 2}],
 "checkers": []}|}
    first_turn

let terse = "You are a terse assistant."

(* The host's own sandbox is named, so the server's strict mode on macOS does
   not ask for a virtual machine image. *)
let host_backend =
  match Bactrian_runtime.Sandbox.host_backend () with
  | `Seatbelt -> "seatbelt"
  | `Bubblewrap -> "bubblewrap"

let experiment_text ?(extra = "") ?model argv =
  let model_env =
    match model with Some m -> [ ("AGENT_MODEL", `String m) ] | None -> []
  in
  let variants =
    `Assoc
      [
        ("base", `Assoc [ ("env", `Assoc model_env) ]);
        ( "terse",
          `Assoc
            [
              ("env", `Assoc (model_env @ [ (prompt_variable, `String terse) ]));
            ] );
      ]
  in
  Printf.sprintf
    {|{"version": 1, "name": "weather",
 "agent": {"protocol": "command", "command": %s},
 "scenarios": ["scenarios/*.json"],
 "variants": %s,
 "repeats": 2, "concurrency": 1, "seed": 3,
 "sandbox": {"mode": "off", "backend": "%s"},
 "caps_usd": {"openai": 100},
 "compare": {"baseline": "base", "thresholds": {"pass_rate": 0.1}}%s}|}
    (Yojson.Safe.to_string (`List (List.map (fun s -> `String s) argv)))
    (Yojson.Safe.to_string variants)
    host_backend extra

let write_experiment ?extra ?model dir argv =
  write (Filename.concat dir "scenarios/weather.json") scenario_text;
  write (Filename.concat dir "exp.json") (experiment_text ?extra ?model argv)

(* A fake upstream in a process of its own: the binary under test runs while
   the test waits, so an upstream in the test's own process would never get to
   answer. Its standard input is the lifeline: when the test ends, however it
   ends, the server sees the end of input and exits. *)

type upstream = { url : string; log : string }

(* Waits for the server's exit; a signal of the test's own may interrupt it. *)
let rec reap pid =
  match Unix.waitpid [] pid with
  | _ -> ()
  | exception Unix.Unix_error (EINTR, _, _) -> reap pid

let upstream_server = "FAKE_UPSTREAM_SERVER"

let with_upstream ?hold ~dir f =
  let log = Filename.concat dir "upstream.log" in
  let fixtures =
    Filename.concat (Sys.getcwd ()) "../fixtures/protocols/openai_chat"
  in
  let exe = executable upstream_server in
  let args =
    [ exe; "--fixtures"; fixtures; "--log"; log ]
    @ match hold with Some h -> [ "--hold"; h ] | None -> []
  in
  let stdin_r, stdin_w = Unix.pipe ~cloexec:true () in
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process exe (Array.of_list args) stdin_r stdout_w Unix.stderr
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  let from_server = Unix.in_channel_of_descr stdout_r in
  Fun.protect ~finally:(fun () ->
      Unix.close stdin_w;
      close_in_noerr from_server;
      reap pid)
  @@ fun () ->
  match int_of_string_opt (String.trim (input_line from_server)) with
  | Some port -> f { url = Printf.sprintf "http://127.0.0.1:%d" port; log }
  | None -> Alcotest.fail "the fake upstream did not print its port"

let upstream_url u = u.url
let upstream_requests u = if Sys.file_exists u.log then lines u.log else []
