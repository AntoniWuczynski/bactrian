open Bactrian_core

exception Protocol_violation of string
exception Timeout of string

type era = Modern | Legacy
type error = { code : int; message : string; data : Canon.t option }

type t = {
  pid : int;
  input : Unix.file_descr;
  output : Unix.file_descr;
  timeout : float;
  buffer : Buffer.t;  (** Bytes read and not yet returned as a line. *)
  mutable era : era option;
  mutable next_id : int;
  mutable input_open : bool;
  mutable running : bool;  (** Not yet reaped. *)
}

let violation fmt = Printf.ksprintf (fun m -> raise (Protocol_violation m)) fmt
let pid t = t.pid
let era t = t.era

let obj fields =
  List.fold_left
    (fun o (k, v) -> Canon.set_member k v o)
    Canon.empty_object fields

let rec waitpid flags pid =
  match Unix.waitpid flags pid with
  | result -> Some result
  | exception Unix.Unix_error (EINTR, _, _) -> waitpid flags pid
  | exception Unix.Unix_error (ECHILD, _, _) -> None

let reap t =
  if t.running then begin
    t.running <- false;
    Option.map snd (waitpid [] t.pid)
  end
  else None

let dispose t =
  if t.running then begin
    (try Unix.kill t.pid Sys.sigkill with Unix.Unix_error (ESRCH, _, _) -> ());
    ignore (reap t : Unix.process_status option)
  end;
  if t.input_open then begin
    t.input_open <- false;
    Unix.close t.input
  end;
  try Unix.close t.output with Unix.Unix_error (EBADF, _, _) -> ()

let timed_out t what =
  dispose t;
  raise (Timeout what)

let environment overrides =
  let key s =
    match String.index_opt s '=' with Some i -> String.sub s 0 i | None -> s
  in
  let kept =
    List.filter
      (fun s -> not (List.mem_assoc (key s) overrides))
      (Array.to_list (Unix.environment ()))
  in
  Array.of_list (kept @ List.map (fun (k, v) -> k ^ "=" ^ v) overrides)

let spawn ?(env = []) ?(args = []) ?(timeout = 60.) ~exe ~dir () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let stdin_r, stdin_w = Unix.pipe ~cloexec:true () in
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process_env "/bin/sh"
      (Array.of_list
         ([
            "sh";
            "-c";
            "cd \"$1\" && exe=\"$2\" && shift 2 && exec \"$exe\" serve-mcp \
             \"$@\"";
            "sh";
            dir;
            exe;
          ]
         @ args))
      (environment env) stdin_r stdout_w Unix.stderr
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  {
    pid;
    input = stdin_w;
    output = stdout_r;
    timeout;
    buffer = Buffer.create 4096;
    era = None;
    next_id = 0;
    input_open = true;
    running = true;
  }

let send t message =
  let line = Mcp_msg.encode message ^ "\n" in
  match Unix.write_substring t.input line 0 (String.length line) with
  | n when n = String.length line -> ()
  | _ -> violation "short write to the server"
  | exception Unix.Unix_error (e, _, _) ->
      violation "cannot write to the server: %s" (Unix.error_message e)

(* The next line of standard output without its newline, or [None] at end of
   file. Waits at most until [deadline]. *)
let rec next_line t ~deadline =
  let contents = Buffer.contents t.buffer in
  match String.index_opt contents '\n' with
  | Some i ->
      Buffer.clear t.buffer;
      Buffer.add_substring t.buffer contents (i + 1)
        (String.length contents - i - 1);
      Some (String.sub contents 0 i)
  | None -> (
      let wait = deadline -. Unix.gettimeofday () in
      if wait <= 0. then None
      else
        match Unix.select [ t.output ] [] [] wait with
        | [], _, _ -> None
        | _ -> (
            let chunk = Bytes.create 65536 in
            match Unix.read t.output chunk 0 (Bytes.length chunk) with
            | 0 ->
                if Buffer.length t.buffer > 0 then
                  violation "the server's output ends without a newline: %S"
                    (Buffer.contents t.buffer)
                else raise End_of_file
            | n ->
                Buffer.add_subbytes t.buffer chunk 0 n;
                next_line t ~deadline
            | exception Unix.Unix_error (EINTR, _, _) -> next_line t ~deadline)
        | exception Unix.Unix_error (EINTR, _, _) -> next_line t ~deadline)

let read_message t ~what =
  match next_line t ~deadline:(Unix.gettimeofday () +. t.timeout) with
  | None -> timed_out t (Printf.sprintf "no reply to %s in %gs" what t.timeout)
  | exception End_of_file ->
      violation "the server closed its output during %s" what
  | Some line -> (
      match Mcp_msg.decode line with
      | Ok message -> message
      | Error (Invalid_json m | Not_jsonrpc m) ->
          violation "not a JSON-RPC line (%s): %S" m line)

let raw_request t method_ params =
  t.next_id <- t.next_id + 1;
  let id = Mcp_msg.Int_id t.next_id in
  send t (Request { id; method_; params = Some params });
  match read_message t ~what:method_ with
  | Response { id = got; result } when got = id -> Ok result
  | Error_response { id = Some got; code; message; data } when got = id ->
      Error { code; message; data }
  | Response _ | Error_response _ ->
      violation "the reply to %s names another request" method_
  | Request _ | Notification _ ->
      violation "the server sent a request or a notification during %s" method_

let version_of = function Modern -> "2026-07-28" | Legacy -> "2025-11-25"

let meta =
  obj
    [
      ("io.modelcontextprotocol/protocolVersion", Canon.string "2026-07-28");
      ("io.modelcontextprotocol/clientCapabilities", Canon.empty_object);
    ]

let request t method_ params =
  let params =
    match t.era with
    | Some Modern -> Canon.set_member "_meta" meta params
    | Some Legacy | None -> params
  in
  raw_request t method_ params

let notify t method_ params =
  send t (Notification { method_; params = Some params })

let must method_ = function
  | Ok result -> result
  | Error e -> violation "%s failed with %d: %s" method_ e.code e.message

let connect t era =
  match era with
  | Modern ->
      t.era <- Some Modern;
      must "server/discover" (request t "server/discover" Canon.empty_object)
  | Legacy ->
      let answer =
        must "initialize"
          (raw_request t "initialize"
             (obj
                [
                  ("protocolVersion", Canon.string (version_of Legacy));
                  ("capabilities", Canon.empty_object);
                  ( "clientInfo",
                    obj
                      [
                        ("name", Canon.string "mcp_client");
                        ("version", Canon.string "0");
                      ] );
                ]))
      in
      t.era <- Some Legacy;
      notify t "notifications/initialized" Canon.empty_object;
      answer

let start ?env ?args ?timeout ~exe ~dir era =
  let t = spawn ?env ?args ?timeout ~exe ~dir () in
  match connect t era with
  | _ -> t
  | exception e ->
      dispose t;
      raise e

let tools_list t = must "tools/list" (request t "tools/list" Canon.empty_object)

type outcome = Structured of Canon.t | Tool_error of Canon.t

let call_tool t name arguments =
  let result =
    must "tools/call"
      (request t "tools/call"
         (obj [ ("name", Canon.string name); ("arguments", arguments) ]))
  in
  (match (t.era, Canon.str_member "resultType" result) with
  | Some Modern, Some "complete" | Some Legacy, None | None, _ -> ()
  | Some Modern, _ | Some Legacy, Some _ ->
      violation "%s: unexpected resultType" name);
  let structured =
    match Canon.member "structuredContent" result with
    | Some s -> s
    | None -> violation "%s: no structuredContent" name
  in
  let text =
    match Canon.list_member "content" result with
    | Some [ block ] -> (
        match Canon.str_member "text" block with
        | Some s -> s
        | None -> violation "%s: the content block has no text" name)
    | Some _ | None -> violation "%s: expected one content block" name
  in
  (match Canon.of_string text with
  | Ok c when Canon.equal c structured -> ()
  | Ok _ | Error _ ->
      violation "%s: the text differs from structuredContent" name);
  match Canon.bool_member "isError" result with
  | Some true -> Tool_error structured
  | Some false | None -> Structured structured

let poll_job ?(timeout = 120.) ?(interval = 0.1) t job_id =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec go () =
    match
      call_tool t "job_status" (obj [ ("job_id", Canon.string job_id) ])
    with
    | Tool_error e -> violation "job_status: %s" (Canon.to_string e)
    | Structured status -> (
        match Canon.str_member "state" status with
        | Some "running" ->
            if Unix.gettimeofday () > deadline then
              timed_out t
                (Printf.sprintf "job %s still running after %gs" job_id timeout)
            else begin
              Unix.sleepf interval;
              go ()
            end
        | Some _ -> status
        | None -> violation "job_status: no state")
  in
  go ()

let close ?(timeout = 30.) t =
  if t.input_open then begin
    t.input_open <- false;
    Unix.close t.input
  end;
  let deadline = Unix.gettimeofday () +. timeout in
  (match next_line t ~deadline with
  | exception End_of_file -> ()
  | Some line -> violation "output after the input closed: %S" line
  | None ->
      timed_out t (Printf.sprintf "the server did not exit in %gs" timeout));
  let rec wait () =
    match waitpid [ WNOHANG ] t.pid with
    | None -> violation "the server's exit status is not available"
    | Some (0, _) ->
        if Unix.gettimeofday () > deadline then
          timed_out t (Printf.sprintf "the server did not exit in %gs" timeout)
        else begin
          Unix.sleepf 0.02;
          wait ()
        end
    | Some (_, status) ->
        t.running <- false;
        status
  in
  let status = wait () in
  dispose t;
  status
