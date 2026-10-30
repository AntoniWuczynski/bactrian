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

let environment ?(unset = []) overrides =
  let key s =
    match String.index_opt s '=' with Some i -> String.sub s 0 i | None -> s
  in
  let kept =
    List.filter
      (fun s ->
        not (List.mem_assoc (key s) overrides || List.mem (key s) unset))
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
let tasks_extension = "io.modelcontextprotocol/tasks"

let meta ~tasks =
  obj
    [
      ("io.modelcontextprotocol/protocolVersion", Canon.string "2026-07-28");
      ( "io.modelcontextprotocol/clientCapabilities",
        if tasks then
          obj [ ("extensions", obj [ (tasks_extension, Canon.empty_object) ]) ]
        else Canon.empty_object );
    ]

let request ?(tasks = false) t method_ params =
  let params =
    match t.era with
    | Some Modern -> Canon.set_member "_meta" (meta ~tasks) params
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

let tool_params name arguments =
  obj [ ("name", Canon.string name); ("arguments", arguments) ]

let tool_outcome ~name result =
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

let call_tool t name arguments =
  let result =
    must "tools/call" (request t "tools/call" (tool_params name arguments))
  in
  (match (t.era, Canon.str_member "resultType" result) with
  | Some Modern, Some "complete" | Some Legacy, None | None, _ -> ()
  | Some Modern, _ | Some Legacy, Some _ ->
      violation "%s: unexpected resultType" name);
  tool_outcome ~name result

let check_task ~what result =
  match
    (Canon.str_member "resultType" result, Canon.str_member "taskId" result)
  with
  | Some "task", Some _ -> result
  | _ -> violation "%s: not a task: %s" what (Canon.to_string result)

let call_task t name arguments =
  check_task ~what:name
    (must "tools/call"
       (request ~tasks:true t "tools/call" (tool_params name arguments)))

let task_ended status =
  List.mem status [ Some "completed"; Some "failed"; Some "cancelled" ]

let poll_until ~timeout ~interval ~what step =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec go () =
    match step () with
    | Some v -> v
    | None ->
        if Unix.gettimeofday () > deadline then
          raise (Timeout (Printf.sprintf "%s after %gs" what timeout))
        else begin
          Unix.sleepf interval;
          go ()
        end
  in
  go ()

let poll_task ?(timeout = 120.) ?(interval = 0.1) t task_id =
  let get () =
    must "tasks/get"
      (request ~tasks:true t "tasks/get"
         (obj [ ("taskId", Canon.string task_id) ]))
  in
  match
    poll_until ~timeout ~interval
      ~what:("task " ^ task_id ^ " still working")
      (fun () ->
        let task = get () in
        if task_ended (Canon.str_member "status" task) then Some task else None)
  with
  | task -> task
  | exception (Timeout _ as e) ->
      dispose t;
      raise e

let poll_job ?(timeout = 120.) ?(interval = 0.1) t job_id =
  match
    poll_until ~timeout ~interval
      ~what:("job " ^ job_id ^ " still running")
      (fun () ->
        match
          call_tool t "job_status" (obj [ ("job_id", Canon.string job_id) ])
        with
        | Tool_error e -> violation "job_status: %s" (Canon.to_string e)
        | Structured status -> (
            match Canon.str_member "state" status with
            | Some "running" -> None
            | Some _ -> Some status
            | None -> violation "job_status: no state"))
  with
  | status -> status
  | exception (Timeout _ as e) ->
      dispose t;
      raise e

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

module Http = struct
  type response = {
    status : int;
    headers : (string * string) list;
    body : string;
  }

  let has name headers =
    List.exists
      (fun (k, _) ->
        String.equal (String.lowercase_ascii k) (String.lowercase_ascii name))
      headers

  let connect ~timeout port =
    let fd = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
    Unix.setsockopt_float fd SO_RCVTIMEO timeout;
    Unix.setsockopt_float fd SO_SNDTIMEO timeout;
    (match Unix.connect fd (ADDR_INET (Unix.inet_addr_loopback, port)) with
    | () -> ()
    | exception e ->
        Unix.close fd;
        raise e);
    fd

  let write_all fd s =
    let rec go off =
      if off < String.length s then
        go (off + Unix.write_substring fd s off (String.length s - off))
    in
    go 0

  let request ?(meth = "POST") ?(path = "/mcp") ?(close = true) ~port headers
      body =
    let headers =
      (if has "host" headers then []
       else [ ("Host", Printf.sprintf "127.0.0.1:%d" port) ])
      @ headers
      @ [ ("Content-Length", string_of_int (String.length body)) ]
      @ if close then [ ("Connection", "close") ] else []
    in
    Printf.sprintf "%s %s HTTP/1.1\r\n%s\r\n%s" meth path
      (String.concat ""
         (List.map (fun (k, v) -> k ^ ": " ^ v ^ "\r\n") headers))
      body

  let read_some fd buf =
    let chunk = Bytes.create 65536 in
    match Unix.read fd chunk 0 (Bytes.length chunk) with
    | 0 -> false
    | n ->
        Buffer.add_subbytes buf chunk 0 n;
        true
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
        raise (Timeout "no answer from the HTTP server")

  let find ?(from = 0) s sub =
    let n = String.length sub in
    let rec go i =
      if i + n > String.length s then None
      else if String.equal (String.sub s i n) sub then Some i
      else go (i + 1)
    in
    go from

  let header r name =
    List.find_map
      (fun (k, v) ->
        if String.equal (String.lowercase_ascii k) (String.lowercase_ascii name)
        then Some v
        else None)
      r.headers

  (* The chunks from [pos], and where they end. *)
  let rec dechunk s pos =
    match find ~from:pos s "\r\n" with
    | None -> violation "a chunked body without a size line"
    | Some i -> (
        match
          int_of_string_opt ("0x" ^ String.trim (String.sub s pos (i - pos)))
        with
        | None -> violation "a chunk size that is not hex"
        | Some 0 -> ("", i + 4)
        | Some n ->
            let rest, stop = dechunk s (i + 4 + n) in
            (String.sub s (i + 2) n ^ rest, stop))

  let head text =
    match String.split_on_char '\n' text with
    | [] -> violation "an empty HTTP head"
    | status_line :: lines ->
        let status =
          match String.split_on_char ' ' status_line with
          | _ :: code :: _ -> (
              match int_of_string_opt code with
              | Some c -> c
              | None -> violation "a status line without a code")
          | _ -> violation "a malformed status line: %S" status_line
        in
        let headers =
          List.filter_map
            (fun l ->
              match String.index_opt l ':' with
              | Some j ->
                  Some
                    ( String.sub l 0 j,
                      String.trim
                        (String.sub l (j + 1) (String.length l - j - 1)) )
              | None -> None)
            lines
        in
        { status; headers; body = "" }

  let answers text =
    let rec from pos acc =
      if pos >= String.length text then List.rev acc
      else
        match find ~from:pos text "\r\n\r\n" with
        | None -> violation "an HTTP answer without a head: %S" text
        | Some i ->
            let r = head (String.sub text pos (i - pos)) in
            let start = i + 4 in
            let body, stop =
              match
                (header r "transfer-encoding", header r "content-length")
              with
              | Some "chunked", _ -> dechunk text start
              | (Some _ | None), Some n -> (
                  match int_of_string_opt n with
                  | Some n when start + n <= String.length text ->
                      (String.sub text start n, start + n)
                  | Some _ | None -> violation "a short or bad body: %S" text)
              | (Some _ | None), None ->
                  ( String.sub text start (String.length text - start),
                    String.length text )
            in
            from stop ({ r with body } :: acc)
    in
    from 0 []

  let parse text =
    match answers text with r :: _ -> r | [] -> violation "no HTTP answer"

  let exchange ?(timeout = 60.) ~port text =
    let fd = connect ~timeout port in
    Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
    write_all fd text;
    let buf = Buffer.create 4096 in
    while read_some fd buf do
      ()
    done;
    Buffer.contents buf

  let send ?meth ?path ?timeout ~port headers body =
    parse (exchange ?timeout ~port (request ?meth ?path ~port headers body))

  let hang_up ?(timeout = 60.) ?(linger = 0.2) ~port headers body =
    let fd = connect ~timeout port in
    Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
    write_all fd (request ~port headers body);
    let buf = Buffer.create 4096 in
    let rec head () =
      match find (Buffer.contents buf) "\r\n\r\n" with
      | Some _ -> ()
      | None ->
          if read_some fd buf then head ()
          else violation "the connection closed before the head"
    in
    head ();
    Unix.sleepf linger

  let events body =
    String.split_on_char '\n' body
    |> List.filter_map (fun l ->
        if String.starts_with ~prefix:"data: " l then
          Some (String.sub l 6 (String.length l - 6))
        else None)

  let message r =
    let text =
      match header r "content-type" with
      | Some "text/event-stream" -> (
          match events r.body with
          | [ data ] -> data
          | _ -> violation "expected one SSE event: %S" r.body)
      | Some _ | None -> r.body
    in
    match Mcp_msg.decode text with
    | Ok m -> m
    | Error (Invalid_json e | Not_jsonrpc e) ->
        violation "not a JSON-RPC message (%s): %S" e text

  let outcome r =
    match message r with
    | Response { result; _ } -> Ok result
    | Error_response { code; message; data; _ } -> Error { code; message; data }
    | Request _ | Notification _ -> violation "a request in an HTTP answer"

  type server = {
    pid : int;
    port : int;
    token : string;
    log : string;
    mutable running : bool;
  }

  let test_token = "bactrian-test-token-0123456789abcdef"
  let port s = s.port
  let token s = s.token
  let pid s = s.pid
  let log s = In_channel.with_open_bin s.log In_channel.input_all

  let dispose s =
    if s.running then begin
      s.running <- false;
      (try Unix.kill s.pid Sys.sigkill
       with Unix.Unix_error (ESRCH, _, _) -> ());
      ignore (waitpid [] s.pid : (int * Unix.process_status) option)
    end;
    try Sys.remove s.log with Sys_error _ -> ()

  let listening text =
    let prefix = "listening on http://127.0.0.1:" in
    Option.bind (find text prefix) (fun i ->
        let rest =
          String.sub text
            (i + String.length prefix)
            (String.length text - i - String.length prefix)
        in
        Option.bind (String.index_opt rest '/') (fun j ->
            int_of_string_opt (String.sub rest 0 j)))

  let printed_token text =
    let prefix = "bearer token: " in
    Option.bind (find text prefix) (fun i ->
        let start = i + String.length prefix in
        Option.map
          (fun j -> String.sub text start (j - start))
          (String.index_from_opt text start ' '))

  let start ?(env = []) ?(args = []) ?(token = Some test_token) ?(timeout = 60.)
      ~exe ~dir () =
    let log = Filename.temp_file "serve-mcp-http" ".log" in
    let err = Unix.openfile log [ O_WRONLY; O_TRUNC; O_CLOEXEC ] 0o600 in
    let null = Unix.openfile "/dev/null" [ O_RDONLY; O_CLOEXEC ] 0 in
    let pid =
      Unix.create_process_env "/bin/sh"
        (Array.of_list
           ([
              "sh";
              "-c";
              "cd \"$1\" && exe=\"$2\" && shift 2 && exec \"$exe\" serve-mcp \
               --http 0 \"$@\"";
              "sh";
              dir;
              exe;
            ]
           @ args))
        (match token with
        | Some t -> environment (("BACTRIAN_MCP_TOKEN", t) :: env)
        | None -> environment ~unset:[ "BACTRIAN_MCP_TOKEN" ] env)
        null err err
    in
    Unix.close err;
    Unix.close null;
    let s = { pid; port = 0; token = ""; log; running = true } in
    match
      poll_until ~timeout ~interval:0.05 ~what:"the server did not listen"
        (fun () ->
          let text = In_channel.with_open_bin log In_channel.input_all in
          match (listening text, token) with
          | Some port, Some token -> Some { s with port; token }
          | Some port, None ->
              Option.map
                (fun token -> { s with port; token })
                (printed_token text)
          | None, (Some _ | None) -> None)
    with
    | s -> s
    | exception e ->
        dispose s;
        raise e

  let stop ?(timeout = 30.) s =
    Unix.kill s.pid Sys.sigterm;
    match
      poll_until ~timeout ~interval:0.02 ~what:"the server did not exit"
        (fun () ->
          match waitpid [ WNOHANG ] s.pid with
          | Some (0, _) -> None
          | Some (_, status) -> Some status
          | None -> violation "the server's exit status is not available")
    with
    | status ->
        s.running <- false;
        status
    | exception e ->
        dispose s;
        raise e

  let auth s = ("Authorization", "Bearer " ^ s.token)

  let rpc_headers ?(accept = "application/json, text/event-stream") ~token
      method_ params =
    let named =
      match method_ with
      | "tools/call" -> Canon.str_member "name" params
      | "tasks/get" | "tasks/update" | "tasks/cancel" ->
          Canon.str_member "taskId" params
      | _ -> None
    in
    [
      ("Authorization", "Bearer " ^ token);
      ("Content-Type", "application/json");
      ("Accept", accept);
      ("MCP-Protocol-Version", "2026-07-28");
      ("Mcp-Method", method_);
    ]
    @ Option.fold ~none:[] ~some:(fun n -> [ ("Mcp-Name", n) ]) named

  let next_id = ref 0

  let body ?id method_ params =
    let id =
      match id with
      | Some id -> id
      | None ->
          incr next_id;
          !next_id
    in
    Mcp_msg.encode (Request { id = Int_id id; method_; params = Some params })

  let with_meta ~tasks params = Canon.set_member "_meta" (meta ~tasks) params

  let rpc ?(tasks = false) ?accept ?(headers = fun h -> h) s method_ params =
    let params = with_meta ~tasks params in
    send ~port:s.port
      (headers (rpc_headers ?accept ~token:s.token method_ params))
      (body method_ params)

  let must_ok method_ r =
    match outcome r with
    | Ok result when r.status = 200 -> result
    | Ok _ -> violation "%s: a result with status %d" method_ r.status
    | Error e -> violation "%s failed with %d: %s" method_ e.code e.message

  let call_tool ?accept s name arguments =
    let result =
      must_ok "tools/call"
        (rpc ?accept s "tools/call" (tool_params name arguments))
    in
    if
      not
        (Option.equal String.equal
           (Canon.str_member "resultType" result)
           (Some "complete"))
    then violation "%s: unexpected resultType" name;
    tool_outcome ~name result

  let call_task s name arguments =
    check_task ~what:name
      (must_ok "tools/call"
         (rpc ~tasks:true s "tools/call" (tool_params name arguments)))

  let task s method_ task_id =
    outcome
      (rpc ~tasks:true s method_ (obj [ ("taskId", Canon.string task_id) ]))

  let poll_task ?(timeout = 120.) ?(interval = 0.1) s task_id =
    poll_until ~timeout ~interval
      ~what:("task " ^ task_id ^ " still working")
      (fun () ->
        match task s "tasks/get" task_id with
        | Ok t when task_ended (Canon.str_member "status" t) -> Some t
        | Ok _ -> None
        | Error e -> violation "tasks/get failed with %d: %s" e.code e.message)
end
