open Bactrian_core

let max_line = 64 * 1024 * 1024

(* The proxy answers every MCP request within its own upstream timeout. *)
let timeout = 660.

let post ~net ~clock ~target line =
  let head = ref None and buf = Buffer.create 256 in
  let result =
    Eio.Switch.run @@ fun sw ->
    Upstream.call ~sw ~net ~clock ~timeout Upstream.plain
      {
        meth = "POST";
        url = target;
        headers = [ ("content-type", "application/json") ];
        body = line;
      }
      ~on_head:(fun h -> head := Some h)
      ~on_chunk:(fun ~dt_ms:_ piece -> Buffer.add_string buf piece)
  in
  match (result, !head) with
  | Ok (), Some (h : Upstream.head) -> Ok (h.status, Buffer.contents buf)
  | Ok (), None -> Error "no response from bactrian"
  | Error e, _ -> Error (Upstream.describe_error e)

let failure line reason =
  match Mcp_msg.decode line with
  | Ok (Request { id; _ }) ->
      Some
        (Mcp_msg.encode
           (Error_response
              {
                id = Some id;
                code = -32000;
                message = "bactrian mcp-shim: " ^ reason;
                data = None;
              }))
  | Ok (Notification _ | Response _ | Error_response _) | Error _ -> None

(* What goes back to the agent for one line: the reply when it is a JSON-RPC
   message, nothing for an accepted notification, an error otherwise. An error
   without an id would match none of the agent's requests and leave it
   waiting, so a request gets it with its own id. *)
let answer line = function
  | Error reason -> failure line reason
  | Ok (status, body) -> (
      let body = String.trim body in
      match Mcp_msg.decode body with
      | Ok (Error_response { id = None; message; _ }) -> (
          match
            failure line
              (Printf.sprintf "bactrian answered %d: %s" status message)
          with
          | Some _ as failed -> failed
          | None -> Some body)
      | Ok (Request _ | Notification _ | Response _ | Error_response _) ->
          Some body
      | Error _ when String.equal body "" && status >= 200 && status < 300 ->
          None
      | Error _ -> failure line (Printf.sprintf "bactrian answered %d" status))

let run ~net ~clock ~stdin ~stdout ~url ~name =
  let target = Uri.of_string (url ^ "/" ^ name) in
  let lock = Eio.Mutex.create () in
  let emit text =
    Eio.Mutex.use_rw ~protect:true lock (fun () ->
        try Eio.Flow.copy_string (text ^ "\n") stdout
        with Eio.Io _ -> (* the agent stopped reading *) ())
  in
  let send line =
    Option.iter emit (answer line (post ~net ~clock ~target line))
  in
  let input = Eio.Buf_read.of_flow ~max_size:max_line stdin in
  Eio.Switch.run @@ fun sw ->
  let rec loop () =
    match Eio.Buf_read.line input with
    | exception (End_of_file | Eio.Buf_read.Buffer_limit_exceeded) -> ()
    | line when String.equal (String.trim line) "" -> loop ()
    | line ->
        (* bactrian answers a notification or a response only once it is on
           the server's stdin, so waiting for that answer keeps such a message
           ahead of every line the agent wrote after it. Requests run in their
           own fibres, so parallel calls stay parallel. *)
        (match Mcp_msg.decode line with
        | Ok (Request _) -> Eio.Fiber.fork ~sw (fun () -> send line)
        | Ok (Notification _ | Response _ | Error_response _) | Error _ ->
            send line);
        loop ()
  in
  loop ()
