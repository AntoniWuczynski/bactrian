open Bactrian_core
module Interrupt = Bactrian_runtime.Interrupt

let ( let* ) = Result.bind
let endpoint = "/mcp"
let max_body = 16 * 1024 * 1024

type config = { port : int; token : string; allowed_origins : string list }
type early = { status : int; headers : (string * string) list; body : string }

let local_hosts port =
  [ Printf.sprintf "127.0.0.1:%d" port; Printf.sprintf "localhost:%d" port ]

let json = ("content-type", "application/json")

let unidentified code message =
  Mcp_msg.encode (Error_response { id = None; code; message; data = None })

let refuse ?(headers = []) status body = Error { status; headers; body }

let refuse_rpc status message =
  refuse ~headers:[ json ] status (unidentified (-32600) message)

let refuse_text ?(headers = []) status message =
  refuse
    ~headers:(("content-type", "text/plain; charset=utf-8") :: headers)
    status (message ^ "\n")

(* Equal lengths in constant time: the length of the token is no secret. *)
let same_secret a b =
  String.length a = String.length b
  &&
  let d = ref 0 in
  String.iteri (fun i c -> d := !d lor (Char.code c lxor Char.code b.[i])) a;
  !d = 0

let path resource =
  match String.index_opt resource '?' with
  | Some i -> String.sub resource 0 i
  | None -> resource

let origin v =
  let o = String.lowercase_ascii v in
  let digit c = c >= '0' && c <= '9' in
  let host_char c = digit c || (c >= 'a' && c <= 'z') || c = '-' || c = '.' in
  let port p =
    String.equal p ""
    || String.length p > 1
       && Char.equal p.[0] ':'
       &&
       let d = String.sub p 1 (String.length p - 1) in
       String.length d <= 5
       && String.for_all digit d
       && Option.fold ~none:false
            ~some:(fun n -> n >= 1 && n <= 65535)
            (int_of_string_opt d)
  in
  let split i a = (String.sub a 0 i, String.sub a i (String.length a - i)) in
  let authority =
    List.find_map
      (fun scheme ->
        if String.starts_with ~prefix:scheme o then
          Some
            (String.sub o (String.length scheme)
               (String.length o - String.length scheme))
        else None)
      [ "http://"; "https://" ]
  in
  let valid =
    match authority with
    | None -> false
    | Some a when String.starts_with ~prefix:"[" a -> (
        match String.index_opt a ']' with
        | Some i ->
            let host, rest = split (i + 1) a in
            String.length host > 2
            && String.for_all
                 (fun c -> Char.equal c ':' || Char.equal c '.' || host_char c)
                 (String.sub host 1 (String.length host - 2))
            && port rest
        | None -> false)
    | Some a ->
        let host, rest =
          match String.index_opt a ':' with
          | Some i -> split i a
          | None -> (a, "")
        in
        (not (String.equal host ""))
        && String.for_all host_char host
        && port rest
  in
  if valid then Ok o
  else
    Error
      (Printf.sprintf
         "%s is not an origin: give scheme://host or scheme://host:port, with \
          http or https and no path, such as http://localhost:5173"
         v)

let allowed config o =
  List.exists
    (String.equal (String.lowercase_ascii o))
    (List.map (( ^ ) "http://") (local_hosts config.port)
    @ config.allowed_origins)

(* The media type before its parameters. *)
let media_type v =
  match String.split_on_char ';' v with
  | first :: _ -> String.lowercase_ascii (String.trim first)
  | [] -> ""

let admit config request =
  let get = Http.Header.get (Http.Request.headers request) in
  let* () =
    match get "host" with
    | Some h
      when List.exists
             (String.equal (String.lowercase_ascii h))
             (local_hosts config.port) ->
        Ok ()
    | Some _ | None ->
        refuse_rpc 403
          "Forbidden: the Host header must name this server, on 127.0.0.1 or \
           localhost"
  in
  let* () =
    match get "origin" with
    | None -> Ok ()
    | Some o when allowed config o -> Ok ()
    | Some o ->
        refuse_rpc 403
          (Printf.sprintf
             "Forbidden: origin %s is not allowed (serve-mcp \
              --http-allow-origin allows one)"
             o)
  in
  let* () =
    if String.equal (path (Http.Request.resource request)) endpoint then Ok ()
    else refuse_text 404 ("Not Found: the MCP endpoint is " ^ endpoint)
  in
  let* () =
    match (Http.Request.meth request, get "origin") with
    | `POST, _ -> Ok ()
    | `OPTIONS, Some _ ->
        (* The preflight of a page from an allowed origin, whose POST carries
           the token and JSON. The headers it may send are the MCP ones, whose
           Mcp-Param-* names vary, so they are allowed as asked. *)
        refuse 204
          ~headers:
            ([
               ("access-control-allow-methods", "POST");
               ("access-control-max-age", "600");
             ]
            @ Option.fold ~none:[]
                ~some:(fun h -> [ ("access-control-allow-headers", h) ])
                (get "access-control-request-headers"))
          ""
    | _, (Some _ | None) ->
        refuse_text
          ~headers:[ ("allow", "POST") ]
          405
          ("Method Not Allowed: POST each JSON-RPC message to " ^ endpoint)
  in
  let* () =
    let bearer v =
      match String.index_opt v ' ' with
      | Some i ->
          String.equal (String.lowercase_ascii (String.sub v 0 i)) "bearer"
          && same_secret
               (String.trim (String.sub v i (String.length v - i)))
               config.token
      | None -> false
    in
    match get "authorization" with
    | Some v when bearer v -> Ok ()
    | Some _ ->
        refuse_text
          ~headers:[ ("www-authenticate", {|Bearer error="invalid_token"|}) ]
          401 "Unauthorized: wrong bearer token"
    | None ->
        refuse_text
          ~headers:[ ("www-authenticate", "Bearer") ]
          401
          "Unauthorized: send Authorization: Bearer with the token serve-mcp \
           printed or BACTRIAN_MCP_TOKEN"
  in
  match Option.map media_type (get "content-type") with
  | Some "application/json" -> Ok ()
  | Some _ | None ->
      refuse_text 415 "Unsupported Media Type: send application/json"

let header_value v =
  let visible =
    String.for_all (fun c -> Char.equal c '\t' || (c >= ' ' && c <= '~')) v
  and prefix = "=?base64?"
  and suffix = "?=" in
  let p = String.length prefix and s = String.length suffix in
  if not visible then None
  else if
    String.length v >= p + s
    && String.starts_with ~prefix v
    && String.ends_with ~suffix v
  then
    Result.to_option (Base64.decode (String.sub v p (String.length v - p - s)))
  else Some v

let version_key = "io.modelcontextprotocol/protocolVersion"

let mismatch id message : Mcp_msg.message =
  Error_response
    {
      id = Some id;
      code = -32020;
      message = "Header mismatch: " ^ message;
      data = None;
    }

(* A header that must be there and equal what the body says. Only [Mcp-Name]
   may come in Base64. *)
let matching ?(encoded = false) headers id name ~body =
  match Http.Header.get headers name with
  | None -> Error (mismatch id (name ^ " header is missing"))
  | Some raw -> (
      match if encoded then header_value raw else Some raw with
      | None -> Error (mismatch id (name ^ " header holds invalid characters"))
      | Some v when String.equal v body -> Ok ()
      | Some v ->
          Error
            (mismatch id
               (Printf.sprintf
                  "%s header value '%s' does not match body value '%s'" name v
                  body)))

(* What [Mcp-Name] repeats: the tool of a call, the task of a task method. *)
let named_member = function
  | "tools/call" -> Some "name"
  | "tasks/get" | "tasks/update" | "tasks/cancel" -> Some "taskId"
  | _ -> None

let era_of_request headers (msg : Mcp_msg.message) :
    (Mcp_dispatch.era, Mcp_msg.message) result =
  match msg with
  | Response _ | Error_response _ | Notification _ -> Ok Mcp_dispatch.Undecided
  | Request { id; method_; params } -> (
      let str key = Option.bind params (Canon.str_member key) in
      match Mcp_msg.era_of_request msg with
      | Some Legacy -> Ok Mcp_dispatch.Undecided
      | Some Modern ->
          (* A version that is not a string is the dispatcher's -32602. *)
          let body_version =
            Option.bind
              (Option.bind params (Canon.member "_meta"))
              (fun m ->
                Option.bind (Canon.member version_key m) Canon.to_string_opt)
          in
          let* () =
            match body_version with
            | Some v -> matching headers id "MCP-Protocol-Version" ~body:v
            | None -> Ok ()
          in
          let* () = matching headers id "Mcp-Method" ~body:method_ in
          let* () =
            match Option.bind (named_member method_) str with
            | Some name ->
                matching ~encoded:true headers id "Mcp-Name" ~body:name
            | None -> Ok ()
          in
          Ok Mcp_dispatch.Modern
      | None -> (
          match Http.Header.get headers "mcp-protocol-version" with
          | None -> Ok (Mcp_dispatch.Legacy "2025-03-26")
          | Some v when String.equal v Mcp_dispatch.modern_version ->
              Ok Mcp_dispatch.Modern
          | Some v
            when List.exists (String.equal v) Mcp_dispatch.legacy_versions ->
              Ok (Mcp_dispatch.Legacy v)
          | Some v ->
              Error
                (Error_response
                   {
                     id = Some id;
                     code = -32600;
                     message =
                       "Invalid Request: unsupported MCP-Protocol-Version " ^ v;
                     data =
                       Some
                         (Canon.of_fields
                            [
                              ( "supported",
                                Canon.list
                                  (List.map Canon.string
                                     (Mcp_dispatch.modern_version
                                    :: Mcp_dispatch.legacy_versions)) );
                            ]);
                   })))

let status (era : Mcp_dispatch.era) (msg : Mcp_msg.message) =
  match (msg, era) with
  | (Response _ | Request _ | Notification _), _ -> 200
  | Error_response { code; _ }, Modern -> (
      match code with -32601 -> 404 | -32603 -> 500 | _ -> 400)
  | Error_response { code; _ }, (Legacy _ | Undecided) -> (
      match code with -32700 | -32600 -> 400 | _ -> 200)

let listen ~sw ~net port =
  match
    Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  with
  | exception (Eio.Io _ as ex) -> Error (Printexc.to_string ex)
  | sock -> (
      match Eio.Net.listening_addr sock with
      | `Unix _ -> Error "the listener is not TCP"
      | `Tcp (_, port) -> Ok (sock, port))

(* Answers *)

type answer = Http.Response.t * (Eio.Buf_read.t -> Eio.Buf_write.t -> unit)

let fixed ~status ~headers body : answer =
  let headers =
    Http.Header.of_list
      (("content-length", string_of_int (String.length body)) :: headers)
  in
  ( Http.Response.make ~status:(Http.Status.of_int status) ~headers (),
    fun _ic oc ->
      Eio.Buf_write.string oc body;
      Eio.Buf_write.flush oc )

let of_early (r : early) = fixed ~status:r.status ~headers:r.headers r.body

let message ~status m =
  fixed ~status ~headers:[ json ] (Mcp_msg.encode ~short_floats:true m)

(* Returns once the client has closed the connection. Bytes it sends meanwhile
   are a next request, left in the buffer for the server's loop. *)
let closed ic =
  match Eio.Buf_read.ensure ic 1 with
  | () -> Eio.Fiber.await_cancel ()
  | exception (End_of_file | Eio.Io _) -> ()

let chunk oc s =
  Eio.Buf_write.printf oc "%x\r\n" (String.length s);
  Eio.Buf_write.string oc s;
  Eio.Buf_write.string oc "\r\n"

(* The head goes out at once and the result when the call ends, unless the
   client hangs up first, which cancels the call. *)
let streamed ~sse (run : unit -> Mcp_msg.message) : answer =
  let headers =
    [
      ("content-type", if sse then "text/event-stream" else "application/json");
      ("cache-control", "no-cache");
      ("transfer-encoding", "chunked");
    ]
    @ if sse then [ ("x-accel-buffering", "no") ] else []
  in
  ( Http.Response.make ~status:`OK ~headers:(Http.Header.of_list headers) (),
    fun ic oc ->
      Eio.Buf_write.flush oc;
      match
        Eio.Fiber.first
          (fun () ->
            closed ic;
            None)
          (fun () -> Some (run ()))
      with
      | None -> ()
      | Some m ->
          let text = Mcp_msg.encode ~short_floats:true m in
          chunk oc
            (if sse then "event: message\ndata: " ^ text ^ "\n\n" else text);
          Eio.Buf_write.string oc "0\r\n\r\n";
          Eio.Buf_write.flush oc )

let wants_sse request =
  match Http.Header.get (Http.Request.headers request) "accept" with
  | Some v ->
      List.exists
        (fun t -> String.equal (media_type t) "text/event-stream")
        (String.split_on_char ',' v)
  | None -> false

let read_body body =
  match Eio.Buf_read.(take_all (of_flow ~max_size:max_body body)) with
  | text -> Ok text
  | exception Eio.Buf_read.Buffer_limit_exceeded ->
      refuse ~headers:[ json ] 413
        (unidentified (-32600)
           (Printf.sprintf "Invalid Request: longer than %d bytes" max_body))

(* What a page from an allowed origin needs to read an answer. *)
let with_cors config request ((response, write) : answer) : answer =
  match Http.Header.get (Http.Request.headers request) "origin" with
  | Some o when allowed config o ->
      ( {
          response with
          headers =
            Http.Header.add_list response.headers
              [ ("access-control-allow-origin", o); ("vary", "Origin") ];
        },
        write )
  | Some _ | None -> (response, write)

(* Ends a connection. *)
exception Hang_up

(* An answer after which the server ends the connection. *)
let closing ((response, write) : answer) : answer =
  ( {
      response with
      headers = Http.Header.replace response.headers "connection" "close";
    },
    fun ic oc ->
      write ic oc;
      raise Hang_up )

(* A refused request's body is read and dropped, so that the next request on
   the connection starts where it should: unread, the body would be parsed as
   one, which no check of this request's head would have seen. A longer body
   ends the connection instead. *)
let max_refused_body = 64 * 1024

let refused body r =
  match Eio.Buf_read.(take_all (of_flow ~max_size:max_refused_body body)) with
  | (_ : string) -> of_early r
  | exception Eio.Buf_read.Buffer_limit_exceeded -> closing (of_early r)

let max_connections = 64

(* cohttp buffers a request's head whole until it parses. *)
let max_head = 64 * 1024

(* The socket as the server reads and writes it: by a deadline until a request
   is admitted, so a client that sends nothing, sends a head byte by byte or
   does not read the answers to its refused requests holds no connection for
   long, and freely from then until the answer is out. *)
type guard = {
  socket : [ `Generic | `Unix ] Eio.Net.stream_socket_ty Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  head_timeout : float;
  mutable deadline : float option;
}

let waiting g = g.deadline <- Some (Eio.Time.now g.clock +. g.head_timeout)
let admitted g = g.deadline <- None

let by_deadline g f =
  match g.deadline with
  | None -> f ()
  | Some d -> (
      match
        Eio.Time.with_timeout g.clock
          (d -. Eio.Time.now g.clock)
          (fun () -> Ok (f ()))
      with
      | Ok n -> n
      | Error `Timeout -> raise Hang_up)

module Guarded_source = struct
  type t = guard

  let read_methods = []

  let single_read g buf =
    by_deadline g (fun () -> Eio.Flow.single_read g.socket buf)
end

module Guarded_sink = struct
  type t = guard

  let single_write g bufs =
    by_deadline g (fun () -> Eio.Flow.single_write g.socket bufs)

  let copy g ~src = Eio.Flow.Pi.simple_copy ~single_write g ~src
end

let guarded_source = Eio.Flow.Pi.source (module Guarded_source)
let guarded_sink = Eio.Flow.Pi.sink (module Guarded_sink)

let admitted_answer ~server ~(handlers : Mcp_dispatch.handlers) request read =
  match read with
  | Error r -> closing (of_early r)
  | Ok text -> (
      match Mcp_msg.decode text with
      | Error e -> message ~status:400 (Mcp_dispatch.undecodable e)
      | Ok (Response _ | Error_response _) ->
          message ~status:400
            (Error_response
               {
                 id = None;
                 code = -32600;
                 message = "Invalid Request: a client sends no responses";
                 data = None;
               })
      | Ok (Notification _) -> fixed ~status:202 ~headers:[] ""
      | Ok (Request _ as msg) -> (
          match era_of_request (Http.Request.headers request) msg with
          | Error m -> message ~status:400 m
          | Ok era -> (
              let d = Mcp_dispatch.create ~era server in
              let action = Mcp_dispatch.receive d msg in
              let reply m = message ~status:(status (Mcp_dispatch.era d) m) m in
              match action with
              | Reply m -> reply m
              | Task_op { id; op; task_id } ->
                  reply
                    (Mcp_dispatch.task_reply d id op (handlers.task op task_id))
              | Call { id; name; arguments; tasks } ->
                  streamed ~sse:(wants_sse request) (fun () ->
                      Mcp_dispatch.tool_reply d id
                        (handlers.call ~tasks name arguments))
              (* Requests only reach here, and they are never ignored or
                 cancellations. *)
              | Cancel _ | Ignore -> fixed ~status:202 ~headers:[] "")))

let answer ~server ~(handlers : Mcp_dispatch.handlers) ~guard config request
    body =
  with_cors config request
  @@
  match admit config request with
  | Error r -> refused body r
  | Ok () ->
      admitted guard;
      let response, write =
        admitted_answer ~server ~handlers request (read_body body)
      in
      ( response,
        fun ic oc ->
          write ic oc;
          waiting guard )

(* Closing with bytes unread would reset the connection, which can destroy the
   answer before the client reads it, so the server stops sending and drops
   what still comes, for a while. *)
let linger ~clock flow =
  Eio.Flow.shutdown flow `Send;
  let rest = Eio.Buf_read.of_flow ~max_size:max_head flow in
  let rec drop () =
    Eio.Buf_read.ensure rest 1;
    Eio.Buf_read.consume rest (Eio.Buf_read.buffered_bytes rest);
    drop ()
  in
  match
    Eio.Time.with_timeout clock 2. (fun () ->
        match drop () with () -> Ok () | exception End_of_file -> Ok ())
  with
  | Ok () | Error `Timeout -> ()

let serve ~server ~handlers ~stop ~log ~clock ?(head_timeout = 30.) config
    socket =
  let on_error = function
    | Hang_up | Eio.Io _ | Eio.Buf_read.Buffer_limit_exceeded -> ()
    | ex -> log ("HTTP connection failed: " ^ Printexc.to_string ex)
  in
  let connection
      (flow : [ `Generic | `Unix ] Eio.Net.stream_socket_ty Eio.Resource.t) peer
      =
    Eio.Switch.run @@ fun sw ->
    let g = { socket = flow; clock; head_timeout; deadline = None } in
    waiting g;
    let callback _conn request body =
      answer ~server ~handlers ~guard:g config request body
    in
    match
      Eio.Buf_write.with_flow (Eio.Resource.T (g, guarded_sink))
      @@ fun output ->
      Cohttp_eio.Server.callback
        (Cohttp_eio.Server.make_expert ~callback ())
        (sw, peer)
        (Eio.Buf_read.of_flow ~max_size:max_head
           (Eio.Resource.T (g, guarded_source)))
        output
    with
    | () -> ()
    | exception Hang_up -> linger ~clock flow
  in
  match
    Interrupt.run ~stop (fun () ->
        Eio.Net.run_server ~max_connections ~on_error socket connection)
  with
  | Ok ended -> ended
  | Error signal -> Mcp_server.Stopped signal
