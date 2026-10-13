type https =
  Uri.t ->
  [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r ->
  [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r

type connector = https option

let plain = None
let of_https f = Some f

type request = {
  meth : string;
  url : Uri.t;
  headers : (string * string) list;
  body : string;
}

type head = { status : int; headers : (string * string) list }

type error =
  | Connect_failed of string
  | Failed_after_send of string
  | Timed_out
  | Truncated of { bytes : int; reason : string }

let describe_error = function
  | Connect_failed reason -> "upstream unreachable: " ^ reason
  | Failed_after_send reason ->
      "upstream connection failed after the request was sent: " ^ reason
  | Timed_out -> "upstream timed out"
  | Truncated { bytes; reason } ->
      Printf.sprintf "upstream body cut off after %d bytes: %s" bytes reason

let allowed name =
  String.equal name "content-type"
  || String.equal name "retry-after"
  || String.equal name "x-request-id"
  || String.equal name "mcp-session-id"
  || String.starts_with ~prefix:"x-ratelimit-" name
  || String.starts_with ~prefix:"anthropic-ratelimit-" name

let allowed_headers headers =
  List.filter_map
    (fun (k, v) ->
      let k = String.lowercase_ascii k in
      if allowed k then Some (k, v) else None)
    headers

let request_headers headers =
  let dropped =
    [ "accept-encoding"; "host"; "content-length"; "transfer-encoding" ]
  in
  let kept =
    List.filter
      (fun (k, _) -> not (List.mem (String.lowercase_ascii k) dropped))
      headers
  in
  kept

type framing = Fixed of int | Chunked | Until_close

let header_value headers name =
  List.find_map
    (fun (k, v) ->
      if String.equal (String.lowercase_ascii k) name then Some (String.trim v)
      else None)
    headers

let framing ~status headers =
  if status = 204 || status = 304 || (status >= 100 && status < 200) then
    Fixed 0
  else
    match header_value headers "transfer-encoding" with
    | Some te when String.equal (String.lowercase_ascii te) "chunked" -> Chunked
    | Some _ | None -> (
        match
          Option.bind (header_value headers "content-length") int_of_string_opt
        with
        | Some n when n >= 0 -> Fixed n
        | Some _ | None -> Until_close)

(* Response head: status line and header lines. Cohttp-eio is not used here
   because its body reader reports a connection dropped inside a chunked body
   as a clean end, and a dropped stream has to be told apart from a finished
   one. *)
let read_head r =
  let status_line = Eio.Buf_read.line r in
  let status =
    match String.split_on_char ' ' status_line with
    | version :: code :: _ when String.starts_with ~prefix:"HTTP/" version ->
        int_of_string_opt code
    | _ -> None
  in
  let rec headers acc =
    match Eio.Buf_read.line r with
    | "" -> List.rev acc
    | line -> (
        match String.index_opt line ':' with
        | Some i ->
            let k = String.sub line 0 i
            and v = String.sub line (i + 1) (String.length line - i - 1) in
            headers ((String.lowercase_ascii k, String.trim v) :: acc)
        | None -> headers acc)
  in
  match status with
  | Some status -> Ok (status, headers [])
  | None -> Error (Failed_after_send ("bad status line: " ^ status_line))

exception Dropped of string

(* Passes up to [n] body bytes to [deliver] as they arrive. *)
let rec copy_bytes r n deliver =
  if n > 0 then begin
    (try Eio.Buf_read.ensure r 1
     with End_of_file -> raise (Dropped "connection closed inside the body"));
    let k = min n (Eio.Buf_read.buffered_bytes r) in
    deliver (Eio.Buf_read.take k r);
    copy_bytes r (n - k) deliver
  end

let line_or_drop r =
  try Eio.Buf_read.line r
  with End_of_file -> raise (Dropped "connection closed inside the body")

let chunk_size line =
  let hex =
    match String.index_opt line ';' with
    | Some i -> String.sub line 0 i
    | None -> line
  in
  int_of_string_opt ("0x" ^ String.trim hex)

let read_chunked r deliver =
  let rec loop () =
    match chunk_size (line_or_drop r) with
    | None -> raise (Dropped "malformed chunk size")
    | Some 0 ->
        let rec trailers () =
          if not (String.equal (line_or_drop r) "") then trailers ()
        in
        trailers ()
    | Some n ->
        copy_bytes r n deliver;
        ignore (line_or_drop r);
        loop ()
  in
  loop ()

let read_until_close r deliver =
  let rec loop () =
    match Eio.Buf_read.ensure r 1 with
    | () ->
        deliver (Eio.Buf_read.take (Eio.Buf_read.buffered_bytes r) r);
        loop ()
    | exception End_of_file -> ()
  in
  loop ()

let read_body ~clock ~t_head ~on_chunk framing r =
  let last = ref t_head and total = ref 0 in
  let deliver s =
    let now = Eio.Time.now clock in
    let dt_ms = int_of_float ((now -. !last) *. 1000.) in
    last := now;
    total := !total + String.length s;
    on_chunk ~dt_ms s
  in
  match
    match framing with
    | Fixed n -> copy_bytes r n deliver
    | Chunked -> read_chunked r deliver
    | Until_close -> read_until_close r deliver
  with
  | () -> Ok ()
  | exception Dropped reason -> Error (Truncated { bytes = !total; reason })
  | exception (Eio.Io _ as e) ->
      Error (Truncated { bytes = !total; reason = Printexc.to_string e })
  | exception Eio.Buf_read.Buffer_limit_exceeded ->
      Error (Truncated { bytes = !total; reason = "chunk header too large" })

let request_text (req : request) ~host =
  let target = match Uri.path_and_query req.url with "" -> "/" | t -> t in
  let head =
    Printf.sprintf
      "%s %s HTTP/1.1\r\n\
       Host: %s\r\n\
       Connection: close\r\n\
       Accept-Encoding: identity\r\n\
       Content-Length: %d\r\n\
       %s\r\n"
      req.meth target host (String.length req.body)
      (String.concat ""
         (List.map
            (fun (k, v) -> Printf.sprintf "%s: %s\r\n" k v)
            (request_headers req.headers)))
  in
  head ^ req.body

let run ~net ~clock connector (req : request) ~on_head ~on_chunk =
  match Uri.host req.url with
  | None -> Error (Connect_failed "url has no host")
  | Some host -> (
      let https =
        Option.equal String.equal (Uri.scheme req.url) (Some "https")
      in
      let port =
        Option.value (Uri.port req.url) ~default:(if https then 443 else 80)
      in
      let host_header =
        match Uri.port req.url with
        | Some p -> Printf.sprintf "%s:%d" host p
        | None -> host
      in
      let sent = ref false in
      let failed reason =
        Error
          (if !sent then Failed_after_send reason else Connect_failed reason)
      in
      try
        Eio.Net.with_tcp_connect ~host ~service:(string_of_int port) net
          (fun socket ->
            let flow =
              let raw =
                (socket
                  :> [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r)
              in
              match connector with Some wrap -> wrap req.url raw | None -> raw
            in
            Eio.Flow.copy_string (request_text req ~host:host_header) flow;
            sent := true;
            let r = Eio.Buf_read.of_flow ~max_size:1_048_576 flow in
            match read_head r with
            | Error _ as e -> e
            | Ok (status, raw_headers) ->
                let t_head = Eio.Time.now clock in
                on_head { status; headers = allowed_headers raw_headers };
                read_body ~clock ~t_head ~on_chunk
                  (framing ~status raw_headers)
                  r)
      with
      | Eio.Io _ as e -> failed (Printexc.to_string e)
      | End_of_file -> failed "connection closed before the response head"
      | Eio.Buf_read.Buffer_limit_exceeded -> failed "response head too large")

let call ~sw:_ ~net ~clock ~timeout connector (req : request) ~on_head ~on_chunk
    =
  match (Uri.scheme req.url, connector) with
  | Some "https", None -> Error (Connect_failed "https needs a TLS connector")
  | _ -> (
      match
        Eio.Time.with_timeout clock timeout (fun () ->
            Ok (run ~net ~clock connector req ~on_head ~on_chunk))
      with
      | Ok r -> r
      | Error `Timeout -> Error Timed_out)
