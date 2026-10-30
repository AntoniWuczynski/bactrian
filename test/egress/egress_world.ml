(* What the egress tests share: the project CA in a test directory, and a
   model proxy whose runs intercept egress to a fake HTTPS server, with an
   agent's HTTPS calls through it. *)

open Bactrian_core
open Bactrian_runtime
open Proxy_support
module E = Egress
module F = Fake_upstream
module Ca = Bactrian_tls.Ca

(* The project CA *)
let with_dir name f () =
  let dir = Filename.concat (Sys.getcwd ()) ("tmp-ca-" ^ name) in
  ignore (Sys.command ("rm -rf " ^ Filename.quote dir));
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command ("rm -rf " ^ Filename.quote dir)))
  @@ fun () ->
  Eio_main.run @@ fun _ -> f (Filename.concat dir "ca")

let now () = Ptime_clock.now ()
let mode path = (Unix.stat path).st_perm

let load dir =
  match Ca.load_or_create ~dir ~now:(now ()) with
  | Ok v -> v
  | Error m -> Alcotest.fail m

let ca_cert ca =
  match X509.Certificate.decode_pem (Ca.certificate_pem ca) with
  | Ok c -> c
  | Error (`Msg m) -> Alcotest.fail m

let verify ca ~host chain =
  let host =
    Result.to_option (Result.bind (Domain_name.of_string host) Domain_name.host)
  in
  (* The chain as a server sends it, the CA last, as tls verifies it. *)
  X509.Validation.verify_chain_of_trust ~host
    ~time:(fun () -> Some (now ()))
    ~anchors:[ ca_cert ca ]
    chain

(* The proxy *)

type world = {
  ctx : ctx;
  up : F.t;  (** The plain backend behind the fake HTTPS front. *)
  https : int;  (** The fake HTTPS server's port. *)
  connector : Upstream.connector;  (** Trusts the fake server's CA. *)
  project_ca : Ca.t;
}

let backend (r : F.request) : F.reply =
  {
    status = 200;
    headers = [ ("content-type", "application/json") ];
    chunks =
      [
        Printf.sprintf {|{"path":%S,"query":%S,"body":%S}|} r.path r.query
          r.body;
      ];
    ending = Complete;
  }

(* A backend that answers each connection with the bytes [answer] gives for
   its request line, then closes it: for heads no HTTP library would send. *)
let raw_backend ctx answer =
  let socket =
    Eio.Net.listen ~sw:ctx.sw ~backlog:8 ~reuse_addr:true
      (Eio.Stdenv.net ctx.env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  Eio.Fiber.fork_daemon ~sw:ctx.sw (fun () ->
      Eio.Net.run_server socket ~on_error:raise (fun flow _ ->
          let r = Eio.Buf_read.of_flow ~max_size:65_536 flow in
          let line = Eio.Buf_read.line r in
          let rec skip () =
            if not (String.equal (Eio.Buf_read.line r) "") then skip ()
          in
          skip ();
          Eio.Flow.copy_string (answer line) flow));
  match Eio.Net.listening_addr socket with
  | `Tcp (_, port) -> port
  | `Unix _ -> Alcotest.fail "a TCP listener"

(* [handler] answers behind the fake HTTPS front, or [raw] does when given. *)
let with_world ?(handler = backend) ?raw ?max_body name f =
  with_ctx ?max_body name @@ fun ctx ->
  let up = fake ctx (F.Handler handler) in
  let test_ca = Fake_https.ca () in
  let https =
    Fake_https.port
      (Fake_https.start ~sw:ctx.sw ~net:(Eio.Stdenv.net ctx.env)
         ~chain:(Fake_https.leaf test_ca [ Ip "127.0.0.1"; Dns "localhost" ])
         ~backend:
           (match raw with
           | Some answer -> raw_backend ctx answer
           | None -> F.port up)
         ())
  in
  let project_ca, _ = load (Eio.Path.native_exn Eio.Path.(ctx.dir / "ca")) in
  f { ctx; up; https; connector = Fake_https.connector test_ca; project_ca }

let policy hosts : E.t = { default = Block; hosts }

let register w ?connector ?(mode = Model_proxy.Record) ?(record = true) ?redact
    ?(sim_now = "") ~token egress =
  let run =
    {
      (make_run
         ~connector:(Option.value connector ~default:w.connector)
         ~mode ~record ?redact w.ctx ~token ~upstream:"http://127.0.0.1:9")
      with
      egress =
        Option.map
          (fun p -> (p, Bactrian_tls.Egress_tls.tls w.project_ca ~sim_now))
          egress;
    }
  in
  Model_proxy.register w.ctx.proxy run;
  run

type answer = {
  status : int;
  head : string;  (** The status line and headers, lowercased. *)
  body : string;
  ended : bool;  (** A chunked body had its terminal chunk. *)
}

(* A flow that sends the CONNECT head with its first write and skips the
   proxy's 200 head on its first read, so the ClientHello travels in the same
   write as the CONNECT. *)
module Pipelined = struct
  type t = {
    socket : [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t;
    reader : Eio.Buf_read.t;
    mutable head : string option;
    mutable skipped : bool;
  }

  let read_methods = []

  let single_read t buf =
    if not t.skipped then (
      t.skipped <- true;
      let rec skip () =
        if not (String.equal (Eio.Buf_read.line t.reader) "") then skip ()
      in
      skip ());
    Eio.Buf_read.ensure t.reader 1;
    let available = Eio.Buf_read.peek t.reader in
    let n = Int.min (Cstruct.length buf) (Cstruct.length available) in
    Cstruct.blit available 0 buf 0 n;
    Eio.Buf_read.consume t.reader n;
    n

  let single_write t bufs =
    let data = Cstruct.copyv bufs in
    let out =
      match t.head with
      | Some h ->
          t.head <- None;
          h ^ data
      | None -> data
    in
    Eio.Flow.copy_string out t.socket;
    String.length data

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown t cmd = Eio.Flow.shutdown t.socket cmd

  let handler =
    Eio.Resource.handler
      (H (Eio.Resource.Close, fun _ -> ())
      :: Eio.Resource.bindings
           (Eio.Flow.Pi.two_way
              (module struct
                type nonrec t = t

                let read_methods = read_methods
                let single_read = single_read
                let single_write = single_write
                let copy = copy
                let shutdown = shutdown
              end)))
end

let dechunk text =
  let rec go acc s =
    match String.index_opt s '\r' with
    | None -> acc
    | Some i -> (
        match int_of_string_opt ("0x" ^ String.sub s 0 i) with
        | Some 0 | None -> acc
        | Some n ->
            go
              (acc ^ String.sub s (i + 2) n)
              (String.sub s (i + 4 + n) (String.length s - i - 4 - n)))
  in
  go "" text

let split_response text =
  let status =
    match String.split_on_char ' ' text with
    | _ :: code :: _ -> Option.value (int_of_string_opt code) ~default:0
    | _ -> 0
  in
  match Re.exec_opt (Re.compile (Re.str "\r\n\r\n")) text with
  | None ->
      { status; head = String.lowercase_ascii text; body = ""; ended = false }
  | Some g ->
      let stop = Re.Group.stop g 0 in
      let head = String.lowercase_ascii (String.sub text 0 stop) in
      let rest = String.sub text stop (String.length text - stop) in
      let chunked = Re.execp (Re.compile (Re.str "chunked")) head in
      {
        status;
        head;
        body = (if chunked then dechunk rest else rest);
        ended = (not chunked) || String.ends_with ~suffix:"\r\n0\r\n\r\n" rest;
      }

let request_text ?(host = "127.0.0.1") ?(headers = []) ?(close = true) ~meth
    ~path ?(body = "") () =
  Printf.sprintf
    "%s %s HTTP/1.1\r\n\
     Host: %s\r\n\
     Content-Type: application/json\r\n\
     Content-Length: %d\r\n\
     Connection: %s\r\n\
     %s\r\n\
     %s"
    meth path host (String.length body)
    (if close then "close" else "keep-alive")
    (String.concat ""
       (List.map (fun (k, v) -> Printf.sprintf "%s: %s\r\n" k v) headers))
    body

(* [f] on an agent's TLS connection through the proxy, or the status of the
   refused CONNECT. [creds] is the Proxy-Authorization user and password, none
   when [None]. *)
let with_tunnel ?(pipelined = false) ?creds ?(target_host = "127.0.0.1")
    ?(at = now) ?anchors w f =
  let net = Eio.Stdenv.net w.ctx.env in
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.connect ~sw net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, Model_proxy.port w.ctx.proxy))
  in
  let target = Printf.sprintf "%s:%d" target_host w.https in
  let head =
    Printf.sprintf "CONNECT %s HTTP/1.1\r\nHost: %s\r\n%s\r\n" target target
      (match creds with
      | Some (u, p) ->
          "Proxy-Authorization: "
          ^ Cohttp.Auth.string_of_credential (`Basic (u, p))
          ^ "\r\n"
      | None -> "")
  in
  let reader = Eio.Buf_read.of_flow ~max_size:1_000_000 socket in
  let tls_config =
    Result.get_ok
      (Tls.Config.client
         ~authenticator:
           (X509.Authenticator.chain_of_trust
              ~time:(fun () -> Some (at ()))
              (Option.value anchors ~default:[ ca_cert w.project_ca ]))
         ())
  in
  let ip = Ipaddr.of_string_exn "127.0.0.1" in
  let pipe ~head ~skipped : Upstream.flow =
    Eio.Resource.T
      ( {
          Pipelined.socket :> [ `Generic ] Eio.Net.stream_socket_ty
                              Eio.Resource.t;
          reader;
          head;
          skipped;
        },
        Pipelined.handler )
  in
  let flow =
    if pipelined then Ok (pipe ~head:(Some head) ~skipped:false)
    else (
      Eio.Flow.copy_string head socket;
      let status_line = Eio.Buf_read.line reader in
      if not (Re.execp (Re.compile (Re.str " 200 ")) (status_line ^ " ")) then
        Error (split_response status_line).status
      else
        let rec skip () =
          if not (String.equal (Eio.Buf_read.line reader) "") then skip ()
        in
        skip ();
        Ok (pipe ~head:None ~skipped:true))
  in
  match flow with
  | Error status -> Error status
  | Ok flow ->
      Ok
        (f
           (match Ipaddr.of_string target_host with
           | Ok _ -> Tls_eio.client_of_flow tls_config ~ip flow
           | Error _ ->
               Tls_eio.client_of_flow tls_config
                 ~host:
                   (Domain_name.host_exn
                      (Domain_name.of_string_exn target_host))
                 flow))

(* An agent's HTTPS call through the proxy, on a tunnel of its own. *)
let through ?pipelined ?creds ?target_host ?host_header ?headers ?at ?anchors w
    ~meth ~path ?body () =
  match
    with_tunnel ?pipelined ?creds ?target_host ?at ?anchors w (fun tls ->
        Eio.Flow.copy_string
          (request_text ?host:host_header ?headers ~meth ~path ?body ())
          tls;
        split_response
          (try
             Eio.Buf_read.take_all
               (Eio.Buf_read.of_flow ~max_size:1_000_000 tls)
           with End_of_file -> ""))
  with
  | Error status -> { status; head = ""; body = ""; ended = false }
  | Ok a -> a

(* One response from [r], chunked or with a length, when several share a
   connection. *)
let read_response r =
  let status_line = Eio.Buf_read.line r in
  let rec head acc =
    match Eio.Buf_read.line r with
    | "" -> List.rev acc
    | l -> head (String.lowercase_ascii l :: acc)
  in
  let lines = head [] in
  let value name =
    let prefix = name ^ ":" in
    let n = String.length prefix in
    List.find_map
      (fun l ->
        if String.starts_with ~prefix l then
          Some (String.trim (String.sub l n (String.length l - n)))
        else None)
      lines
  in
  let body =
    match (value "transfer-encoding", value "content-length") with
    | Some "chunked", _ ->
        let rec chunks acc =
          match
            int_of_string_opt ("0x" ^ String.trim (Eio.Buf_read.line r))
          with
          | Some 0 | None ->
              ignore (Eio.Buf_read.line r : string);
              acc
          | Some n ->
              let piece = Eio.Buf_read.take n r in
              ignore (Eio.Buf_read.line r : string);
              chunks (acc ^ piece)
        in
        chunks ""
    | _, Some n -> Eio.Buf_read.take (int_of_string n) r
    | _, None -> ""
  in
  let status = (split_response status_line).status in
  { status; head = String.concat "\n" lines; body; ended = true }

(* The status of a refused CONNECT. With [until_closed], waits for the proxy to
   end the tunnel, by when it has noted what went wrong inside it. *)
let connect_status ?creds ?(until_closed = false) w ~host =
  let net = Eio.Stdenv.net w.ctx.env in
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.connect ~sw net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, Model_proxy.port w.ctx.proxy))
  in
  let target = Printf.sprintf "%s:%d" host w.https in
  Eio.Flow.copy_string
    (Printf.sprintf "CONNECT %s HTTP/1.1\r\nHost: %s\r\n%s\r\n" target target
       (match creds with
       | Some (u, p) ->
           "Proxy-Authorization: "
           ^ Cohttp.Auth.string_of_credential (`Basic (u, p))
           ^ "\r\n"
       | None -> ""))
    socket;
  let reader = Eio.Buf_read.of_flow ~max_size:1_000_000 socket in
  let status_line = Eio.Buf_read.line reader in
  let rec headers acc =
    match Eio.Buf_read.line reader with
    | "" -> acc
    | l -> headers (String.lowercase_ascii l :: acc)
  in
  let hs = headers [] in
  if until_closed then ignore (Eio.Buf_read.take_all reader);
  ( (match String.split_on_char ' ' status_line with
    | _ :: code :: _ -> int_of_string code
    | _ -> 0),
    hs )

let creds token = (run_id_of token, token)

let calls (f : Model_proxy.findings) =
  List.map
    (fun (c : Trace_builder.http_call) -> (c.meth, c.host, c.path, c.status))
    f.http_calls

let call = Alcotest.(list (pair (pair string string) (pair string int)))
let flat l = List.map (fun (m, h, p, s) -> ((m, h), (p, s))) l
