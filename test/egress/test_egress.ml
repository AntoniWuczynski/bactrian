(* Egress interception: the pure policy and keys, the project CA, and the
   CONNECT proxy end to end in one process, with a fake HTTPS server behind a
   CA minted for the test. *)

open Bactrian_core
open Bactrian_runtime
open Proxy_support
module E = Egress
module F = Fake_upstream
module Ca = Bactrian_tls.Ca

let () = Mirage_crypto_rng_unix.use_default ()

(* Pure *)

let test_authority () =
  let ok s =
    match E.authority s with Ok v -> v | Error m -> Alcotest.fail m
  in
  let pair = Alcotest.(pair string int) in
  Alcotest.check pair "name" ("api.example.com", 443) (ok "API.Example.com:443");
  Alcotest.check pair "address" ("127.0.0.1", 8443) (ok "127.0.0.1:8443");
  Alcotest.check pair "IPv6" ("::1", 9) (ok "[::1]:9");
  List.iter
    (fun bad ->
      Alcotest.(check bool) bad true (Result.is_error (E.authority bad)))
    [
      "example.com";
      "example.com:";
      ":443";
      "h:0";
      "h:65536";
      "h:+1";
      "[::1]";
      "::1:443";
      "[]:1";
    ];
  Alcotest.(check string) "host_port" "[::1]:9" (E.host_port "::1" 9)

let test_policy () =
  let t : E.t =
    {
      default = Block;
      hosts =
        [ ("Api.Example.com", Record); ("api.example.com:8443", Passthrough) ];
    }
  in
  let name p = Experiment.policy_name p in
  let at host port = name (E.policy t ~host ~port) in
  Alcotest.(check string) "by host" "record" (at "api.example.com" 443);
  Alcotest.(check string)
    "host:port wins" "passthrough"
    (at "api.example.com" 8443);
  Alcotest.(check string) "default" "block" (at "other.example.com" 443);
  Alcotest.(check string) "case" "record" (at "API.EXAMPLE.COM" 443);
  match E.of_json (E.to_json t) with
  | Ok back ->
      Alcotest.(check string)
        "json round trip"
        (Yojson.Safe.to_string (E.to_json t))
        (Yojson.Safe.to_string (E.to_json back))
  | Error m -> Alcotest.fail m

let test_flags () =
  (match
     E.of_flags ~default:None
       [ "a.example=record"; "b.example:8443=passthrough" ]
   with
  | Ok t ->
      Alcotest.(check string)
        "flags"
        {|{"default":"block","hosts":{"a.example":"record","b.example:8443":"passthrough"}}|}
        (Yojson.Safe.to_string (E.to_json t))
  | Error m -> Alcotest.fail m);
  List.iter
    (fun (default, flags) ->
      Alcotest.(check bool)
        "refused" true
        (Result.is_error (E.of_flags ~default flags)))
    [
      (None, [ "a.example" ]);
      (None, [ "=record" ]);
      (None, [ "a=allow" ]);
      (Some "open", []);
    ]

let test_canonical () =
  let c body ct =
    Canon.to_string
      (E.canonical ~authority:"h:443" ~meth:"POST" ~path:"/p" ~query:[]
         ~content_type:ct body)
  in
  Alcotest.(check string)
    "JSON is canonical"
    (c {|{"b":1,"a":2}|} (Some "application/json"))
    (c {|{ "a": 2, "b": 1 }|} (Some "application/json"));
  Alcotest.(check bool)
    "bytes are hashed" true
    (let s = c "\x00\x01" None in
     String.length s < 200 && not (String.equal s (c "\x00\x02" None)));
  Alcotest.(check string)
    "an empty body is null"
    {|{"authority":"h:443","body":null,"content-type":null,"method":"POST","path":"/p","query":[]}|}
    (c "" None)

let test_node_versions () =
  List.iter
    (fun (v, expected) ->
      Alcotest.(check (result bool string)) v (Ok expected) (E.node_env_proxy v))
    [
      ("v24.0.0", true);
      ("v24.3.0", true);
      ("v25.1.0", true);
      ("v22.21.0", true);
      ("v22.20.9", false);
      ("v23.11.1", false);
      ("v20.19.0", false);
    ];
  Alcotest.(check bool)
    "garbage" true
    (Result.is_error (E.node_env_proxy "node"))

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

let test_ca_files =
  with_dir "files" @@ fun dir ->
  let ca, origin = load dir in
  Alcotest.(check bool) "created" true (origin = Ca.Created);
  Alcotest.(check int) "directory 0700" 0o700 (mode dir);
  Alcotest.(check int) "key 0600" 0o600 (mode (Filename.concat dir Ca.key_file));
  Alcotest.(check int)
    "certificate 0644" 0o644
    (mode (Filename.concat dir Ca.certificate_file));
  let days =
    Ptime.Span.to_float_s (Ptime.diff (Ca.not_after ca) (now ())) /. 86_400.
  in
  Alcotest.(check bool) "valid for a year" true (days > 364. && days < 366.);
  let again, origin = load dir in
  Alcotest.(check bool) "loaded" true (origin = Ca.Loaded);
  Alcotest.(check string)
    "the same CA" (Ca.fingerprint ca) (Ca.fingerprint again);
  let rotated =
    match Ca.rotate ~dir ~now:(now ()) with
    | Ok c -> c
    | Error m -> Alcotest.fail m
  in
  Alcotest.(check bool)
    "rotate makes another" false
    (String.equal (Ca.fingerprint ca) (Ca.fingerprint rotated));
  Alcotest.(check int)
    "key still 0600" 0o600
    (mode (Filename.concat dir Ca.key_file));
  Unix.chmod (Filename.concat dir Ca.key_file) 0o644;
  (match Ca.load_or_create ~dir ~now:(now ()) with
  | Error m ->
      Alcotest.(check bool)
        "names chmod" true
        (Re.execp (Re.compile (Re.str "chmod")) m)
  | Ok _ -> Alcotest.fail "a key others can read is refused");
  Unix.chmod (Filename.concat dir Ca.key_file) 0o600;
  Sys.remove (Filename.concat dir Ca.certificate_file);
  Alcotest.(check bool)
    "one file alone is refused" true
    (Result.is_error (Ca.load_or_create ~dir ~now:(now ())));
  let late =
    Option.get (Ptime.add_span (now ()) (Ptime.Span.of_int_s (400 * 86_400)))
  in
  ignore (Ca.rotate ~dir ~now:(now ()));
  match Ca.load_or_create ~dir ~now:late with
  | Error m ->
      Alcotest.(check bool)
        "expired names rotate" true
        (Re.execp (Re.compile (Re.str "ca rotate")) m)
  | Ok _ -> Alcotest.fail "an expired CA is refused"

let test_ca_leaves =
  with_dir "leaves" @@ fun dir ->
  let ca, _ = load dir in
  let leaf host =
    match Ca.leaf ca ~now:(now ()) ~host with
    | Ok (c, _) -> c
    | Error m -> Alcotest.fail m
  in
  let chain = leaf "api.example.com" in
  (match verify ca ~host:"api.example.com" chain with
  | Ok _ -> ()
  | Error e ->
      Alcotest.failf "verifies for its host: %a"
        X509.Validation.pp_validation_error e);
  Alcotest.(check bool)
    "not for another" true
    (Result.is_error (verify ca ~host:"other.example.com" chain));
  let first = List.hd chain in
  let from, until = X509.Certificate.validity first in
  let span a b = Ptime.Span.to_float_s (Ptime.diff a b) /. 86_400. in
  Alcotest.(check bool) "back-dated two days" true (span (now ()) from > 1.9);
  Alcotest.(check bool) "valid seven days" true (span until (now ()) > 6.9);
  let ip_chain = leaf "127.0.0.1" in
  Alcotest.(check bool)
    "an address leaf" true
    (Result.is_ok
       (X509.Validation.verify_chain_of_trust
          ~ip:(Ipaddr.of_string_exn "127.0.0.1")
          ~host:None
          ~time:(fun () -> Some (now ()))
          ~anchors:[ ca_cert ca ]
          ip_chain));
  Alcotest.(check bool)
    "a bad host" true
    (Result.is_error (Ca.leaf ca ~now:(now ()) ~host:"a b"));
  let leaves =
    Eio.Fiber.List.map
      (fun () -> leaf "same.example.com")
      (List.init 8 (fun _ -> ()))
  in
  Alcotest.(check bool)
    "minted once for concurrent requests" true
    (List.for_all (fun c -> List.hd c == List.hd (List.hd leaves)) leaves)

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

let with_world name f =
  with_ctx name @@ fun ctx ->
  let up = fake ctx (F.Handler backend) in
  let test_ca = Fake_https.ca () in
  let https =
    Fake_https.port
      (Fake_https.start ~sw:ctx.sw ~net:(Eio.Stdenv.net ctx.env)
         ~chain:(Fake_https.leaf test_ca [ Ip "127.0.0.1"; Dns "localhost" ])
         ~backend:(F.port up) ())
  in
  let project_ca, _ = load (Eio.Path.native_exn Eio.Path.(ctx.dir / "ca")) in
  f { ctx; up; https; connector = Fake_https.connector test_ca; project_ca }

let policy hosts : E.t = { default = Block; hosts }

let register w ?connector ?(mode = Model_proxy.Record) ?(record = true) ~token
    egress =
  let run =
    {
      (make_run
         ~connector:(Option.value connector ~default:w.connector)
         ~mode ~record w.ctx ~token ~upstream:"http://127.0.0.1:9")
      with
      egress =
        Option.map
          (fun p -> (p, Bactrian_tls.Egress_tls.tls w.project_ca))
          egress;
    }
  in
  Model_proxy.register w.ctx.proxy run;
  run

type answer = { status : int; body : string }

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
  | None -> { status; body = "" }
  | Some g ->
      let stop = Re.Group.stop g 0 in
      let head = String.lowercase_ascii (String.sub text 0 stop) in
      let rest = String.sub text stop (String.length text - stop) in
      {
        status;
        body =
          (if Re.execp (Re.compile (Re.str "chunked")) head then dechunk rest
           else rest);
      }

let request_text ~meth ~path ?(body = "") () =
  Printf.sprintf
    "%s %s HTTP/1.1\r\n\
     Host: 127.0.0.1\r\n\
     Content-Type: application/json\r\n\
     Content-Length: %d\r\n\
     Connection: close\r\n\
     \r\n\
     %s"
    meth path (String.length body) body

(* An agent's HTTPS call through the proxy. [creds] is the Proxy-Authorization
   user and password, none when [None]. *)
let through ?(pipelined = false) ?creds ?(target_host = "127.0.0.1") w ~meth
    ~path ?body () =
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
              ~time:(fun () -> Some (now ()))
              [ ca_cert w.project_ca ])
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
  | Error status -> { status; body = "" }
  | Ok flow ->
      let tls =
        match Ipaddr.of_string target_host with
        | Ok _ -> Tls_eio.client_of_flow tls_config ~ip flow
        | Error _ ->
            Tls_eio.client_of_flow tls_config
              ~host:
                (Domain_name.host_exn (Domain_name.of_string_exn target_host))
              flow
      in
      Eio.Flow.copy_string (request_text ~meth ~path ?body ()) tls;
      let text =
        try Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:1_000_000 tls)
        with End_of_file -> ""
      in
      split_response text

(* The status of a refused CONNECT. *)
let connect_status ?creds w ~host =
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

let test_auth =
  with_world "auth" @@ fun w ->
  let _ = register w ~token:token_a (Some (policy [ ("127.0.0.1", Record) ])) in
  let _ = register w ~token:token_b None in
  let status, headers = connect_status w ~host:"127.0.0.1" in
  Alcotest.(check int) "no credentials: 407" 407 status;
  Alcotest.(check bool)
    "with a challenge" true
    (List.exists
       (String.starts_with ~prefix:"proxy-authenticate: basic")
       headers);
  let status, _ =
    connect_status ~creds:(run_id_of token_a, token_c) w ~host:"127.0.0.1"
  in
  Alcotest.(check int) "unknown token: 403" 403 status;
  let status, _ =
    connect_status ~creds:(run_id_of token_b, token_a) w ~host:"127.0.0.1"
  in
  Alcotest.(check int) "another run's ID: 403" 403 status;
  let status, _ = connect_status ~creds:(creds token_b) w ~host:"127.0.0.1" in
  Alcotest.(check int) "a run without egress: 403" 403 status;
  Alcotest.(check int)
    "nothing reached the server" 0
    (List.length (F.requests w.up));
  let f = Model_proxy.unregister w.ctx.proxy ~token:token_a in
  Alcotest.check call "no call is attributed to the run" [] (flat (calls f));
  ignore (Model_proxy.unregister w.ctx.proxy ~token:token_b)

let test_blocked =
  with_world "blocked" @@ fun w ->
  let _ = register w ~token:token_a (Some (policy [ ("localhost", Record) ])) in
  let status, _ = connect_status ~creds:(creds token_a) w ~host:"127.0.0.1" in
  Alcotest.(check int) "403" 403 status;
  Alcotest.(check int)
    "nothing reached the server" 0
    (List.length (F.requests w.up));
  let f = Model_proxy.unregister w.ctx.proxy ~token:token_a in
  Alcotest.check call "the attempt is a call"
    [ (("CONNECT", "127.0.0.1"), ("", 403)) ]
    (flat (calls f));
  Alcotest.(check int) "not an exchange" 0 f.exchanges

let test_record_then_replay =
  with_world "record-replay" @@ fun w ->
  let p = policy [ ("127.0.0.1", Record) ] in
  let run = register w ~token:token_a (Some p) in
  let a =
    through ~creds:(creds token_a) w ~meth:"GET" ~path:"/weather?city=Paris" ()
  in
  Alcotest.(check int) "GET answered" 200 a.status;
  Alcotest.(check string)
    "GET body" {|{"path":"/weather","query":"city=Paris","body":""}|} a.body;
  let b =
    through ~pipelined:true ~creds:(creds token_a) w ~meth:"POST" ~path:"/echo"
      ~body:{|{"x": 1}|} ()
  in
  Alcotest.(check int) "pipelined POST answered" 200 b.status;
  Alcotest.(check string)
    "POST body" {|{"path":"/echo","query":"","body":"{\"x\": 1}"}|} b.body;
  let f = Model_proxy.unregister w.ctx.proxy ~token:token_a in
  Alcotest.(check int) "two exchanges" 2 f.exchanges;
  Alcotest.check call "two calls"
    [
      (("GET", "127.0.0.1"), ("/weather", 200));
      (("POST", "127.0.0.1"), ("/echo", 200));
    ]
    (flat (calls f));
  let recorded = recording w.ctx run in
  Alcotest.(check (list string))
    "on the http channel" [ "http"; "http" ]
    (List.map (fun (e : Cassette.exchange) -> e.protocol) recorded.exchanges);
  let served = List.length (F.requests w.up) in
  let replay =
    Model_proxy.Replay
      {
        recording = recorded;
        matcher = Strict;
        allow_reorder = false;
        timing = Fast;
      }
  in
  (* A connector that cannot reach anything: a replay must not try. *)
  let _ =
    register w ~connector:Upstream.plain ~mode:replay ~token:token_c (Some p)
  in
  let a' =
    through ~creds:(creds token_c) w ~meth:"GET" ~path:"/weather?city=Paris" ()
  in
  Alcotest.(check (pair int string))
    "replayed GET" (a.status, a.body) (a'.status, a'.body);
  let b' =
    through ~creds:(creds token_c) w ~meth:"POST" ~path:"/echo"
      ~body:{|{ "x" : 1 }|} ()
  in
  Alcotest.(check (pair int string))
    "replayed POST, JSON canonical" (b.status, b.body) (b'.status, b'.body);
  let miss =
    through ~creds:(creds token_c) w ~meth:"GET" ~path:"/weather?city=Rome" ()
  in
  Alcotest.(check int) "a miss: 409" 409 miss.status;
  Alcotest.(check bool)
    "bactrian_replay_miss" true
    (Re.execp (Re.compile (Re.str "bactrian_replay_miss")) miss.body);
  let f = Model_proxy.unregister w.ctx.proxy ~token:token_c in
  Alcotest.(check int)
    "the server was not called" served
    (List.length (F.requests w.up));
  Alcotest.(check int) "one divergence" 1 (List.length f.divergences);
  Alcotest.check call "every call in the trace"
    [
      (("GET", "127.0.0.1"), ("/weather", 200));
      (("POST", "127.0.0.1"), ("/echo", 200));
      (("GET", "127.0.0.1"), ("/weather", 409));
    ]
    (flat (calls f))

let test_passthrough_and_replay_policy =
  with_world "passthrough" @@ fun w ->
  let run =
    register w ~token:token_a
      (Some (policy [ ("127.0.0.1", Passthrough); ("localhost", Replay) ]))
  in
  let a = through ~creds:(creds token_a) w ~meth:"GET" ~path:"/x" () in
  Alcotest.(check int) "passed through" 200 a.status;
  let r =
    through ~target_host:"localhost" ~creds:(creds token_a) w ~meth:"GET"
      ~path:"/x" ()
  in
  Alcotest.(check int) "replay without a recording: 409" 409 r.status;
  let _ = Model_proxy.unregister w.ctx.proxy ~token:token_a in
  Alcotest.(check int)
    "only the passthrough reached the server" 1
    (List.length (F.requests w.up));
  Alcotest.(check int)
    "nothing recorded" 0
    (List.length (recording w.ctx run).exchanges)

let () =
  Alcotest.run "egress"
    [
      ( "pure",
        [
          Alcotest.test_case "CONNECT authority" `Quick test_authority;
          Alcotest.test_case "per-host policy" `Quick test_policy;
          Alcotest.test_case "record flags" `Quick test_flags;
          Alcotest.test_case "canonical request" `Quick test_canonical;
          Alcotest.test_case "Node versions" `Quick test_node_versions;
        ] );
      ( "ca",
        [
          Alcotest.test_case "files, modes, rotation and expiry" `Quick
            test_ca_files;
          Alcotest.test_case "leaves" `Quick test_ca_leaves;
        ] );
      ( "proxy",
        [
          Alcotest.test_case "authentication" `Quick test_auth;
          Alcotest.test_case "a blocked host" `Quick test_blocked;
          Alcotest.test_case "record, then strict replay" `Quick
            test_record_then_replay;
          Alcotest.test_case "passthrough and replay policies" `Quick
            test_passthrough_and_replay_policy;
        ] );
    ]
