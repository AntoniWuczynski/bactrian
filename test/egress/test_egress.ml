(* Egress interception: the pure policy and keys, the project CA, and the
   CONNECT proxy end to end in one process, with a fake HTTPS server behind a
   CA minted for the test. *)

open Bactrian_core
open Bactrian_runtime
open Proxy_support
open Egress_world

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
  Alcotest.(check bool)
    "no staged file is left" false
    (Sys.file_exists (Filename.concat dir (Ca.key_file ^ ".new")));
  let pem = Filename.concat dir Ca.certificate_file in
  let old = In_channel.with_open_bin pem In_channel.input_all in
  ignore (Ca.rotate ~dir ~now:(now ()));
  Out_channel.with_open_bin pem (fun oc -> output_string oc old);
  (match Ca.load_or_create ~dir ~now:(now ()) with
  | Error m ->
      Alcotest.(check bool)
        "a pair that does not match names rotate" true
        (Re.execp (Re.compile (Re.str "ca rotate")) m)
  | Ok _ -> Alcotest.fail "a key and another CA's certificate are refused");
  ignore (Ca.rotate ~dir ~now:(now ()));
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
  let leaf ?(sim_now = now ()) host =
    match Ca.leaf ca ~now:(now ()) ~sim_now ~host with
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
    (Result.is_error (Ca.leaf ca ~now:(now ()) ~sim_now:(now ()) ~host:"a b"));
  let leaves =
    Eio.Fiber.List.map
      (fun () -> leaf "same.example.com")
      (List.init 8 (fun _ -> ()))
  in
  Alcotest.(check bool)
    "minted once for concurrent requests" true
    (List.for_all (fun c -> List.hd c == List.hd (List.hd leaves)) leaves);
  (* An agent under libfaketime checks the chain at the run's SIM_NOW: a
     recording's, or a scenario's fixed date, long before the CA was made. *)
  let days_ago d =
    Option.get (Ptime.sub_span (now ()) (Ptime.Span.of_int_s (d * 86_400)))
  in
  let at time chain =
    X509.Validation.verify_chain_of_trust
      ~host:
        (Some (Domain_name.host_exn (Domain_name.of_string_exn "past.example")))
      ~time:(fun () -> Some time)
      ~anchors:[ ca_cert ca ]
      chain
  in
  List.iter
    (fun d ->
      let chain = leaf ~sim_now:(days_ago d) "past.example" in
      List.iter
        (fun (what, time) ->
          match at time chain with
          | Ok _ -> ()
          | Error e ->
              Alcotest.failf "SIM_NOW %d days back, valid %s: %a" d what
                X509.Validation.pp_validation_error e)
        [ ("then", days_ago d); ("now", now ()) ])
    [ 30; 3 * 365 ];
  let recent = leaf ~sim_now:(days_ago 30) "past.example" in
  Alcotest.(check bool)
    "reused for the same SIM_NOW" true
    (List.hd recent == List.hd (leaf ~sim_now:(days_ago 30) "past.example"))

(* The proxy *)

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

(* Two tool calls at once, each fetching a URL: the slow one is recorded after
   the fast one, and a strict replay that asks in the other order must still
   serve both (design §6.3). *)
let fast_done, fast_answered = Eio.Promise.create ()

let test_parallel_fetches =
  let handler (r : F.request) =
    if String.equal r.path "/slow" then Eio.Promise.await fast_done;
    backend r
  in
  with_world ~handler "parallel" @@ fun w ->
  let p = policy [ ("127.0.0.1", Record) ] in
  let run = register w ~token:token_a (Some p) in
  let get ~token path =
    (through ~creds:(creds token) w ~meth:"GET" ~path ()).status
  in
  let slow, fast =
    Eio.Fiber.pair
      (fun () -> get ~token:token_a "/slow")
      (fun () ->
        let status = get ~token:token_a "/fast" in
        Eio.Promise.resolve fast_answered ();
        status)
  in
  Alcotest.(check (pair int int)) "both answered" (200, 200) (slow, fast);
  ignore (Model_proxy.unregister w.ctx.proxy ~token:token_a);
  let recorded = recording w.ctx run in
  Alcotest.(check (list string))
    "recorded as they finished" [ "/fast"; "/slow" ]
    (List.map (fun (e : Cassette.exchange) -> e.path) recorded.exchanges);
  let replay =
    Model_proxy.Replay
      {
        recording = recorded;
        matcher = Strict;
        allow_reorder = false;
        timing = Fast;
      }
  in
  let _ =
    register w ~connector:Upstream.plain ~mode:replay ~token:token_c (Some p)
  in
  let slow = get ~token:token_c "/slow" in
  let fast = get ~token:token_c "/fast" in
  Alcotest.(check (pair int int)) "both replayed" (200, 200) (slow, fast);
  let f = Model_proxy.unregister w.ctx.proxy ~token:token_c in
  Alcotest.(check int) "no divergence" 0 (List.length f.divergences)

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
          Alcotest.test_case "parallel fetches, replayed in another order"
            `Quick test_parallel_fetches;
          Alcotest.test_case "passthrough and replay policies" `Quick
            test_passthrough_and_replay_policy;
        ] );
    ]
