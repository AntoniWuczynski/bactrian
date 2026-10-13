open Bactrian_core
open Bactrian_runtime
module P = Model_proxy
module F = Fake_upstream

let fixtures_root = "../fixtures/protocols"
let token_a = String.make 32 'a'
let token_b = String.make 32 'b'
let token_c = String.make 32 'c'
let token_d = String.make 32 'd'

let real_keys =
  [
    (P.Openai, "real-openai-key");
    (P.Anthropic, "real-anthropic-key");
    (P.Gemini, "real-gemini-key");
    (P.Openrouter, "real-openrouter-key");
  ]

let caps =
  [
    ("openai", 100.); ("anthropic", 100.); ("gemini", 100.); ("openrouter", 100.);
  ]

let today () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

(* Fixtures *)

type fixture = {
  protocol : string;
  meth : string;
  path : string;
  query : string;
  request_headers : (string * string) list;
  status : int;
  content_type : string;
  request : string;
  raw : string;
}

let read path = In_channel.with_open_bin path In_channel.input_all

let canon s =
  match Canon.of_string s with
  | Ok v -> v
  | Error e -> Alcotest.failf "bad JSON: %a" Canon.pp_error e

let str k v = Option.get (Canon.str_member k v)

let fixture protocol case =
  let file name =
    Filename.concat
      (Filename.concat (Filename.concat fixtures_root protocol) case)
      name
  in
  let meta = canon (read (file "meta.json")) in
  let fields k =
    Option.value ~default:[]
      (Option.bind (Canon.member k meta) Canon.to_fields_opt)
    |> List.map (fun (k, v) -> (k, Option.get (Canon.to_string_opt v)))
  in
  {
    protocol;
    meth = str "method" meta;
    path = str "path" meta;
    query = str "query" meta;
    request_headers = fields "request_headers";
    status = Option.get (Canon.int_member "status" meta);
    content_type = List.assoc "content-type" (fields "response_headers");
    request = read (file "request.json");
    raw = read (file "response.raw");
  }

let provider_of f =
  match f.protocol with
  | "anthropic_messages" -> P.Anthropic
  | "gemini_generate" | "gemini_interactions" -> P.Gemini
  | _ when String.starts_with ~prefix:"/api/" f.path -> P.Openrouter
  | _ -> P.Openai

let segment = function
  | P.Openai -> "openai"
  | P.Anthropic -> "anthropic"
  | P.Gemini -> "gemini"
  | P.Openrouter -> "openrouter"

let key_header provider key =
  match provider with
  | P.Openai | P.Openrouter -> ("authorization", "Bearer " ^ key)
  | P.Anthropic -> ("x-api-key", key)
  | P.Gemini -> ("x-goog-api-key", key)

let adapter f =
  match Protocols.find ~meth:f.meth ~path:f.path with
  | Some a -> a
  | None -> Alcotest.failf "no adapter for %s" f.path

let query_pairs q =
  if String.equal q "" then []
  else
    List.concat_map
      (fun (k, vs) -> List.map (fun v -> (k, v)) vs)
      (Uri.query_of_encoded q)

let expected_key f =
  let (module A : Protocol.S) = adapter f in
  let canonical =
    match A.canonical_request ~path:f.path (canon f.request) with
    | Ok c -> c
    | Error e -> Alcotest.failf "canonical: %a" Protocol.pp_error e
  in
  Matcher.key ~headers:f.request_headers ~meth:f.meth ~path:f.path
    ~query:(query_pairs f.query) canonical

let expected_cost f =
  let (module A : Protocol.S) = adapter f in
  let body : Protocol.body =
    if String.starts_with ~prefix:"text/event-stream" f.content_type then (
      let st, items = Sse.feed Sse.init f.raw in
      (match Sse.finish st with
      | `Clean -> ()
      | `Truncated _ -> Alcotest.fail "fixture stream truncated");
      Sse items)
    else
      let v = canon f.raw in
      match Canon.to_list_opt v with
      | Some items
        when A.streaming ~path:f.path ~query:f.query (canon f.request) ->
          Json_stream items
      | Some _ | None -> Json v
  in
  match A.summarise body with
  | Ok { usage = Some u; _ } ->
      let model =
        str "model"
          (Result.get_ok (A.canonical_request ~path:f.path (canon f.request)))
      in
      Option.get (Pricing.cost ~model ~date:(today ()) u)
  | Ok { usage = None; _ } -> Alcotest.fail "fixture has no usage"
  | Error e -> Alcotest.failf "summarise: %a" Protocol.pp_error e

(* Test context *)

type ctx = {
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  proxy : P.t;
  store : Store.t;
  dir : Eio.Fs.dir_ty Eio.Path.t;
}

let with_ctx name f () =
  Eio_main.run @@ fun env ->
  let dir = Eio.Path.(Eio.Stdenv.cwd env / ("tmp-proxy-" ^ name)) in
  Eio.Path.rmtree ~missing_ok:true dir;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let proxy =
    match
      P.start ~sw ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
    with
    | Ok p -> p
    | Error e -> Alcotest.fail e
  in
  f { env; sw; proxy; store = Store.create ~root:dir; dir }

let fake ctx source =
  match F.start ~sw:ctx.sw ~net:(Eio.Stdenv.net ctx.env) source with
  | Ok f -> f
  | Error e -> Alcotest.failf "fake upstream: %a" F.pp_error e

let store_ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "store: %a" Store.pp_error e

let header run_id : Cassette.header =
  {
    run_id;
    scenario = "s";
    variant = "v";
    bactrian_version = "test";
    recorded_at = "2026-10-13T00:00:00Z";
    sim_now = "2026-10-13T00:00:00Z";
    clock = Trace.Contract_only;
  }

let run_id_of token = "run-" ^ String.sub token 0 4

let open_cassette ctx run_id =
  store_ok (Store.open_cassette ~sw:ctx.sw ctx.store (header run_id))

let make_run ?(mode = P.Record) ?(record = true) ctx ~token ~upstream =
  let run_id = run_id_of token in
  {
    P.run_id;
    token;
    mode;
    store = ctx.store;
    cassette = (if record then Some (open_cassette ctx run_id) else None);
    ledger =
      Ledger.create
        ~path:Eio.Path.(ctx.dir / ("ledger-" ^ run_id ^ ".jsonl"))
        ~caps;
    keys = real_keys;
    upstream = (fun _ -> Uri.of_string upstream);
    connector = Upstream.plain;
    run_started = Eio.Time.now (Eio.Stdenv.clock ctx.env);
  }

let recording ctx (run : P.run) =
  Option.iter Store.close run.cassette;
  store_ok (Store.read_cassette ctx.store ~run_id:run.run_id)

let run_cassette = recording

(* Agent-side client. [Upstream] reports a body cut off mid-stream as
   [Truncated], which cohttp-eio's client does not. *)

type reply = {
  result : (unit, Upstream.error) result;
  status : int;
  headers : (string * string) list;
  body : string;
  arrivals : (float * int) list;  (** Time and total bytes so far. *)
}

let send ctx ?(headers = []) ?seg ~token ~provider ?(query = "") ~meth path body
    =
  let clock = Eio.Stdenv.clock ctx.env in
  let url =
    Printf.sprintf "http://127.0.0.1:%d/r/%s/%s%s%s" (P.port ctx.proxy) token
      (Option.value seg ~default:(segment provider))
      path
      (if String.equal query "" then "" else "?" ^ query)
  in
  let head = ref None and buf = Buffer.create 1024 and arrivals = ref [] in
  let result =
    Eio.Switch.run @@ fun sw ->
    Upstream.call ~sw ~net:(Eio.Stdenv.net ctx.env) ~clock ~timeout:10.
      Upstream.plain
      { meth; url = Uri.of_string url; headers; body }
      ~on_head:(fun h -> head := Some h)
      ~on_chunk:(fun ~dt_ms:_ s ->
        Buffer.add_string buf s;
        arrivals := (Eio.Time.now clock, Buffer.length buf) :: !arrivals)
  in
  let status, headers =
    match !head with
    | Some { Upstream.status; headers } -> (status, headers)
    | None -> (0, [])
  in
  {
    result;
    status;
    headers;
    body = Buffer.contents buf;
    arrivals = List.rev !arrivals;
  }

let send_fixture ?key ?(body_of = Fun.id) ctx ~token f =
  let provider = provider_of f in
  let key = Option.value key ~default:("bactrian-" ^ token) in
  send ctx ~token ~provider ~query:f.query ~meth:f.meth f.path
    ~headers:(key_header provider key :: f.request_headers)
    (body_of f.request)

let check_ok r =
  match r.result with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "client call failed"

let error_type r =
  match Canon.of_string r.body with
  | Ok v ->
      Option.bind (Canon.member "error" v) (Canon.str_member "type")
      |> Option.value ~default:"<none>"
  | Error _ -> "<not json>"

let close_to a b = Float.abs (a -. b) <= 1e-9 *. Float.max 1. (Float.abs b)

let set_model model body =
  Canon.to_string (Canon.set_member "model" (Canon.string model) (canon body))

let only = function
  | [ x ] -> x
  | l -> Alcotest.failf "expected one item, got %d" (List.length l)

let response_of (e : Cassette.exchange) =
  match e.outcome with
  | Response r -> r
  | Transport_error s -> Alcotest.failf "transport error %s" s

let stream_bytes (r : Cassette.response) =
  match r.body with
  | Stream { chunks; _ } -> String.concat "" (List.map snd chunks)
  | Whole (Inline s) -> s
  | Whole (Blob _) -> Alcotest.fail "unexpected blob"

let is_missing = function
  | Matcher.Missing_request _ -> true
  | Unconsumed _ | Reordered _ | Shape_mismatch _ -> false

(* Record mode, one fixture per protocol *)

let record_case (protocol, case) =
  let name = protocol ^ "/" ^ case in
  Alcotest.test_case name `Quick
  @@ with_ctx (protocol ^ "-" ^ case)
  @@ fun ctx ->
  let f = fixture protocol case in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let r = send_fixture ctx ~token:token_a f in
  check_ok r;
  Alcotest.(check int) "status" f.status r.status;
  Alcotest.(check string) "bytes to the agent" f.raw r.body;
  let provider = provider_of f in
  let sent = only (F.requests up) in
  let name, value = key_header provider (List.assoc provider real_keys) in
  Alcotest.(check (option string))
    "real key upstream" (Some value)
    (List.assoc_opt name sent.headers);
  Alcotest.(check bool)
    "no placeholder upstream" false
    (List.exists
       (fun (_, v) ->
         String.starts_with ~prefix:"bactrian-" v
         || String.ends_with ~suffix:token_a v)
       sent.headers);
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "exchanges" 1 findings.exchanges;
  Alcotest.(check int) "usage gaps" 0 findings.usage_gaps;
  let e = only (recording ctx run).exchanges in
  Alcotest.(check string) "key" (expected_key f) e.key;
  Alcotest.(check string) "protocol" protocol e.protocol;
  Alcotest.(check string) "recorded bytes" f.raw (stream_bytes (response_of e));
  let spent = Ledger.spent run.ledger ~provider:(segment provider) in
  let cost = expected_cost f in
  if not (close_to spent cost) then
    Alcotest.failf "ledger spent %.12f, expected %.12f" spent cost

let record_cases =
  List.map record_case
    [
      ("openai_chat", "luna_nonstream");
      ("openai_responses", "luna_stream");
      ("anthropic_messages", "haiku_stream");
      ("gemini_generate", "flash_stream_json");
      ("gemini_interactions", "flash_stream");
    ]

(* Streaming arrives event by event *)

let slow_upstream ctx ~chunks ~gap =
  let sock =
    Eio.Net.listen ~sw:ctx.sw ~backlog:16 ~reuse_addr:true
      (Eio.Stdenv.net ctx.env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr sock with
    | `Tcp (_, p) -> p
    | `Unix _ -> Alcotest.fail "unix listener"
  in
  Eio.Fiber.fork_daemon ~sw:ctx.sw (fun () ->
      Eio.Net.run_server sock
        ~on_error:(fun _ -> ())
        (fun flow _ ->
          let r = Eio.Buf_read.of_flow ~max_size:1_000_000 flow in
          let rec head len =
            match Eio.Buf_read.line r with
            | "" -> len
            | line -> (
                match String.split_on_char ':' line with
                | k :: v :: _
                  when String.equal (String.lowercase_ascii k) "content-length"
                  ->
                    head (int_of_string (String.trim v))
                | _ -> head len)
          in
          ignore (Eio.Buf_read.take (head 0) r);
          Eio.Flow.copy_string
            "HTTP/1.1 200 OK\r\n\
             content-type: text/event-stream\r\n\
             transfer-encoding: chunked\r\n\
             \r\n"
            flow;
          List.iter
            (fun c ->
              Eio.Flow.copy_string
                (Printf.sprintf "%x\r\n%s\r\n" (String.length c) c)
                flow;
              Eio.Time.sleep (Eio.Stdenv.clock ctx.env) gap)
            chunks;
          Eio.Flow.copy_string "0\r\n\r\n" flow));
  Printf.sprintf "http://127.0.0.1:%d" port

(* Seconds between the moments each event was complete at the agent. *)
let event_gaps r events =
  let ends =
    List.fold_left_map
      (fun acc e ->
        let n = acc + String.length e in
        (n, n))
      0 events
    |> snd
  in
  let times =
    List.map
      (fun n -> fst (List.find (fun (_, total) -> total >= n) r.arrivals))
      ends
  in
  let rec gaps = function
    | a :: (b :: _ as rest) -> (b -. a) :: gaps rest
    | [ _ ] | [] -> []
  in
  gaps times

let streaming =
  Alcotest.test_case "stream reaches the agent event by event" `Quick
  @@ with_ctx "timing"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_stream" in
  let events =
    List.init 5 (fun i -> Printf.sprintf "data: event-%d\n\n" (i + 1))
  in
  let base = slow_upstream ctx ~chunks:events ~gap:0.1 in
  let run = make_run ctx ~token:token_a ~upstream:base in
  P.register ctx.proxy run;
  let r = send_fixture ctx ~token:token_a f in
  check_ok r;
  Alcotest.(check string) "bytes" (String.concat "" events) r.body;
  List.iter
    (fun g ->
      if g < 0.06 then Alcotest.failf "events arrived %.0f ms apart" (g *. 1000.))
    (event_gaps r events);
  ignore (P.unregister ctx.proxy ~token:token_a);
  match (response_of (only (recording ctx run).exchanges)).body with
  | Stream { chunks; complete } ->
      Alcotest.(check bool) "complete" true complete;
      Alcotest.(check int) "recorded chunks" 5 (List.length chunks);
      List.iteri
        (fun i (dt, _) ->
          if i > 0 && dt < 60 then Alcotest.failf "chunk %d dt_ms %d" i dt)
        chunks
  | Whole _ -> Alcotest.fail "expected a recorded stream"

(* Placeholder keys *)

let placeholder_keys =
  Alcotest.test_case "wrong, missing and ended keys get 403" `Quick
  @@ with_ctx "keys"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let wrong = send_fixture ctx ~token:token_a ~key:("bactrian-" ^ token_b) f in
  Alcotest.(check int) "wrong key" 403 wrong.status;
  let missing =
    send ctx ~token:token_a ~provider:P.Openai ~meth:f.meth f.path
      ~headers:f.request_headers f.request
  in
  Alcotest.(check int) "missing key" 403 missing.status;
  let other_header =
    send ctx ~token:token_a ~provider:P.Openai ~meth:f.meth f.path
      ~headers:(("x-api-key", "bactrian-" ^ token_a) :: f.request_headers)
      f.request
  in
  Alcotest.(check int)
    "key in another provider's header" 403 other_header.status;
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "refused keys" 3 findings.refused_keys;
  let ended = send_fixture ctx ~token:token_a f in
  Alcotest.(check int) "ended run" 403 ended.status;
  let unknown = send_fixture ctx ~token:token_b f in
  Alcotest.(check int) "unknown token" 403 unknown.status;
  Alcotest.(check int) "nothing forwarded" 0 (List.length (F.requests up))

(* Replay *)

let record_all ctx ~token fixtures =
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run = make_run ctx ~token ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  List.iter (fun f -> check_ok (send_fixture ctx ~token f)) fixtures;
  ignore (P.unregister ctx.proxy ~token);
  recording ctx run

let replay_run ?(record = true) ?(matcher = Matcher.Strict) ?(timing = P.Fast)
    ctx ~token ~upstream recording =
  make_run ctx ~token ~upstream ~record
    ~mode:(P.Replay { recording; matcher; allow_reorder = false; timing })

let replay_strict =
  Alcotest.test_case "strict replay serves the recording" `Quick
  @@ with_ctx "replay"
  @@ fun ctx ->
  let fs =
    [
      fixture "openai_chat" "luna_nonstream";
      fixture "anthropic_messages" "haiku_stream";
    ]
  in
  let recording = record_all ctx ~token:token_a fs in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run = replay_run ctx ~token:token_b ~upstream:(F.base_url up) recording in
  P.register ctx.proxy run;
  List.iter
    (fun f ->
      let r = send_fixture ctx ~token:token_b f in
      check_ok r;
      Alcotest.(check int) "status" f.status r.status;
      Alcotest.(check string) "identical bytes" f.raw r.body)
    fs;
  let findings = P.unregister ctx.proxy ~token:token_b in
  Alcotest.(check int) "divergences" 0 (List.length findings.divergences);
  Alcotest.(check int) "exchanges" 2 findings.exchanges;
  let copies = (run_cassette ctx run).exchanges in
  Alcotest.(check int) "served exchanges copied" 2 (List.length copies);
  List.iteri
    (fun i ((c : Cassette.exchange), (o : Cassette.exchange)) ->
      Alcotest.(check int) "fresh seq" i c.seq;
      Alcotest.(check string) "key" o.key c.key;
      Alcotest.(check int) "occurrence" o.occurrence c.occurrence;
      Alcotest.(check bool)
        "canonical" true
        (Canon.equal o.canonical c.canonical);
      Alcotest.(check bool) "request" true (o.request = c.request);
      Alcotest.(check bool) "outcome" true (o.outcome = c.outcome))
    (List.combine copies recording.exchanges);
  Alcotest.(check int) "nothing upstream" 0 (List.length (F.requests up))

let replay_miss =
  Alcotest.test_case "a changed request gets 409 replay miss" `Quick
  @@ with_ctx "miss"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let recording = record_all ctx ~token:token_a [ f ] in
  let recorded_seq = (only recording.exchanges).seq in
  let up = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy
    (replay_run ctx ~token:token_b ~upstream:(F.base_url up) recording);
  let changed body =
    let v = canon body in
    Canon.to_string
      (Canon.set_member "temperature" (Canon.float 0.5 |> Result.get_ok) v)
  in
  let r = send_fixture ctx ~token:token_b ~body_of:changed f in
  Alcotest.(check int) "status" 409 r.status;
  Alcotest.(check string) "type" "bactrian_replay_miss" (error_type r);
  let divergence =
    Option.get
      (Option.bind
         (Canon.member "error" (canon r.body))
         (Canon.member "divergence"))
  in
  Alcotest.(check (option string))
    "kind" (Some "missing_request")
    (Canon.str_member "kind" divergence);
  Alcotest.(check (option int))
    "nearest seq in body" (Some recorded_seq)
    (Canon.int_member "nearest_seq" divergence);
  let findings = P.unregister ctx.proxy ~token:token_b in
  (match List.filter is_missing findings.divergences with
  | [ Matcher.Missing_request { nearest = Some (e, _, _); _ } ] ->
      Alcotest.(check int) "nearest seq" recorded_seq e.seq
  | [ Matcher.Missing_request { nearest = None; _ } ]
  | [ (Unconsumed _ | Reordered _ | Shape_mismatch _) ]
  | [] | _ :: _ :: _ ->
      Alcotest.fail "expected one Missing_request with a nearest exchange");
  Alcotest.(check int) "nothing upstream" 0 (List.length (F.requests up))

let until_divergence =
  Alcotest.test_case "until-divergence goes live and records the tail" `Quick
  @@ with_ctx "tail"
  @@ fun ctx ->
  let recorded = fixture "openai_chat" "luna_nonstream" in
  let live = fixture "openai_chat" "luna_stream" in
  let prefix = record_all ctx ~token:token_a [ recorded ] in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run =
    replay_run ctx ~record:true ~matcher:Matcher.Until_divergence ~token:token_b
      ~upstream:(F.base_url up) prefix
  in
  P.register ctx.proxy run;
  let r = send_fixture ctx ~token:token_b live in
  check_ok r;
  Alcotest.(check string) "live bytes" live.raw r.body;
  Alcotest.(check int) "forwarded" 1 (List.length (F.requests up));
  let findings = P.unregister ctx.proxy ~token:token_b in
  Alcotest.(check int)
    "missing request reported" 1
    (List.length (List.filter is_missing findings.divergences));
  let e = only (recording ctx run).exchanges in
  Alcotest.(check string) "tail key" (expected_key live) e.key

let drop_script chunks : F.source =
  F.Script
    [
      {
        status = 200;
        headers = [ ("content-type", "text/event-stream") ];
        chunks;
        ending = Drop;
      };
    ]

let replay_truncated =
  Alcotest.test_case "a recorded cut-off stream replays cut off" `Quick
  @@ with_ctx "replay-drop"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_stream" in
  let chunks =
    [ "data: {\"id\":\"x\",\"choices\":[]}\n\n"; "data: {\"id\":" ]
  in
  let up = fake ctx (drop_script chunks) in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let live = send_fixture ctx ~token:token_a f in
  ignore (P.unregister ctx.proxy ~token:token_a);
  let rec_ = recording ctx run in
  let up2 = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy
    (replay_run ctx ~token:token_b ~upstream:(F.base_url up2) rec_);
  let r = send_fixture ctx ~token:token_b f in
  let partial = String.concat "" chunks in
  (match (live.result, r.result) with
  | Error (Truncated a), Error (Truncated b) ->
      Alcotest.(check int) "live bytes" (String.length partial) a.bytes;
      Alcotest.(check int) "replayed bytes" a.bytes b.bytes
  | ( (Ok () | Error (Connect_failed _ | Timed_out)),
      (Ok () | Error (Connect_failed _ | Timed_out | Truncated _)) )
  | Error (Truncated _), (Ok () | Error (Connect_failed _ | Timed_out)) ->
      Alcotest.fail "expected truncated bodies live and in replay");
  Alcotest.(check string) "partial body" partial r.body;
  let findings = P.unregister ctx.proxy ~token:token_b in
  Alcotest.(check int) "exchanges" 1 findings.exchanges;
  Alcotest.(check int) "no divergences" 0 (List.length findings.divergences);
  Alcotest.(check int) "nothing upstream" 0 (List.length (F.requests up2))

(* A client that reads the first piece of the response and closes its socket. *)
let read_first_piece_then_close ctx ~token (f : fixture) =
  let net = Eio.Stdenv.net ctx.env in
  Eio.Net.with_tcp_connect ~host:"127.0.0.1"
    ~service:(string_of_int (P.port ctx.proxy))
    net
  @@ fun flow ->
  Eio.Flow.copy_string
    (Printf.sprintf
       "%s /r/%s/openai%s HTTP/1.1\r\n\
        host: x\r\n\
        authorization: Bearer bactrian-%s\r\n\
        content-type: application/json\r\n\
        content-length: %d\r\n\
        \r\n\
        %s"
       f.meth token f.path token (String.length f.request) f.request)
    flow;
  let buf = Cstruct.create 4096 in
  let seen = Buffer.create 256 in
  let contains_event () =
    let hay = Buffer.contents seen in
    let rec at i =
      i + 7 <= String.length hay
      && (String.equal (String.sub hay i 7) "event-1" || at (i + 1))
    in
    at 0
  in
  while not (contains_event ()) do
    let n = Eio.Flow.single_read flow buf in
    Buffer.add_string seen (Cstruct.to_string buf ~len:n)
  done

let agent_disconnect =
  Alcotest.test_case "an agent that goes away mid-stream is recorded" `Quick
  @@ with_ctx "agent-gone"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_stream" in
  let events =
    List.init 10 (fun i -> Printf.sprintf "data: event-%d\n\n" (i + 1))
  in
  let base = slow_upstream ctx ~chunks:events ~gap:0.1 in
  let run = make_run ctx ~token:token_a ~upstream:base in
  P.register ctx.proxy run;
  read_first_piece_then_close ctx ~token:token_a f;
  Eio.Time.sleep (Eio.Stdenv.clock ctx.env) 0.6;
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "exchanges" 1 findings.exchanges;
  Alcotest.(check int) "usage gaps" 1 findings.usage_gaps;
  Alcotest.(check bool) "errors noted" true (findings.errors <> []);
  (match (response_of (only (recording ctx run).exchanges)).body with
  | Stream { chunks; complete } ->
      Alcotest.(check bool) "marked as cut off" false complete;
      let n = List.length chunks in
      if n < 1 || n >= 10 then Alcotest.failf "recorded %d chunks" n
  | Whole _ -> Alcotest.fail "expected a recorded stream");
  Alcotest.(check bool)
    "reservation kept" true
    (Ledger.spent run.ledger ~provider:"openai" > 0.)

(* Replay pacing *)

let replay_timing =
  Alcotest.test_case "Recorded timing keeps the pacing, Fast does not" `Quick
  @@ with_ctx "pacing"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_stream" in
  let events =
    List.init 4 (fun i -> Printf.sprintf "data: event-%d\n\n" (i + 1))
  in
  let base = slow_upstream ctx ~chunks:events ~gap:0.1 in
  let run = make_run ctx ~token:token_a ~upstream:base in
  P.register ctx.proxy run;
  check_ok (send_fixture ctx ~token:token_a f);
  ignore (P.unregister ctx.proxy ~token:token_a);
  let rec_ = recording ctx run in
  let replayed timing token =
    P.register ctx.proxy (replay_run ctx ~timing ~token ~upstream:base rec_);
    let r = send_fixture ctx ~token f in
    check_ok r;
    Alcotest.(check string) "bytes" (String.concat "" events) r.body;
    ignore (P.unregister ctx.proxy ~token);
    event_gaps r events
  in
  List.iter
    (fun g ->
      if g < 0.06 then
        Alcotest.failf "recorded pacing lost: %.0f ms" (g *. 1000.))
    (replayed P.Recorded token_b);
  List.iter
    (fun g ->
      if g > 0.05 then Alcotest.failf "fast replay paced: %.0f ms" (g *. 1000.))
    (replayed P.Fast token_c)

(* Gemini key in the query *)

let gemini_query_key =
  Alcotest.test_case "Gemini accepts the placeholder in the key query" `Quick
  @@ with_ctx "gemini-key"
  @@ fun ctx ->
  let f = fixture "gemini_generate" "flash_stream_json" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy (make_run ctx ~token:token_a ~upstream:(F.base_url up));
  let with_key k =
    send ctx ~token:token_a ~provider:P.Gemini ~meth:f.meth f.path
      ~query:
        (f.query ^ (if String.equal f.query "" then "" else "&") ^ "key=" ^ k)
      ~headers:f.request_headers f.request
  in
  let r = with_key ("bactrian-" ^ token_a) in
  check_ok r;
  Alcotest.(check int) "status" f.status r.status;
  Alcotest.(check string) "bytes" f.raw r.body;
  let sent = only (F.requests up) in
  Alcotest.(check (option string))
    "real key in the header" (Some "real-gemini-key")
    (List.assoc_opt "x-goog-api-key" sent.headers);
  Alcotest.(check string) "key removed from the query" f.query sent.query;
  Alcotest.(check int) "wrong key" 403 (with_key "bactrian-wrong").status;
  Alcotest.(check int)
    "still one request upstream" 1
    (List.length (F.requests up));
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "refused keys" 1 findings.refused_keys

(* Errors in findings *)

let errors_in_findings =
  Alcotest.test_case "refusals and bad bodies land in the run's errors" `Quick
  @@ with_ctx "errors"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy (make_run ctx ~token:token_a ~upstream:(F.base_url up));
  let bad = send_fixture ctx ~token:token_a ~body_of:(fun _ -> "not json") f in
  Alcotest.(check int) "bad body" 400 bad.status;
  let refused = send_fixture ctx ~token:token_a ~key:"nope" f in
  Alcotest.(check int) "wrong key" 403 refused.status;
  let findings = P.unregister ctx.proxy ~token:token_a in
  match findings.errors with
  | [ first; second ] ->
      Alcotest.(check bool) "bad body first" true (first <> "");
      Alcotest.(check string)
        "key refusal second" "missing or wrong placeholder key in authorization"
        second
  | l -> Alcotest.failf "expected two errors, got %d" (List.length l)

let unattributed_count =
  Alcotest.test_case "unknown tokens and providers are counted proxy-wide"
    `Quick
  @@ with_ctx "unattributed"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy (make_run ctx ~token:token_a ~upstream:(F.base_url up));
  Alcotest.(check int) "none yet" 0 (P.unattributed ctx.proxy);
  ignore (send_fixture ctx ~token:token_b f);
  Alcotest.(check int) "unknown token" 1 (P.unattributed ctx.proxy);
  ignore
    (send ctx ~seg:"nope" ~token:token_a ~provider:P.Openai ~meth:"POST" "/v1/x"
       "{}");
  Alcotest.(check int) "unknown provider" 2 (P.unattributed ctx.proxy);
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check (list string)) "not in the run's errors" [] findings.errors

(* Provider errors and dropped streams *)

let scripted_429 =
  Alcotest.test_case "a 429 passes through and is recorded" `Quick
  @@ with_ctx "429"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let body = {|{"error":{"message":"slow down","type":"rate_limit"}}|} in
  let up =
    fake ctx
      (F.Script
         [
           {
             status = 429;
             headers =
               [ ("content-type", "application/json"); ("retry-after", "7") ];
             chunks = [ body ];
             ending = Complete;
           };
         ])
  in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let r = send_fixture ctx ~token:token_a f in
  check_ok r;
  Alcotest.(check int) "status" 429 r.status;
  Alcotest.(check string) "body" body r.body;
  Alcotest.(check (option string))
    "retry-after" (Some "7")
    (List.assoc_opt "retry-after" r.headers);
  ignore (P.unregister ctx.proxy ~token:token_a);
  let resp = response_of (only (recording ctx run).exchanges) in
  Alcotest.(check int) "recorded status" 429 resp.status;
  Alcotest.(check string) "recorded body" body (stream_bytes resp);
  Alcotest.(check (float 0.))
    "not charged" 0.
    (Ledger.spent run.ledger ~provider:"openai")

let dropped_stream =
  Alcotest.test_case "a dropped stream drops the agent's connection" `Quick
  @@ with_ctx "drop"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_stream" in
  let chunks =
    [ "data: {\"id\":\"x\",\"choices\":[]}\n\n"; "data: {\"id\":" ]
  in
  let up =
    fake ctx
      (F.Script
         [
           {
             status = 200;
             headers = [ ("content-type", "text/event-stream") ];
             chunks;
             ending = Drop;
           };
         ])
  in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let r = send_fixture ctx ~token:token_a f in
  let partial = String.concat "" chunks in
  (match r.result with
  | Error (Truncated { bytes; _ }) ->
      Alcotest.(check int) "bytes before the drop" (String.length partial) bytes
  | Error (Connect_failed _ | Timed_out) | Ok () ->
      Alcotest.fail "expected a truncated body");
  Alcotest.(check string) "partial body" partial r.body;
  let findings = P.unregister ctx.proxy ~token:token_a in
  Alcotest.(check int) "usage gaps" 1 findings.usage_gaps;
  Alcotest.(check int) "transport errors" 1 findings.transport_errors;
  let resp = response_of (only (recording ctx run).exchanges) in
  Alcotest.(check string) "partial recorded" partial (stream_bytes resp);
  (match resp.body with
  | Stream { complete; _ } ->
      Alcotest.(check bool) "marked as cut off" false complete
  | Whole _ -> Alcotest.fail "expected a recorded stream");
  (match findings.errors with
  | [ e ] ->
      let prefix =
        Printf.sprintf "upstream body cut off after %d bytes"
          (String.length partial)
      in
      Alcotest.(check bool) ("error " ^ e) true (String.starts_with ~prefix e)
  | [] | _ :: _ :: _ -> Alcotest.fail "expected one error");
  Alcotest.(check bool)
    "reservation kept" true
    (Ledger.spent run.ledger ~provider:"openai" > 0.)

let unknown_model =
  Alcotest.test_case "an unknown model gets 402" `Quick
  @@ with_ctx "model"
  @@ fun ctx ->
  let f = fixture "openai_chat" "luna_nonstream" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  let r =
    send_fixture ctx ~token:token_a ~body_of:(set_model "no-such-model") f
  in
  Alcotest.(check int) "status" 402 r.status;
  let findings = P.unregister ctx.proxy ~token:token_a in
  (match findings.cap_refusals with
  | [ Ledger.Unknown_model m ] ->
      Alcotest.(check string) "model" "no-such-model" m
  | [ Ledger.Cap_reached _ ] | [] | _ :: _ :: _ ->
      Alcotest.fail "expected one Unknown_model refusal");
  Alcotest.(check int) "nothing forwarded" 0 (List.length (F.requests up));
  Alcotest.(check int)
    "nothing recorded" 0
    (List.length (recording ctx run).exchanges)

(* Two runs at once *)

let two_runs =
  Alcotest.test_case "concurrent runs do not see each other's recordings" `Quick
  @@ with_ctx "two"
  @@ fun ctx ->
  let fa = fixture "openai_chat" "luna_nonstream" in
  let fb = fixture "anthropic_messages" "haiku_nonstream" in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let ra = make_run ctx ~token:token_a ~upstream:(F.base_url up) in
  let rb = make_run ctx ~token:token_b ~upstream:(F.base_url up) in
  P.register ctx.proxy ra;
  P.register ctx.proxy rb;
  Eio.Fiber.both
    (fun () -> check_ok (send_fixture ctx ~token:token_a fa))
    (fun () -> check_ok (send_fixture ctx ~token:token_b fb));
  let cross = send_fixture ctx ~token:token_b ~key:("bactrian-" ^ token_a) fb in
  Alcotest.(check int) "a's key on b's route" 403 cross.status;
  ignore (P.unregister ctx.proxy ~token:token_a);
  ignore (P.unregister ctx.proxy ~token:token_b);
  let ca = recording ctx ra and cb = recording ctx rb in
  Alcotest.(check string)
    "a recorded its own" "openai_chat" (only ca.exchanges).protocol;
  Alcotest.(check string)
    "b recorded its own" "anthropic_messages" (only cb.exchanges).protocol;
  let up2 = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy
    (replay_run ctx ~token:token_c ~upstream:(F.base_url up2) ca);
  P.register ctx.proxy
    (replay_run ctx ~token:token_d ~upstream:(F.base_url up2) cb);
  let c_own, d_other =
    Eio.Fiber.pair
      (fun () -> send_fixture ctx ~token:token_c fa)
      (fun () -> send_fixture ctx ~token:token_d fa)
  in
  Alcotest.(check string) "c serves a's recording" fa.raw c_own.body;
  Alcotest.(check int) "d does not have a's recording" 409 d_other.status;
  ignore (P.unregister ctx.proxy ~token:token_c);
  ignore (P.unregister ctx.proxy ~token:token_d);
  Alcotest.(check int)
    "nothing upstream in replay" 0
    (List.length (F.requests up2))

(* Routing *)

let routing =
  Alcotest.test_case "routes and base URLs" `Quick
  @@ with_ctx "routes"
  @@ fun ctx ->
  let up = fake ctx (F.Fixtures fixtures_root) in
  P.register ctx.proxy (make_run ctx ~token:token_a ~upstream:(F.base_url up));
  let port = P.port ctx.proxy in
  Alcotest.(check string)
    "openai base"
    (Printf.sprintf "http://127.0.0.1:%d/r/%s/openai/v1" port token_a)
    (P.base_url ctx.proxy ~token:token_a P.Openai);
  Alcotest.(check string)
    "openrouter base"
    (Printf.sprintf "http://127.0.0.1:%d/r/%s/openrouter/api/v1" port token_a)
    (P.base_url ctx.proxy ~token:token_a P.Openrouter);
  Alcotest.(check string)
    "anthropic base"
    (Printf.sprintf "http://127.0.0.1:%d/r/%s/anthropic" port token_a)
    (P.base_url ctx.proxy ~token:token_a P.Anthropic);
  let auth = [ ("authorization", "Bearer bactrian-" ^ token_a) ] in
  let unknown_provider =
    send ctx ~seg:"nope" ~token:token_a ~provider:P.Openai ~meth:"POST" "/v1/x"
      ~headers:auth "{}"
  in
  Alcotest.(check int) "unknown provider" 404 unknown_provider.status;
  let unknown_endpoint =
    send ctx ~token:token_a ~provider:P.Openai ~meth:"POST" "/v1/embeddings"
      ~headers:auth "{}"
  in
  Alcotest.(check int) "unmodelled endpoint" 404 unknown_endpoint.status;
  ignore (P.unregister ctx.proxy ~token:token_a);
  Alcotest.(check int) "nothing forwarded" 0 (List.length (F.requests up))

let () =
  Alcotest.run "model_proxy"
    [
      ("record", record_cases);
      ( "streaming",
        [ streaming; dropped_stream; agent_disconnect; replay_timing ] );
      ("keys", [ placeholder_keys; gemini_query_key; unknown_model; routing ]);
      ( "replay",
        [ replay_strict; replay_miss; until_divergence; replay_truncated ] );
      ("errors", [ scripted_429; errors_in_findings; unattributed_count ]);
      ("isolation", [ two_runs ]);
    ]
