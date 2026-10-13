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
        ~process_mgr:(Eio.Stdenv.process_mgr env)
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

let compile source =
  match Regex.compile source with
  | Ok re -> re
  | Error e -> Alcotest.failf "pattern %S: %s" source e

let redaction source =
  match Redact.pattern source with
  | Ok p -> p
  | Error _ -> Alcotest.failf "pattern %S" source

let make_run ?(mode = P.Record) ?(record = true) ?(caps = caps)
    ?(normalise = []) ?(redact = []) ?(pricing = Pricing.default) ?(mcp = [])
    ctx ~token ~upstream =
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
    normalise = List.map compile normalise;
    redact = List.map redaction redact;
    pricing;
    run_started = Eio.Time.now (Eio.Stdenv.clock ctx.env);
    mcp;
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

let truncated_bytes r =
  match r.result with
  | Error (Truncated { bytes; _ }) -> Some bytes
  | Ok () | Error (Connect_failed _ | Timed_out | Failed_after_send _) -> None

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
  | Whole { payload = Inline s; complete = _ } -> s
  | Whole { payload = Blob _; complete = _ } -> Alcotest.fail "unexpected blob"

let is_missing = function
  | Matcher.Missing_request _ -> true
  | Unconsumed _ | Reordered _ | Shape_mismatch _ -> false

(* A raw TCP upstream: reads the whole request, writes [reply] verbatim, then
   closes the connection. *)
let raw_upstream ctx ~reply =
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
          Eio.Flow.copy_string reply flow));
  Printf.sprintf "http://127.0.0.1:%d" port

(* A URL on which nothing listens. *)
let refused_url ctx =
  match
    Eio.Switch.run (fun inner ->
        let sock =
          Eio.Net.listen ~sw:inner ~backlog:1 (Eio.Stdenv.net ctx.env)
            (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
        in
        match Eio.Net.listening_addr sock with
        | `Tcp (_, p) -> Eio.Switch.fail inner (Failure (string_of_int p))
        | `Unix _ -> Alcotest.fail "unix listener")
  with
  | () -> Alcotest.fail "unreachable"
  | exception Failure p -> "http://127.0.0.1:" ^ p

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

let record_all ctx ~token fixtures =
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run = make_run ctx ~token ~upstream:(F.base_url up) in
  P.register ctx.proxy run;
  List.iter (fun f -> check_ok (send_fixture ctx ~token f)) fixtures;
  ignore (P.unregister ctx.proxy ~token);
  recording ctx run

let replay_run ?(record = true) ?(matcher = Matcher.Strict) ?(timing = P.Fast)
    ?mcp ctx ~token ~upstream recording =
  make_run ?mcp ctx ~token ~upstream ~record
    ~mode:(P.Replay { recording; matcher; allow_reorder = false; timing })
