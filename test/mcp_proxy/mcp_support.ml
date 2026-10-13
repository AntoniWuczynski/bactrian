open Bactrian_core
open Bactrian_runtime
include Proxy_support
module M = Mcp_proxy

let fake_exe =
  Filename.concat (Sys.getcwd ()) "../support/fake_mcp/fake_mcp_server.exe"

(* Nothing listens here: model traffic is not part of these tests. *)
let no_upstream = "http://127.0.0.1:9"

let obj fields =
  match Canon.obj fields with
  | Ok v -> v
  | Error e -> Alcotest.failf "obj: %a" Canon.pp_error e

let s = Canon.string

(* Messages *)

let modern_meta client =
  obj
    [
      ("io.modelcontextprotocol/protocolVersion", s "2026-07-28");
      ("io.modelcontextprotocol/clientCapabilities", Canon.empty_object);
      ( "io.modelcontextprotocol/clientInfo",
        obj [ ("name", s client); ("version", s "1") ] );
      ("traceparent", s ("00-" ^ client ^ "-01"));
    ]

let request ?(era = Fake_mcp.Modern) ?(client = "client-a") id method_ fields =
  let fields =
    match era with
    | Modern -> ("_meta", modern_meta client) :: fields
    | Legacy -> fields
  in
  Mcp_msg.encode (Request { id; method_; params = Some (obj fields) })

let discover ?client id = request ?client id "server/discover" []
let tools_list ?era ?client id = request ?era ?client id "tools/list" []

let call ?era ?client id city =
  request ?era ?client id "tools/call"
    [ ("name", s "get_weather"); ("arguments", obj [ ("city", s city) ]) ]

let initialize ?(client = "client-a") id =
  request ~era:Legacy id "initialize"
    [
      ("protocolVersion", s "2025-11-25");
      ("capabilities", Canon.empty_object);
      ("clientInfo", obj [ ("name", s client); ("version", s "1") ]);
    ]

let initialized =
  Mcp_msg.encode
    (Notification { method_ = "notifications/initialized"; params = None })

(* Replies *)

let decode text =
  match Mcp_msg.decode text with
  | Ok m -> m
  | Error _ -> Alcotest.failf "not a JSON-RPC message: %S" text

let id_testable =
  Alcotest.testable
    (fun ppf (id : Mcp_msg.id) ->
      match id with
      | Int_id n -> Format.fprintf ppf "%d" n
      | String_id s -> Format.fprintf ppf "%S" s)
    (fun (a : Mcp_msg.id) b ->
      match (a, b) with
      | Int_id x, Int_id y -> Int.equal x y
      | String_id x, String_id y -> String.equal x y
      | Int_id _, String_id _ | String_id _, Int_id _ -> false)

let canon_testable =
  Alcotest.testable
    (fun ppf v -> Format.pp_print_string ppf (Canon.to_string v))
    Canon.equal

let result_of text =
  match decode text with
  | Response { id; result } -> (id, result)
  | Request _ | Notification _ | Error_response _ ->
      Alcotest.failf "expected a result: %s" text

let error_of text =
  match decode text with
  | Error_response { id = Some id; code; message; data } ->
      (id, code, message, data)
  | Error_response { id = None; _ } ->
      Alcotest.failf "error without id: %s" text
  | Request _ | Notification _ | Response _ ->
      Alcotest.failf "expected an error: %s" text

let weather_text result =
  match Canon.list_member "content" result with
  | Some (first :: _) -> Option.get (Canon.str_member "text" first)
  | Some [] | None -> Alcotest.failf "no content: %s" (Canon.to_string result)

let error_type data =
  Option.bind data (Canon.str_member "type") |> Option.value ~default:"<none>"

(* Servers and runs *)

let era_name = function Fake_mcp.Modern -> "modern" | Legacy -> "legacy"

let stdio ?(policy = M.Record) ?(era = Fake_mcp.Modern) ?(args = []) ?(env = [])
    name =
  {
    M.name;
    policy;
    server =
      Stdio
        { argv = fake_exe :: "--era" :: era_name era :: args; env; cwd = None };
  }

let http ?(policy = M.Record) name url =
  { M.name; policy; server = Http { url = Uri.of_string url } }

(* An absolute path inside the test's directory. *)
let path ctx name =
  let p = Eio.Path.native_exn Eio.Path.(ctx.dir / name) in
  if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p

let post ctx ~token ~name body =
  send ctx ~seg:"mcp" ~token ~provider:P.Openai ~meth:"POST" ("/" ^ name) body

let modern_headers ?(name = "") method_ =
  [
    ("content-type", "application/json");
    ("accept", "application/json, text/event-stream");
    ("mcp-protocol-version", "2026-07-28");
    ("mcp-method", method_);
  ]
  @ if String.equal name "" then [] else [ ("mcp-name", name) ]

let post_http ?(headers = [ ("content-type", "application/json") ]) ctx ~token
    ~name body =
  send ctx ~seg:"mcp-http" ~token ~provider:P.Openai ~meth:"POST" ~headers
    ("/" ^ name) body

let mcp_exchanges (c : Cassette.t) =
  List.filter
    (fun (e : Cassette.exchange) ->
      match e.channel with Mcp -> true | Model | Http -> false)
    c.exchanges

let record_run ?mode ctx ~token mcp =
  let run = make_run ?mode ~mcp ctx ~token ~upstream:no_upstream in
  P.register ctx.proxy run;
  run

let replay_mcp ctx ~token recording mcp =
  let run = replay_run ~mcp ctx ~token ~upstream:no_upstream recording in
  P.register ctx.proxy run;
  run

let no_divergence (f : P.findings) =
  Alcotest.(check int) "divergences" 0 (List.length f.divergences)

let check_status what expected (r : reply) =
  if r.status <> expected then
    Alcotest.failf "%s: status %d, expected %d, body %S" what r.status expected
      r.body

let lines_of file =
  String.split_on_char '\n' (read file)
  |> List.filter (fun l -> not (String.equal l ""))

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0
