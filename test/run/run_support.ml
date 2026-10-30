open Bactrian_core
open Bactrian_runtime
module P = Model_proxy
module F = Fake_upstream

let () = Mirage_crypto_rng_unix.use_default ()
let fixtures_root = "../fixtures/protocols"
let real_openai_key = "real-openai-key"

let caps value =
  [
    ("openai", value);
    ("anthropic", value);
    ("gemini", value);
    ("openrouter", value);
  ]

let python_agent = Weather_agent.script

let first_turn =
  "What is the weather in Paris right now? Use the get_weather tool."

(* Context *)

type ctx = {
  env : Eio_unix.Stdenv.base;
  proxy : P.t;
  store : Store.t;
  dir : Eio.Fs.dir_ty Eio.Path.t;
  up : F.t;
}

let counter = ref 0

let random_bytes n =
  incr counter;
  String.init n (fun i -> Char.chr ((!counter + (i * 7)) land 255))

let with_ctx name f () =
  Eio_main.run @@ fun env ->
  let path = "tmp-run-" ^ name in
  let dir = Eio.Path.(Eio.Stdenv.cwd env / path) in
  Eio.Path.rmtree ~missing_ok:true dir;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let proxy =
    match
      P.start ~sw ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
        ~process_mgr:(Eio.Stdenv.process_mgr env)
        ()
    with
    | Ok p -> p
    | Error e -> Alcotest.fail e
  in
  let up =
    match F.start ~sw ~net:(Eio.Stdenv.net env) (F.Fixtures fixtures_root) with
    | Ok u -> u
    | Error e -> Alcotest.failf "fake upstream: %a" F.pp_error e
  in
  f
    {
      env;
      proxy;
      store = Store.create ~root:Eio.Path.(dir / ".bactrian");
      dir;
      up;
    }

let config ?(agent = [ "python3"; "-I"; "-c"; python_agent ]) ?replay
    ?(turns = [ first_turn ]) () : Run.config =
  {
    agent = Command agent;
    cwd = ".";
    turns;
    scenario = "weather";
    variant = "base";
    sim_now = "";
    timezone = "UTC";
    replay;
    env_passthrough = [];
    variant_env = [];
    fixtures = None;
    normalise = [];
    redact = [];
    pricing = Pricing.default;
    checks = None;
    unenforced = [];
    turn_timeout = 20.;
    total_timeout = 30.;
    mcp = [];
    sandbox = Sandbox.off;
    faketime = false;
    egress = None;
  }

let execute ?connector ?interception ?random_bytes:(rb = random_bytes)
    ?(cap = 100.) ?(ledger_name = string_of_int !counter) ctx config =
  let ledger =
    Ledger.create
      ~path:Eio.Path.(ctx.dir / ("ledger-" ^ ledger_name ^ ".jsonl"))
      ~caps:(caps cap)
  in
  Run.execute ?connector ?interception ~random_bytes:rb
    ~keys:[ (P.Openai, real_openai_key) ]
    ~upstream:(fun _ -> Uri.of_string (F.base_url ctx.up))
    ~env:ctx.env ~proxy:ctx.proxy ~store:ctx.store ~ledger config

let ok = function Ok v -> v | Error e -> Alcotest.failf "execute: %s" e

let outcome_name (r : Run.result) =
  match r.outcome with
  | Passed -> "passed"
  | Failed s -> "failed: " ^ s
  | Diverged -> "diverged"
  | Errored s -> "errored: " ^ s
  | Not_run s -> "not_run: " ^ s

let read path = In_channel.with_open_bin path In_channel.input_all

let lines path =
  String.split_on_char '\n' (read path) |> List.filter (( <> ) "")

let file (r : Run.result) name = Filename.concat r.run_dir name

let trace_types (r : Run.result) =
  List.map
    (fun l ->
      Yojson.Safe.Util.(to_string (member "type" (Yojson.Safe.from_string l))))
    (lines (file r "trace.jsonl"))

let tool_names (r : Run.result) =
  List.filter_map
    (fun l ->
      let j = Yojson.Safe.from_string l in
      if String.equal Yojson.Safe.Util.(to_string (member "type" j)) "tool_call"
      then Some Yojson.Safe.Util.(to_string (member "name" j))
      else None)
    (lines (file r "trace.jsonl"))

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let strs = Alcotest.(list string)
let record ctx = ok (execute ctx (config ()))

(* The command line, run in the context's directory. *)

let bactrian_bin = Filename.concat (Sys.getcwd ()) (Sys.getenv "BACTRIAN_BIN")

(* The host's own sandbox, named, so a run on macOS is not sent to the virtual
   machine or warned about it. *)
let host_backend =
  match Bactrian_runtime.Sandbox.host_backend () with
  | `Seatbelt -> "seatbelt"
  | `Bubblewrap -> "bubblewrap"

let cli ctx args =
  let out = Filename.concat (Sys.getcwd ()) "cli-out.txt" in
  let cmd =
    Printf.sprintf "cd %s && %s %s > %s 2>&1"
      (Filename.quote
         (Filename.concat (Sys.getcwd ()) (Eio.Path.native_exn ctx.dir)))
      (Filename.quote bactrian_bin)
      args (Filename.quote out)
  in
  let code = Sys.command cmd in
  let text = read out in
  Sys.remove out;
  (code, text)

(* Checks *)

let checks ?(assertions = []) ?(checkers = []) () : Run.checks option =
  Some
    {
      assertions;
      checkers;
      checks_cwd = ".";
      checker_timeout = 10.;
      checker_sandbox = Sandbox.off;
    }

let json (r : Run.result) name = Yojson.Safe.from_file (file r name)

let verdict_names (r : Run.result) =
  List.map
    (fun item -> Yojson.Safe.Util.(to_string (member "verdict" item)))
    Yojson.Safe.Util.(to_list (member "items" (json r "verdicts.json")))

(* The weather agent, opening a tunnel to a host no policy lists before its
   first model call. The host is blocked before any TLS, and the attempt is
   still a call that no_egress sees. *)
let egress_agent =
  let connect =
    {|
import base64, socket, urllib.parse
def connect():
    u = urllib.parse.urlparse(os.environ["HTTPS_PROXY"])
    s = socket.create_connection((u.hostname, u.port))
    cred = base64.b64encode((u.username + ":" + u.password).encode()).decode()
    s.sendall(("CONNECT api.weather.example:443 HTTP/1.1\r\nHost: api.weather.example:443\r\nProxy-Authorization: Basic " + cred + "\r\n\r\n").encode())
    s.recv(4096)
    s.close()
|}
  in
  let script =
    Re.replace_string ~all:false
      (Re.compile (Re.str "msg = chat(messages)\n"))
      ~by:"connect(); msg = chat(messages)\n" python_agent
  in
  [ "python3"; "-I"; "-c"; "import os\n" ^ connect ^ script ]

(* A run of [egress_agent] in [sandbox], checked with [no_egress] for the host
   it calls and for one it does not. *)
let no_egress_run ctx sandbox =
  ok
    (execute ~interception:Egress_stub.interception ctx
       {
         (config ~agent:egress_agent ()) with
         sandbox;
         egress = Some Egress.block_all;
         checks =
           checks
             ~assertions:
               [
                 No_egress [ "api.weather.example" ];
                 No_egress [ "other.example" ];
               ]
             ();
       })
