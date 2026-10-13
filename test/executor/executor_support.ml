open Bactrian_runtime
module E = Executor
module P = Model_proxy
module F = Fake_upstream

let () = Mirage_crypto_rng_unix.use_default ()

(* Running *)

let fixtures_root = "../fixtures/protocols"

let weather_turn =
  "What is the weather in Paris right now? Use the get_weather tool."

let counter = ref 0

let random_bytes n =
  incr counter;
  String.init n (fun i -> Char.chr ((!counter + (i * 7)) land 255))

(* Appends "S <run id>" when the agent starts and "E <run id>" 0.2 s later. If
   the block file exists it holds N: the agent then hangs once N agents have
   started. *)
let agent_prefix =
  {|
import os, time
log = os.environ["AGENT_LOG"]
def w(tag):
    with open(log, "a") as f:
        f.write(tag + " " + os.environ["BACTRIAN_RUN_ID"] + "\n")
w("S")
block = os.environ["AGENT_BLOCK"]
if os.path.exists(block):
    started = sum(1 for l in open(log) if l.startswith("S "))
    if started > int(open(block).read()):
        time.sleep(600)
time.sleep(0.2)
w("E")
|}

type ctx = {
  env : Eio_unix.Stdenv.base;
  dir : string;  (** Relative to the working directory. *)
  root : Eio.Fs.dir_ty Eio.Path.t;
  up : F.t;
}

let write ctx name text =
  let path = Eio.Path.(Eio.Stdenv.cwd ctx.env / ctx.dir / name) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700
    (match Eio.Path.split path with Some (d, _) -> d | None -> assert false);
  Eio.Path.save ~create:(`Or_truncate 0o600) path text

let read ctx name =
  Eio.Path.load Eio.Path.(Eio.Stdenv.cwd ctx.env / ctx.dir / name)

let log_path ctx = Filename.concat (Sys.getcwd ()) (ctx.dir ^ "/agent.log")
let block_path ctx = Filename.concat (Sys.getcwd ()) (ctx.dir ^ "/block")

let scenario_json
    ?(assertions =
      {|[{"tool_sequence": {"mode": "exact", "tools": ["get_weather"]}}, {"max_steps": 2}]|})
    ?(checkers = "[]") ?(extra = "") id =
  Printf.sprintf
    {|{"version": 1, "id": %S, "timezone": "Europe/London",
 "turns": [{"user": %S}], "assertions": %s, "checkers": %s%s}|}
    id weather_turn assertions checkers extra

let experiment_json ctx ?(repeats = 2) ?(concurrency = 2)
    ?(caps = {|{"openai": 100}|}) ?(seed = 3) ?(extra = "") ?(prelude = "") () =
  let variant name =
    Printf.sprintf
      {|%S: {"env": {"AGENT_LOG": %S, "AGENT_BLOCK": %S, "VARIANT_NAME": %S}}|}
      name (log_path ctx) (block_path ctx) name
  in
  Printf.sprintf
    {|{"version": 1, "name": "demo",
 "agent": {"protocol": "command", "command": ["python3", "-I", "-c", %s]},
 "scenarios": ["scenarios/*.json"],
 "variants": {%s, %s},
 "repeats": %d, "concurrency": %d, "caps_usd": %s, "seed": %d%s}|}
    (Yojson.Safe.to_string
       (`String (prelude ^ agent_prefix ^ Weather_agent.script)))
    (variant "a") (variant "b") repeats concurrency caps seed extra

let with_ctx name ?(scenarios = [ "s1"; "s2" ]) f () =
  Eio_main.run @@ fun env ->
  let dir = "tmp-exec-" ^ name in
  let abs = Eio.Path.(Eio.Stdenv.cwd env / dir) in
  Eio.Path.rmtree ~missing_ok:true abs;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 abs;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true abs)
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let up =
    match F.start ~sw ~net:(Eio.Stdenv.net env) (F.Fixtures fixtures_root) with
    | Ok u -> u
    | Error e -> Alcotest.failf "fake upstream: %a" F.pp_error e
  in
  let ctx = { env; dir; root = Eio.Path.(abs / ".bactrian"); up } in
  List.iter
    (fun id ->
      write ctx (Printf.sprintf "scenarios/%s.json" id) (scenario_json id))
    scenarios;
  write ctx "exp.json" (experiment_json ctx ());
  f ctx

(* The agents here write outside their state directory, so the sandbox is off
   unless a test asks for it. *)
let go ?(resume = None) ?announce ?warn ?(sandbox = Some `Off) ctx =
  E.run_experiment ~root:ctx.root ~random_bytes ?announce ?warn ?sandbox
    ~bactrian_exe:"/nonexistent/bactrian"
    ~keys:[ (P.Openai, "real-key") ]
    ~upstream:(fun _ -> Uri.of_string (F.base_url ctx.up))
    ~env:ctx.env ~experiment_file:(ctx.dir ^ "/exp.json") ~resume ()

let ok = function
  | Ok v -> v
  | Error (E.Configuration _) -> Alcotest.fail "configuration error"
  | Error (E.Environment m) -> Alcotest.failf "environment: %s" m
  | Error (E.Usage m) -> Alcotest.failf "usage: %s" m

let lines ctx name =
  match read ctx name with
  | text -> String.split_on_char '\n' text |> List.filter (( <> ) "")
  | exception Eio.Io _ -> []

let agent_log ctx = lines ctx "agent.log"

let count_prefix p l =
  List.length (List.filter (String.starts_with ~prefix:p) l)

let manifest ctx (s : E.summary) =
  Yojson.Safe.from_string
    (Eio.Path.load
       Eio.Path.(ctx.root / "experiments" / s.experiment_id / "manifest.json"))

let member = Yojson.Safe.Util.member
let str j k = Yojson.Safe.Util.to_string (member k j)

let status_kind : E.status -> string = function
  | Pending -> "pending"
  | Interrupted -> "interrupted"
  | Done o -> Run.outcome_kind o

let kinds (s : E.summary) = List.map (fun (_, st, _) -> status_kind st) s.runs

let run_meta ctx id =
  Yojson.Safe.from_string
    (read ctx (Printf.sprintf ".bactrian/runs/%s/meta.json" id))

let run_ids (s : E.summary) = List.filter_map (fun (_, _, id) -> id) s.runs

let spent ctx id =
  Yojson.Safe.Util.(to_float (member "cost_usd" (run_meta ctx id)))
