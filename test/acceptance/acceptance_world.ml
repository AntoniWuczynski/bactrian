open Bactrian_core
open Bactrian_runtime
module P = Model_proxy
module F = Fake_upstream

let () = Mirage_crypto_rng_unix.use_default ()
let fixtures = "../fixtures/protocols/openai_chat"
let prompt_variable = "AGENT_SYSTEM_PROMPT"

let first_turn =
  "What is the weather in Paris right now? Use the get_weather tool."

(* The repository root: dune sets DUNE_SOURCEROOT in every action. *)
let repo_root =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root -> root
  | None -> Alcotest.failf "DUNE_SOURCEROOT is not set (run through dune)"

let in_repo path = Filename.concat repo_root path

(* A missing tool is a failure, never a skip. *)
let require tool =
  let cmd = Printf.sprintf "%s --version > /dev/null 2>&1" tool in
  if Sys.command cmd <> 0 then
    Alcotest.failf "%s is required for the acceptance test but is not usable"
      tool

type agent = { name : string; argv : unit -> string list }

let agents =
  [
    {
      name = "typescript";
      argv =
        (fun () ->
          require "node";
          [ "node"; in_repo "examples/agents/typescript/dist/agent.js" ]);
    };
    {
      name = "python";
      argv =
        (fun () ->
          require "uv";
          let project = in_repo "examples/agents/python" in
          (* uv run would lock the project's virtual environment, a write
             outside the run that the sandbox refuses. *)
          [
            "uv";
            "run";
            "--no-sync";
            "--project";
            project;
            Filename.concat project "agent.py";
          ]);
    };
  ]

let read path = In_channel.with_open_bin path In_channel.input_all

let reply case : F.reply =
  {
    status = 200;
    headers = [ ("content-type", "application/json") ];
    chunks = [ read (Filename.concat fixtures (case ^ "/response.raw")) ];
    ending = Complete;
  }

let caps =
  List.map
    (fun p -> (p, 100.))
    [ "openai"; "anthropic"; "gemini"; "openrouter" ]

let counter = ref 0

let random_bytes n =
  incr counter;
  String.init n (fun i -> Char.chr ((!counter + (i * 7)) land 255))

let lines path =
  String.split_on_char '\n' (read path) |> List.filter (( <> ) "")

let trace (r : Run.result) =
  List.map
    (fun l -> Yojson.Safe.from_string l)
    (lines (Filename.concat r.run_dir "trace.jsonl"))

let entries kind (r : Run.result) =
  List.filter
    (fun j -> String.equal Yojson.Safe.Util.(to_string (member "type" j)) kind)
    (trace r)

let outcome_name (r : Run.result) =
  match r.outcome with
  | Passed -> "passed"
  | Failed s -> "failed: " ^ s
  | Diverged -> "diverged"
  | Errored s -> "errored: " ^ s
  | Not_run s -> "not_run: " ^ s

let ok = function Ok v -> v | Error e -> Alcotest.failf "execute: %s" e

(* The provider key bactrian holds. No agent may ever see it. *)
let real_key = "real-openai-key"

(* dune gives the paths of the executables relative to the test's directory. *)
let executable variable =
  match Sys.getenv_opt variable with
  | Some p when Filename.is_relative p -> Filename.concat (Sys.getcwd ()) p
  | Some p -> p
  | None -> Alcotest.failf "%s is not set (run through dune)" variable

(* Every run here is sandboxed where the host has a sandbox (design §5.3), with
   strict, so a sandbox that does not work fails the test. *)
let on_path name =
  List.exists
    (fun dir ->
      (not (String.equal dir "")) && Sys.file_exists (Filename.concat dir name))
    (String.split_on_char ':'
       (Option.value (Sys.getenv_opt "PATH") ~default:""))

let macos = Sys.file_exists "/usr/bin/sandbox-exec"
let sandbox_available = macos || on_path "bwrap"

let () =
  if not sandbox_available then
    print_endline
      "no sandbox-exec or bwrap on this host: runs are not sandboxed"

let sandbox_flag = if sandbox_available then "strict" else "off"

let sandbox ~env ~dir ~deny_read =
  if not sandbox_available then Sandbox.off
  else
    match
      Sandbox.preflight ~env
        ~warn:(fun w -> Alcotest.failf "warning: %s" w)
        {
          mode = `Strict;
          allow_write = [];
          deny_read;
          bactrian_exe = executable "BACTRIAN_EXE";
          bactrian_dir = Filename.concat dir ".bactrian";
        }
    with
    | Ok t -> t
    | Error m -> Alcotest.failf "sandbox preflight: %s" m

let check_sandboxed (r : Run.result) =
  Alcotest.(check bool) "sandboxed" sandbox_available r.sandbox.applied

type world = {
  env : Eio_unix.Stdenv.base;
  up : F.t;
  dir : string;  (** The world's directory, with its [.bactrian] store. *)
  execute :
    agent:Run.agent ->
    ?replay:Run.replay ->
    ?mcp:Mcp_proxy.config list ->
    ?variant_env:(string * string) list ->
    unit ->
    Run.result;
}

(* [deny_read] names directories to create in the world and deny to the agent.
   The fake upstream answers [rounds] times the two replies of the weather
   conversation. *)
let with_world ?(deny_read = []) ?(rounds = 1) name f =
  Eio_main.run @@ fun env ->
  let dir = Eio.Path.(Eio.Stdenv.cwd env / ("tmp-acceptance-" ^ name)) in
  Eio.Path.rmtree ~missing_ok:true dir;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let proxy =
    match
      P.start ~sw ~net ~clock:(Eio.Stdenv.clock env)
        ~process_mgr:(Eio.Stdenv.process_mgr env)
    with
    | Ok p -> p
    | Error e -> Alcotest.fail e
  in
  let up =
    match
      F.start ~sw ~net
        (F.Script
           (List.concat
              (List.init rounds (fun _ ->
                   [ reply "luna_nonstream"; reply "luna_turn2" ]))))
    with
    | Ok u -> u
    | Error e -> Alcotest.failf "fake upstream: %a" F.pp_error e
  in
  let store = Store.create ~root:Eio.Path.(dir / ".bactrian") in
  let abs_dir = Filename.concat (Sys.getcwd ()) (Eio.Path.native_exn dir) in
  List.iter
    (fun name -> Eio.Path.mkdirs ~perm:0o700 Eio.Path.(dir / name))
    deny_read;
  let sandbox =
    sandbox ~env ~dir:abs_dir
      ~deny_read:(List.map (Filename.concat abs_dir) deny_read)
  in
  let execute ~agent ?replay ?(mcp = []) ?(variant_env = []) () =
    incr counter;
    let ledger =
      Ledger.create
        ~path:Eio.Path.(dir / ("ledger-" ^ string_of_int !counter ^ ".jsonl"))
        ~caps
    in
    let config : Run.config =
      {
        agent;
        cwd = ".";
        turns = (if replay = None then [ first_turn ] else []);
        scenario = "weather";
        variant = "base";
        sim_now = "";
        timezone = "UTC";
        replay;
        env_passthrough = [ prompt_variable ];
        variant_env;
        fixtures = None;
        normalise = [];
        redact = [];
        pricing = Pricing.default;
        checks = None;
        unenforced = [];
        turn_timeout = 60.;
        total_timeout = 90.;
        mcp;
        sandbox =
          (match agent with
          | Command _ -> sandbox
          | Http _ -> Sandbox.http_mode `Auto);
        faketime = true;
      }
    in
    ok
      (Run.execute ~random_bytes
         ~keys:[ (P.Openai, real_key) ]
         ~upstream:(fun _ -> Uri.of_string (F.base_url up))
         ~env ~proxy ~store ~ledger config)
  in
  f { env; up; dir = Filename.concat (Sys.getcwd ()) (snd dir); execute }

let strict from : Run.replay =
  { from; matcher = Strict; allow_reorder = false; timing = P.Fast }

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0
