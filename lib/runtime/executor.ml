open Bactrian_core
module P = Model_proxy

type planned = {
  index : int;
  scenario : string;
  variant : string;
  repeat : int;
}

type status = Pending | Done of Run.outcome | Interrupted

type summary = {
  experiment_id : string;
  runs : (planned * status * string option) list;
}

type manifest = {
  name : string;
  experiment_file : string;
  seed : int;
  fingerprint : Fingerprint.t;
  previous_run_ids : (int * string list) list;
  summary : summary;
}

type error =
  | Configuration of Config_loader.located_error list
  | Environment of string
  | Usage of string

let ( let* ) = Result.bind

(* Allocation *)

let shuffle state a =
  for i = Array.length a - 1 downto 1 do
    let j = Random.State.int state (i + 1) in
    let x = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- x
  done

let rec interleave lists =
  match List.filter (fun l -> not (List.is_empty l)) lists with
  | [] -> []
  | nonempty ->
      List.concat_map (function [] -> [] | x :: _ -> [ x ]) nonempty
      @ interleave (List.map (function [] -> [] | _ :: rest -> rest) nonempty)

let plan ~seed ~scenarios ~variants ~repeats =
  List.mapi
    (fun position scenario ->
      let runs =
        Array.of_list
          (List.concat_map
             (fun variant ->
               List.init repeats (fun r -> (scenario, variant, r + 1)))
             variants)
      in
      shuffle (Random.State.make [| seed; position |]) runs;
      Array.to_list runs)
    scenarios
  |> interleave
  |> List.mapi (fun index (scenario, variant, repeat) ->
      { index; scenario; variant; repeat })

(* The manifest *)

type wire_run = {
  index : int;
  scenario : string;
  variant : string;
  repeat : int;
  status : string;
  outcome : string option; [@default None]
  reason : string option; [@default None]
  run_id : string option; [@default None]
  previous_run_ids : string list; [@default []]
}
[@@deriving yojson { strict = true }]

type wire = {
  version : int;
  experiment_id : string;
  experiment : string;
  experiment_file : string;
  seed : int;
  fingerprint : Yojson.Safe.t;
  runs : wire_run list;
}
[@@deriving yojson { strict = true }]

let wire_run (((p : planned), status, run_id), previous_run_ids) : wire_run =
  let status, outcome, reason =
    match status with
    | Pending -> ("pending", None, None)
    | Interrupted -> ("interrupted", None, None)
    | Done o -> ("done", Some (Run.outcome_kind o), Run.outcome_reason o)
  in
  {
    index = p.index;
    scenario = p.scenario;
    variant = p.variant;
    repeat = p.repeat;
    status;
    outcome;
    reason;
    run_id;
    previous_run_ids;
  }

let outcome_of ~kind ~reason : (Run.outcome, string) result =
  let why = Option.value reason ~default:"" in
  match kind with
  | "passed" -> Ok Passed
  | "failed" -> Ok (Failed why)
  | "diverged" -> Ok Diverged
  | "errored" -> Ok (Errored why)
  | "not_run" -> Ok (Not_run why)
  | other -> Error (Printf.sprintf "unknown outcome %S" other)

let unwire_run (w : wire_run) =
  let planned =
    {
      index = w.index;
      scenario = w.scenario;
      variant = w.variant;
      repeat = w.repeat;
    }
  in
  let* status =
    match (w.status, w.outcome) with
    | "pending", None -> Ok Pending
    | "interrupted", None -> Ok Interrupted
    | "done", Some kind ->
        Result.map (fun o -> Done o) (outcome_of ~kind ~reason:w.reason)
    | other, _ -> Error (Printf.sprintf "bad status %S" other)
  in
  Ok (planned, status, w.run_id)

let read_manifest path =
  match Eio.Path.load path with
  | exception Eio.Io _ -> Error None
  | text -> (
      let bad msg = Error (Some ("manifest.json: " ^ msg)) in
      match Yojson.Safe.from_string text with
      | exception Yojson.Json_error m -> bad m
      | json -> (
          match wire_of_yojson json with
          | Error m -> bad m
          | Ok w when w.version <> 1 -> bad "unsupported version"
          | Ok w -> (
              match
                List.fold_left
                  (fun acc r ->
                    let* l = acc in
                    let* x = unwire_run r in
                    Ok (x :: l))
                  (Ok []) w.runs
              with
              | Ok runs -> (
                  match Fingerprint.of_yojson w.fingerprint with
                  | Error m -> bad ("fingerprint: " ^ m)
                  | Ok fingerprint ->
                      Ok
                        {
                          name = w.experiment;
                          experiment_file = w.experiment_file;
                          seed = w.seed;
                          fingerprint;
                          previous_run_ids =
                            List.filter_map
                              (fun (r : wire_run) ->
                                match r.previous_run_ids with
                                | [] -> None
                                | ids -> Some (r.index, ids))
                              w.runs;
                          summary =
                            {
                              experiment_id = w.experiment_id;
                              runs = List.rev runs;
                            };
                        })
              | Error m -> bad m)))

(* Preflight *)

let funded caps =
  List.filter
    (fun (_, name) ->
      match List.assoc_opt name caps with Some usd -> usd > 0. | None -> false)
    Run.providers

let no_tls what =
  Environment
    (Printf.sprintf
       "this build has no TLS support (bactrian.tls), so it cannot call %s \
        over HTTPS"
       what)

(* An HTTP agent at an https URL needs the connector, like an https upstream. *)
let agent_preflight ~connector : Run.agent -> (unit, error) result = function
  | Command _ -> Ok ()
  | Http { url; _ } ->
      if Option.is_none connector && Upstream.is_https (Uri.of_string url) then
        Error (no_tls ("the agent at " ^ url))
      else Ok ()

let preflight ~caps ~keys ~upstream ~connector =
  match funded caps with
  | [] ->
      Error
        (Environment
           "no provider has a spend cap above 0: set caps_usd in the experiment")
  | funded -> (
      match
        List.find_opt
          (fun (p, _) -> Option.is_none (List.assoc_opt p keys))
          funded
      with
      | Some (p, name) ->
          Error
            (Environment
               (Printf.sprintf "%s has a spend cap but %s is not set" name
                  (Run.key_variable p)))
      | None -> (
          match
            List.filter (fun (p, _) -> Upstream.is_https (upstream p)) funded
          with
          | _ :: _ as https when Option.is_none connector ->
              Error
                (no_tls
                   (Printf.sprintf "providers (%s)"
                      (String.concat ", " (List.map snd https))))
          | _ :: _ | [] -> Ok ()))

(* Loading *)

let valid_id id =
  not
    (String.equal id "" || String.equal id "." || String.equal id ".."
   || String.contains id '/' || String.contains id '\000')

let manifest_json ~experiment_id ~(experiment : Experiment.t) ~experiment_file
    ~fingerprint runs =
  wire_to_yojson
    {
      version = 1;
      experiment_id;
      experiment = experiment.name;
      experiment_file;
      seed = experiment.seed;
      fingerprint = Fingerprint.to_yojson fingerprint;
      runs = List.map wire_run runs;
    }

let same_plan a b =
  List.equal
    (fun (x : planned) (y : planned) ->
      x.index = y.index
      && String.equal x.scenario y.scenario
      && String.equal x.variant y.variant
      && x.repeat = y.repeat)
    a b

type contents = {
  experiment : Experiment.t;
  dir : string;
  scenarios : (string * Scenario.t) list;
}

type loaded = {
  experiment : Experiment.t;
  dir : string;
  scenarios : (string * Scenario.t) list;  (** Path relative to [dir]. *)
  agent : Run.agent;
  turn_timeout : float;
  total_timeout : float;
  pricing : Pricing.table;
  fingerprint : Fingerprint.t;
}

let strings_member key canon =
  Option.bind (Canon.list_member key canon) (fun items ->
      List.fold_right
        (fun item acc ->
          Option.bind acc (fun rest ->
              Option.map (fun s -> s :: rest) (Canon.to_string_opt item)))
        items (Some []))

(* An error about the experiment file's own key [path]. *)
let at_key ~experiment_file ~find path message : Config_loader.located_error =
  let line, column = Option.value (find path) ~default:(1, 1) in
  { file = experiment_file; line; column; path; message }

(* [expand_globs] knows only the pattern: point at it in the experiment file. *)
let locate_globs ~experiment_file ~find ~globs errors =
  List.map
    (fun (e : Config_loader.located_error) ->
      let path =
        match List.find_index (String.equal e.path) globs with
        | Some i -> Printf.sprintf "scenarios[%d]" i
        | None -> "scenarios"
      in
      at_key ~experiment_file ~find path e.message)
    errors

(* Two scenario files with one [id] would run under one name and be merged in
   the results: point at the second [id]. *)
let duplicate_ids ~fs ~dir scenarios =
  let rec go seen = function
    | [] -> []
    | (rel, (s : Scenario.t)) :: rest -> (
        match List.assoc_opt s.id seen with
        | None -> go ((s.id, rel) :: seen) rest
        | Some first ->
            let path = Eio.Path.(fs / dir / rel) in
            let line, column =
              match Config_loader.load path with
              | Ok (_, find) -> Option.value (find "id") ~default:(1, 1)
              | Error _ -> (1, 1)
            in
            {
              Config_loader.file = snd path;
              line;
              column;
              path = "id";
              message =
                Printf.sprintf "scenario id %S is already used by %s" s.id first;
            }
            :: go seen rest)
  in
  go [] scenarios

(* The command line's [--sandbox strict] cannot be met by an agent bactrian does
   not start; the experiment's own [sandbox: strict] is refused when it is
   decoded. *)
let http_agent_strict ~experiment_file ~find experiment sandbox_override =
  match (experiment, sandbox_override) with
  | Ok ({ agent = Http _; _ } : Experiment.t), Some `Strict ->
      [
        at_key ~experiment_file ~find "agent.protocol"
          "--sandbox strict cannot be met with protocol http: bactrian does \
           not start the agent, so it cannot sandbox it (use auto or off)";
      ]
  | Ok { agent = Command _; _ }, _ | Ok _, (None | Some (`Auto | `Off)) -> []
  | Error _, _ -> []

(* Every stage reports, so one round shows every mistake: the experiment, its
   globs, each scenario and the price file. A stage that needs a field of an
   experiment that did not decode reads it straight from the document. *)
let load ~fs ~experiment_file ~sandbox_override =
  let exp_path = Eio.Path.(fs / experiment_file) in
  let dir = Filename.dirname experiment_file in
  let base = Eio.Path.(fs / dir) in
  let experiment = Config_loader.load_experiment exp_path in
  let document = Config_loader.load exp_path in
  let find = match document with Ok (_, f) -> f | Error _ -> fun _ -> None in
  let globs, pricing_file =
    match (experiment, document) with
    | Ok (e : Experiment.t), _ -> (Some e.scenarios, e.pricing)
    | Error _, Ok (canon, _) ->
        (strings_member "scenarios" canon, Canon.str_member "pricing" canon)
    | Error _, Error _ -> (None, None)
  in
  let files, glob_errors =
    (* One glob at a time, so a typo does not hide the files of the others. *)
    let expanded =
      List.map
        (fun glob -> Config_loader.expand_globs ~base [ glob ])
        (Option.value globs ~default:[])
    in
    ( List.sort_uniq String.compare
        (List.concat_map
           (function Ok files -> files | Error _ -> [])
           expanded),
      locate_globs ~experiment_file ~find
        ~globs:(Option.value globs ~default:[])
        (List.concat_map (function Ok _ -> [] | Error es -> es) expanded) )
  in
  let loaded_scenarios =
    List.map
      (fun rel ->
        Result.map
          (fun s -> (rel, s))
          (Config_loader.load_scenario Eio.Path.(base / rel)))
      files
  in
  let scenarios = List.filter_map Result.to_option loaded_scenarios in
  let pricing =
    match pricing_file with
    | None -> Ok Pricing.default
    | Some file -> (
        let missing why =
          Error
            [
              at_key ~experiment_file ~find "pricing"
                (Printf.sprintf "%s %s" file why);
            ]
        in
        match Eio.Path.is_file Eio.Path.(base / file) with
        | true -> Config_loader.load_pricing Eio.Path.(base / file)
        | false -> missing "does not exist"
        | exception Eio.Io _ -> missing "cannot be opened")
  in
  let errors =
    Config_loader.sort_errors
      (List.concat
         [
           (match experiment with Ok _ -> [] | Error es -> es);
           glob_errors;
           List.concat_map
             (function Ok _ -> [] | Error es -> es)
             loaded_scenarios;
           duplicate_ids ~fs ~dir scenarios;
           http_agent_strict ~experiment_file ~find experiment sandbox_override;
           (match pricing with Ok _ -> [] | Error es -> es);
         ])
  in
  match (errors, experiment, pricing, document) with
  | _ :: _, _, _, _ -> Error (Configuration errors)
  | [], Error es, _, _ | [], Ok _, Error es, _ -> Error (Configuration es)
  | [], Ok _, Ok _, Error es -> Error (Configuration es)
  | [], Ok experiment, Ok pricing, Ok (canon, _) ->
      let* fingerprint =
        Result.map_error
          (fun m -> Environment m)
          (Fingerprint.compute ~base ~experiment:canon ~pricing_file
             ~scenario_files:files)
      in
      let agent, turn_timeout, total_timeout =
        match experiment.agent with
        | Command { argv; turn_timeout; total_timeout } ->
            (Run.Command argv, turn_timeout, total_timeout)
        | Http { url; namespace_header; turn_timeout; total_timeout } ->
            (Run.Http { url; namespace_header }, turn_timeout, total_timeout)
      in
      Ok
        {
          experiment;
          dir;
          scenarios;
          agent;
          turn_timeout;
          total_timeout;
          pricing;
          fingerprint;
        }

(* Running *)

(* A stdio server runs where the agent does, in the experiment's directory,
   unless it names another one. *)
let mcp_config ~fs (l : loaded) (name, (s : Experiment.mcp_server)) :
    Mcp_proxy.config =
  let server : Mcp_proxy.server =
    match s.transport with
    | Mcp_stdio { argv; env; cwd } ->
        let dir =
          match cwd with
          | None -> l.dir
          | Some d when Filename.is_relative d -> Filename.concat l.dir d
          | Some d -> d
        in
        Stdio { argv; env; cwd = Some Eio.Path.(fs / dir) }
    | Mcp_http { url } -> Http { url = Uri.of_string url }
  in
  { name; server; policy = s.policy }

let mcp_configs ~fs (l : loaded) =
  if List.mem `Mcp l.experiment.intercept then
    List.map (mcp_config ~fs l) l.experiment.mcp
  else []

let run_config ~fs ~sandbox (l : loaded) (scenario : Scenario.t) ~scenario_dir
    (variant : Experiment.variant) : Run.config =
  {
    agent = l.agent;
    cwd = l.dir;
    turns = List.map (fun (t : Scenario.turn) -> t.user) scenario.turns;
    scenario = scenario.id;
    variant = variant.name;
    sim_now = Option.value scenario.now ~default:"";
    timezone = Option.value scenario.timezone ~default:"UTC";
    replay = None;
    env_passthrough = l.experiment.env_passthrough;
    variant_env = variant.env;
    checks =
      Some
        {
          assertions = scenario.assertions;
          checkers = scenario.checkers;
          checks_cwd = scenario_dir;
          checker_timeout = l.turn_timeout;
        };
    fixtures =
      Option.map (fun dir -> Filename.concat scenario_dir dir) scenario.fixtures;
    normalise = l.experiment.normalise;
    redact = l.experiment.redact;
    pricing = l.pricing;
    unenforced = Experiment.unenforced l.experiment;
    turn_timeout = l.turn_timeout;
    total_timeout = l.total_timeout;
    mcp = mcp_configs ~fs l;
    sandbox;
    faketime = l.experiment.faketime;
  }

(* The lock files this process holds. A [lockf] lock belongs to the process,
   so a second taker in the same process (another job of the MCP server) would
   be granted it, and the first one's [close] would release both. *)
let held = Hashtbl.create 4
let held_mutex = Mutex.create ()

let claim path =
  Mutex.protect held_mutex (fun () ->
      if Hashtbl.mem held path then false
      else (
        Hashtbl.replace held path ();
        true))

let release path = Mutex.protect held_mutex (fun () -> Hashtbl.remove held path)

(* One process runs an experiment at a time, and one job within it: two would
   both start the pending runs and overwrite each other's manifest. *)
let with_lock ~experiment_id dir f =
  let path = Eio.Path.native_exn Eio.Path.(dir / "run.lock") in
  let running holder =
    Error
      (Usage
         (Printf.sprintf "experiment %s is already running (%s holds %s)"
            experiment_id holder path))
  in
  match Unix.openfile path [ O_WRONLY; O_CREAT; O_CLOEXEC ] 0o600 with
  | exception Unix.Unix_error (e, _, _) ->
      Error (Environment (Printf.sprintf "%s: %s" path (Unix.error_message e)))
  | fd -> (
      Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
      let key = Unix.realpath path in
      if not (claim key) then running "this process"
      else
        Fun.protect ~finally:(fun () -> release key) @@ fun () ->
        match Unix.lockf fd F_TLOCK 0 with
        | exception Unix.Unix_error ((EAGAIN | EACCES | EWOULDBLOCK), _, _) ->
            running "another process"
        | exception Unix.Unix_error (e, _, _) ->
            Error
              (Environment (Printf.sprintf "%s: %s" path (Unix.error_message e)))
        | () -> f ())

(* A resume repeats what has no outcome worth keeping: a run that was lost,
   interrupted, stopped by a cap (missing data) or hit a fault of ours. The
   others are final. Each repeated run keeps its old ID in its previous IDs. *)
let for_resume (runs, previous) =
  let reset ((p : planned), status, run_id) =
    match status with
    | Done (Passed | Failed _ | Diverged) -> ((p, status, run_id), [])
    | Pending | Interrupted | Done (Errored _ | Not_run _) ->
        ((p, Pending, None), Option.to_list run_id)
  in
  List.map
    (fun (((p : planned), _, _) as run) ->
      let run, old = reset run in
      (run, Option.value ~default:[] (List.assoc_opt p.index previous) @ old))
    runs

let start_or_resume ~root ~experiment_file ~random_bytes ~env ~resume
    (l : loaded) planned =
  match resume with
  | None ->
      let id =
        Run.fresh_id ~random_bytes (Eio.Time.now (Eio.Stdenv.clock env))
      in
      Ok (id, List.map (fun p -> ((p, Pending, None), [])) planned)
  | Some id when not (valid_id id) ->
      Error (Usage (Printf.sprintf "invalid experiment ID %S" id))
  | Some id -> (
      let path = Eio.Path.(root / "experiments" / id / "manifest.json") in
      match read_manifest path with
      | Error None -> Error (Usage (Printf.sprintf "no experiment %S" id))
      | Error (Some m) -> Error (Usage m)
      | Ok m -> (
          if
            not
              (same_plan (List.map (fun (p, _, _) -> p) m.summary.runs) planned)
          then
            Error
              (Usage
                 (Printf.sprintf
                    "experiment %s was planned from different scenarios, \
                     variants, repeats or seed than %s now gives"
                    id experiment_file))
          else
            match Fingerprint.changes ~recorded:m.fingerprint l.fingerprint with
            | [] -> Ok (id, for_resume (m.summary.runs, m.previous_run_ids))
            | changes ->
                Error
                  (Usage
                     (Printf.sprintf
                        "experiment %s cannot be resumed because %s since it \
                         was started: start a new experiment instead"
                        id
                        (String.concat ", " changes)))))

let run_experiment ?root ?(random_bytes = fun n -> Mirage_crypto_rng.generate n)
    ?(announce = fun (_ : string) -> ()) ?(warn = fun (_ : string) -> ()) ?keys
    ?(upstream = P.default_upstream) ?connector ?sandbox
    ?(on_sandbox = fun (_ : Sandbox.label) -> ()) ~bactrian_exe ~env ?files
    ?(vet = fun (_ : contents) -> Ok ()) ~experiment_file ~resume () =
  let fs = Eio.Stdenv.fs env in
  let files = Option.value files ~default:fs in
  let root =
    match root with
    | Some r -> r
    | None -> Eio.Path.(Eio.Stdenv.cwd env / ".bactrian")
  in
  let* l = load ~fs:files ~experiment_file ~sandbox_override:sandbox in
  let* () =
    Result.map_error
      (fun m -> Usage m)
      (vet { experiment = l.experiment; dir = l.dir; scenarios = l.scenarios })
  in
  let experiment = l.experiment in
  let planned =
    plan ~seed:experiment.seed ~scenarios:(List.map fst l.scenarios)
      ~variants:
        (List.map (fun (v : Experiment.variant) -> v.name) experiment.variants)
      ~repeats:experiment.repeats
  in
  let keys =
    match keys with Some k -> k | None -> Run.keys_from_environment ()
  in
  let* experiment_id, initial =
    start_or_resume ~root ~experiment_file ~random_bytes ~env ~resume l planned
  in
  let* () = agent_preflight ~connector l.agent in
  let* () = preflight ~caps:experiment.caps_usd ~keys ~upstream ~connector in
  let in_dir path =
    if Filename.is_relative path then Filename.concat l.dir path else path
  in
  let mode = Option.value sandbox ~default:experiment.sandbox in
  let* sandbox =
    match l.agent with
    | Http _ -> Ok (Sandbox.http_mode mode)
    | Command _ ->
        Result.map_error
          (fun m -> Environment m)
          (Sandbox.preflight ~env ~warn
             {
               mode;
               allow_write = List.map in_dir experiment.sandbox_allow_write;
               deny_read = List.map in_dir experiment.sandbox_deny_read;
               bactrian_exe;
               bactrian_dir = Eio.Path.native_exn root;
             })
  in
  on_sandbox (Sandbox.label sandbox);
  Option.iter warn (Sandbox.mcp_warning sandbox (mcp_configs ~fs l));
  let* jobs =
    List.mapi
      (fun i (p : planned) ->
        match
          ( List.assoc_opt p.scenario l.scenarios,
            List.find_opt
              (fun (v : Experiment.variant) -> String.equal v.name p.variant)
              experiment.variants )
        with
        | Some scenario, Some variant ->
            Ok
              ( i,
                run_config ~fs ~sandbox l scenario
                  ~scenario_dir:
                    (Filename.concat l.dir (Filename.dirname p.scenario))
                  variant )
        | None, _ | _, None ->
            Error (Usage ("the plan names an unknown run " ^ p.scenario)))
      planned
    |> List.fold_left
         (fun acc job ->
           let* jobs = acc in
           let* job = job in
           Ok (job :: jobs))
         (Ok [])
    |> Result.map List.rev
  in
  let dir = Eio.Path.(root / "experiments" / experiment_id) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  with_lock ~experiment_id dir @@ fun () ->
  match
    Ledger.create
      ~path:Eio.Path.(dir / "ledger.jsonl")
      ~caps:experiment.caps_usd
  with
  | exception Ledger.Corrupt { line; reason } ->
      Error
        (Environment
           (Printf.sprintf
              "the experiment's spend ledger is corrupt (line %d: %s)" line
              reason))
  | exception (Eio.Io _ as e) -> Error (Environment (Printexc.to_string e))
  | ledger ->
      Eio.Switch.run @@ fun sw ->
      let* proxy =
        Result.map_error
          (fun m -> Environment m)
          (P.start ~sw ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
             ~process_mgr:(Eio.Stdenv.process_mgr env))
      in
      announce experiment_id;
      List.iter warn (Experiment.unenforced experiment);
      let store = Store.create ~root in
      let table = Array.of_list (List.map fst initial) in
      let previous = Array.of_list (List.map snd initial) in
      let lock = Eio.Mutex.create () in
      let write_manifest () =
        Eio.Mutex.use_rw ~protect:true lock (fun () ->
            Store.write_atomic
              Eio.Path.(dir / "manifest.json")
              (Yojson.Safe.pretty_to_string
                 (manifest_json ~experiment_id ~experiment ~experiment_file
                    ~fingerprint:l.fingerprint
                    (List.combine (Array.to_list table) (Array.to_list previous)))
              ^ "\n"))
      in
      let set i f =
        let p, status, id = table.(i) in
        table.(i) <- f (p, status, id)
      in
      let capped = ref false in
      let execute (i, config) =
        let _, status, _ = table.(i) in
        let skip =
          match status with Done _ -> true | Pending | Interrupted -> !capped
        in
        if not skip then (
          let finish status run_id =
            set i (fun (p, _, _) -> (p, status, run_id))
          in
          (match
             Run.execute ~random_bytes
               ~on_start:(fun id -> set i (fun (p, s, _) -> (p, s, Some id)))
               ~keys ~upstream ?connector ~env ~proxy ~store ~ledger config
           with
          | Ok r ->
              (match r.outcome with
              | Not_run _ -> capped := true
              | Passed | Failed _ | Diverged | Errored _ -> ());
              finish (Done r.outcome) (Some r.run_id)
          | Error m -> set i (fun (p, _, id) -> (p, Done (Errored m), id))
          | exception (Eio.Cancel.Cancelled _ as ex) ->
              set i (fun (p, _, id) -> (p, Interrupted, id));
              raise ex
          | exception ((Eio.Io _ | Eio.Mutex.Poisoned _) as e) ->
              set i (fun (p, _, id) ->
                  (p, Done (Errored (Printexc.to_string e)), id)));
          write_manifest ())
      in
      write_manifest ();
      (match
         Eio.Fiber.List.iter ~max_fibers:experiment.concurrency execute jobs
       with
      | () -> ()
      | exception (Eio.Cancel.Cancelled _ as ex) ->
          write_manifest ();
          raise ex);
      Ok { experiment_id; runs = Array.to_list table }
