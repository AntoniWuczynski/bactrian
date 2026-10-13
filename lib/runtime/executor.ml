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
}
[@@deriving yojson { strict = true }]

type wire = {
  version : int;
  experiment_id : string;
  experiment : string;
  experiment_file : string;
  seed : int;
  runs : wire_run list;
}
[@@deriving yojson { strict = true }]

let wire_run ((p : planned), status, run_id) : wire_run =
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
              | Ok runs ->
                  Ok
                    {
                      name = w.experiment;
                      experiment_file = w.experiment_file;
                      seed = w.seed;
                      summary =
                        {
                          experiment_id = w.experiment_id;
                          runs = List.rev runs;
                        };
                    }
              | Error m -> bad m)))

(* Preflight *)

let funded caps =
  List.filter
    (fun (_, name) ->
      match List.assoc_opt name caps with Some usd -> usd > 0. | None -> false)
    Run.providers

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
      | None ->
          if
            Option.is_none connector
            && List.exists
                 (fun (p, _) ->
                   Option.equal String.equal
                     (Uri.scheme (upstream p))
                     (Some "https"))
                 funded
          then
            Error
              (Environment
                 "this build has no TLS support (bactrian.tls), so it cannot \
                  call providers over HTTPS")
          else Ok ())

(* Loading *)

let located_all results =
  match List.concat_map (function Ok _ -> [] | Error e -> e) results with
  | [] -> Ok (List.filter_map Result.to_option results)
  | errors -> Error (Configuration errors)

let valid_id id =
  not
    (String.equal id "" || String.equal id "." || String.equal id ".."
   || String.contains id '/' || String.contains id '\000')

let manifest_json ~experiment_id ~(experiment : Experiment.t) ~experiment_file
    runs =
  wire_to_yojson
    {
      version = 1;
      experiment_id;
      experiment = experiment.name;
      experiment_file;
      seed = experiment.seed;
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

type loaded = {
  experiment : Experiment.t;
  dir : string;
  scenarios : (string * Scenario.t) list;  (** Path relative to [dir]. *)
  argv : string list;
  turn_timeout : float;
  total_timeout : float;
  pricing : Pricing.table;
}

(* [expand_globs] knows only the pattern: point at it in the experiment file. *)
let locate_globs ~fs ~experiment_file (experiment : Experiment.t) errors =
  let find =
    match Config_loader.load Eio.Path.(fs / experiment_file) with
    | Ok (_, find) -> find
    | Error _ -> fun _ -> None
  in
  List.map
    (fun (e : Config_loader.located_error) ->
      let index = List.find_index (String.equal e.path) experiment.scenarios in
      let path =
        match index with
        | Some i -> Printf.sprintf "scenarios[%d]" i
        | None -> "scenarios"
      in
      let line, column = Option.value (find path) ~default:(1, 1) in
      { e with file = experiment_file; line; column; path })
    errors

let load ~fs ~experiment_file =
  let* experiment =
    Result.map_error
      (fun e -> Configuration e)
      (Config_loader.load_experiment Eio.Path.(fs / experiment_file))
  in
  let dir = Filename.dirname experiment_file in
  let base = Eio.Path.(fs / dir) in
  let* files =
    Result.map_error
      (fun errors ->
        Configuration (locate_globs ~fs ~experiment_file experiment errors))
      (Config_loader.expand_globs ~base experiment.scenarios)
  in
  let* scenarios =
    located_all
      (List.map
         (fun rel ->
           Result.map
             (fun s -> (rel, s))
             (Config_loader.load_scenario Eio.Path.(base / rel)))
         files)
  in
  let* pricing =
    match experiment.pricing with
    | None -> Ok Pricing.default
    | Some file ->
        Result.map_error
          (fun e -> Configuration e)
          (Config_loader.load_pricing Eio.Path.(base / file))
  in
  match experiment.agent with
  | Http _ -> Error (Usage "http agents are not supported yet")
  | Command { argv; turn_timeout; total_timeout } ->
      Ok
        {
          experiment;
          dir;
          scenarios;
          argv;
          turn_timeout;
          total_timeout;
          pricing;
        }

(* Running *)

let run_config (l : loaded) (scenario : Scenario.t) ~scenario_dir
    (variant : Experiment.variant) : Run.config =
  {
    agent = l.argv;
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
    turn_timeout = l.turn_timeout;
    total_timeout = l.total_timeout;
  }

let run_experiment ?root ?(random_bytes = fun n -> Mirage_crypto_rng.generate n)
    ?(announce = fun (_ : string) -> ()) ?keys ?(upstream = P.default_upstream)
    ?connector ~env ~experiment_file ~resume () =
  let fs = Eio.Stdenv.fs env in
  let root =
    match root with
    | Some r -> r
    | None -> Eio.Path.(Eio.Stdenv.cwd env / ".bactrian")
  in
  let* l = load ~fs ~experiment_file in
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
    match resume with
    | None ->
        let id =
          Run.fresh_id ~random_bytes (Eio.Time.now (Eio.Stdenv.clock env))
        in
        Ok (id, List.map (fun p -> (p, Pending, None)) planned)
    | Some id when not (valid_id id) ->
        Error (Usage (Printf.sprintf "invalid experiment ID %S" id))
    | Some id -> (
        let path = Eio.Path.(root / "experiments" / id / "manifest.json") in
        match read_manifest path with
        | Error None -> Error (Usage (Printf.sprintf "no experiment %S" id))
        | Error (Some m) -> Error (Usage m)
        | Ok { summary = { runs; _ }; _ }
          when same_plan (List.map (fun (p, _, _) -> p) runs) planned ->
            Ok (id, runs)
        | Ok _ ->
            Error
              (Usage
                 (Printf.sprintf
                    "experiment %s was planned from different scenarios, \
                     variants, repeats or seed than %s now gives"
                    id experiment_file)))
  in
  let* () = preflight ~caps:experiment.caps_usd ~keys ~upstream ~connector in
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
                run_config l scenario
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
          (P.start ~sw ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env))
      in
      announce experiment_id;
      let store = Store.create ~root in
      let table = Array.of_list initial in
      let lock = Eio.Mutex.create () in
      let write_manifest () =
        Eio.Mutex.use_rw ~protect:true lock (fun () ->
            Store.write_atomic
              Eio.Path.(dir / "manifest.json")
              (Yojson.Safe.pretty_to_string
                 (manifest_json ~experiment_id ~experiment ~experiment_file
                    (Array.to_list table))
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
