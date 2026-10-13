open Bactrian_core
module P = Model_proxy
module D = Agent_driver

type outcome =
  | Passed
  | Failed of string
  | Diverged
  | Errored of string
  | Not_run of string

let outcome_kind = function
  | Passed -> "passed"
  | Failed _ -> "failed"
  | Diverged -> "diverged"
  | Errored _ -> "errored"
  | Not_run _ -> "not_run"

let outcome_reason = function
  | Passed | Diverged -> None
  | Failed r | Errored r | Not_run r -> Some r

type replay = {
  from : string;
  matcher : Matcher.mode;
  allow_reorder : bool;
  timing : P.timing;
}

type config = {
  agent : string list;
  cwd : string;
  turns : string list;
  scenario : string;
  variant : string;
  sim_now : string;
  timezone : string;
  replay : replay option;
  env_passthrough : string list;
  turn_timeout : float;
  total_timeout : float;
}

type result = {
  run_id : string;
  outcome : outcome;
  run_dir : string;
  findings : P.findings;
  spend : (string * float) list;
}

let ( let* ) = Result.bind

(* Providers: the segment (also the ledger's name) and the base URL variable
   the agent gets. *)
let table =
  [
    (P.Openai, "openai", "OPENAI_BASE_URL");
    (P.Anthropic, "anthropic", "ANTHROPIC_BASE_URL");
    (P.Gemini, "gemini", "GOOGLE_GEMINI_BASE_URL");
    (P.Openrouter, "openrouter", "OPENROUTER_BASE_URL");
  ]

let providers = List.map (fun (p, name, _) -> (p, name)) table

let key_variable : P.provider -> string = function
  | Openai -> "OPENAI_API_KEY"
  | Anthropic -> "ANTHROPIC_API_KEY"
  | Gemini -> "GEMINI_API_KEY"
  | Openrouter -> "OPENROUTER_API_KEY"

let keys_from_environment () =
  List.filter_map
    (fun (p, _) ->
      match Sys.getenv_opt (key_variable p) with
      | Some k when not (String.equal k "") -> Some (p, k)
      | Some _ | None -> None)
    providers

(* Formatting *)

let hex s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02x" (Char.code s.[i])))

let rfc3339 t =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let run_id_of ~random_bytes now =
  let tm = Unix.gmtime now in
  Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ-%s" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
    (hex (random_bytes 4))

let store_error e = Format.asprintf "%a" Store.pp_error e

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let describe_exit : Eio.Process.exit_status -> string = function
  | `Exited n -> Printf.sprintf "exit status %d" n
  | `Signaled n -> Printf.sprintf "killed by signal %d" n

let describe_driver_error : D.error -> string = function
  | Spawn_failed s -> "spawn_failed: " ^ s
  | Write_failed s -> "write_failed: " ^ s
  | Protocol_violation { line; error = _ } -> "protocol_violation: " ^ line
  | Line_too_long -> "protocol_violation: line over 1 MiB"
  | Turn_timeout -> "timeout: turn"
  | Total_timeout -> "timeout: total"
  | Agent_exited None -> "crashed: stdout closed before the final reply"
  | Agent_exited (Some st) ->
      "crashed: " ^ describe_exit st ^ " before the final reply"

let describe_refusal : Ledger.refusal -> string = function
  | Cap_reached { provider; cap; spent; requested } ->
      Printf.sprintf
        "cap reached for %s (cap %.4f USD, spent %.4f, requested %.4f)" provider
        cap spent requested
  | Unknown_model m -> "no price for model " ^ m

(* The recording a replay reads *)

let recording_id from =
  let trimmed =
    if String.ends_with ~suffix:"/" from then
      String.sub from 0 (String.length from - 1)
    else from
  in
  Filename.basename trimmed

let recorded_turns store ~run_id =
  let* dir = Result.map_error store_error (Store.run_dir store ~run_id) in
  match Eio.Path.load Eio.Path.(dir / "trace.jsonl") with
  | exception Eio.Io _ ->
      Error
        (Printf.sprintf "recording %s has no trace.jsonl: pass the turns" run_id)
  | text ->
      String.split_on_char '\n' text
      |> List.filter (fun l -> not (String.equal l ""))
      |> List.fold_left
           (fun acc line ->
             let* turns = acc in
             match Trace.of_line line with
             | Ok { event = Turn_start { content; _ }; _ } ->
                 Ok (content :: turns)
             | Ok
                 {
                   event =
                     ( Run_start _ | Model_call _ | Tool_call _ | Tool_result _
                     | Http_call _ | Agent_event _ | Reply _
                     | Sandbox_violation _ | Done );
                   _;
                 } ->
                 Ok turns
             | Error e ->
                 Error (Format.asprintf "trace.jsonl: %a" Trace.pp_error e))
           (Ok [])
      |> Result.map List.rev

(* The agent's environment *)

let base_env =
  [ "PATH"; "HOME"; "LANG"; "TERM" ]
  |> List.filter_map (fun k -> Option.map (fun v -> (k, v)) (Sys.getenv_opt k))

let build_env config ~proxy ~token ~run_id ~state_dir ~sim_now =
  let ours =
    [
      ("SIM_NOW", sim_now);
      ("TZ", config.timezone);
      ("BACTRIAN_RUN_ID", run_id);
      ("BACTRIAN_STATE_DIR", state_dir);
    ]
    @ List.concat_map
        (fun (p, _, url_var) ->
          [
            (url_var, P.base_url proxy ~token p);
            (key_variable p, "bactrian-" ^ token);
          ])
        table
  in
  let passthrough =
    List.filter_map
      (fun k -> Option.map (fun v -> (k, v)) (Sys.getenv_opt k))
      config.env_passthrough
  in
  (* Later entries win, so nothing the caller passes through can replace a
     placeholder key or a bactrian variable. *)
  List.fold_left
    (fun acc (k, v) -> (k, v) :: List.remove_assoc k acc)
    []
    (base_env @ passthrough @ ours)

(* The conversation *)

let converse ~t_ms ~add driver ~(start : Agent_protocol.to_agent) turns =
  let send m = Result.map_error describe_driver_error (D.send driver m) in
  let rec await n =
    match D.recv driver with
    | Error e -> Error (describe_driver_error e)
    | Ok (Event { turn; name; data }) ->
        add (Trace_builder.Got_event { t_ms = t_ms (); turn; name; data });
        await n
    | Ok (Reply { turn; content }) when turn = n ->
        add (Got_reply { t_ms = t_ms (); turn; content });
        Ok ()
    | Ok (Reply { turn; content = _ }) ->
        Error
          (Printf.sprintf "protocol_violation: reply for turn %d, expected %d"
             turn n)
    | Ok (Agent_error { turn; message }) ->
        add (Got_error { t_ms = t_ms (); turn; message });
        Error ("agent_error: " ^ message)
  in
  let* () = send start in
  let rec go n = function
    | [] -> send End
    | content :: rest ->
        add (Sent_turn { t_ms = t_ms (); turn = n; content });
        let* () = send (Turn { turn = n; content }) in
        let* () = await n in
        go (n + 1) rest
  in
  go 1 turns

(* Outcome *)

let refusing ~allow_reorder : Matcher.divergence -> bool = function
  | Reordered _ -> not allow_reorder
  | Missing_request _ | Unconsumed _ | Shape_mismatch _ -> true

let decide_outcome ~replay (findings : P.findings) failure =
  match (findings.cap_refusals, failure) with
  | r :: _, _ -> Not_run (describe_refusal r)
  | [], _ -> (
      let diverged =
        match replay with
        | None -> false
        | Some r ->
            List.exists
              (refusing ~allow_reorder:r.allow_reorder)
              findings.divergences
      in
      match (diverged, failure) with
      | true, _ -> Diverged
      | false, Some reason -> Failed reason
      | false, None -> Passed)

(* Files *)

let meta_json ~run_id ~config ~sim_now ~outcome ~(findings : P.findings) ~spend
    ~replay_of =
  let count l = `Int (List.length l) in
  `Assoc
    ([
       ("run_id", `String run_id);
       ("scenario", `String config.scenario);
       ("variant", `String config.variant);
       ("bactrian_version", `String Version.current);
       ("sim_now", `String sim_now);
       ("clock", `String "contract_only");
       ("sandbox", `String "unsandboxed");
       ("outcome", `String (outcome_kind outcome));
       ( "reason",
         match outcome_reason outcome with Some r -> `String r | None -> `Null
       );
       ( "findings",
         `Assoc
           [
             ("exchanges", `Int findings.exchanges);
             ("transport_errors", `Int findings.transport_errors);
             ("usage_gaps", `Int findings.usage_gaps);
             ("refused_keys", `Int findings.refused_keys);
             ("cap_refusals", count findings.cap_refusals);
             ("divergences", count findings.divergences);
             ("errors", count findings.errors);
           ] );
       ("spend_usd", `Assoc (List.map (fun (p, usd) -> (p, `Float usd)) spend));
     ]
    @ match replay_of with Some r -> [ ("replay_of", `String r) ] | None -> [])

let write_trace dir entries =
  Store.write_atomic
    Eio.Path.(dir / "trace.jsonl")
    (String.concat "" (List.map (fun e -> Trace.to_line e ^ "\n") entries))

let spent_by_provider ledger =
  List.map
    (fun (_, name) -> (name, Ledger.spent ledger ~provider:name))
    providers

let no_findings : P.findings =
  {
    divergences = [];
    exchanges = 0;
    transport_errors = 0;
    usage_gaps = 0;
    refused_keys = 0;
    cap_refusals = [];
    errors = [];
  }

(* One run *)

let execute ?(random_bytes = fun n -> Mirage_crypto_rng.generate n) ?keys
    ?(upstream = P.default_upstream) ?(connector = Upstream.plain) ~env ~proxy
    ~store ~ledger config =
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let run_id = run_id_of ~random_bytes now in
  let token = hex (random_bytes 16) in
  let keys = match keys with Some k -> k | None -> keys_from_environment () in
  let* run_dir =
    Result.map_error store_error (Store.ensure_run_dir store ~run_id)
  in
  let* recording =
    match config.replay with
    | None -> Ok None
    | Some r ->
        let id = recording_id r.from in
        let* cassette =
          Result.map_error store_error (Store.read_cassette store ~run_id:id)
        in
        Ok (Some (r, id, cassette))
  in
  let* turns =
    match (config.turns, recording) with
    | [], Some (_, id, _) -> recorded_turns store ~run_id:id
    | turns, (Some _ | None) -> Ok turns
  in
  let sim_now =
    match (config.sim_now, recording) with
    | "", Some (_, _, c) -> c.header.sim_now
    | "", None -> rfc3339 now
    | s, (Some _ | None) -> s
  in
  let run_dir_path = absolute (Eio.Path.native_exn run_dir) in
  let state_dir = Eio.Path.(run_dir / "state") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 state_dir;
  let state_dir_path = Filename.concat run_dir_path "state" in
  let header : Cassette.header =
    {
      run_id;
      scenario = config.scenario;
      variant = config.variant;
      bactrian_version = Version.current;
      recorded_at = rfc3339 now;
      sim_now;
      clock = Contract_only;
    }
  in
  let mode : P.mode =
    match recording with
    | None -> Record
    | Some (r, _, cassette) ->
        Replay
          {
            recording = cassette;
            matcher = r.matcher;
            allow_reorder = r.allow_reorder;
            timing = r.timing;
          }
  in
  let spend_before = spent_by_provider ledger in
  let started = ref 0. in
  let t_ms () = Float.to_int ((Eio.Time.now clock -. !started) *. 1000.) in
  let events = ref [] in
  let add e = events := e :: !events in
  let registered = ref false in
  let findings_cell = ref None in
  let unregister () =
    match !findings_cell with
    | Some f -> f
    | None ->
        let f =
          if !registered then P.unregister proxy ~token else no_findings
        in
        findings_cell := Some f;
        f
  in
  let drive () =
    Eio.Switch.run @@ fun sw ->
    let* writer =
      Result.map Option.some
        (Result.map_error store_error (Store.open_cassette ~sw store header))
    in
    started := Eio.Time.now clock;
    P.register proxy
      {
        run_id;
        token;
        mode;
        store;
        cassette = writer;
        ledger;
        keys;
        upstream;
        connector;
        run_started = !started;
      };
    registered := true;
    Fun.protect ~finally:(fun () -> ignore (unregister ())) @@ fun () ->
    let driver_config : D.config =
      {
        argv = config.agent;
        env =
          build_env config ~proxy ~token ~run_id ~state_dir:state_dir_path
            ~sim_now;
        cwd = Some Eio.Path.(Eio.Stdenv.fs env / config.cwd);
        stderr_path = Eio.Path.(run_dir / "agent.stderr");
        turn_timeout = config.turn_timeout;
        total_timeout = config.total_timeout;
        exit_grace = config.total_timeout;
        kill_grace = 5.;
      }
    in
    let start : Agent_protocol.to_agent =
      Start
        {
          run_id;
          scenario = config.scenario;
          variant = config.variant;
          sim_now;
          state_dir = state_dir_path;
        }
    in
    let session =
      D.with_agent ~process_mgr:(Eio.Stdenv.process_mgr env)
        ~clock driver_config (fun d -> converse ~t_ms ~add d ~start turns)
    in
    let failure =
      match session with
      | Error e -> Some (describe_driver_error e)
      | Ok (Error reason, _) -> Some reason
      | Ok (Ok (), `Exited 0) -> None
      | Ok (Ok (), ((`Exited _ | `Signaled _) as st)) ->
          Some ("exit_nonzero: " ^ describe_exit st)
    in
    let findings = unregister () in
    Option.iter Store.close writer;
    Ok (failure, findings)
  in
  let driven =
    try drive () with Eio.Io _ as e -> Error (Printexc.to_string e)
  in
  let failure, findings, fault =
    match driven with
    | Ok (failure, findings) -> (failure, findings, None)
    | Error msg -> (None, unregister (), Some msg)
  in
  let replay = Option.map (fun (r, _, _) -> r) recording in
  let cassette =
    Result.map_error store_error (Store.read_cassette store ~run_id)
  in
  let fault, cassette =
    match (fault, cassette) with
    | None, Ok c -> (None, c)
    | Some m, Ok c -> (Some m, c)
    | Some m, Error _ ->
        (Some m, { Cassette.header; exchanges = []; complete = true })
    | None, Error m ->
        (Some m, { Cassette.header; exchanges = []; complete = true })
  in
  let outcome =
    match fault with
    | Some m -> Errored m
    | None -> decide_outcome ~replay findings failure
  in
  let spend =
    List.filter_map
      (fun (name, after) ->
        let before =
          Option.value (List.assoc_opt name spend_before) ~default:0.
        in
        if after > before then Some (name, after -. before) else None)
      (spent_by_provider ledger)
  in
  let entries =
    Trace_builder.build
      {
        run_id;
        scenario = config.scenario;
        variant = config.variant;
        sim_now;
        clock = Contract_only;
        cassette;
        agent = List.rev !events;
        resolve = (fun p -> Result.to_option (Store.read_payload store p));
      }
  in
  match
    write_trace run_dir entries;
    Store.write_atomic
      Eio.Path.(run_dir / "meta.json")
      (Yojson.Safe.pretty_to_string
         (meta_json ~run_id ~config ~sim_now ~outcome ~findings ~spend
            ~replay_of:(Option.map (fun (_, id, _) -> id) recording))
      ^ "\n")
  with
  | () -> Ok { run_id; outcome; run_dir = run_dir_path; findings; spend }
  | exception (Eio.Io _ as e) -> Error (Printexc.to_string e)
