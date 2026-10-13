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

type checks = {
  assertions : Assertion.t list;
  checkers : string list list;
  checks_cwd : string;
  checker_timeout : float;
}

type agent =
  | Command of string list
  | Http of { url : string; namespace_header : string }

type config = {
  agent : agent;
  cwd : string;
  turns : string list;
  scenario : string;
  variant : string;
  sim_now : string;
  timezone : string;
  replay : replay option;
  env_passthrough : string list;
  variant_env : (string * string) list;
  checks : checks option;
  unenforced : string list;
  fixtures : string option;
  normalise : string list;
  redact : string list;
  pricing : Pricing.table;
  turn_timeout : float;
  total_timeout : float;
  mcp : Mcp_proxy.config list;
  sandbox : Sandbox.t;
  faketime : bool;
}

type result = {
  run_id : string;
  outcome : outcome;
  run_dir : string;
  findings : P.findings;
  spend : float;
  sandbox : Sandbox.label;
  violations : Sandbox.violations;
  clock : Clock_label.t;
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

let overridden_upstreams upstream =
  List.filter_map
    (fun (p, name) ->
      let url = upstream p in
      if Uri.equal url (P.default_upstream p) then None else Some (name, url))
    providers

let keys_from_environment () =
  List.filter_map
    (fun (p, _) ->
      match Sys.getenv_opt (key_variable p) with
      | Some k when not (String.equal k "") -> Some (p, k)
      | Some _ | None -> None)
    providers

(* Formatting *)

let rfc3339 t =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let fresh_id ~random_bytes now =
  let tm = Unix.gmtime now in
  Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ-%s" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
    (Proc_util.hex (random_bytes 4))

let store_error e = Format.asprintf "%a" Store.pp_error e

let describe_driver_error : D.error -> string = function
  | Spawn_failed s -> "spawn_failed: " ^ s
  | Write_failed s -> "write_failed: " ^ s
  | Protocol_violation { line; error = _ } -> "protocol_violation: " ^ line
  | Line_too_long -> "protocol_violation: line over 1 MiB"
  | Turn_timeout -> "timeout: turn"
  | Total_timeout -> "timeout: total"
  | Agent_exited None ->
      "protocol_violation: stdout closed before the final reply"
  | Agent_exited (Some st) ->
      "protocol_violation: stdout closed before the final reply ("
      ^ Proc_util.describe_exit st ^ ")"
  | Http_error s -> "agent_http_error: " ^ s
  | Http_status { status; body } ->
      Printf.sprintf "agent_http_error: status %d: %s" status body
  | No_message -> "protocol_violation: the response ended before the reply"

let describe_refusal : Ledger.refusal -> string = function
  | Cap_reached { provider; cap; spent; requested } ->
      Printf.sprintf
        "cap reached for %s (cap %.4f USD, spent %.4f, requested %.4f)" provider
        cap spent requested
  | Unknown_model m -> "no price for model " ^ m
  | Unreadable { line; reason } ->
      Printf.sprintf "the spend ledger cannot be read (line %d: %s)" line reason

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

(* A recording's meta.json fields. Recordings from before a field was saved
   lack it, and a recording with no meta.json has none of them. *)
let recorded_meta store ~run_id =
  let* dir = Result.map_error store_error (Store.run_dir store ~run_id) in
  match Eio.Path.load Eio.Path.(dir / "meta.json") with
  | exception Eio.Io _ -> Ok []
  | text -> (
      match Yojson.Safe.from_string text with
      | exception Yojson.Json_error m ->
          Error (Printf.sprintf "recording %s: meta.json: %s" run_id m)
      | `Assoc fields -> Ok fields
      | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
          Error
            (Printf.sprintf "recording %s: meta.json is not an object" run_id))

(* The patterns a recording was made under. *)
let recorded_patterns store ~run_id =
  let* fields = recorded_meta store ~run_id in
  let strings key =
    match List.assoc_opt key fields with
    | None -> Ok []
    | Some (`List items) ->
        List.fold_right
          (fun item acc ->
            let* rest = acc in
            match item with
            | `String s -> Ok (s :: rest)
            | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `List _
            | `Assoc _ ->
                Error
                  (Printf.sprintf "recording %s: meta.json %s: not a string"
                     run_id key))
          items (Ok [])
    | Some
        (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _)
      ->
        Error
          (Printf.sprintf "recording %s: meta.json %s: not a list" run_id key)
  in
  let* normalise = strings "normalise" in
  let* redact = strings "redact" in
  Ok (normalise, redact)

let recorded_mcp store ~from =
  let run_id = recording_id from in
  let* fields = recorded_meta store ~run_id in
  match List.assoc_opt "mcp" fields with
  | None -> Ok []
  | Some json ->
      Result.map_error
        (Printf.sprintf "recording %s: meta.json mcp: %s" run_id)
        (Mcp_meta.replaying json)

(* The agent's environment *)

let base_env =
  [ "PATH"; "LANG"; "TERM" ]
  |> List.filter_map (fun k -> Option.map (fun v -> (k, v)) (Sys.getenv_opt k))

(* The agent's home and caches are its state directory, and its temporary files
   go to the run's temporary directory, sandboxed or not, so the environment
   is the same either way (design §5 step 2). They belong to the agent's own
   process, so an HTTP-mode agent is not sent them (design §5.1). *)
let home_env ~state_dir ~temp_dir =
  let under = Filename.concat state_dir in
  [
    ("HOME", state_dir);
    ("XDG_CACHE_HOME", under ".cache");
    ("XDG_CONFIG_HOME", under ".config");
    ("XDG_DATA_HOME", under ".local/share");
    ("npm_config_cache", under ".npm");
    ("TMPDIR", temp_dir);
    ("PYTHONDONTWRITEBYTECODE", "1");
  ]

(* Later entries win. *)
let merge vars =
  List.fold_left (fun acc (k, v) -> (k, v) :: List.remove_assoc k acc) [] vars

(* What bactrian sets for one run (design §5 step 2), with the variant's own
   variables. A command agent gets them as its environment and an HTTP agent in
   the [env] of its [start] message (design §5.1), so the two cannot differ. *)
let run_variables config ~proxy ~port ~token ~run_id ~state_dir ~sim_now =
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
            (url_var, P.base_url proxy ~port ~token p);
            (key_variable p, "bactrian-" ^ token);
          ])
        table
    @ (match config.mcp with
      | [] -> []
      | _ :: _ -> [ (Mcp_proxy.url_variable, P.mcp_url proxy ~port ~token) ])
    @ List.filter_map
        (fun (c : Mcp_proxy.config) ->
          match c.server with
          | Http _ ->
              Some
                ( Mcp_proxy.http_variable c.name,
                  P.mcp_http_url proxy ~port ~token c.name )
          | Stdio _ -> None)
        config.mcp
  in
  (* Ours come last, so a variant cannot replace a placeholder key or a
     bactrian variable. *)
  merge (config.variant_env @ ours)

let process_env config variables ~state_dir ~temp_dir =
  let passthrough =
    List.filter_map
      (fun k -> Option.map (fun v -> (k, v)) (Sys.getenv_opt k))
      config.env_passthrough
  in
  merge (base_env @ passthrough @ variables @ home_env ~state_dir ~temp_dir)

(* The conversation *)

let wrong_turn ~what ~turn ~expected =
  Printf.sprintf "protocol_violation: %s for turn %d, expected %d" what turn
    expected

let converse ~t_ms ~add driver ~(start : Agent_protocol.to_agent) turns =
  let send m = Result.map_error describe_driver_error (D.send driver m) in
  let rec await n =
    match D.recv driver with
    | Error e -> Error (describe_driver_error e)
    | Ok (Event { turn; name; data }) when turn = n ->
        add (Trace_builder.Got_event { t_ms = t_ms (); turn; name; data });
        await n
    | Ok (Event { turn; name = _; data = _ }) ->
        Error (wrong_turn ~what:"event" ~turn ~expected:n)
    | Ok (Reply { turn; content }) when turn = n ->
        add (Got_reply { t_ms = t_ms (); turn; content });
        Ok ()
    | Ok (Reply { turn; content = _ }) ->
        Error (wrong_turn ~what:"reply" ~turn ~expected:n)
    | Ok (Agent_error { turn; message }) when turn = n ->
        add (Got_error { t_ms = t_ms (); turn; message });
        Error ("agent_error: " ^ message)
    | Ok (Agent_error { turn; message = _ }) ->
        Error (wrong_turn ~what:"error" ~turn ~expected:n)
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
  (* An unpriced model is a configuration problem and an unreadable ledger a
     fault, not missing data. *)
  let unpriced =
    List.find_map
      (function
        | (Ledger.Unknown_model _ as r) | (Unreadable _ as r) ->
            Some (describe_refusal r)
        | Cap_reached _ -> None)
      findings.cap_refusals
  in
  match (unpriced, findings.cap_refusals, failure) with
  | Some m, _, _ -> Errored m
  | None, r :: _, _ -> Not_run (describe_refusal r)
  | None, [], _ -> (
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
    ~latency_ms ~tokens ~normalise ~redact ~replay_of ~violations ~clock
    ~upstreams =
  let count l = `Int (List.length l) in
  `Assoc
    ([
       ("run_id", `String run_id);
       ("scenario", `String config.scenario);
       ("variant", `String config.variant);
       ("bactrian_version", `String Version.current);
       ("sim_now", `String sim_now);
       ("clock", Clock_label.to_json clock);
       ( "upstreams",
         `Assoc
           (List.map
              (fun (name, url) -> (name, `String (Uri.to_string url)))
              upstreams) );
       ("sandbox", Sandbox.label_json (Sandbox.label config.sandbox));
       ("sandbox_violations", Sandbox.violations_json violations);
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
             ("normalise_masks", `Int findings.masks_fired);
             ("cap_refusals", count findings.cap_refusals);
             ("divergences", count findings.divergences);
             ("errors", count findings.errors);
           ] );
       ("cost_usd", `Float spend);
       ( "latency_ms",
         match latency_ms with Some ms -> `Float ms | None -> `Null );
       ("tokens", match tokens with Some n -> `Int n | None -> `Null);
       ("normalise", `List (List.map (fun p -> `String p) normalise));
       ("redact", `List (List.map (fun p -> `String p) redact));
       ("mcp", Mcp_meta.to_json config.mcp);
       ("unenforced", `List (List.map (fun w -> `String w) config.unenforced));
     ]
    @ match replay_of with Some r -> [ ("replay_of", `String r) ] | None -> [])

let write_trace dir entries =
  Store.write_atomic
    Eio.Path.(dir / "trace.jsonl")
    (String.concat "" (List.map (fun e -> Trace.to_line e ^ "\n") entries))

let no_findings : P.findings =
  {
    divergences = [];
    exchanges = 0;
    transport_errors = 0;
    usage_gaps = 0;
    refused_keys = 0;
    masks_fired = 0;
    cap_refusals = [];
    errors = [];
  }

let compile_patterns ~what sources =
  List.fold_right
    (fun source acc ->
      let* rest = acc in
      match Regex.compile source with
      | Ok re -> Ok (re :: rest)
      | Error reason ->
          Error (Printf.sprintf "invalid %s pattern %S: %s" what source reason))
    sources (Ok [])

let redaction_patterns sources =
  List.fold_right
    (fun source acc ->
      let* rest = acc in
      match Redact.pattern source with
      | Ok p -> Ok (p :: rest)
      | Error e -> Error (Format.asprintf "%a" Redact.pp_error e))
    sources (Ok [])

(* Copies the regular files and directories of [src] into [dst]. Symbolic links
   and other special files are left out, so a link cannot lead out of the
   fixtures directory. *)
let rec copy_tree src dst =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dst;
  List.iter
    (fun (kind, name) ->
      let from = Eio.Path.(src / name) and into = Eio.Path.(dst / name) in
      match kind with
      | `Directory -> copy_tree from into
      | `Regular_file ->
          let perm = (Eio.Path.stat ~follow:false from).perm land 0o777 in
          Eio.Path.save ~create:(`Or_truncate perm) into (Eio.Path.load from)
      | `Unknown | `Fifo | `Character_special | `Symbolic_link | `Block_device
      | `Socket ->
          ())
    (Eio.Path.read_dir_entries src)

let copy_fixtures ~env ~state_dir = function
  | None -> Ok ()
  | Some dir -> (
      try
        copy_tree Eio.Path.(Eio.Stdenv.fs env / dir) state_dir;
        Ok ()
      with Eio.Io _ as e ->
        Error (Printf.sprintf "fixtures %S: %s" dir (Printexc.to_string e)))

(* After [end] the agent gets this long to exit on its own, and after SIGTERM
   this long before SIGKILL. *)
let exit_grace = 5.
let kill_grace = 5.

(* One run *)

let execute ?(random_bytes = fun n -> Mirage_crypto_rng.generate n)
    ?(on_start = fun (_ : string) -> ()) ?keys ?(upstream = P.default_upstream)
    ?(connector = Upstream.plain) ~env ~proxy ~store ~ledger config =
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let run_id = fresh_id ~random_bytes now in
  let token = Proc_util.hex (random_bytes 16) in
  on_start run_id;
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
  (* A replay keys its requests with the patterns the recording was made
     under, whatever the caller passes. *)
  let* normalise_sources, redact_sources =
    match recording with
    | None -> Ok (config.normalise, config.redact)
    | Some (_, id, _) -> recorded_patterns store ~run_id:id
  in
  let* normalise = compile_patterns ~what:"normalise" normalise_sources in
  let* redact = redaction_patterns redact_sources in
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
  let run_dir_path = Proc_util.absolute (Eio.Path.native_exn run_dir) in
  let state_dir = Eio.Path.(run_dir / "state") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 state_dir;
  let state_dir_path = Filename.concat run_dir_path "state" in
  let temp_dir = Eio.Path.(run_dir / "tmp") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 temp_dir;
  let temp_dir_path = Filename.concat run_dir_path "tmp" in
  let* () = copy_fixtures ~env ~state_dir config.fixtures in
  (* Lives as long as the run: its own port, where the sandbox needs one. *)
  Eio.Switch.run @@ fun run_sw ->
  let* port =
    match config.agent with
    | Command _ when Sandbox.own_port config.sandbox ->
        P.listen_tcp proxy ~sw:run_sw ~net:(Eio.Stdenv.net env) ~token
    | Command _ | Http _ -> Ok (P.port proxy)
  in
  let variables =
    run_variables config ~proxy ~port ~token ~run_id ~state_dir:state_dir_path
      ~sim_now
  in
  let agent_process_env =
    process_env config variables ~state_dir:state_dir_path
      ~temp_dir:temp_dir_path
  in
  let agent_clock =
    match config.agent with
    | Command argv ->
        Agent_clock.decide
          ~hidden:(Sandbox.preload_hidden config.sandbox)
          ~stripped:(Sandbox.strips_preload config.sandbox)
          ~linux:(Agent_clock.linux ()) ~faketime:config.faketime
          ~library_override:(Sys.getenv_opt Agent_clock.override_variable)
          ~env:agent_process_env
          ~cwd:(Proc_util.absolute config.cwd)
          ~agent:argv ~sim_now ()
    | Http _ -> Agent_clock.http_mode
  in
  let clock_label = Agent_clock.label agent_clock in
  let header : Cassette.header =
    {
      run_id;
      scenario = config.scenario;
      variant = config.variant;
      bactrian_version = Version.current;
      recorded_at = rfc3339 now;
      sim_now;
      clock = Clock_label.to_clock clock_label;
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
  (* A run that only serves a recording calls nobody. *)
  let upstreams =
    match mode with
    | Record | Passthrough | Replay { matcher = Until_divergence; _ } ->
        overridden_upstreams upstream
    | Replay { matcher = Strict | Fuzzy _; _ } -> []
  in
  let started = ref 0. in
  let t_ms () = Float.to_int ((Eio.Time.now clock -. !started) *. 1000.) in
  let events = ref [] in
  let add e = events := e :: !events in
  let registered = ref false in
  let violations = ref None in
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
        normalise;
        redact;
        pricing = config.pricing;
        run_started = !started;
        mcp = config.mcp;
      };
    registered := true;
    Fun.protect ~finally:(fun () -> ignore (unregister ())) @@ fun () ->
    let start env : Agent_protocol.to_agent =
      Start
        {
          run_id;
          scenario = config.scenario;
          variant = config.variant;
          sim_now;
          state_dir = state_dir_path;
          env;
        }
    in
    let failure =
      match config.agent with
      | Command argv -> (
          match
            Result.map_error
              (fun m -> "sandbox: " ^ m)
              (Sandbox.start ~sw config.sandbox
                 {
                   run_id;
                   run_dir = run_dir_path;
                   state_dir = state_dir_path;
                   temp_dir = temp_dir_path;
                   port;
                   preload = Agent_clock.library agent_clock;
                   listen_unix =
                     (fun path ->
                       P.listen_unix proxy ~sw ~net:(Eio.Stdenv.net env) ~token
                         path);
                 }
                 argv)
          with
          | Error reason -> Error reason
          | Ok (sandboxed, sandbox_started) ->
              let driver_config : D.config =
                {
                  argv = sandboxed.argv;
                  env =
                    Agent_clock.inject agent_clock agent_process_env
                    @ sandboxed.env;
                  cwd = Some Eio.Path.(Eio.Stdenv.fs env / config.cwd);
                  stderr_path = Eio.Path.(run_dir / "agent.stderr");
                  turn_timeout = config.turn_timeout;
                  total_timeout = config.total_timeout;
                  exit_grace;
                  kill_grace;
                }
              in
              let session =
                D.with_agent ~fds:sandboxed.fds
                  ~process_mgr:(Eio.Stdenv.process_mgr env) ~clock driver_config
                  (fun d -> converse ~t_ms ~add d ~start:(start None) turns)
              in
              let collected = Sandbox.collect ~env sandbox_started in
              violations := Some collected;
              (match collected with
              | Collected { items; _ } ->
                  List.iter
                    (fun (v : Sandbox_log.violation) ->
                      add
                        (Trace_builder.Got_violation
                           { t_ms = t_ms (); kind = v.kind; detail = v.detail }))
                    items
              | Not_collected _ -> ());
              Ok
                (match session with
                | Error e -> Some (describe_driver_error e)
                | Ok (Error reason, _) -> Some reason
                | Ok (Ok (), `Exited 0) -> None
                | Ok (Ok (), ((`Exited _ | `Signaled _) as st)) ->
                    Some ("exit_nonzero: " ^ Proc_util.describe_exit st)))
      | Http { url; namespace_header } -> (
          let http_config : D.http_config =
            {
              url = Uri.of_string url;
              namespace_header;
              run_id;
              turn_timeout = config.turn_timeout;
              total_timeout = config.total_timeout;
            }
          in
          match
            D.with_http_agent ~net:(Eio.Stdenv.net env) ~clock ~connector
              http_config (fun d ->
                converse ~t_ms ~add d ~start:(start (Some variables)) turns)
          with
          | Error reason -> Ok (Some reason)
          | Ok () -> Ok None)
    in
    let* failure = failure in
    let findings = unregister () in
    Option.iter Store.close writer;
    Ok (failure, findings)
  in
  let interrupted = ref None in
  let driven =
    try drive () with
    | Eio.Io _ as e -> Error (Printexc.to_string e)
    | Eio.Cancel.Cancelled _ as ex ->
        interrupted := Some ex;
        Error "interrupted"
  in
  let finalise () =
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
    let spend = Ledger.spent_by ledger ~owner:run_id in
    let label = Sandbox.label config.sandbox in
    let violations : Sandbox.violations =
      match (!violations, label.applied) with
      | Some v, _ -> v
      | None, true -> Not_collected "the run stopped before its agent did"
      | None, false -> Sandbox.unsandboxed_violations
    in
    (match Eio.Path.rmtree ~missing_ok:true temp_dir with
    | () -> ()
    | exception Eio.Io _ -> ());
    let entries =
      Trace_builder.build
        {
          run_id;
          scenario = config.scenario;
          variant = config.variant;
          sim_now;
          clock = Clock_label.to_clock clock_label;
          cassette;
          agent = List.rev !events;
          resolve = (fun p -> Result.to_option (Store.read_payload store p));
        }
    in
    let outcome, verdicts =
      match (outcome, config.checks) with
      | (Passed | Failed _ | Diverged), Some c ->
          let items =
            Checks.evaluate ~stderr_dir:run_dir
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~table:config.pricing ~clock ~timeout:c.checker_timeout
              ~cwd:Eio.Path.(Eio.Stdenv.fs env / c.checks_cwd)
              ~date:(String.sub (rfc3339 now) 0 10)
              ~assertions:c.assertions ~checkers:c.checkers entries
          in
          (* Only a run that passed is judged by its checks. The others keep
             their outcome and still get their verdicts, for information. *)
          let outcome =
            match outcome with
            | Passed -> (
                match (Checks.checker_errors items, Checks.problems items) with
                | (_ :: _ as errors), _ ->
                    Errored ("checker: " ^ String.concat "; " errors)
                | [], [] -> Passed
                | [], problems -> Failed (String.concat "; " problems))
            | Failed _ | Diverged | Errored _ | Not_run _ -> outcome
          in
          (outcome, Some items)
      | (Errored _ | Not_run _), (Some _ | None) | _, None -> (outcome, None)
    in
    match
      write_trace run_dir entries;
      Store.write_atomic
        Eio.Path.(run_dir / "divergences.json")
        (Yojson.Safe.pretty_to_string
           (Divergences.file_json
              (List.map Divergences.of_matcher findings.divergences))
        ^ "\n");
      Option.iter
        (fun items ->
          Store.write_atomic
            Eio.Path.(run_dir / "verdicts.json")
            (Yojson.Safe.pretty_to_string (Checks.to_json items) ^ "\n"))
        verdicts;
      Store.write_atomic
        Eio.Path.(run_dir / "meta.json")
        (Yojson.Safe.pretty_to_string
           (meta_json ~run_id ~config ~sim_now ~outcome ~findings ~spend
              ~latency_ms:(Report_data.latency_ms entries)
              ~tokens:(Report_data.tokens entries)
              ~normalise:normalise_sources ~redact:redact_sources
              ~replay_of:(Option.map (fun (_, id, _) -> id) recording)
              ~violations ~clock:clock_label ~upstreams)
        ^ "\n")
    with
    | () ->
        Ok
          {
            run_id;
            outcome;
            run_dir = run_dir_path;
            findings;
            spend;
            sandbox = label;
            violations;
            clock = clock_label;
          }
    | exception (Eio.Io _ as e) -> Error (Printexc.to_string e)
  in
  match !interrupted with
  | None -> finalise ()
  | Some ex ->
      (* Teardown has to finish whatever the cancellation: meta.json says the
         run was interrupted, then the cancellation goes on. *)
      ignore (Eio.Cancel.protect finalise);
      raise ex
