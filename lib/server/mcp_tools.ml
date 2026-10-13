open Bactrian_core
open Bactrian_app
module Executor = Bactrian_runtime.Executor
module Sandbox = Bactrian_runtime.Sandbox

let ( let* ) = Result.bind

type context = {
  env : Eio_unix.Stdenv.base;
  root : Confine.t;
  policy : Policy.t;
  jobs : Jobs.t;
  bactrian_exe : string;
  on_progress : Progress.t -> unit;
}

(* What a tool shows: the exact text of its JSON and the same as a value. *)
type shown = { text : string; structured : Canon.t }

let of_json json =
  match Canon.of_yojson json with
  | Ok structured -> Ok { text = Yojson.Safe.to_string json; structured }
  | Error e ->
      Error
        (Command_error.Internal
           (Format.asprintf "the result is not valid JSON: %a" Canon.pp_error e))

let of_canon c = Ok { text = Canon.to_string c; structured = c }
let usage r = Result.map_error (fun m -> Command_error.Usage m) r

type annotations = {
  read_only : bool;
  destructive : bool;
  idempotent : bool;
  open_world : bool;
}

(* Reads files under the root and nothing else. *)
let reads =
  {
    read_only = true;
    destructive = false;
    idempotent = true;
    open_world = false;
  }

(* Starts agents, which may call providers live within the spend caps. *)
let spends =
  {
    read_only = false;
    destructive = false;
    idempotent = false;
    open_world = true;
  }

type tool = {
  name : string;
  title : string;
  description : string;
  properties : Tool_schema.property list;
  annotations : annotations;
  run : context -> Canon.t option -> (shown, Command_error.t) result;
}

(* A tool whose arguments [decode] reads once [properties] accept them. *)
let tool ~name ~title ~description ~annotations properties decode body =
  {
    name;
    title;
    description;
    properties;
    annotations;
    run =
      (fun context arguments ->
        let* args = usage (Tool_schema.decode properties decode arguments) in
        body context args);
  }

let prop ?(required = false) name kind description =
  { Tool_schema.name; kind; required; description }

let enum ~field values = function
  | None -> Ok None
  | Some v -> (
      match List.assoc_opt v values with
      | Some x -> Ok (Some x)
      | None ->
          Error
            (Printf.sprintf "argument %s: must be one of %s" field
               (String.concat ", " (List.map fst values))))

let names values = List.map fst values

(* The CPU-bound part of compare, report and plan runs in a domain of its own,
   so that the server keeps reading messages, serving the jobs' proxies and
   noticing cancellation and the end of input meanwhile. A systhread would not
   do that: OCaml threads of one domain share its runtime lock, so a computing
   thread leaves the event loop a turn only at each tick of the master lock. A
   new domain per call costs a millisecond or so, and the work, pure and given
   only its own values, is safe to run there. *)
let compute context = Compute.in_domain (Eio.Stdenv.domain_mgr context.env)

(* The commands read and write files through the server's working directory as
   a capability, which refuses absolute paths and paths that climb out of it,
   symbolic links included, when they are opened. So what an experiment or a
   scenario names cannot reach elsewhere, nor can a directory that turns into
   a link after its tool argument was checked. Path arguments are therefore
   passed on relative to the root, as {!Confine.path} gives them. *)
let files context = Eio.Stdenv.cwd context.env

(* Confinement of optional arguments. *)
let each check = function None -> Ok () | Some v -> check v

let optional check = function
  | None -> Ok None
  | Some v -> Result.map Option.some (check v)

(* Jobs *)

(* Starts a job unless the server already runs as many as it may. *)
let start_job context ~kind ~progress command =
  let running = Jobs.running context.jobs in
  if running >= context.policy.max_jobs then
    Error
      (Command_error.Usage
         (Printf.sprintf
            "%d jobs are running, the most this server allows: wait for one to \
             finish or cancel it with job_cancel (serve-mcp --max-jobs sets \
             the limit)"
            running))
  else
    let id = Jobs.start context.jobs ~kind ~progress command in
    of_canon (Canon.set_member "job_id" (Canon.string id) Canon.empty_object)

let one_run ~finished =
  { Jobs.done_ = (if finished then 1 else 0); total = Some 1 }

(* An experiment's progress is in its manifest, rewritten after every run. *)
let experiment_progress context id ~finished:_ =
  let none = { Jobs.done_ = 0; total = None } in
  match !id with
  | None -> none
  | Some id -> (
      match
        Executor.read_manifest
          Eio.Path.(
            Eio.Stdenv.cwd context.env / ".bactrian" / "experiments" / id
            / "manifest.json")
      with
      | Error _ -> none
      | Ok m ->
          let runs = m.summary.runs in
          {
            done_ =
              List.length
                (List.filter
                   (fun (_, (status : Executor.status), _) ->
                     match status with
                     | Done _ -> true
                     | Pending | Interrupted -> false)
                   runs);
            total = Some (List.length runs);
          })

let sandbox_property =
  prop "sandbox"
    (Enum (names Sandbox.modes))
    "Run the agent in the operating-system sandbox: auto (when the host \
     supports it, otherwise unsandboxed with a warning and a label), strict \
     (fail when it does not), off. Unless the server was started with \
     --allow-unsandboxed, every job is strict and auto and off are refused."

let sandbox_mode = enum ~field:"sandbox" Sandbox.modes

type run_args = {
  experiment : string;
  resume : string option; [@default None]
  sandbox : string option; [@default None]
}
[@@deriving of_yojson { strict = true }]

let run_start =
  tool ~name:"run_start" ~title:"Start an experiment" ~annotations:spends
    ~description:
      "Start `bactrian run EXPERIMENT` (every scenario, variant and repeat of \
       an experiment file) as a job and return its job_id at once. Poll \
       job_status until the state is no longer running: its result is what \
       `bactrian run --json` prints. A finished job's status is kept for an \
       hour, and for the newest 100 finished jobs only; the runs stay under \
       .bactrian. Live runs spend money within the BACTRIAN_CAP_* spend caps \
       set in the server's environment. job_cancel stops it as Ctrl-C would, \
       and the experiment_id in the job's error resumes it."
    [
      prop ~required:true "experiment" String
        "The experiment file (YAML or JSON), inside the working directory.";
      prop "resume" String
        "The ID of an experiment under .bactrian/experiments to continue: runs \
         that finished are not repeated.";
      sandbox_property;
    ]
    run_args_of_yojson
    (fun context (a : run_args) ->
      let* experiment =
        usage (Confine.path context.root ~field:"experiment" a.experiment)
      in
      let* () = usage (each (Confine.id ~field:"resume") a.resume) in
      let* sandbox = usage (sandbox_mode a.sandbox) in
      let* sandbox = usage (Policy.sandbox context.policy sandbox) in
      let experiment_id = ref a.resume in
      let on_progress (p : Progress.t) =
        (match p with
        | Experiment_started id -> experiment_id := Some id
        | Warning _ -> ());
        context.on_progress p
      in
      start_job context ~kind:"run"
        ~progress:(experiment_progress context experiment_id) (fun stop ->
          Run_command.run ~stop ~files:(files context)
            ~vet:(Policy.experiment context.policy context.root)
            ~on_progress ~bactrian_exe:context.bactrian_exe ~env:context.env
            { experiment; resume = a.resume; sandbox }
          |> Result.map (fun r ->
              (Run_command.to_json r, Run_command.exit_code r))))

(* The arguments [record_start] and [replay_start] share. Decoded leniently
   beside each tool's own part, after the whole object was checked. *)
type common_args = {
  agent : string list;
  cwd : string; [@default Single_run.defaults.cwd]
  scenario : string; [@default Single_run.defaults.scenario]
  variant : string; [@default Single_run.defaults.variant]
  sim_now : string; [@default Single_run.defaults.sim_now]
  timezone : string; [@default Single_run.defaults.timezone]
  env : string list; [@default Single_run.defaults.env_passthrough]
  turn_timeout : float; [@default Single_run.defaults.turn_timeout]
  total_timeout : float; [@default Single_run.defaults.total_timeout]
  sandbox : string option; [@default None]
}
[@@deriving of_yojson { strict = false }]

let common_properties =
  [
    prop ~required:true "agent" Nonempty_strings
      "The agent's command and its arguments.";
    prop "cwd" String
      "Working directory of the agent, inside the server's (default \".\").";
    prop "scenario" String
      "Scenario name recorded with the run (default adhoc).";
    prop "variant" String
      "Variant name recorded with the run (default default).";
    prop "sim_now" String
      "RFC 3339 simulated time given to the agent. Defaults to now, or in a \
       replay to the recording's.";
    prop "timezone" String "Value of TZ for the agent (default UTC).";
    prop "env" Strings
      "Names of variables to pass from the server's environment to the agent \
       (refused unless the server was started with --trust-project).";
    prop "turn_timeout" Number
      "Longest wait for one agent message, in seconds (default 120).";
    prop "total_timeout" Number
      "Longest the whole run may take, in seconds (default 600).";
    sandbox_property;
  ]

let common context (c : common_args) =
  let* cwd = Confine.path context.root ~field:"cwd" c.cwd in
  let* sandbox = sandbox_mode c.sandbox in
  let* sandbox = Policy.sandbox context.policy sandbox in
  let* () = Policy.env context.policy c.env in
  Ok
    {
      Single_run.agent = c.agent;
      cwd;
      scenario = c.scenario;
      variant = c.variant;
      sim_now = c.sim_now;
      timezone = c.timezone;
      env_passthrough = c.env;
      turn_timeout = c.turn_timeout;
      total_timeout = c.total_timeout;
      sandbox = Option.value sandbox ~default:Single_run.defaults.sandbox;
    }

let with_common own json =
  let* c = common_args_of_yojson json in
  let* o = own json in
  Ok (c, o)

let single_run context ~kind run =
  start_job context ~kind ~progress:one_run (fun stop ->
      run ~stop ~on_progress:context.on_progress
        ~bactrian_exe:context.bactrian_exe ~env:context.env
      |> Result.map (fun r -> (Single_run.to_json r, Single_run.exit_code r)))

type record_args = {
  turns : string list;
  mcp_stdio : string list; [@default []]
  mcp_http : string list; [@default []]
  mcp_policy : string list; [@default []]
}
[@@deriving of_yojson { strict = false }]

let record_start =
  tool ~name:"record_start" ~title:"Record an agent run" ~annotations:spends
    ~description:
      "Start `bactrian record` as a job and return its job_id at once: run the \
       agent against the real providers with the scripted turns and record its \
       model and MCP traffic under .bactrian/runs. Poll job_status: its result \
       is what `bactrian record --json` prints, kept for an hour after the job \
       ends (and for the newest 100 finished jobs only). Spends money within \
       the BACTRIAN_CAP_* spend caps set in the server's environment."
    (common_properties
    @ [
        prop ~required:true "turns" Nonempty_strings
          "The scripted user turns, in order.";
        prop "mcp_stdio" Strings
          "NAME=JSON declarations of stdio MCP servers, the command as a JSON \
           array of strings (refused unless the server was started with \
           --trust-project).";
        prop "mcp_http" Strings
          "NAME=URL declarations of Streamable HTTP MCP servers (refused \
           unless the server was started with --trust-project).";
        prop "mcp_policy" Strings
          "NAME=POLICY: record (default), passthrough or block.";
      ])
    (with_common record_args_of_yojson)
    (fun context ((c : common_args), (r : record_args)) ->
      let* common = usage (common context c) in
      let* () =
        usage (Policy.mcp context.policy ~stdio:r.mcp_stdio ~http:r.mcp_http)
      in
      let request : Record_command.request =
        {
          common;
          turns = r.turns;
          mcp_stdio = r.mcp_stdio;
          mcp_http = r.mcp_http;
          mcp_policy = r.mcp_policy;
        }
      in
      let* () = usage (Record_command.validate request) in
      single_run context ~kind:"record" (fun ~stop ->
          Record_command.run ~stop request))

type replay_args = {
  run : string;
  mode : string option; [@default None]
  threshold : float option; [@default None]
  allow_reorder : bool; [@default false]
  timing : string option; [@default None]
}
[@@deriving of_yojson { strict = false }]

let replay_start =
  tool ~name:"replay_start" ~title:"Replay a recorded run" ~annotations:spends
    ~description:
      "Start `bactrian replay RUN -- AGENT` as a job and return its job_id at \
       once: run the agent with the recording's turns and answer its model and \
       MCP requests from the recording. Poll job_status: its result is what \
       `bactrian replay --json` prints, with any divergences, kept for an hour \
       after the job ends (and for the newest 100 finished jobs only). Only \
       until-divergence mode can go live and spend."
    (common_properties
    @ [
        prop ~required:true "run" String
          "Run ID under .bactrian/runs, or a run directory, to replay.";
        prop "mode"
          (Enum (names Replay_command.modes))
          "strict (default) refuses any request the recording lacks, \
           until-divergence goes live at the first one, fuzzy serves the \
           nearest recorded exchange above the threshold.";
        prop "threshold" Number
          "Minimum similarity for fuzzy mode, from 0 to 1 (0.9).";
        prop "allow_reorder" Boolean
          "Serve recorded exchanges out of order, as a finding only.";
        prop "timing"
          (Enum (names Replay_command.timings))
          "Serve streams fast (default) or with their recorded pacing.";
      ])
    (with_common replay_args_of_yojson)
    (fun context ((c : common_args), (r : replay_args)) ->
      let* common = usage (common context c) in
      let* run = usage (Confine.run context.root ~field:"run" r.run) in
      let* mode = usage (enum ~field:"mode" Replay_command.modes r.mode) in
      let* timing =
        usage (enum ~field:"timing" Replay_command.timings r.timing)
      in
      let request : Replay_command.request =
        {
          common;
          from = run;
          mode = Option.value mode ~default:Replay_command.Strict;
          threshold = r.threshold;
          allow_reorder = r.allow_reorder;
          timing =
            Option.value timing ~default:Bactrian_runtime.Model_proxy.Fast;
        }
      in
      let* () = usage (Replay_command.validate request) in
      single_run context ~kind:"replay" (fun ~stop ->
          Replay_command.run ~stop request))

type job_args = { job_id : string } [@@deriving of_yojson { strict = true }]

let job_property =
  prop ~required:true "job_id" String
    "The ID run_start, record_start or replay_start returned."

let find_job context id =
  match Jobs.find context.jobs id with
  | Some job -> Ok job
  | None ->
      Error
        (Command_error.Usage
           (Printf.sprintf
              "argument job_id: no job %S (a finished job is kept for an hour, \
               and only the newest 100)"
              id))

let job_status =
  tool ~name:"job_status" ~title:"Job status" ~annotations:reads
    ~description:
      "The state of a job (running, succeeded, failed or cancelled), its \
       progress in runs, and once finished its exit status with the command's \
       --json output as result, or its error. A cancelled experiment's error \
       holds the experiment_id to resume." [ job_property ] job_args_of_yojson
    (fun context (a : job_args) ->
      let* job = find_job context a.job_id in
      of_canon (Jobs.status job))

let job_cancel =
  tool ~name:"job_cancel" ~title:"Cancel a job"
    ~annotations:
      {
        read_only = false;
        destructive = false;
        idempotent = true;
        open_world = false;
      }
    ~description:
      "Stop a running job as Ctrl-C stops the command, and return its status \
       once its agents are gone. An experiment stays resumable with run_start \
       and resume. A finished job is left as it is."
    [ job_property ] job_args_of_yojson (fun context (a : job_args) ->
      let* job = find_job context a.job_id in
      Jobs.cancel job;
      of_canon (Jobs.status job))

(* Commands that answer at once *)

let experiment_id_property =
  prop ~required:true "experiment_id" String
    "An experiment under .bactrian/experiments."

let baseline_property = prop "baseline" String "The baseline variant."
let candidate_property = prop "candidate" String "The candidate variant."

type compare_args = {
  experiment_id : string;
  baseline : string option; [@default None]
  candidate : string option; [@default None]
  fail_on_inconclusive : bool; [@default false]
}
[@@deriving of_yojson { strict = true }]

let compare =
  tool ~name:"compare" ~title:"Compare two variants" ~annotations:reads
    ~description:
      "Compare a candidate variant of an experiment with its baseline, with \
       confidence intervals and a verdict per metric, as `bactrian compare \
       --json` prints it. The gate's exit status is in the exit field."
    [
      experiment_id_property;
      baseline_property;
      candidate_property;
      prop "fail_on_inconclusive" Boolean
        "Make an inconclusive gate metric give the gate status 2.";
    ]
    compare_args_of_yojson
    (fun context (a : compare_args) ->
      let* () = usage (Confine.id ~field:"experiment_id" a.experiment_id) in
      let* data =
        Compare_command.run ~compute:(compute context) ~files:(files context)
          ~env:context.env
          {
            experiment_id = a.experiment_id;
            baseline = a.baseline;
            candidate = a.candidate;
            fail_on_inconclusive = a.fail_on_inconclusive;
          }
      in
      of_json (Compare_command.to_json data))

type plan_args = {
  baseline_rates : float list; [@default []]
  scenarios : int option; [@default None]
  from : string option; [@default None]
  variant : string option; [@default None]
  effect_ : float option; [@key "effect"] [@default None]
  threshold : float;
  population : string option; [@default None]
  between_sd : float option; [@default None]
  gate_metrics : int; [@default Plan_command.default_gate_metrics]
  power : float; [@default Plan_command.default_power]
  confidence : float; [@default Plan_command.default_confidence]
  max_repeats : int; [@default Plan_command.default_max_repeats]
  simulations : int; [@default Plan_command.default_simulations]
  seed : int; [@default Plan_command.default_seed]
}
[@@deriving of_yojson { strict = true }]

let plan =
  tool ~name:"plan" ~title:"Plan the repeats" ~annotations:reads
    ~description:
      "Say how many repeats per scenario and variant an experiment needs to \
       reach the correct verdict with the requested power, by simulating the \
       decision rule of compare, as `bactrian plan --json` prints it."
    [
      prop "baseline_rates" Numbers
        "Baseline pass rates from 0 to 1, one per scenario or one for all.";
      prop "scenarios" Integer "The number of scenarios (default: of rates).";
      prop "from" String
        "An earlier experiment to take the scenarios and rates from instead.";
      prop "variant" String
        "With from, the variant whose rates to take (default: its baseline).";
      prop "effect" Number
        "The assumed true drop in pass rate (default twice the threshold).";
      prop ~required:true "threshold" Number "The regression threshold.";
      prop "population"
        (Enum (names Plan_command.populations))
        "The estimand, suite (default) or scenarios.";
      prop "between_sd" Number
        "The spread of the effect between scenarios, needed for scenarios.";
      prop "gate_metrics" Integer "The number of gate metrics, 1 to 4 (1).";
      prop "power" Number "The probability of the correct verdict (0.8).";
      prop "confidence" Number "The confidence level (0.95).";
      prop "max_repeats" Integer "The largest repeat count to try (200).";
      prop "simulations" Integer "Simulated experiments per count (500).";
      prop "seed" Integer "The random seed (0).";
    ]
    plan_args_of_yojson
    (fun context (a : plan_args) ->
      let* () = usage (each (Confine.id ~field:"from") a.from) in
      let* population =
        usage (enum ~field:"population" Plan_command.populations a.population)
      in
      let* plan =
        Plan_command.run ~compute:(compute context) ~files:(files context)
          ~env:context.env
          {
            rates = a.baseline_rates;
            scenarios = a.scenarios;
            from = a.from;
            variant = a.variant;
            effect_ = a.effect_;
            threshold = a.threshold;
            population = Option.value population ~default:`Suite;
            between_sd = a.between_sd;
            gate_metrics = a.gate_metrics;
            power = a.power;
            confidence = a.confidence;
            max_repeats = a.max_repeats;
            simulations = a.simulations;
            seed = a.seed;
          }
      in
      of_json (Plan_command.to_json plan))

(* bactrian's own state and the repository's are no place for a report: an
   HTML file over the ledger or a manifest breaks every later command, and one
   in .git/hooks could run. *)
let own_output output =
  match String.split_on_char '/' output with
  | ((".bactrian" | ".git") as top) :: _ ->
      Error
        (Printf.sprintf "output: %S is inside %s, which bactrian or git owns"
           output top)
  | _ -> Ok ()

type report_args = {
  experiment_id : string;
  baseline : string option; [@default None]
  candidate : string option; [@default None]
  output : string;
}
[@@deriving of_yojson { strict = true }]

let report =
  tool ~name:"report" ~title:"Write an HTML report"
    ~annotations:
      {
        read_only = false;
        destructive = true;
        idempotent = true;
        open_world = false;
      }
    ~description:
      "Write a self-contained HTML report of an experiment to output, \
       replacing that file, and return the file and the comparison, as \
       `bactrian report --json` prints them."
    [
      experiment_id_property;
      baseline_property;
      candidate_property;
      prop ~required:true "output" String
        "Where to write the HTML, inside the working directory.";
    ]
    report_args_of_yojson
    (fun context (a : report_args) ->
      let* () = usage (Confine.id ~field:"experiment_id" a.experiment_id) in
      let* output =
        usage (Confine.path context.root ~field:"output" a.output)
      in
      let* () = usage (own_output output) in
      let* r =
        Report_command.run ~compute:(compute context) ~files:(files context)
          ~env:context.env
          {
            experiment_id = a.experiment_id;
            baseline = a.baseline;
            candidate = a.candidate;
            output;
          }
      in
      of_json (Report_command.to_json r))

type check_args = { scenarios : string list }
[@@deriving of_yojson { strict = true }]

(* Runs the scenarios' checker commands, which may do anything the user can. *)
let check =
  tool ~name:"check" ~title:"Check scenarios"
    ~annotations:
      {
        read_only = false;
        destructive = true;
        idempotent = false;
        open_world = true;
      }
    ~description:
      "Check scenarios' assertions and checkers against their golden traces: \
       every pass trace must pass them all and every fail trace must fail one, \
       as `bactrian check --json` prints it. This runs each scenario's checker \
       commands as programs on this machine, outside any sandbox, so a \
       scenario with checkers is refused unless the server was started with \
       --trust-project."
    [
      prop ~required:true "scenarios" Nonempty_strings
        "Scenario files (YAML or JSON), inside the working directory.";
    ]
    check_args_of_yojson
    (fun context (a : check_args) ->
      let* scenarios =
        usage
          (List.fold_right
             (fun s acc ->
               let* rest = acc in
               let* s = Confine.path context.root ~field:"scenarios" s in
               Ok (s :: rest))
             a.scenarios (Ok []))
      in
      let* reports =
        Check_command.run ~files:(files context)
          ~vet:(Policy.scenario context.policy)
          ~env:context.env { scenarios }
      in
      of_json (Check_command.to_json reports))

type doctor_args = { experiment : string option [@default None] }
[@@deriving of_yojson { strict = true }]

let doctor =
  tool ~name:"doctor" ~title:"Check the host" ~annotations:reads
    ~description:
      "Check that this host is ready to record and run (provider keys by name, \
       a loopback port, the agent, the sandbox, the clock, HTTPS), without \
       starting an agent or contacting anything, as `bactrian doctor --json` \
       prints it."
    [
      prop "experiment" String
        "An experiment file whose needs to check, inside the working directory.";
    ]
    doctor_args_of_yojson
    (fun context (a : doctor_args) ->
      let* experiment =
        usage
          (optional
             (Confine.path context.root ~field:"experiment")
             a.experiment)
      in
      let* report =
        Doctor_command.run ~files:(files context)
          ~bactrian_exe:context.bactrian_exe ~env:context.env { experiment }
      in
      of_json (Doctor_command.to_json report))

type divergences_args = { run : string }
[@@deriving of_yojson { strict = true }]

let divergences =
  tool ~name:"divergences" ~title:"Divergences of a run" ~annotations:reads
    ~description:
      "The replay divergences stored with one run, each with its kind, the \
       nearest recorded exchange, the similarity and the paths that differ, as \
       `bactrian replay --json` reports them."
    [
      prop ~required:true "run" String
        "Run ID under .bactrian/runs, or its directory .bactrian/runs/<ID>.";
    ]
    divergences_args_of_yojson
    (fun context (a : divergences_args) ->
      let* run = usage (Confine.run context.root ~field:"run" a.run) in
      let* d = Divergences_command.run ~env:context.env { run } in
      of_json (Divergences_command.to_json d))

let tools =
  [
    run_start;
    record_start;
    replay_start;
    job_status;
    job_cancel;
    compare;
    plan;
    report;
    check;
    doctor;
    divergences;
  ]

let obj = Canon.of_fields

let definition t =
  obj
    [
      ("name", Canon.string t.name);
      ("title", Canon.string t.title);
      ("description", Canon.string t.description);
      ("inputSchema", Tool_schema.input_schema t.properties);
      ( "annotations",
        obj
          [
            ("readOnlyHint", Canon.bool t.annotations.read_only);
            ("destructiveHint", Canon.bool t.annotations.destructive);
            ("idempotentHint", Canon.bool t.annotations.idempotent);
            ("openWorldHint", Canon.bool t.annotations.open_world);
          ] );
    ]

let definitions = List.map (fun t -> (t.name, definition t)) tools

let result ~is_error s =
  obj
    [
      ( "content",
        Canon.list
          [
            obj [ ("type", Canon.string "text"); ("text", Canon.string s.text) ];
          ] );
      ("structuredContent", s.structured);
      ("isError", Canon.bool is_error);
    ]

let failure e =
  match of_json (Command_error.to_json e) with
  | Ok s -> result ~is_error:true s
  | Error _ ->
      result ~is_error:true
        { text = Command_error.message e; structured = Canon.empty_object }

let call context name arguments =
  match List.find_opt (fun t -> String.equal t.name name) tools with
  | None -> failure (Command_error.Usage ("unknown tool " ^ name))
  | Some t -> (
      match t.run context arguments with
      | Ok s -> result ~is_error:false s
      | Error e -> failure e
      | exception Eio.Io (err, _) ->
          failure
            (Command_error.Internal (Format.asprintf "%a" Eio.Exn.pp_err err))
      | exception (Eio.Cancel.Cancelled _ as ex) -> raise ex
      (* A fault of ours in one call fails that call, not the server and its
         jobs. *)
      | exception ex ->
          context.on_progress
            (Warning
               (Printf.sprintf "tool %s failed: %s\n%s" name
                  (Printexc.to_string ex)
                  (Printexc.get_backtrace ())));
          failure (Command_error.Internal (Printexc.to_string ex)))
