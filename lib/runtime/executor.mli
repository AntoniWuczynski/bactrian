(** Experiments: scenarios × variants × repeats under a concurrency limit, with
    spend caps, a manifest and resume (design §5 experiment level, §9). *)

type planned = {
  index : int;
  scenario : string;
  variant : string;
  repeat : int;
}
(** One run of the plan. [index] is its place in the execution order, [repeat]
    counts from 1. *)

val plan :
  seed:int ->
  scenarios:string list ->
  variants:string list ->
  repeats:int ->
  planned list
(** Blocked and interleaved allocation (design §5). For each scenario, all the
    variants' repeats are put in one random order drawn from [seed] and the
    scenario's position, then the scenarios' orders are interleaved one run at a
    time. Pure: the same arguments give the same plan, with [index] 0, 1, 2, …
*)

type status =
  | Pending  (** Not started, or started and lost to a crash. *)
  | Done of Run.outcome
  | Interrupted  (** In flight when the experiment was interrupted. *)

type summary = {
  experiment_id : string;
  runs : (planned * status * string option) list;
      (** In plan order, with the run ID where a run was started. *)
}

type manifest = {
  name : string;  (** The experiment's name. *)
  experiment_file : string;  (** As given to {!run_experiment}. *)
  seed : int;
  fingerprint : Fingerprint.t;
      (** What the experiment was started from, checked by a resume. *)
  previous_run_ids : (int * string list) list;
      (** By plan index: the IDs of earlier attempts of a run that a resume
          repeated, oldest first. Only indices that have some. *)
  summary : summary;
}

val read_manifest : _ Eio.Path.t -> (manifest, string option) result
(** Reads an experiment's [manifest.json]. [Error None] when the file does not
    exist, [Error (Some reason)] when it cannot be decoded. *)

val funded : (string * float) list -> (Model_proxy.provider * string) list
(** The providers (with their names) whose spend cap in [caps_usd] is above 0:
    the ones an experiment calls live. *)

type error =
  | Configuration of Config_loader.located_error list
  | Environment of string
      (** A spend cap of 0 everywhere, a missing key, no TLS (for a provider or
          an [https] agent), a sandbox that does not work under
          [sandbox: strict]. *)
  | Usage of string
      (** An unknown experiment to resume, a plan or fingerprint that no longer
          matches the experiment being resumed, an experiment that another
          process is running. *)

type contents = {
  experiment : Bactrian_core.Experiment.t;
  dir : string;  (** The experiment file's directory. *)
  scenarios : (string * Bactrian_core.Scenario.t) list;
      (** Each scenario with its file, relative to [dir]. *)
}
(** What a loaded experiment holds, for [vet]. *)

val run_experiment :
  ?root:Eio.Fs.dir_ty Eio.Path.t ->
  ?random_bytes:(int -> string) ->
  ?announce:(string -> unit) ->
  ?warn:(string -> unit) ->
  ?keys:(Model_proxy.provider * string) list ->
  ?upstream:(Model_proxy.provider -> Uri.t) ->
  ?connector:Upstream.connector ->
  ?sandbox:Sandbox.mode ->
  ?on_sandbox:(Sandbox.label -> unit) ->
  bactrian_exe:string ->
  env:Eio_unix.Stdenv.base ->
  ?files:Eio.Fs.dir_ty Eio.Path.t ->
  ?vet:(contents -> (unit, string) result) ->
  experiment_file:string ->
  resume:string option ->
  unit ->
  (summary, error) result
(** Loads the experiment and its scenarios (every configuration error of every
    stage is collected and sorted by position: the experiment, the scenario
    globs, each scenario, duplicate scenario IDs and the price file), plans the
    runs and executes them with at most the experiment's [concurrency] in
    flight. [root] is the [.bactrian] directory (default [./.bactrian]).
    [experiment_file], its scenarios and its price file are read through [files]
    (default the whole filesystem), and once they are loaded [vet] may refuse
    them with a [Usage] error before anything runs. Scenario globs are relative
    to the experiment file's directory, where the agent also runs. A scenario's
    checkers run in the scenario file's directory.

    [.bactrian/experiments/<id>/manifest.json] lists every planned run with its
    status and run ID and is rewritten atomically after every run. The
    experiment's own [ledger.jsonl] beside it holds the spend, with the caps
    from [caps_usd], so caps hold across a resume. Once a run is [Not_run]
    because a cap was reached, no further run is started (they stay [Pending]).

    With [resume = Some id] the manifest of that experiment is read. Runs that
    are [Done] as [Passed], [Failed] or [Diverged] are never repeated. Every
    other run is run again: not started, interrupted, [Not_run] (missing data,
    typically after a cap was raised) and [Errored] (a fault of ours, or a
    checker that gave no verdict). A repeated run gets a new run ID and keeps
    the old one in the manifest's previous IDs. The resume is refused ([Usage])
    when the plan differs or when the manifest's {!Fingerprint.t} does not match
    the files now: the experiment (except [caps_usd] and [concurrency], which
    may change), the agent, any scenario file or the price file. The message
    names what changed. An exclusive lock on [experiments/<id>/run.lock] keeps a
    second process from running the same experiment, and a table of the locks
    this process holds keeps a second caller in the same process (another job of
    the MCP server) from doing so.

    Before the first run, {!Sandbox.preflight} checks the sandbox once, in the
    experiment's [sandbox] mode or in [sandbox] when given (the command line's
    [--sandbox]), with its [allow_write] and [deny_read] paths taken relative to
    the experiment file. [bactrian_exe] is the executable the probe re-runs. A
    sandbox that does not work is [Environment] (exit 69) under [strict], and a
    warning and unsandboxed runs under [auto]. The scenarios' checkers run in
    the same sandbox ({!Sandbox.checker}). [on_sandbox] is called with the label
    every run will carry. [warn] is also called once per setting that is
    accepted but not enforced (see {!Bactrian_core.Experiment.unenforced})
    before the first run, and the same lines are recorded in every run's
    [meta.json].

    An [http] agent is not started by bactrian, so its runs are unsandboxed
    whatever the mode, labelled with the reason ["http mode"]
    ({!Sandbox.http_mode}), and the preflight is run only when a scenario has
    checkers, for them. [sandbox: strict], in the experiment or as [sandbox], is
    a [Configuration] error.

    Cancelling the calling fibre (see {!Interrupt}) cancels the runs in flight,
    marks them [Interrupted], writes the manifest, and re-raises the
    cancellation.

    [announce] is called with the experiment ID once it is known and the
    manifest is about to be written, so an interrupted caller can say what to
    resume.

    [random_bytes], [keys], [upstream] and [connector] are passed to
    {!Run.execute}. Without [connector], an https upstream of a provider with a
    spend cap, or an https HTTP agent, is refused with [Environment] (this build
    has no TLS). An [http] upstream needs no connector. *)
