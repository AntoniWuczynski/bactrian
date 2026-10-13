(** Experiments: scenarios × variants × repeats under a concurrency limit, with
    spend caps, a manifest and resume (spec §5 experiment level, §9). *)

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
(** Blocked and interleaved allocation (spec §5). For each scenario, all the
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

type error =
  | Configuration of Config_loader.located_error list
  | Environment of string
      (** A spend cap of 0 everywhere, a missing key, no TLS. *)
  | Usage of string
      (** An unsupported agent, an unknown experiment to resume, a plan that no
          longer matches the experiment being resumed. *)

val run_experiment :
  ?root:Eio.Fs.dir_ty Eio.Path.t ->
  ?random_bytes:(int -> string) ->
  ?announce:(string -> unit) ->
  ?keys:(Model_proxy.provider * string) list ->
  ?upstream:(Model_proxy.provider -> Uri.t) ->
  ?connector:Upstream.connector ->
  env:Eio_unix.Stdenv.base ->
  experiment_file:string ->
  resume:string option ->
  unit ->
  (summary, error) result
(** Loads the experiment and its scenarios (every configuration error is
    collected), plans the runs and executes them with at most the experiment's
    [concurrency] in flight. [root] is the [.bactrian] directory (default
    [./.bactrian]). Scenario globs are relative to the experiment file's
    directory, where the agent also runs. A scenario's checkers run in the
    scenario file's directory.

    [.bactrian/experiments/<id>/manifest.json] lists every planned run with its
    status and run ID and is rewritten atomically after every run. The
    experiment's own [ledger.jsonl] beside it holds the spend, with the caps
    from [caps_usd], so caps hold across a resume. Once a run is [Not_run]
    because a cap was reached, no further run is started (they stay [Pending]).

    With [resume = Some id] the manifest of that experiment is read and every
    run that is [Done] is skipped, the others are run again. The plan must be
    the one the manifest holds.

    Cancelling the calling fibre (see {!Interrupt}) cancels the runs in flight,
    marks them [Interrupted], writes the manifest, and re-raises the
    cancellation. Only command agents are supported.

    [announce] is called with the experiment ID once it is known and the
    manifest is about to be written, so an interrupted caller can say what to
    resume.

    [random_bytes], [keys], [upstream] and [connector] are passed to
    {!Run.execute}. Without [connector], an https upstream is refused with
    [Environment] (this build has no TLS). *)
