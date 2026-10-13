(** An experiment's results on disk, read back for [bactrian compare] and
    [bactrian report]: the manifest in [<root>/experiments/<id>/manifest.json]
    and each started run's directory under [<root>/runs/<run-id>/]
    ([trace.jsonl], [verdicts.json], [meta.json]). *)

open Bactrian_core

type loaded = {
  manifest : Executor.manifest;
  experiment : Experiment.t option;
      (** Re-read from the experiment file the manifest records. [None] when
          that file no longer exists. *)
  runs : Report_data.run list;  (** In plan order. *)
}

type error =
  | No_such_experiment of string
  | Bad_manifest of string
  | Bad_run of { run_id : string; file : string; reason : string }
  | Bad_experiment of Config_loader.located_error list
      (** The experiment file exists but does not load: every error, with its
          location. *)

val pp_error : Format.formatter -> error -> unit

val load :
  fs:_ Eio.Path.t ->
  root:_ Eio.Path.t ->
  experiment_id:string ->
  (loaded, error) result
(** [fs] resolves the experiment file as the manifest records it (relative to
    the working directory at the time, or absolute). [root] is the [.bactrian]
    directory. A run that was never started, or whose directory has no trace (an
    errored run), has an empty trace. A malformed trace or verdicts file is an
    error.

    Cost, latency and tokens are read from the run's [meta.json] ([cost_usd] is
    what the ledger attributed to the run, [latency_ms] and [tokens] are the
    totals of its model calls, see [Report_data.latency_ms]), not recomputed.
    They are [None] for a run without a [meta.json] or whose trace had no model
    call. Divergences are read from [divergences.json] (empty without one). A
    malformed [meta.json] or [divergences.json] is an error. *)

val settings :
  loaded ->
  baseline:string option ->
  candidate:string option ->
  fail_on_inconclusive:bool ->
  (Compare.input, string) result
(** The comparison to run. Baseline, confidence, gate, thresholds and population
    come from the experiment's [compare] section, and a [baseline] or
    [candidate] given here wins over it. The candidate defaults to the other
    variant when the experiment has exactly two. Thresholds of metrics outside
    the gate become report metrics. The seed is the experiment's, with 10,000
    resamples.

    Without a [compare] section both variants must be given, but the gate has no
    threshold to use: a threshold is never defaulted, so this is an [Error]
    naming the pass rate.

    [Error] for a missing or unknown variant, the same variant twice, an unknown
    metric name, or a gate metric with no threshold (named in the message). *)
