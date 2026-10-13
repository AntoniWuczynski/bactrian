(** Everything a report shows, built purely from an experiment's runs and a
    {!Compare.result} (design §8, Outputs). {!Report_html} and {!to_json} are
    two views of this one value. *)

val schema_version : int
(** 1 *)

(** {1 Inputs} *)

type status =
  | Passed
  | Failed
  | Diverged
  | Errored
  | Not_run
  | Pending  (** Not started, or lost to a crash. *)
  | Interrupted

type check_status = Check_pass | Check_fail | Check_error

type check = { name : string; status : check_status; reason : string option }
(** One item of a run's [verdicts.json]. [Check_error] is a checker that could
    not give a verdict. *)

type divergence = { description : string; changes : Diff.change list }

type run = {
  index : int;  (** Place in the execution order. *)
  repeat : int;  (** From 1. *)
  scenario : string;
  variant : string;
  run_id : string option;
  status : status;
  reason : string option;
  checks : check list;
  trace : Trace.entry list;  (** Empty when the run left no trace. *)
  divergences : divergence list;
  cost : float option;  (** USD. *)
  latency_ms : float option;
  tokens : int option;
  unsandboxed : string option;
      (** Why the run ran outside the operating-system sandbox, from its
          [meta.json]. [None] when it was sandboxed or left no [meta.json]. *)
  clock_unenforced : string option;
      (** Why the run's clock was contract-only, from its [meta.json] (a
          {!Clock_label.reason_name}). [None] when it was enforced or the run
          left no [meta.json]. *)
  sandbox_violations : string option;
      (** What the sandbox recorded denying, in one line that says how far it
          can be trusted ("at least 2 (the system log can drop denials, ...)"),
          from its [meta.json]. [None] for a run outside the sandbox, or one
          recorded before the coverage was. *)
}

val latency_ms : Trace.entry list -> float option
(** The sum of the model calls' durations, [None] without a model call. *)

val ttft_ms : Trace.entry list -> float list
(** The time to first token of each model call that has one, in order. *)

val tokens : Trace.entry list -> int option
(** Input plus output tokens over the model calls that report usage, [None] when
    none does. *)

val compare_run : run -> Compare.run
(** [Passed] is a pass. [Failed] and [Diverged] are failures. [Errored] is
    missing as {!Compare.Errored}. [Not_run], [Pending] and [Interrupted] are
    missing as {!Compare.Not_run}: unfinished work is missing data, never
    silently dropped. *)

val metric_name : Compare.metric -> string
(** ["pass_rate"], ["cost"], ["latency_p50"], ["tokens"]. *)

val metric_of_name : string -> Compare.metric option

val verdict_name : Compare.verdict -> string
(** ["regression"], ["improvement"], ["no_change"], ["inconclusive"]. *)

val lean_name : Compare.verdict -> string option
(** [Some "worse" | "better" | "none"] for an inconclusive verdict. *)

(** {1 Behaviour without statistics} *)

val signature : Trace.entry list -> string list
(** The tool names of the run's tool calls, in order. *)

val new_behaviours :
  baseline:string list list -> candidate:string list list -> string list list
(** [baseline] and [candidate] are the signatures of one scenario's runs in each
    arm. A new behaviour is a signature in at least 10% of the candidate's runs
    and none of the baseline's. Empty when either arm has no run. Sorted by
    descending share, then by signature. *)

(** {1 The report} *)

type dist = { n : int; median : float; p90 : float }

type extra_repeats = [ `Repeats of int | `Cannot_resolve | `Not_simulated ]
(** {!Plan_sim.extra_runs_needed}. *)

type metric_row = {
  name : string;
  result : Compare.metric_result;
  delta : float;  (** The declared threshold, as given (a relative change). *)
  relative : (float * Stats.interval) option;
      (** Estimate and interval back-transformed to a relative change, for
          metrics on the log ratio scale. [None] for pass rate or an undefined
          metric. *)
  extra_repeats : extra_repeats option;
      (** For an inconclusive gate metric that is defined, the extra repeats per
          scenario and variant that would resolve it, simulated with the
          observed rates as the truth. [None] for any other metric. *)
}

type scenario_row = {
  scenario : string;
  baseline_passes : int;
  baseline_runs : int;
  candidate_passes : int;
  candidate_runs : int;
  difference : float;  (** Candidate minus baseline pass rate. *)
  interval : Stats.interval;
  verdict : Compare.verdict;
}

type arm_row = { scenario : string; variant : string; stats : Arm_stats.t }
(** One scenario's runs in one compared variant. *)

type summary = {
  variant : string;
  cost : dist option;
  latency : dist option;
  tokens : dist option;
}
(** Over the variant's finished runs in the scenarios both arms finished (not
    the [dropped] ones), the runs the verdicts are about. *)

type signature_count = { signature : string list; count : int }

type behaviour = {
  scenario : string;
  baseline_signatures : signature_count list;
  baseline_traced : int;  (** Runs with a trace in the baseline arm. *)
  candidate_signatures : signature_count list;
  candidate_traced : int;
  new_signatures : string list list;
  baseline_steps : (int * int) list;
      (** Model calls per run and the number of runs, ascending. *)
  candidate_steps : (int * int) list;
}

type failure = {
  run : run;
  messages : string list;
      (** Each failing assertion or checker, or the run's reason when none
          failed, then its divergences. *)
}

type t = {
  experiment_id : string;
  experiment : string;
  baseline : string;
  candidate : string;
  population : [ `Suite | `Scenarios ];
  confidence : float;
  gate : (string * float) list;  (** Metric name and threshold. *)
  gate_level : float;  (** Per-interval level after Bonferroni. *)
  seed : int;
  resamples : int;
  missing : (string * int * int * int) list;
      (** Variant, runs planned, errored, not run (with unfinished). *)
  dropped : string list;
      (** {!Compare.result.dropped}: scenarios with no finished run in one arm.
      *)
  exit : int;
  metrics : metric_row list;
  scenarios : scenario_row list;
  arms : arm_row list;
      (** Every scenario with runs in either compared variant (sorted), the
          baseline then the candidate. *)
  arms_level : float;
      (** The level of the arms' intervals: the confidence, Bonferroni-adjusted
          across those scenarios. *)
  summaries : summary list;  (** Baseline, then candidate. *)
  behaviours : behaviour list;
  failures : failure list;
  order : run list;  (** Every run of the experiment, by [index]. *)
}

val build :
  ?check:(unit -> unit) ->
  experiment_id:string ->
  experiment:string ->
  settings:Compare.input ->
  result:Compare.result ->
  run list ->
  t
(** [settings.runs] is not read: the runs come from the last argument. Each
    inconclusive gate metric runs {!Plan_sim.extra_runs_needed}, which is
    deterministic (its seed is [settings.seed]) and fast under [`Suite], and
    runs a hierarchical bootstrap per simulated experiment under [`Scenarios].
    [check] is passed to it. *)

val to_json : t -> Yojson.Safe.t
(** Deterministic: fixed key order, runs in execution order, scenarios sorted,
    floats shortest-round-trip, [null] for NaN and infinities. Carries
    ["schema_version"]. *)

(** Each run carries ["unsandboxed"], ["clock_unenforced"] and
    ["sandbox_violations"] (the text, or [null]) and the header
    ["unsandboxed_runs"] and ["clock_unenforced_runs"], the number of runs
    outside the sandbox and with a contract-only clock. *)

val summary_json : t -> Yojson.Safe.t
(** {!to_json} without behaviours, failures (and their traces) and the execution
    order: the header, verdicts, per-scenario results, the statistics of each
    scenario and variant, and cost, latency and tokens summaries. What
    [bactrian compare --json] prints. *)
