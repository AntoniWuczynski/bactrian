(** Statistics of one scenario's runs in one variant (spec §8, per scenario and
    variant): the pass rate with a Wilson interval, pass^k for every k up to the
    number of finished runs, and the median and 90th percentile of cost, tokens,
    total latency and time to first token, with bootstrap intervals. Pure: every
    random draw comes from the [rng] passed in. *)

type outcome = Passed | Failed | Errored | Not_run

type run = {
  outcome : outcome;
      (** [Failed] covers diverged runs, [Not_run] unfinished ones. *)
  cost : float option;
  tokens : int option;
  latency_ms : float option;  (** Total over the run's model calls. *)
  ttft_ms : float list;  (** Time to first token of each model call. *)
}

type measure = {
  runs : int;  (** Finished runs with a value. *)
  values : int;  (** Values: one per run, or one per model call for TTFT. *)
  median : float;
  p90 : float;
  median_interval : Stats.interval option;
  p90_interval : Stats.interval option;
      (** Percentile bootstrap intervals that resample runs, keeping each run's
          values together. [None] below {!min_runs} runs. *)
}

type t = {
  finished : int;  (** Passed and failed runs: the [n] of the statistics. *)
  passes : int;
  errored : int;
  not_run : int;
  wilson : Stats.interval option;  (** [None] without a finished run. *)
  pass_hat_k : float list;
      (** {!Stats.pass_hat_k} for k = 1 to [finished], in order. *)
  cost : measure option;  (** [None] when no finished run has a value. *)
  tokens : measure option;
  latency_ms : measure option;
  ttft_ms : measure option;
}

val min_runs : int
(** 5, as for the comparison of cost and latency: below it a bootstrap of a
    median is too coarse to give an interval. *)

val compute :
  rng:Random.State.t -> resamples:int -> confidence:float -> run list -> t
(** Only finished runs enter the statistics. The intervals are at [confidence].
    Raises [Invalid_argument] if [resamples < 1] or [confidence] is outside
    [(0, 1)]. *)
