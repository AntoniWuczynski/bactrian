(** Text for the report: numbers, labels and trace lines. Pure. *)

val points : float -> string
(** A difference of proportions as signed percentage points, ["-3.3 pp"].
    ["n/a"] for NaN. *)

val percent : float -> string
(** A relative change as a signed percentage, ["+9.1%"]. *)

val usd : float -> string
val ms : float -> string

val share : int -> int -> string
(** [share k n] is ["k/n"]. *)

val rate : int -> int -> string
(** [rate k n] is the percentage with no decimals, ["70%"]; ["n/a"] for [n = 0].
*)

val signature : string list -> string
(** The tool names joined by [" > "], ["(no tool calls)"] when empty. *)

val verdict : Compare.verdict -> string
(** ["Regression"], ["Improvement"], ["No change"],
    ["Inconclusive, leans worse"]. *)

val verdict_class : Compare.verdict -> string
(** A CSS class: ["regression"], ["improvement"], ["no-change"] or
    ["inconclusive"]. *)

val metric : Compare.metric -> string
(** ["Pass rate"], ["Cost"], ["Median latency"], ["Tokens"]. *)

val status : Report_data.status -> string

val truncate : int -> string -> string
(** At most the given number of bytes, cut on a UTF-8 boundary, with an ellipsis
    when something was cut. *)

val trace_line : Trace.entry -> string
(** One line: sequence number, time, event type and its fields. Long contents
    are truncated. *)

val exit_meaning : int -> string
(** What a gate exit status means. *)

val clock_warning : Report_data.t -> string option
(** Names the runs whose clock was only a contract (design §5 step 2), with the
    reasons, and says that an agent reading the system clock saw real time in
    them. When only some runs are affected it adds that a difference between
    arms may come from the clock. [None] when every run's clock was enforced. *)

val value_estimand_text : string
(** What the cost, tokens and latency comparisons estimate, for a report header.
*)

val interval : (float -> string) -> Stats.interval -> string
(** [lower to upper]. *)

val threshold_text : Compare.metric -> float -> string
(** A declared threshold as text. Pass rate: ["±10.0 pp"]. Cost, latency and
    tokens apply [δ] as [log (1 + δ)], which is asymmetric in percent, so both
    sides are given: [threshold_text Cost 0.2] is
    ["20.0% dearer, 16.7% cheaper"] (a regression is more than 20% dearer, an
    improvement more than 16.7% cheaper). Latency says slower and faster, tokens
    more and fewer. *)

val undefined_text : Report_data.metric_row -> string option
(** For an undefined metric, one sentence naming it, saying whether it counts as
    inconclusive in the gate, and why it is undefined. *)

val zero_width_text : Report_data.metric_row -> string option
(** For a cost, latency or tokens interval of zero width, one sentence saying
    that every resample gave the same value, which is certain only if the values
    are deterministic. *)

val notes : Report_data.metric_row -> string list
(** {!undefined_text}, {!zero_width_text} and {!extra_repeats_text}, those that
    apply, in that order. *)

val extra_repeats_text : Report_data.metric_row -> string option
(** For an inconclusive gate metric, one sentence on the extra repeats that
    would resolve it, naming the metric. *)

val metric_texts : Report_data.metric_row -> string * string * string
(** The estimate, the interval and the threshold ({!threshold_text}) of a
    verdict row, as text. Pass rates in percentage points, other metrics as
    relative changes, ["undefined"] when the metric has no estimate. *)
