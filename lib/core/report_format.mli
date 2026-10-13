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

val interval : (float -> string) -> Stats.interval -> string
(** [lower to upper]. *)

val metric_texts : Report_data.metric_row -> string * string * string
(** The estimate, the interval and the threshold of a verdict row, as text. Pass
    rates in percentage points, other metrics as relative changes, ["undefined"]
    when the metric has no estimate. *)
