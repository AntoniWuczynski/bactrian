type sequence_mode = Exact | In_order | Contains
type args_mode = Exact_args | Partial | Schema

type output_check =
  | Regex of string
  | Jsonpath of { path : string; equals : Canon.t option }
  | Equals of string

type t =
  | Tool_sequence of { mode : sequence_mode; tools : string list }
  | Tool_args of { tool : string; mode : args_mode; expected : Canon.t }
  | Max_steps of int
  | Output of { turn : [ `Last | `Index of int ]; check : output_check }
  | Max_cost_usd of float
  | Max_tokens of int
  | Max_latency_ms of int
  | No_egress of string list  (** Hosts that must not appear in HTTP calls. *)

type outcome = Pass | Fail of string  (** Human-readable reason. *)

val decode : Canon.t -> (t, Config_error.error list) result
(** One assertion: an object with a single key naming the kind.
    - [tool_sequence: {mode: exact|in_order|contains, tools: [...]}]
    - [tool_args: {tool, mode?: exact|partial|schema, match | schema}]: [mode]
      defaults to [partial]. [match] is the expected arguments for [exact] and
      [partial]. [schema] is the JSON Schema for [schema] mode.
    - [max_steps: n], [max_tokens: n], [max_cost_usd: x]
    - [max_latency: ms | "500ms" | "1.5s" | "2m"]
    - [output: {turn?: last | n, regex | equals | jsonpath, equals?}]: [turn]
      defaults to [last] and counts from 1. [jsonpath] may carry an [equals]
      value of any shape. Without it, the path only has to resolve.
    - [no_egress: [host, ...]]

    Strict: unknown fields are errors, and every error is collected. Paths are
    relative to the assertion (["tool_args.tool"]). *)

val evaluate :
  cost:(Trace.entry list -> float option) ->
  egress_intercepted:bool ->
  t ->
  Trace.entry list ->
  outcome
(** [cost] prices a trace (supplied by the caller, from Pricing).
    [egress_intercepted] says whether the run's HTTP egress was intercepted (and
    so recorded in the trace as HTTP calls). When it was not, [No_egress] fails
    as unverifiable instead of passing vacuously. Schema mode supports a
    documented subset of JSON Schema: type, properties, required, enum, items,
    additionalProperties false. Jsonpath supports $, .key, [n] and ["key"]. A
    step is a model call.

    Semantics worth knowing:
    - [Exact] needs the called tools to equal [tools]. [In_order] needs [tools]
      as a subsequence of the calls. [Contains] needs every listed tool called
      at least as many times as it is listed, in any order.
    - [Tool_args] passes when at least one call of [tool] matches. [Partial]
      means every expected field is present with a partially matching value, and
      every expected list element is matched by an actual element of its own
      (each actual element is used at most once, so expecting [a] twice needs
      two [a]). An empty [match] object matches any call of the tool.
    - [Output] checks the reply of the given turn (counting from 1), or the last
      reply. No such reply is a failure.
    - [Max_tokens] sums input and output tokens over the model calls, and fails
      when any call has no usage (an interrupted stream, a provider that omits
      it), like [Max_cost_usd]. [Max_latency_ms] is the span from the first to
      the last entry.
    - An unpriced trace, or one with a model call that has no usage, fails
      [Max_cost_usd]. *)
