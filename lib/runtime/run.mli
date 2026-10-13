(** One run end to end (spec §5): setup, the agent conversation, teardown, trace
    and metadata. *)

open Bactrian_core

val providers : (Model_proxy.provider * string) list
(** Every provider with its name, which is also its ledger name. *)

val keys_from_environment : unit -> (Model_proxy.provider * string) list
(** The non-empty real provider keys of bactrian's own environment. *)

val fresh_id : random_bytes:(int -> string) -> float -> string
(** [YYYYMMDDTHHMMSSZ-xxxxxxxx] for the Unix time given, with 4 random bytes as
    hex. Used for run and experiment IDs. *)

val key_variable : Model_proxy.provider -> string
(** The environment variable holding bactrian's real key for the provider, and
    the one that carries the placeholder in the agent's environment. *)

type outcome =
  | Passed
      (** The run completed and, when it has {!type-checks}, every assertion and
          checker passed. *)
  | Failed of string
      (** Agent failure (exit, timeout, protocol violation, agent error), or the
          failing assertions and checkers, one per [;]-separated item. *)
  | Diverged
      (** Replay only: at least one refusing divergence (an [Unconsumed]
          recorded exchange counts; a [Reordered] one does not when
          [allow_reorder] is set). *)
  | Errored of string  (** A fault inside bactrian. *)
  | Not_run of string  (** Spend cap reached. *)

val outcome_kind : outcome -> string
(** ["passed"], ["failed"], ["diverged"], ["errored"] or ["not_run"]. *)

val outcome_reason : outcome -> string option

type replay = {
  from : string;
      (** A run ID, or a path to a run directory whose last component is the run
          ID. Either way the run is read from the store passed to {!execute}. *)
  matcher : Matcher.mode;
  allow_reorder : bool;
  timing : Model_proxy.timing;
}

type checks = {
  assertions : Assertion.t list;
  checkers : string list list;
  checks_cwd : string;  (** Where the checker commands run. *)
  checker_timeout : float;  (** Per checker, seconds. *)
}
(** A scenario's assertions and checkers (spec §6). *)

type config = {
  agent : string list;  (** argv *)
  cwd : string;
  turns : string list;
      (** In a replay, [[]] means the turns of the recording's [trace.jsonl]. *)
  scenario : string;
  variant : string;
  sim_now : string;
      (** RFC 3339; [""] defaults to the recording's sim_now in replay, else
          now. *)
  timezone : string;
  replay : replay option;  (** None = record. *)
  env_passthrough : string list;
  variant_env : (string * string) list;
      (** The variant's variables, given to the agent after the passthrough
          ones. They cannot replace bactrian's own variables. *)
  checks : checks option;
      (** Evaluated over the trace of a run that passed, failed or diverged, and
          written to [verdicts.json] (atomically) in the run directory. Only a
          run that otherwise passed is judged by them: a failing assertion or
          checker makes it [Failed], and a checker that gives no verdict makes
          it [Errored] ("checker: ..."). [None] for record and replay from the
          command line. *)
  unenforced : string list;
      (** Experiment settings this build does not enforce yet, one line each,
          recorded in [meta.json]. *)
  fixtures : string option;
      (** A directory whose files are copied, recursively, into the state
          directory before the agent starts (spec §5 step 1). Symbolic links are
          not followed. *)
  normalise : string list;
      (** Experiment [normalise] patterns, see {!Model_proxy.run}. Saved in
          [meta.json]. A replay ignores them and uses its recording's. *)
  redact : string list;
      (** Experiment [redact] patterns. Saved in [meta.json]. A replay ignores
          them and uses its recording's. *)
  pricing : Pricing.table;
      (** For the run's reservations, settlement and [max_cost_usd]. *)
  turn_timeout : float;
  total_timeout : float;  (** Longest the whole run may take. *)
}

type result = {
  run_id : string;
  outcome : outcome;
  run_dir : string;  (** Absolute. *)
  findings : Model_proxy.findings;
  spend : float;
      (** USD the ledger attributes to this run ({!Ledger.spent_by}): what its
          model calls settled at plus its usage gaps, whatever other runs spend
          at the same time. Written to [meta.json] as [cost_usd], beside
          [latency_ms] and [tokens] (null when the trace has no model call). *)
}

val execute :
  ?random_bytes:(int -> string) ->
  ?on_start:(string -> unit) ->
  ?keys:(Model_proxy.provider * string) list ->
  ?upstream:(Model_proxy.provider -> Uri.t) ->
  ?connector:Upstream.connector ->
  env:Eio_unix.Stdenv.base ->
  proxy:Model_proxy.t ->
  store:Store.t ->
  ledger:Ledger.t ->
  config ->
  (result, string) Stdlib.result
(** Creates the run directory, cassette and state dir, builds the agent's
    environment (spec §5 step 2 minus egress, sandbox and libfaketime: provider
    base URLs from the proxy, placeholder keys, SIM_NOW, TZ, BACTRIAN_RUN_ID,
    BACTRIAN_STATE_DIR, PATH, HOME, LANG, TERM and the passthrough list, and
    nothing else), drives the turns, tears down, derives and writes trace.jsonl
    and meta.json (atomically), and returns the outcome.

    [Error] means the run could not be set up (unreadable recording, store
    failure, unreadable fixtures, an invalid pattern): no agent was started.

    [on_start] is called with the run ID as soon as it is chosen, before
    anything can fail or be cancelled, so the caller knows which run directory a
    cancelled run left behind.

    [random_bytes] makes the run ID suffix and the run token (16 bytes, 32 hex
    digits); the default is [Mirage_crypto_rng.generate], so the caller must
    have initialised the generator. [keys] are the real provider keys, default
    the non-empty [OPENAI_API_KEY], [ANTHROPIC_API_KEY], [GEMINI_API_KEY] and
    [OPENROUTER_API_KEY] of bactrian's own environment. [upstream] and
    [connector] default to {!Model_proxy.default_upstream} and
    {!Upstream.plain}.

    In a replay, the run's own cassette holds a copy of every exchange served
    from the recording plus any live-tail exchanges, so a strict replay's trace
    has the same model calls as the recording's. *)
