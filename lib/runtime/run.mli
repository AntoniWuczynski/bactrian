(** One run end to end (spec §5): setup, the agent conversation, teardown, trace
    and metadata. *)

open Bactrian_core

val providers : (Model_proxy.provider * string) list
(** Every provider with its name, which is also its ledger name. *)

val key_variable : Model_proxy.provider -> string
(** The environment variable holding bactrian's real key for the provider, and
    the one that carries the placeholder in the agent's environment. *)

type outcome =
  | Passed  (** M4 has no assertions yet: a run that completes is Passed. *)
  | Failed of string
      (** Agent failure: exit, timeout, protocol violation, agent error. *)
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
  turn_timeout : float;
  total_timeout : float;  (** Longest the whole run may take. *)
}

type result = {
  run_id : string;
  outcome : outcome;
  run_dir : string;  (** Absolute. *)
  findings : Model_proxy.findings;
  spend : (string * float) list;
      (** USD this run added to the ledger, per provider that is not zero. *)
}

val execute :
  ?random_bytes:(int -> string) ->
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
    failure): no agent was started.

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
