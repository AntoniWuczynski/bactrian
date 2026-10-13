(** The model proxy: one loopback listener serving every registered run.
    Requests arrive at /r/<token>/<provider>/<provider path>. *)

open Bactrian_core

type provider =
  | Openai
  | Anthropic
  | Gemini
  | Openrouter
      (** Path segments "openai", "anthropic", "gemini", "openrouter". Default
          upstreams: https://api.openai.com, https://api.anthropic.com,
          https://generativelanguage.googleapis.com, https://openrouter.ai. *)

val default_upstream : provider -> Uri.t
(** The default upstreams listed above. *)

type timing = Fast | Recorded

type mode =
  | Record
  | Replay of {
      recording : Cassette.t;
      matcher : Matcher.mode;
      allow_reorder : bool;
      timing : timing;
    }
  | Passthrough  (** Forward and charge the ledger, record nothing. *)

type run = {
  run_id : string;
  token : string;
      (** 32 lowercase hex characters. The agent's keys are "bactrian-" ^ token.
      *)
  mode : mode;
  store : Store.t;
      (** Where large payloads go when recording and are read from in replay. *)
  cassette : Store.writer option;
      (** Where exchanges go: required for Record, and for Replay with
          Until_divergence (the live tail). *)
  ledger : Ledger.t;
  keys : (provider * string) list;  (** Real keys, never sent to the agent. *)
  upstream : provider -> Uri.t;  (** Tests point this at the fake upstream. *)
  connector : Upstream.connector;
  run_started : float;  (** Eio clock time, for exchange t_ms. *)
}

type findings = {
  divergences : Matcher.divergence list;
      (** Refusals, findings, and Unconsumed at unregister. *)
  exchanges : int;  (** Exchanges recorded or served. *)
  transport_errors : int;
      (** Upstream calls that failed or were cut off mid-body. *)
  usage_gaps : int;
  refused_keys : int;  (** Requests with a missing or wrong placeholder key. *)
  cap_refusals : Ledger.refusal list;
      (** Requests refused with 402 before forwarding: an unknown model or a
          reached cap. *)
}

type t

val start :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  (t, string) result
(** Binds 127.0.0.1 on an OS-chosen port. The listener runs until [sw] ends. *)

val port : t -> int

val register : t -> run -> unit
(** Raises [Invalid_argument] if the token is already registered. *)

val unregister : t -> token:string -> findings
(** Stops serving the token (later requests get 403) and returns its findings.
    Raises [Invalid_argument] for a token that is not registered. *)

val base_url : t -> token:string -> provider -> string
(** e.g. "http://127.0.0.1:<port>/r/<token>/openai/v1" for OpenAI (SDK base URLs
    include /v1 for OpenAI and OpenRouter's /api/v1, not for Anthropic or
    Gemini). *)
