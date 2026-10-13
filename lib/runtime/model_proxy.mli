(** The model proxy: one loopback listener serving every registered run.
    Requests arrive at /r/<token>/<provider>/<provider path>.

    The agent's key is "bactrian-" ^ token in the provider's usual key header.
    For Gemini it may instead (or also) be the [key] query parameter, which must
    then equal the same placeholder (a wrong value gets 403) and is removed from
    the query forwarded upstream. The real key always goes upstream in the
    provider's header ([x-goog-api-key] for Gemini). *)

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

type timing =
  | Fast
  | Recorded
      (** [Recorded] sleeps each chunk's recorded [dt_ms] before serving it, so
          the agent sees the original pacing. [Fast] serves chunks back to back.
          A recorded stream with [complete = false] is served up to its last
          chunk and then the agent's connection is dropped without the terminal
          chunk, as it was live, so the agent sees a truncated body in both
          timings. *)

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
      (** Where exchanges go: required for Record and for Replay. A replay
          appends a copy of every exchange it serves from the recording (this
          run's [seq] and [t_ms], the recorded key, occurrence, canonical
          request and response), so the run's trace has the model calls, plus
          the live-tail exchanges under Until_divergence. *)
  ledger : Ledger.t;
  keys : (provider * string) list;  (** Real keys, never sent to the agent. *)
  upstream : provider -> Uri.t;  (** Tests point this at the fake upstream. *)
  connector : Upstream.connector;
  normalise : Re.re list;
      (** The experiment's [normalise] patterns
          ({!Bactrian_core.Regex.compile}d), masked in each model request's
          canonical form before it is keyed (design §7), when recording and when
          replaying alike. *)
  redact : Redact.pattern list;
      (** The experiment's [redact] patterns, scrubbed in addition to the
          provider key formats and the known literals. *)
  pricing : Pricing.table;
      (** Prices for this run's reservations and settlements. *)
  run_started : float;  (** Eio clock time, for exchange t_ms. *)
  mcp : Mcp_proxy.config list;
      (** The run's MCP servers and their policies, served on
          [/r/<token>/mcp/<name>] (stdio, through [mcp-shim]) and
          [/r/<token>/mcp-http/<name>/...] (Streamable HTTP), see {!Mcp_proxy}.
          Their exchanges share this run's cassette, redaction, masks, matcher
          and findings. *)
}

type findings = {
  divergences : Matcher.divergence list;
      (** Refusals, findings, and Unconsumed at unregister. *)
  exchanges : int;  (** Exchanges recorded or served. *)
  transport_errors : int;
      (** Upstream calls that failed or were cut off mid-body. *)
  usage_gaps : int;
      (** Exchanges whose cost is unknown: no usable usage figure, an upstream
          cut off mid-body, or an agent that disconnected before the response
          ended (the reservation stays charged in each case). *)
  refused_keys : int;  (** Requests with a missing or wrong placeholder key. *)
  masks_fired : int;
      (** Matches of the [normalise] patterns masked across the run's model
          requests. *)
  cap_refusals : Ledger.refusal list;
      (** Requests refused with 402 before forwarding: an unknown model or a
          reached cap. *)
  errors : string list;
      (** What went wrong for this run, in order: upstream transport errors with
          their reason, agent connections that failed or went away mid-response
          (the upstream copy is cancelled and the exchange is recorded with the
          chunks received so far and [complete = false]), malformed request
          bodies, and refused requests (wrong key, cap or unknown model, replay
          miss, unknown endpoint, no configured key). Requests that cannot be
          tied to a run are counted by {!unattributed} instead. *)
}

type t

val start :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  process_mgr:_ Eio_unix.Process.mgr ->
  (t, string) result
(** Binds 127.0.0.1 on an OS-chosen port. The listener runs until [sw] ends.
    [process_mgr] starts the runs' stdio MCP servers, which [sw] also bounds. *)

val port : t -> int

val listen_unix :
  t ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  token:string ->
  string ->
  (unit, string) result
(** [listen_unix t ~sw ~net ~token path] also serves the proxy on a new Unix
    socket at [path] until [sw] ends: the socket a Linux sandbox reaches the
    proxy through (design §5.3). It answers only requests for the run [token],
    as if every other token were unknown (403), so one run's socket cannot serve
    another run. The caller removes the file. *)

val unattributed : t -> int
(** Proxy-wide count of requests and connection failures that cannot be tied to
    a registered run: an unknown or ended token, an unknown provider segment, a
    path outside /r/<token>/<provider>/..., an unreadable request body, and
    connection exceptions raised before a run's response was being written. They
    appear in no run's findings. *)

val register : t -> run -> unit
(** Raises [Invalid_argument] if the token is already registered. *)

val unregister : t -> token:string -> findings
(** Stops serving the token (later requests get 403), kills the run's stdio MCP
    servers and returns its findings. Raises [Invalid_argument] for a token that
    is not registered. *)

val base_url : t -> token:string -> provider -> string
(** e.g. "http://127.0.0.1:<port>/r/<token>/openai/v1" for OpenAI (SDK base URLs
    include /v1 for OpenAI and OpenRouter's /api/v1, not for Anthropic or
    Gemini). *)

val mcp_url : t -> token:string -> string
(** "http://127.0.0.1:<port>/r/<token>/mcp": the run's
    {!Mcp_proxy.url_variable}, to which [mcp-shim NAME] appends "/NAME". *)

val mcp_http_url : t -> token:string -> string -> string
(** "http://127.0.0.1:<port>/r/<token>/mcp-http/<name>": the URL the agent uses
    for the Streamable HTTP server [name]. *)
