type agent =
  | Command of {
      argv : string list;
      turn_timeout : float;
      total_timeout : float;
    }
  | Http of {
      url : string;
      namespace_header : string;
      turn_timeout : float;
      total_timeout : float;
    }

type variant = { name : string; env : (string * string) list }
type policy = Record | Replay | Passthrough | Block

type compare = {
  baseline : string;
  confidence : float;
  thresholds : (string * float) list;  (** metric name to δ *)
  gate : string list;  (** default ["pass_rate"] *)
  population : [ `Suite | `Scenarios ];
}

type t = {
  name : string;
  agent : agent;
  scenarios : string list;  (** Globs, relative to the experiment file. *)
  variants : variant list;  (** At least one, unique names. *)
  repeats : int;  (** >= 1 *)
  concurrency : int;  (** >= 1 *)
  intercept : [ `Model | `Mcp | `Fixtures | `Egress ] list;
  egress_default : policy;  (** default Block *)
  egress_hosts : (string * policy) list;
  mcp : (string * policy) list;
  normalise : string list;  (** Regexes masked before hashing. *)
  redact : string list;
  sandbox : [ `Auto | `Strict | `Off ];
  faketime : bool;
  caps_usd : (string * float) list;
  pricing : string option;  (** Price file, relative to the experiment file. *)
  seed : int;
  env_passthrough : string list;
  compare : compare option;
}

type error = Scenario.error

val decode : Canon.t -> (t, error list) result
(** Durations accept "120s", "2m", "1.5s". Defaults as in spec §6.

    Document shape (spec §6), all keys strict:
    - [version: 1], [name], [scenarios] (non-empty list of globs)
    - [agent: {protocol: command | http, command, url, namespace_header,
       timeouts: {turn, total}, env_passthrough}]: [command] is required for
      [command] and forbidden for [http], [url] and [namespace_header] the
      reverse. Timeouts default to 120s per turn and 600s in total.
    - [variants]: a mapping [name: {env: {K: V}}], or a list of [{name, env}]
      (where duplicate names are possible and an error).
    - [repeats] (default 1), [concurrency] (default 1), [seed] (default 0)
    - [intercept] (default all four), [egress: {default: block, hosts: {h: p}}],
      [mcp: {server: policy}], policies [record|replay|passthrough|block]
    - [normalise], [redact]: lists of regular expressions
    - [sandbox: auto|strict|off] (default auto), [clock: {faketime: auto|off}]
      (default auto), [caps_usd: {provider: n}] ([provider] is one of
      {!Pricing.providers}), [pricing] (a price file, see {!Price_file})
    - [compare: {baseline, confidence (0.95), thresholds, gate (["pass_rate"]),
       population: suite|scenarios (suite)}]; [baseline] must name a variant. *)

val reserved_env : string list
(** The variables bactrian sets in every run's environment: [SIM_NOW], [TZ],
    [BACTRIAN_RUN_ID], [BACTRIAN_STATE_DIR], the four provider base URLs and the
    four provider key placeholders. A variant that sets one is a configuration
    error at [variants.<name>.env.<KEY>], because it would be dropped without a
    word. *)

val unenforced : t -> string list
(** One line per setting this build accepts but does not enforce yet (spec §5.3:
    the sandbox under [auto], egress, MCP interception and faketime), each
    starting with the setting's name and a colon. [sandbox: strict] is not
    listed: it is refused outright where the run starts. *)
