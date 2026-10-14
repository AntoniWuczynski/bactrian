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

type mcp_transport =
  | Mcp_stdio of {
      argv : string list;  (** Non-empty. *)
      env : (string * string) list;  (** The server's declared environment. *)
      cwd : string option;  (** Relative to the experiment file. *)
    }
  | Mcp_http of { url : string }  (** An [http://] or [https://] URL. *)

type mcp_server = { transport : mcp_transport; policy : policy }
(** [policy] is never [Replay]: an experiment's runs record. *)

type compare = {
  baseline : string;
  confidence : float;
  thresholds : (string * float) list;  (** metric name to δ *)
  gate : string list;  (** default ["pass_rate"] *)
  population : [ `Suite | `Scenarios ];
}

type sandbox_backend = [ `Auto | `Vm | `Seatbelt | `Bubblewrap ]
(** Where a sandboxed agent runs (design §5.3): [auto] chooses for the host. *)

type sandbox_runtime = [ `Docker ]
(** The container runtime of a [vm] sandbox. *)

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
  mcp : (string * mcp_server) list;  (** By server name, in name order. *)
  normalise : string list;  (** Regexes masked before hashing. *)
  redact : string list;
  sandbox : [ `Auto | `Strict | `Off ];
  sandbox_backend : sandbox_backend;
  sandbox_image : string option;
      (** The image a [vm] sandbox runs the agent in. Required for [vm]. *)
  sandbox_runtime : sandbox_runtime option;
      (** [None]: the first runtime that works. *)
  sandbox_allow_write : string list;
      (** More writable paths, relative to the experiment file. *)
  sandbox_deny_read : string list;
      (** More unreadable paths, relative to the experiment file. *)
  faketime : bool;  (** [clock: {faketime: auto}], [false] for [off]. *)
  caps_usd : (string * float) list;
  pricing : string option;  (** Price file, relative to the experiment file. *)
  seed : int;
  env_passthrough : string list;
  compare : compare option;
}

type error = Scenario.error

val decode : Canon.t -> (t, error list) result
(** Durations accept "120s", "2m", "1.5s". Defaults as in design §6.

    Document shape (design §6), all keys strict:
    - [version: 1], [name], [scenarios] (non-empty list of globs)
    - [agent: {protocol: command | http, command, url, namespace_header,
       timeouts: {turn, total}, env_passthrough}]: [command] is required for
      [command] and forbidden for [http], [url] (an [http://] or [https://] URL)
      and [namespace_header] the reverse. Timeouts default to 120s per turn and
      600s in total. [sandbox: strict] with [http] is an error at [sandbox]:
      bactrian does not start the agent.
    - [variants]: a mapping [name: {env: {K: V}}], or a list of [{name, env}]
      (where duplicate names are possible and an error).
    - [repeats] (default 1), [concurrency] (default 1), [seed] (default 0)
    - [intercept] (default all four), [egress: {default: block, hosts: {h: p}}],
      [mcp: {server: {command | url, policy, env, cwd}}], policies
      [record|replay|passthrough|block]. A server has either a [command] (stdio,
      with optional [env] and [cwd]) or a [url] (Streamable HTTP), and a
      [policy] (default [record], never [replay]). Names are letters, digits,
      ['_'] and ['-'], and no two HTTP servers may share a {!mcp_http_variable}.
    - [normalise], [redact]: lists of regular expressions
    - [sandbox: auto|strict|off] (default auto) or
      [sandbox: {mode, backend, image, runtime, allow_write, deny_read}] (design
      §5.3): [backend] is [auto] (default), [vm], [seatbelt] or [bubblewrap],
      [image] is required for [vm], and [runtime] is [docker] ([podman] and
      [container] are refused as not yet verified),
      [clock: {faketime: auto|off}] (default auto), [caps_usd: {provider: n}]
      ([provider] is one of {!Pricing.providers}), [pricing] (a price file, see
      {!Price_file})
    - [compare: {baseline, confidence (0.95), thresholds, gate (["pass_rate"]),
       population: suite|scenarios (suite)}]; [baseline] must name a variant. *)

val image_problem : string -> string option
(** What is wrong with a container image reference ([sandbox.image],
    [--sandbox-image], the MCP argument [sandbox_image]): empty, starting with
    [-] (an option to the runtime's client), or holding a NUL byte. *)

val reserved_env : string list
(** The variables bactrian sets in a run's environment: [SIM_NOW], [TZ],
    [BACTRIAN_RUN_ID], [BACTRIAN_STATE_DIR], the four provider base URLs and the
    four provider key placeholders, [HOME], [XDG_CACHE_HOME], [XDG_CONFIG_HOME],
    [XDG_DATA_HOME], [npm_config_cache], [TMPDIR], [PYTHONDONTWRITEBYTECODE]
    and, in a run with MCP servers, [BACTRIAN_MCP_URL]. See also
    {!reserved_env_prefixes}. *)

val reserved_env_prefixes : string list
(** Prefixes of further variables bactrian sets: [BACTRIAN_MCP_HTTP_]. *)

val is_reserved_env : string -> bool
(** Whether a name is in {!reserved_env} or starts with a reserved prefix. A
    variant that sets one is a configuration error at
    [variants.<name>.env.<KEY>], because it would be dropped without a word. *)

val mcp_http_variable : string -> string
(** [mcp_http_variable name] is [BACTRIAN_MCP_HTTP_] followed by [name]
    uppercased, with every character outside [A-Z0-9] written as ['_']: the
    variable that carries an HTTP server's proxied URL to the agent. *)

val valid_mcp_name : string -> bool
(** Letters, digits, ['_'] and ['-'], and not empty: a server's name becomes a
    URL path segment and, for an HTTP server, the suffix of its variable. *)

val valid_mcp_url : string -> bool
(** An [http://] or [https://] URL with something after the scheme. *)

val mcp_http_clash : http_names:string list -> string -> string option
(** [mcp_http_clash ~http_names name] is [name]'s {!mcp_http_variable} when
    another name of [http_names] has the same one, so the agent could not tell
    the two servers apart. *)

val policies : (string * policy) list
(** Every policy with its name in an experiment file: [record], [replay],
    [passthrough] and [block]. *)

val policy_name : policy -> string

val policy_of_name : string -> policy option
(** The inverse of {!policy_name}. *)

val unenforced : t -> string list
(** One line per setting this build accepts but does not enforce (egress, [mcp]
    servers when [intercept] leaves out [mcp], and the isolation of an [http]
    agent), each starting with the setting's name and a colon. The sandbox and
    the clock are enforced where the host allows, and a run they could not cover
    is labelled instead (design §5.3, §5 step 2). *)
