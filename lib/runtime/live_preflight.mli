(** The checks before a command calls providers live, which [record], [replay],
    [run] and [doctor] share (design §9, decision record 0012): a provider is
    called when its spend cap is above 0, it needs its key variable, an [https]
    upstream needs TLS, and a plain [http] upstream on another machine receives
    the real key unencrypted. *)

type problem =
  | No_cap  (** No provider has a spend cap above 0. *)
  | Missing_key of { provider : Model_proxy.provider; name : string }
      (** A provider with a cap has no key. *)
  | No_tls of { providers : string list; reason : string }
      (** These providers, which have a cap, have an [https] upstream and TLS
          cannot work, for [reason]: no TLS connector in the build, or no
          trusted root certificates. *)

val funded : (string * float) list -> (Model_proxy.provider * string) list
(** The providers (with their names) whose spend cap is above 0: the ones a
    command calls live. *)

val problem :
  caps:(string * float) list ->
  has_key:(Model_proxy.provider -> bool) ->
  upstream:(Model_proxy.provider -> Uri.t) ->
  tls:(unit -> (Upstream.trust, string) result) ->
  problem option
(** The first problem, in the order of the constructors. [tls] (usually
    {!Upstream.tls_ready}) is called only when a provider with a cap has an
    [https] upstream. *)

val describe : set_caps:string -> problem -> string
(** The message for a problem. [set_caps] says where caps are set, for [No_cap].
*)

val is_loopback : Uri.t -> bool
(** The host is [localhost], an IPv4 address in 127.0.0.0/8, [::1] or an
    IPv4-mapped IPv6 loopback address, parsed as an address rather than compared
    as text. *)

val cleartext :
  (string * float) list -> (Model_proxy.provider -> Uri.t) -> string list
(** A warning for each provider with a cap whose upstream is plain [http] to a
    host that is not {!is_loopback}: the real key would travel unencrypted. *)

val https_funded :
  (string * float) list -> (Model_proxy.provider -> Uri.t) -> string list
(** The names of the providers with a cap whose upstream is [https]. *)

val mcp_https : replaying:bool -> Mcp_proxy.config list -> string list
(** ["the MCP server NAME at URL"] for each Streamable HTTP server at an [https]
    URL that a run calls live: [Passthrough] always, and [Record] unless the run
    is [replaying]. *)

val https_refusal :
  tls:(unit -> (Upstream.trust, string) result) -> string list -> string option
(** For what a command calls over HTTPS besides the providers
    (["the agent at URL"], {!mcp_https}): the exit 69 message when the list is
    not empty and [tls] fails, naming them and [tls]'s reason. [tls] is called
    only for a non-empty list. *)

val extra_trust : Upstream.extra_trust -> string
(** Says that the variable's certificates are trusted for every HTTPS host, the
    providers included, and how many there are. *)

val trust_warning :
  tls:(unit -> (Upstream.trust, string) result) -> string list -> string option
(** {!extra_trust} when [tls] succeeds with extra certificates and the list of
    what is called over HTTPS is not empty. [tls] is called only then. *)
