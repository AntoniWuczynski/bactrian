(** The provider base URLs a user can point bactrian at instead of the real
    providers, for a self-hosted server or a gateway: one environment variable
    per provider, [BACTRIAN_UPSTREAM_<PROVIDER>]. *)

val variables : (Bactrian_runtime.Model_proxy.provider * string) list
(** Each provider with its variable: [BACTRIAN_UPSTREAM_OPENAI],
    [BACTRIAN_UPSTREAM_ANTHROPIC], [BACTRIAN_UPSTREAM_GEMINI] and
    [BACTRIAN_UPSTREAM_OPENROUTER], built from the provider names in
    {!Bactrian_runtime.Run.providers}. *)

val parse : variable:string -> string -> (Uri.t, string) result
(** Accepts an absolute [http] or [https] URL (the scheme in any case) with a
    host, an optional port from 1 to 65535 and an optional path prefix. Rejects
    anything else: a relative URL, another scheme, user information, a query, a
    fragment, a missing host, a bad port and whitespace or control characters.
    The error names [variable] and the rule that failed, and never repeats the
    value, which could hold a credential. *)

val read :
  (string -> string option) ->
  (Bactrian_runtime.Model_proxy.provider -> Uri.t, string) result
(** The upstream of each provider from the variables [getenv] answers: the URL
    of a variable that is set and non-empty, else
    {!Bactrian_runtime.Model_proxy.default_upstream}. The first invalid variable
    is the [Error]. *)

val from_environment :
  unit -> (Bactrian_runtime.Model_proxy.provider -> Uri.t, string) result
(** {!read} on bactrian's own environment. This is the only place that reads the
    variables: every command that can call a provider takes its upstream from
    here. *)
