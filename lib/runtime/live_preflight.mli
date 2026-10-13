(** The checks before a command calls providers live, which [record], [replay],
    [run] and [doctor] share (design §9, decision record 0012): a provider is
    called when its spend cap is above 0, it needs its key variable, an [https]
    upstream needs TLS, and a plain [http] upstream on another machine receives
    the real key unencrypted. *)

type problem =
  | No_cap  (** No provider has a spend cap above 0. *)
  | Missing_key of { provider : Model_proxy.provider; name : string }
      (** A provider with a cap has no key. *)
  | No_tls of string list
      (** These providers, which have a cap, have an [https] upstream and this
          build has no TLS connector. *)

val funded : (string * float) list -> (Model_proxy.provider * string) list
(** The providers (with their names) whose spend cap is above 0: the ones a
    command calls live. *)

val problem :
  caps:(string * float) list ->
  has_key:(Model_proxy.provider -> bool) ->
  upstream:(Model_proxy.provider -> Uri.t) ->
  tls_available:bool ->
  problem option
(** The first problem, in the order of the constructors. *)

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
