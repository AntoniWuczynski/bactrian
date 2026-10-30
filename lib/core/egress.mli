(** The pure parts of HTTPS egress interception (design §5.3 and §7): the
    per-host policy, the authority of a [CONNECT] request, the canonical form of
    an intercepted request, and the Node versions that honour
    [NODE_USE_ENV_PROXY]. *)

type policy = Experiment.policy

type t = {
  default : policy;  (** For a host the list does not name. *)
  hosts : (string * policy) list;
      (** Keyed by [host] or [host:port] ([[v6]:port] for an IPv6 address),
          compared without regard to case or a trailing dot. *)
}

val block_all : t
(** [default] [Block] and no host: what a run gets when nothing is listed. *)

val policy : t -> host:string -> port:int -> policy
(** The entry for [host:port] if there is one, else the one for [host], else
    [default]. Names are compared as {!Trace.host_name} gives them. *)

val authority : string -> (string * int, string) result
(** The host, lowercased and without IPv6 brackets, and port of a [CONNECT]
    request target: [host:port] or [[v6]:port]. The host is in the form
    {!Trace.host_name} gives, so [Example.com.:443] names [example.com]. The
    port must be from 1 to 65535. *)

val host_port : string -> int -> string
(** [host:port], with brackets around an IPv6 address. *)

val target :
  host:string -> port:int -> string -> (string * string option, string) result
(** The path and the query (as sent, without the ['?']) of a request target on a
    tunnel to [host:port]: the origin form [/path?query], or the absolute form
    [https://host[:port]/path?query] when it names the tunnel's own host and
    port (443 when it gives none). [Error] for any other form (another host,
    user information, another scheme, [*], a fragment), so that a request on a
    tunnel cannot reach a host other than the one its [CONNECT] named. *)

val canonical :
  authority:string ->
  meth:string ->
  path:string ->
  query:(string * string) list ->
  content_type:string option ->
  string ->
  Canon.t
(** The canonical request of an intercepted call (design §7):
    [{"authority", "method", "path", "query", "content-type", "body"}], so that
    the diff of a replay miss names what changed. [query] is given sorted. The
    body is its canonical JSON when it parses as JSON, [null] when empty, and
    otherwise [{"blake2b-256": <hex>}] of its bytes. [content-type] is [null]
    when the request has none. *)

val to_json : t -> Yojson.Safe.t
(** [{"default": <policy>, "hosts": {<host>: <policy>}}], as [meta.json] keeps
    it. *)

val of_json : Yojson.Safe.t -> (t, string) result
(** The inverse of {!to_json}. *)

val node_env_proxy : string -> (bool, string) result
(** Whether the Node.js version that [node --version] printed (["v24.3.0"])
    honours [NODE_USE_ENV_PROXY]: 24.0 and later, and 22.21 and later in the 22
    line. [Error] when the text is not a version. *)

val of_flags : default:string option -> string list -> (t, string) result
(** The policy of [record]'s [--egress-default POLICY] (default [block]) and
    [--egress HOST=POLICY] flags. [Error] names a flag that is not [HOST=POLICY]
    or a policy that is not [record], [replay], [passthrough] or [block], a host
    that {!Experiment.egress_host} refuses, and a host named twice. *)
