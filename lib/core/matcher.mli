(** Deciding, request by request, what a replay serves. Pure: the runtime holds
    the state and calls [decide] under a lock. *)

type mode =
  | Strict
  | Until_divergence
  | Fuzzy of float  (** Minimum similarity. *)

type shape = Streamed | Whole_body

type request = {
  channel : Cassette.channel;
  key : string;
  canonical : Canon.t;
  shape : shape;
}

val key :
  ?headers:(string * string) list ->
  meth:string ->
  path:string ->
  query:(string * string) list ->
  Canon.t ->
  string
(** The replay key: [Canon.digest] of an object holding the method, the path,
    the query and the canonical request.

    [headers] defaults to [[]]. Only [anthropic-version], [anthropic-beta] and
    [openai-beta] are kept, because they change behaviour (decision 0005). Names
    are compared case-insensitively and lowercased in the key, values are taken
    as given, and the pairs are sorted by name. They are hashed only when at
    least one is kept, so a key without headers is the same as before headers
    existed. *)

type divergence =
  | Missing_request of {
      request : request;
      nearest : (Cassette.exchange * Diff.change list * float) option;
          (** Closest unconsumed exchange on the same channel, its diff and
              similarity. *)
    }
  | Unconsumed of Cassette.exchange
  | Reordered of { request : request; expected : Cassette.exchange }
  | Shape_mismatch of { request : request; recorded : Cassette.exchange }

type decision =
  | Serve of { exchange : Cassette.exchange; similarity : float option }
      (** [similarity] is set only when fuzzy matching chose a non-exact
          exchange. *)
  | Go_live
      (** [Until_divergence] after the switch: forward this model request
          upstream. *)
  | Refuse of divergence  (** Answer with [bactrian_replay_miss]. *)

type state

val start : mode:mode -> allow_reorder:bool -> Cassette.exchange list -> state

val decide : state -> request -> state * decision * divergence list
(** The list carries non-fatal findings: [Reordered] when [allow_reorder] is
    set, and the divergence that triggered the switch to live. *)

val switched : state -> bool
(** Whether [Until_divergence] has switched the model channel to live. *)

val finish : state -> divergence list
(** [Unconsumed] for every recorded exchange never served, in [seq] order. Model
    exchanges are not reported after a switch to live. *)
