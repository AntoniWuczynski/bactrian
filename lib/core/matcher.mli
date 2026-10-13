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

    The query is normalised here: every parameter named [key] (case-sensitive,
    it carries API keys) is dropped, and the rest are sorted by name, then by
    value, so neither their order nor the secret affects the key.

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
          (** Closest of the next 64 unconsumed exchanges on the same channel in
              [seq] order (the window keeps a miss cheap), with its diff and
              similarity. Candidates whose leaf counts alone rule out beating
              the best so far are skipped, and ties go to the lowest [seq].
              Fuzzy mode uses the same search. *)
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
    set, the divergence that triggered the switch to live, and, when a strict
    refusal as [Reordered] also has the wrong shape, the [Shape_mismatch]. *)

val switched : state -> bool
(** Whether [Until_divergence] has switched the model channel to live. *)

val finish : state -> divergence list
(** [Unconsumed] for every recorded exchange never served, in [seq] order. Model
    exchanges are not reported after a switch to live. *)
