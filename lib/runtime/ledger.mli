(** Spend per provider against caps (design §5.3, §11).

    Caps are enforced by reservation: the worst-case cost of a request is
    reserved before it is forwarded, and the cap is checked against settled
    spend plus every reservation still outstanding. A reservation that is never
    settled, or is settled with [actual:None], stays charged in full (a usage
    gap), so the cap cannot be exceeded.

    The ledger is an append-only JSONL file, written as events:
    - ["event":"reserve"] with ["reserved_usd"] and the ["owner"] (the run that
      asked) when a reservation is made,
    - ["event":"settle"] or ["event":"gap"] with ["usd"] when it ends.

    On {!create} the file is replayed: a line without an ["event"] (the format
    the capture script wrote) counts as settled spend of its ["usd"], a
    reservation with no settle line counts at its reserved amount. All
    operations are safe to call from many fibres. *)

type t
type reservation

type refusal =
  | Cap_reached of {
      provider : string;
      cap : float;
      spent : float;  (** Settled spend plus outstanding reservations. *)
      requested : float;
    }
  | Unknown_model of string
      (** Never returned by the ledger itself, which does not price models.
          Callers that price a request and find no price use it to refuse. *)

exception Corrupt of { line : int; reason : string }
(** Raised by {!create} when the existing file has a line that cannot be read.
    Skipping it would undercount spend, so the ledger refuses to start. *)

val create : path:Eio.Fs.dir_ty Eio.Path.t -> caps:(string * float) list -> t
(** Opens (creating its directory if needed) the ledger file at [path] and
    reloads spent totals from it. A provider with no entry in [caps] has a cap
    of zero. Raises {!Corrupt}, or [Eio.Io] on filesystem failure. *)

val reserve :
  t ->
  owner:string ->
  provider:string ->
  model:string ->
  usd:float ->
  (reservation, refusal) result
(** Charges [usd] against the provider's cap and attributes it to [owner] (a run
    ID), whose share {!spent_by} reports, or refuses with [Cap_reached] (also
    for a negative or non-finite [usd]). Raises [Eio.Io] if the event cannot be
    written, in which case nothing is charged and the ledger is disabled (later
    calls raise [Eio.Mutex.Poisoned]). *)

val settle : t -> reservation -> actual:float option -> unit
(** Replaces the reservation by [actual]. [None] (or a negative or non-finite
    value) keeps the full reservation as a usage gap. Settling twice is a no-op.
    Raises [Eio.Io] if the event cannot be written, in which case the
    reservation stays charged and the ledger is disabled as for {!reserve}. *)

val spent : t -> provider:string -> float
(** Settled spend plus outstanding reservations. *)

val spent_by : t -> owner:string -> float
(** Spend attributed to [owner] across every provider: what its reservations
    settled at, plus the full reservation of each usage gap, plus reservations
    still outstanding. It is exactly this owner's share of {!spent}, whatever
    other owners do concurrently. Reloading the file restores it, and spend
    written before owners existed has the owner [""]. *)
