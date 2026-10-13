(** Where a command runs its CPU-bound part: the comparison and its simulated
    extra runs for [compare] and [report], the power simulation for [plan]. That
    part is pure, and it calls [check] between simulated experiments. *)

type t = { run : 'a. (check:(unit -> unit) -> 'a) -> 'a }

val inline : t
(** In the calling fibre, with a [check] that does nothing: the command line,
    where nothing else waits. *)

val in_domain : _ Eio.Domain_manager.t -> t
(** In a new domain, so that the calling domain's other fibres keep running. The
    caller waits for the result, and when it is cancelled the work stops at its
    next [check], which raises [Eio.Cancel.Cancelled]. The work must touch
    nothing that another domain uses. *)
