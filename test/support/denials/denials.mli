(** Checks on the Seatbelt denials a sandboxed run collected. *)

val check :
  Bactrian_core.Sandbox_log.violation list ->
  (string * string * string) list ->
  unit
(** [check collected expected] prints every collected denial, then each
    [(name, kind, needle)] of [expected] that no denial of that kind with
    [needle] in its detail matches. The system log can drop denials, so only
    finding none of [expected] fails the test. *)
