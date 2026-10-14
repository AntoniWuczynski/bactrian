(** What the sandbox denied an agent, as {!Sandbox.collect} reads it: the
    Seatbelt denials from the system log on macOS, and the relay failures that
    the Linux forwarder or the virtual machine's bridge wrote to [sandbox.log].
    {!Sandbox} re-exports the types and documents them. *)

type coverage = Seatbelt_log | Relay_failures_only

val coverage_name : coverage -> string
val coverage_of_name : string -> coverage option

type violations =
  | Collected of {
      items : Bactrian_core.Sandbox_log.violation list;
      coverage : coverage;
      gaps : string list;
    }
  | Not_collected of string

val unsandboxed_violations : violations
val violations_json : violations -> Yojson.Safe.t
val describe_violations : violations -> string

val max_log : int
(** 16 MiB: what [sandbox.log] takes before further lines are dropped. *)

val drain : Eio_unix.source_ty Eio.Resource.t -> Unix.file_descr -> bool
(** [drain reader fd] copies the lines of the log's pipe to [fd] until every
    write end is closed, keeping at most {!max_log} bytes. Whether any were
    dropped. *)

val seatbelt :
  env:Eio_unix.Stdenv.base ->
  tag:string ->
  since:float ->
  marker:string ->
  violations
(** The Seatbelt denials logged with [tag] since [since], read with [log show]
    after an end marker is logged by touching [marker] in a profile that denies
    it (see {!Sandbox.collect}). *)

val relay :
  env:Eio_unix.Stdenv.base ->
  path:string ->
  writer:Eio_unix.sink_ty Eio.Resource.t ->
  drained:bool Eio.Promise.or_exn ->
  violations
(** Closes bactrian's [writer] of the log's pipe, waits up to 5 seconds for the
    [drained] copy to end and reads the relay failures in [path]. *)
