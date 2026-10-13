(** Runs an external checker command over a trace (design §6).

    The checker gets the trace as JSON on stdin and must print one verdict
    object on stdout (at most 1 MiB). It runs in its own process group with a
    scrubbed environment (design §5.2): only [BACTRIAN_RUN_ID] (taken from the
    trace's run start, omitted if there is none), [HOME] and [TMPDIR] set to a
    fresh temporary directory of its own, removed when it ends, and [PATH],
    [LANG] and [TZ] when bactrian itself has them. The group is killed as soon
    as its leader exits, so a background child holding stdout open cannot hold
    up the read, and in any case by the time [run] returns. *)

type result =
  | Verdict of Bactrian_core.Checker_protocol.verdict
  | Checker_error of string
      (** Could not start, timed out, exited non-zero, wrote too much, or wrote
          something that is not a verdict. *)

val run :
  process_mgr:_ Eio_unix.Process.mgr ->
  clock:_ Eio.Time.clock ->
  fs:_ Eio.Path.t ->
  sandbox:Sandbox.t ->
  timeout:float ->
  argv:string list ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  Bactrian_core.Trace.entry list ->
  result * string
(** The result and the checker's stderr: its last 64 KiB, never shown on
    bactrian's own terminal. A [Checker_error] ends with the last line of stderr
    that has text, when there is one. The checker runs in [sandbox]
    ({!Sandbox.checker}), so what the sandbox denies it shows only as its own
    error. [fs] is the whole file system, through which the temporary directory
    is removed. *)
