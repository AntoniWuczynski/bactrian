(** Runs an external checker command over a trace (spec §6).

    The checker gets the trace as JSON on stdin and must print one verdict
    object on stdout (at most 1 MiB). It runs in its own process group with a
    scrubbed environment (spec §5.2): only [BACTRIAN_RUN_ID] (taken from the
    trace's run start, omitted if there is none), and [PATH], [HOME], [LANG] and
    [TZ] when bactrian itself has them. When [run] returns, the whole process
    group has been killed, whatever the outcome. *)

type result =
  | Verdict of Bactrian_core.Checker_protocol.verdict
  | Checker_error of string
      (** Could not start, timed out, exited non-zero, wrote too much, or wrote
          something that is not a verdict. *)

val run :
  process_mgr:_ Eio_unix.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  argv:string list ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  Bactrian_core.Trace.entry list ->
  result
