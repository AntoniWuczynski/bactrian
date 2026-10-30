(** A job as a task of the MCP Tasks extension ([io.modelcontextprotocol/tasks],
    decision record 0021). The task ID is the job ID. A running job is
    [working], a job that produced its result or stopped with an error is
    [completed] with the tool result it would have given ([isError] true for an
    error, which the extension keeps out of [failed]), and a cancelled one is
    [cancelled]. *)

open Bactrian_core

val poll_interval_ms : int
(** The [pollIntervalMs] suggested to clients: 1000. *)

val task : Jobs.job -> Canon.t
(** The [Task] fields: [taskId], [status], [statusMessage], [createdAt],
    [lastUpdatedAt], [ttlMs] ([null] while the job runs, then the time from its
    start until it is forgotten) and [pollIntervalMs]. For a [CreateTaskResult].
*)

val detailed :
  result:(is_error:bool -> Canon.t -> Canon.t) -> Jobs.job -> Canon.t
(** {!task} with the [result] of a completed task: [result ~is_error body] is
    the [CallToolResult] carrying the command's JSON or error. For [tasks/get].
*)
