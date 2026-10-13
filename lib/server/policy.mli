(** What the user allows the MCP server to do, set on the command line that
    registers it and never by a tool argument or an experiment file (decision
    record 0011), and the checks that hold the server's work to it. Every
    refusal is a message that names the flag that would allow the action. *)

type t = {
  allow_unsandboxed : bool;
      (** [--allow-unsandboxed]: jobs may run their agent outside the sandbox
          ([sandbox: auto] or [off], an HTTP-mode agent). Without it every job
          is [strict]. *)
  trust_project : bool;
      (** [--trust-project]: the project's files may run commands and reach
          outside the sandbox (checkers, MCP servers, passed environment
          variables, writable paths outside the root). *)
  max_jobs : int;
      (** The most jobs ([run_start], [record_start], [replay_start]) running at
          once ([--max-jobs]). *)
}

val default : t
(** Neither flag, at most 4 jobs. *)

val sandbox :
  t ->
  Bactrian_runtime.Sandbox.mode option ->
  (Bactrian_runtime.Sandbox.mode option, string) result
(** The sandbox mode a job runs in, given the [sandbox] argument: [strict]
    unless {!field-allow_unsandboxed}, which leaves the argument as it is. An
    experiment with an HTTP-mode agent cannot meet [strict], which loading it
    reports. *)

val env : t -> string list -> (unit, string) result
(** Refuses a non-empty [env] argument unless {!field-trust_project}. *)

val mcp : t -> stdio:string list -> http:string list -> (unit, string) result
(** Refuses [mcp_stdio] and [mcp_http] declarations unless
    {!field-trust_project}. *)

val scenario : t -> Bactrian_core.Scenario.t -> (unit, string) result
(** Refuses a scenario with checkers unless {!field-trust_project}, for [check].
*)

val experiment :
  t -> Confine.t -> Bactrian_runtime.Executor.contents -> (unit, string) result
(** Refuses an experiment loaded for [run_start] that the policy does not allow:
    [env_passthrough], MCP servers, checkers in its scenarios, a
    [sandbox.allow_write] path outside the root. Whatever the policy, it refuses
    an MCP stdio server's [cwd] or a scenario's [fixtures] outside the root. The
    error names the key. *)

val describe : t -> string
(** The policy in a few sentences, for the server's instructions. *)
