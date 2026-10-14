(** One agent run, as [record] and [replay] do it: the spend caps, the sandbox
    preflight, the model proxy and {!Bactrian_runtime.Run.execute}. *)

open Bactrian_runtime

type sandbox = {
  mode : Sandbox.mode;
  backend : Sandbox.backend;
  image : string option;  (** Required for [`Vm]. *)
  seatbelt_hint : bool;
      (** Whether a sandbox message may suggest [backend: seatbelt] (see
          {!Bactrian_runtime.Sandbox.preflight}): on the command line, not in
          the MCP server. *)
}
(** The sandbox of a command-line run (design §5.3). *)

type common = {
  agent : string list;  (** The agent's command and arguments. *)
  cwd : string;  (** Working directory of the agent. *)
  scenario : string;  (** Scenario name recorded with the run. *)
  variant : string;  (** Variant name recorded with the run. *)
  sim_now : string;
      (** RFC 3339 simulated time; [""] is now, or in a replay the recording's.
      *)
  timezone : string;  (** Value of [TZ] for the agent. *)
  env_passthrough : string list;
      (** Variables passed from bactrian's environment to the agent. *)
  turn_timeout : float;  (** Seconds. *)
  total_timeout : float;  (** Seconds. *)
  sandbox : sandbox;
}
(** The arguments [record] and [replay] share. *)

val defaults : common
(** What a run gets for an argument it is not given: no agent, the current
    directory, scenario [adhoc], variant [default], [""] for the time, [UTC], no
    passthrough variable, timeouts of 120 seconds per turn and 600 in total, and
    sandbox mode and backend [auto] with no image. *)

val positive_timeouts : common -> (unit, string) result
(** [Error] unless both timeouts are finite and positive. *)

val config :
  common ->
  turns:string list ->
  replay:Run.replay option ->
  mcp:Mcp_proxy.config list ->
  Sandbox.t ->
  Run.config
(** The run configuration of a command-line run: no checks, fixtures, pricing
    other than the default table, or variant environment. *)

val execute :
  ?stop:Interrupt.trigger ->
  on_progress:(Progress.t -> unit) ->
  bactrian_exe:string ->
  env:Eio_unix.Stdenv.base ->
  live:bool ->
  sandbox:sandbox ->
  (Store.t -> Sandbox.t -> (Run.config, string) result) ->
  (Run.result, Command_error.t) result
(** Reads the spend caps from [BACTRIAN_CAP_<PROVIDER>] and the upstreams from
    [BACTRIAN_UPSTREAM_<PROVIDER>] ({!Upstream_env}; an invalid one is [Usage]),
    and when [live] checks that a provider has a cap above 0, its key and, if
    its upstream is an [https] URL, TLS support, then opens the ledger and the
    store under [./.bactrian], preflights the [sandbox] (a backend this host
    lacks, or [vm] without an image, is [Usage]), starts the model proxy and
    runs the configuration the function builds from the store and the
    preflight's decision. [bactrian_exe] is the executable the sandbox re-runs
    as its probe.

    Seeds the random generator, and must be called inside [Eio_main.run]. A
    command that starts agents needs a launch environment free of secrets, so
    the caller runs {!Bactrian_runtime.Launch_env.scrub} first. SIGINT and
    SIGTERM, or [stop] when given, cancel the run and give [Interrupted] (see
    {!Bactrian_runtime.Interrupt.run}); [on_progress] gets a {!Progress.Warning}
    for a sandbox that does not apply and for MCP servers the sandbox does not
    cover. A run that fails or diverges is [Ok]: look at its outcome. *)

val exit_code : Run.result -> int
(** {!Exit_code.of_outcome} of the run's outcome. *)

val to_json : Run.result -> Yojson.Safe.t
(** The run ID, directory, outcome with its reason, divergences with their kinds
    and nearest-exchange diff paths, findings, spend, the sandbox label, the
    clock label and the sandbox violations. *)
