(** [bactrian record]: run an agent against the real providers and record the
    run. *)

type request = {
  common : Single_run.common;
  turns : string list;  (** The scripted user turns, in order. *)
  mcp_stdio : string list;
      (** [NAME=JSON] declarations, see {!Bactrian_runtime.Mcp_flags}. *)
  mcp_http : string list;  (** [NAME=URL] declarations. *)
  mcp_policy : string list;  (** [NAME=POLICY] declarations. *)
}

val validate : request -> (unit, string) result
(** [Error] when there is no turn, a timeout is not positive or an MCP
    declaration is malformed. {!run} checks the same. *)

val run :
  on_progress:(Progress.t -> unit) ->
  bactrian_exe:string ->
  env:Eio_unix.Stdenv.base ->
  request ->
  (Bactrian_runtime.Run.result, Command_error.t) result
(** Records the run (see {!Single_run.execute}, which says what the caller
    provides). An invalid request is a [Usage] error; a missing spend cap, key
    or TLS support is an [Environment] error. *)

val exit_code : Bactrian_runtime.Run.result -> int
(** {!Single_run.exit_code}. *)

val to_json : Bactrian_runtime.Run.result -> Yojson.Safe.t
(** {!Single_run.to_json}. *)
