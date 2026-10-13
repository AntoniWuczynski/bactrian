(** What the acceptance tests share: the real example agents, a world with a
    fake upstream and a store, and readers of what a run left behind. *)

open Bactrian_runtime
module F = Fake_upstream

val prompt_variable : string
(** The variable the example agents read their system prompt from. *)

val first_turn : string

val in_repo : string -> string
(** A path under the repository root. *)

val require : string -> unit
(** Fails the test unless the tool runs. *)

type agent = { name : string; argv : unit -> string list }

val agents : agent list
(** The TypeScript and the Python example agent. *)

val read : string -> string
val lines : string -> string list
val trace : Run.result -> Yojson.Safe.t list

val entries : string -> Run.result -> Yojson.Safe.t list
(** The trace entries of one type. *)

val outcome_name : Run.result -> string

val real_key : string
(** The provider key bactrian holds. No agent may ever see it. *)

val executable : string -> string
(** The absolute path an environment variable of the dune rule names. *)

val sandbox_available : bool
val sandbox_flag : string
val check_sandboxed : Run.result -> unit

type world = {
  env : Eio_unix.Stdenv.base;
  up : F.t;
  dir : string;  (** The world's directory, with its [.bactrian] store. *)
  execute :
    agent:Run.agent ->
    ?replay:Run.replay ->
    ?mcp:Mcp_proxy.config list ->
    ?variant_env:(string * string) list ->
    unit ->
    Run.result;
}

val with_world :
  ?deny_read:string list -> ?rounds:int -> string -> (world -> 'a) -> 'a
(** [deny_read] names directories to create in the world and deny to the agent.
    The fake upstream answers [rounds] (default 1) times the two replies of the
    weather conversation. *)

val strict : string -> Run.replay
val contains : string -> string -> bool
