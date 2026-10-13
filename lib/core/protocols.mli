(** The registry of protocol adapters. *)

val all : (module Protocol.S) list
(** Every adapter, in a fixed order. *)

val find : meth:string -> path:string -> (module Protocol.S) option
(** The first adapter whose [matches] accepts the request. The test suite checks
    that at most one adapter accepts any captured request. *)
