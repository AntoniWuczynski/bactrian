(** [bactrian doctor]. *)

val cmd : bactrian_exe:string -> int Cmdliner.Cmd.t
(** [bactrian_exe] is the executable the sandbox probe re-runs. *)
