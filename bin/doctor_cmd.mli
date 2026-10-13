(** [bactrian doctor]. *)

val cmd : tls_available:bool -> bactrian_exe:string -> int Cmdliner.Cmd.t
(** [tls_available] says whether this build can call HTTPS, and [bactrian_exe]
    is the executable the sandbox probe re-runs. *)
