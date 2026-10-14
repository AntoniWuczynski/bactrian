(** The checks the acceptance tests share, run against any sandbox. *)

val cycle : Acceptance_world.world -> string list -> unit
(** Records a run of the agent [argv] in the world, replays it strictly with no
    divergence, then with a changed system prompt, which must diverge at the
    system message. *)

val escapes : ?vm:string -> name:string -> (dir:string -> string list) -> unit
(** Runs an escape agent (test/sandbox), whose command is [argv_of ~dir] for the
    world's directory, in a world of its own ({!Acceptance_world.with_world}
    with [vm]): its one model call works, and every attempt to get out fails,
    leaves nothing behind and never sees the real key. *)
