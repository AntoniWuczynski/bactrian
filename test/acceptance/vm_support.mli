(** What the virtual machine acceptance tests share: the images, the gate that
    skips a test without a container runtime, and the check that no container is
    left. *)

val node_image : string
(** Node 24 on Alpine 3.24 (musl), pinned by digest. *)

val python_image : string
(** uv with Python 3.12 on Debian trixie (glibc), pinned by digest. *)

val quiet : string -> bool
(** Whether the shell command succeeds, its output dropped. *)

val macos : bool
val runtime : string
val image_present : string -> bool
val pull : string -> bool
val run_prefix : string list
val remove_command : string -> string
val required : bool

val gate : string list -> unit
(** Skips the test without a working runtime (fails it when
    [BACTRIAN_REQUIRE_VM] is 1) and pulls the images that are not present. *)

val run_or_fail : string -> unit
val copy : into:string -> string list -> unit

val container_exists : string -> bool
(** Whether the runtime has a container of that name. *)

val no_container : Bactrian_runtime.Run.result -> unit
(** Fails unless the run's container is gone. *)

val probed :
  ?allow_read:string list ->
  string ->
  files:(Acceptance_world.world -> (string * string) list) ->
  (Acceptance_world.world -> string list) ->
  Acceptance_world.world * (string * string) list
(** [probed name ~files probes] runs a Python agent in the Python image that
    reads each of [probes] (a path, or [w:PATH] to try to create it) and answers
    with what each gave ([ENOENT], [EROFS], the file's text, ...). The [files]
    are written into the world's directory first. Gives the world and the
    answers in order. *)

val sim_now_text : string
val sim_now_epoch : int

val clock_python : string
(** A Python agent that answers with its clock, a child's, and how long a
    [time.sleep 0.2] took. *)

val clock_node : string

val clocked :
  image:string ->
  name:string ->
  files:(string * string * int) list ->
  (Acceptance_world.world -> string list) ->
  Yojson.Safe.t
(** Runs the agent (the files are written into the world's directory as
    [(name, text, mode)]) with [sim_now_text] and checks that the clock is
    labelled enforced and that the agent and the program it starts read
    [sim_now_text]. Gives the agent's reply. *)
