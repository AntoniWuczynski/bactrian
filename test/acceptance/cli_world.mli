(** What the tests that run the real binary share: files and directories, the
    command line with its JSON read back, processes by PID, the example
    experiment, and a fake upstream in a process of its own. *)

open Bactrian_core

val exe : unit -> string
(** The absolute path of [bactrian]. *)

(** {2 JSON} *)

val obj : (string * Canon.t) list -> Canon.t
val strings : string list -> Canon.t
val json : Canon.t Alcotest.testable
val canon_of_file : string -> Canon.t
val get : string -> 'a option -> 'a
val field : string -> Canon.t -> Canon.t
val string_field : string -> Canon.t -> string

(** {2 Directories} *)

val remove_tree : string -> unit
val write : string -> string -> unit

val with_project : string -> (parent:string -> project:string -> 'a) -> 'a
(** An empty project in a directory of its own, with a sibling outside it. *)

(** {2 The command line} *)

val key_env : (string * string) list
(** The provider key bactrian holds. *)

val cli :
  dir:string -> env:(string * string) list -> string list -> int * Canon.t
(** Runs [bactrian] with these arguments in [dir] and these variables added to
    the test's environment. Returns the exit status and what it printed on
    standard output, which must be JSON. Standard error is dropped. *)

(** {2 Processes} *)

val alive : int -> bool
val wait_until : what:string -> seconds:float -> (unit -> bool) -> unit
val read_pid : string -> int
val wait_gone : string -> int -> unit

(** {2 The example experiment} *)

val scenario_text : string
(** The weather conversation as a scenario. *)

val terse : string
(** The system prompt of the [terse] variant. *)

val experiment_text : ?extra:string -> ?model:string -> string list -> string
(** The experiment for an agent command: scenarios [scenarios/*.json], variants
    [base] and [terse], two repeats, a cap of 100 USD on OpenAI and no sandbox.
    [extra] is more members of the object, starting with a comma. [model] is the
    [AGENT_MODEL] of both variants. *)

val write_experiment :
  ?extra:string -> ?model:string -> string -> string list -> unit
(** Writes the scenario under [scenarios/] and the experiment as [exp.json] in
    the directory. *)

(** {2 A fake upstream process} *)

type upstream

val with_upstream : ?hold:string -> dir:string -> (upstream -> 'a) -> 'a
(** Starts the fake upstream server, which answers the weather conversation to
    any model, for the duration of the function. While the file [hold] exists it
    answers no request. The requests it receives are logged in [dir]. *)

val upstream_url : upstream -> string
(** [http://127.0.0.1:<port>]. *)

val upstream_requests : upstream -> string list
(** The path of every request received so far, oldest first. *)
