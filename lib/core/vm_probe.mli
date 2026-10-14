(** The report of the virtual machine sandbox's preflight probe (design §5.3):
    the line [vm/relay.c --probe] writes from inside a container built like a
    run's, and how bactrian judges it. Pure. *)

type outcome = (unit, string) result
(** [Error] carries the error's name, such as ["EROFS"], or ["timeout"]. *)

type report = {
  public : outcome;  (** Connecting to [1.1.1.1:443]. *)
  listener : outcome;
      (** Connecting to bactrian's probe listener through the relay and reading
          its greeting. *)
  outside_write : outcome;  (** Creating a file outside the state directory. *)
  inside_write : outcome;  (** Creating a file inside the state directory. *)
  denied_read : outcome;
      (** Reading a file under a denied directory. [Error "empty"] when it read
          nothing. *)
  masked_read : outcome;  (** Reading a masked file. *)
  masked_case_read : outcome;
      (** Reading the masked file under its name in another case. *)
  root_write : outcome;  (** Creating a file in [/]. *)
  other_run_write : outcome;  (** Creating a file in another run's directory. *)
  interfaces_listed : outcome;  (** Listing [/sys/class/net]. *)
  interfaces : string list;  (** The container's network interfaces. *)
}

val of_line : string -> (report, string) result

val checks :
  listener:outcome ->
  outside_write:outcome ->
  inside_write:outcome ->
  denied_read:outcome ->
  string list
(** The checks every sandbox's probe shares, each wrong result described: the
    listener and the write inside must work, the write outside and the denied
    read must fail. *)

val judge : report -> (unit, string list) result
(** Every check with the wrong result, described. The interfaces must have been
    listed and be [lo] alone, and the public connection must fail at once rather
    than time out, which would mean a route that drops packets. Reads of the
    masked file under either name and the write into another run must fail with
    [ENOENT], the writes to [/] and outside the state directory with [EROFS]:
    another error is a surprise, so it fails the probe too. *)
