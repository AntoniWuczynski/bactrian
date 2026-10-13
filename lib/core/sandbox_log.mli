(** Sandbox violations as the operating system or the forwarder reports them
    (design §5.3). Pure: the caller reads the logs. *)

type violation = { kind : string; detail : string }
(** [kind] is the denied operation ([network-outbound], [file-write-create],
    [file-read-data], ...) or [relay_failed]. [detail] names the target and, on
    macOS, the process. *)

val tag : string -> string
(** [tag id] is ["bactrian-sandbox:" ^ id], with every character of [id] outside
    letters, digits, ['-'] and ['_'] written as ['_']: the text the Seatbelt
    profile appends to each denial of the run [id]. *)

val end_tag : string -> string
(** [end_tag tag] is [tag ^ "-end"]: the tag of the marker denial bactrian
    causes after the agent exits. The system log takes a few seconds to show a
    denial, so the run's denials are read once the marker is there. *)

val predicate : tag:string -> string
(** The [log show --predicate] that selects the denials whose message contains
    [tag], which includes those tagged with {!end_tag}. [tag] comes from {!tag},
    so it needs no quoting. *)

val of_seatbelt_message : tag:string -> string -> violation option
(** One unified-log message, such as
    ["Sandbox: node(812) deny(1) network-outbound remote:*:443\n<tag>"], or the
    same after ["2 duplicate reports for "]. [None] for a message that does not
    end with a newline and [tag], or is not a denial. *)

val of_seatbelt_ndjson : tag:string -> string -> violation option
(** One line of [log show --style ndjson]: its [eventMessage], through
    {!of_seatbelt_message}. The closing summary line and lines that are not JSON
    give [None]. *)

val settle :
  tag:string ->
  read:(unit -> string list) ->
  wait:(float -> unit) ->
  now:(unit -> float) ->
  deadline:float ->
  violation list option
(** The run's denials from the system log. [read] gives the lines of one
    [log show --style ndjson] call, [wait s] sleeps [s] seconds and [now] is the
    time in seconds. Reads every 0.5 seconds until a read holds the {!end_tag}
    marker. The marker does not mean that other processes' earlier denials have
    reached the log, so it then reads again every second until two reads in a
    row give the same violations. [None] if the marker is not there by
    [deadline]. Once it was seen, the last violations read when the next read
    would pass [deadline]. *)

val of_relay_line : string -> (violation, string) result
(** One line of the Linux forwarder's log:
    [{"kind":"relay_failed","port":P,"socket":"S","reason":"R"}]. *)
