(** Sandbox violations as the operating system or the forwarder reports them
    (design §5.3). Pure: the caller reads the logs. *)

type violation = { kind : string; detail : string }
(** [kind] is the denied operation ([network-outbound], [file-write-create],
    [file-read-data], ...), [relay_failed], [dns], or [unparsed] for a tagged
    message in which no known Seatbelt operation follows ["(PID) deny(N) "] (its
    [detail] is then the whole message). [detail] names the target and, on
    macOS, the process. The process name and the target are the agent's to
    choose, so the split is a best reading of text that can be ambiguous. *)

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
    end with a newline and [tag], or has no ["Sandbox: "]. *)

val of_seatbelt_ndjson : tag:string -> string -> violation option
(** One line of [log show --style ndjson]: its [eventMessage], through
    {!of_seatbelt_message}. The closing summary line and lines that are not JSON
    give [None]. *)

type settled =
  | Settled of violation list  (** Two reads in a row agreed. *)
  | Unsettled of violation list
      (** The last read before [deadline], which still differed from the one
          before it. *)
  | No_marker  (** The marker was not there by [deadline]. *)

val settle :
  tag:string ->
  read:(unit -> string list) ->
  wait:(float -> unit) ->
  now:(unit -> float) ->
  mark:(unit -> unit) ->
  deadline:float ->
  settled
(** The run's denials from the system log. [read] gives the lines of one
    [log show --style ndjson] call, [wait s] sleeps [s] seconds and [now] is the
    time in seconds. Reads every 0.5 seconds until a read holds the {!end_tag}
    marker, which the caller has logged once, and calls [mark] to log it again
    every 5 seconds that it is missing, because the log can drop it as it drops
    any denial. The marker does not mean that other processes' earlier denials
    have reached the log, so it then reads again every second until two reads in
    a row give the same violations. Neither shows denials that the log dropped,
    which it does under load, so even [Settled] can miss some. *)

val of_relay_line : string -> (violation, string) result
(** One line of the Linux forwarder's log: a relay failure,
    [{"kind":"relay_failed","port":P,"socket":"S","reason":"R"}], or a DNS
    lookup, [{"kind":"dns","transport":"udp","name":"N","type":"T"}] with the
    name as {!Dns_message.parse} renders it, or with ["error"] in place of
    ["name"] and ["type"] for a query it could not read. *)

type relay_log = {
  items : violation list;  (** The first [max_items], in order. *)
  unreadable : int;  (** Non-empty lines {!of_relay_line} could not read. *)
  not_kept : int;  (** Readable lines after the first [max_items]. *)
}

val max_items : int
(** 1000: an agent can send DNS queries in a loop, and each would otherwise be
    an item of [meta.json] and an event of the trace. *)

val of_relay_log : ?max_items:int -> string -> relay_log
(** The violations of a whole forwarder log, one per line that {!of_relay_line}
    reads, up to [max_items] ({!max_items}). One bad line loses only itself. *)
