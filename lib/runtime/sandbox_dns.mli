(** The Linux forwarder's Domain Name System (DNS) listeners (design §5.3). The
    sandbox's network namespace has no route out, so a lookup can only reach a
    resolver on its own loopback: the forwarder listens there on port 53, over
    the User Datagram Protocol (UDP) and TCP, records each query as a violation
    and answers it with [REFUSED] ({!Bactrian_core.Dns_message.refused}). Every
    message is one line, so a query the parser cannot read is recorded with the
    reason. *)

val addresses : Eio.Net.Ipaddr.v4v6 list
(** [127.0.0.1], where a resolver with no [nameserver] line sends its queries,
    and [127.0.0.53], systemd-resolved's stub, which Ubuntu's [/etc/resolv.conf]
    names. *)

val port : int
(** 53. *)

val line :
  transport:[ `Udp | `Tcp ] ->
  (Bactrian_core.Dns_message.question, Bactrian_core.Dns_message.error) result ->
  string
(** [{"kind":"dns","transport":"udp","name":"example.com","type":"A"}], or with
    ["error"] in place of ["name"] and ["type"] (see
    {!Bactrian_core.Sandbox_log.of_relay_line}). *)

val unanswered_nameservers : string -> string list
(** The addresses of the [nameserver] lines of a [resolv.conf]'s text that are
    not among {!addresses}, in order. A text without any gives none: a resolver
    then asks 127.0.0.1. *)

val unrecorded_resolvers : ?path:string -> unit -> string list
(** The resolvers that the agent's own resolver asks and the forwarder does not
    answer, so that a lookup through them fails unrecorded, read from [path]
    ([/etc/resolv.conf]). None when the file is missing or lies under [/run],
    which the sandbox replaces with an empty directory, so that the agent's
    resolver asks 127.0.0.1, otherwise {!unanswered_nameservers} of the file its
    symbolic links lead to. *)

type error = { address : string; reason : string }

val describe_error : error -> string

val serve :
  ?on_bound:([ `Udp | `Tcp ] -> int -> unit) ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  record:(string -> unit) ->
  (Eio.Net.Ipaddr.v4v6 * int) list ->
  (unit, error) result
(** [serve ~sw ~net ~record addresses] binds a UDP socket and a TCP listener on
    each address and port, then answers in daemon fibres of [sw] until it ends,
    giving [record] one {!line} per query. A failed bind is an [error], and the
    sockets already bound close with [sw]. [on_bound] gets each socket's bound
    port, for tests, which ask for port 0. A TCP query is read with its two-byte
    length prefix, and a connection is ended by an incomplete one. *)
