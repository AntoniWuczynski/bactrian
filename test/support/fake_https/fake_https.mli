(** A TLS front for fake servers in tests. A certificate authority is minted in
    memory for each test (no key is ever written to the repository), and the
    front terminates TLS with a leaf of that CA and relays the plain bytes to a
    server on 127.0.0.1, such as the fake upstream or an HTTP-mode agent. *)

type ca

val ca : unit -> ca
(** A fresh P-256 CA, valid from a day ago for a year. Needs a seeded RNG. *)

val ca_pem : ca -> string
(** The CA certificate in PEM, for [BACTRIAN_EXTRA_CA_CERTS]. *)

val connector : ca -> Bactrian_runtime.Upstream.connector
(** The TLS connector of {!Bactrian_tls.Tls_connector.make}, trusting only this
    CA. *)

type name = Dns of string | Ip of string

val leaf : ca -> name list -> Tls.Config.certchain
(** A server certificate for these names, signed by the CA. *)

type t

(** How the front ends a connection. *)
type ending =
  | Close_notify  (** A TLS close_notify, then the TCP close. *)
  | Tcp_close  (** Only the TCP close, as a cut-off connection would. *)
  | Bad_record
      (** A TLS record that does not decrypt, then the TCP close, so the client
          fails with a TLS error. *)

val start :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  ?record_size:int ->
  ?ending:ending ->
  ?relay:bool ->
  chain:Tls.Config.certchain ->
  backend:int ->
  unit ->
  t
(** Listens on 127.0.0.1 (port 0) until [sw] ends. Each connection is relayed to
    [127.0.0.1:backend]. [record_size] splits what the backend sends into TLS
    records of at most that many bytes (default: one record per read). When the
    backend closes, the front ends the connection as [ending] says (default
    [Close_notify]). With [relay] false the front ends each connection that way
    right after the handshake, before any response. *)

val port : t -> int
