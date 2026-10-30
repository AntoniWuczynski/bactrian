(** A TLS front for fake servers in tests. A certificate authority is minted in
    memory for each test (no key is ever written to the repository), and the
    front terminates TLS with a leaf of that CA and relays the plain bytes to a
    server on 127.0.0.1, such as the fake upstream or an HTTP-mode agent. *)

type ca

val ca : unit -> ca
(** A fresh P-256 CA, valid from a day ago for a year. Needs a seeded RNG. *)

val ca_pem : ca -> string
(** The CA certificate in PEM, for [BACTRIAN_EXTRA_CA_CERTS]. *)

val authenticator : ca -> X509.Authenticator.t
(** Trusts only this CA. *)

type name = Dns of string | Ip of string

val leaf : ca -> name list -> Tls.Config.certchain
(** A server certificate for these names, signed by the CA. *)

type t

val start :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  ?record_size:int ->
  ?close_notify:bool ->
  chain:Tls.Config.certchain ->
  backend:int ->
  unit ->
  t
(** Listens on 127.0.0.1 (port 0) until [sw] ends. Each connection is relayed to
    [127.0.0.1:backend]. [record_size] splits what the backend sends into TLS
    records of at most that many bytes (default: one record per read). When the
    backend closes, the front sends a TLS close_notify first if [close_notify]
    (the default), and otherwise only closes the TCP connection, as a cut-off
    connection would. *)

val port : t -> int
