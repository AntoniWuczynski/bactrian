(** The project certificate authority (CA) that egress interception signs its
    leaf certificates with (design §5.2).

    It lives in [.bactrian/ca/]: the directory has mode 0700, the private key
    [ca.key] mode 0600 and the certificate [ca.pem] mode 0644. It is a P-256
    key, valid for one year from a day before it was made, and is replaced with
    [bactrian ca rotate]. The key never leaves the directory and is never
    printed. Leaf certificates are valid for 7 days from 2 days back (an agent
    whose clock is set back still accepts them), live only in memory and are
    cached per host. *)

type t

val key_file : string
(** ["ca.key"]. *)

val certificate_file : string
(** ["ca.pem"]. *)

type origin = Loaded | Created

val load_or_create : dir:string -> now:Ptime.t -> (t * origin, string) result
(** The CA in [dir], made there when the directory holds neither file. [Error]
    when the directory or the key can be read or changed by another user, when
    only one of the two files is there, when they do not decode or do not belong
    together, or when the certificate has expired (the message names
    [bactrian ca rotate]). *)

val rotate : dir:string -> now:Ptime.t -> (t, string) result
(** Replaces the CA in [dir] with a new one, made as {!load_or_create} makes it.
    Agents and leaves trust the new one from the next run on. *)

val certificate_pem : t -> string
(** The CA's certificate, for the agents' trust bundle. *)

val certificate_path : t -> string
(** Where the certificate is. *)

val not_after : t -> Ptime.t
(** When the CA stops being valid. *)

val fingerprint : t -> string
(** The SHA-256 fingerprint of the certificate, in lowercase hex. *)

val leaf :
  t -> now:Ptime.t -> host:string -> (Tls.Config.certchain, string) result
(** A certificate for [host] (a DNS name, or an IP address as its address)
    signed by the CA, with the CA's certificate after it. A host gets one leaf,
    reused while it is valid for at least another day, however many fibres ask
    for it at once. [Error] for a host that is neither a name nor an address. *)
