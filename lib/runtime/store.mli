(** Run directories, cassettes and content-addressed blobs under a project's
    [.bactrian/] directory (design §4, §7).

    {v
    <root>/runs/<run-id>/cassette.jsonl
    <root>/blobs/blake2b-256/<64 hex digits>
    v}

    Filesystem failures raise [Eio.Io]. *)

open Bactrian_core

type t

type error =
  | Invalid_id of string
      (** A run ID that is empty, ["."], [".."], or contains ['/'] or a NUL. *)
  | Invalid_digest of string  (** Not 64 lowercase hex digits. *)
  | No_such_cassette of string
  | No_such_blob of string
  | Blob_corrupt of string  (** The stored bytes do not hash to the name. *)
  | Bad_cassette of Cassette.error

val create : root:Eio.Fs.dir_ty Eio.Path.t -> t
(** [root] is the [.bactrian] directory. Nothing is created until first use. *)

val pp_error : Format.formatter -> error -> unit

(** {1 Atomic writes} *)

val write_atomic :
  ?before_rename:(unit -> unit) -> _ Eio.Path.t -> string -> unit
(** Writes to a uniquely named temporary file in the same directory, syncs it,
    then renames it over the target, creating missing parent directories. A
    reader sees the old content or the new content, never a mix. [before_rename]
    is a test hook called once the temporary file is complete. If it raises, the
    write is abandoned as in a crash: the target is untouched and the temporary
    file is left behind. A write that fails with [Eio.Io] removes its temporary
    file. Raises [Invalid_argument] for a path with no parent. *)

(** {1 Run directories} *)

val run_dir : t -> run_id:string -> (Eio.Fs.dir_ty Eio.Path.t, error) result
(** The directory for a run. It is not created here. *)

val ensure_run_dir :
  t -> run_id:string -> (Eio.Fs.dir_ty Eio.Path.t, error) result
(** Like {!run_dir}, creating the directory (mode 0o700) if needed. *)

(** {1 Cassettes} *)

type writer
(** An append-only cassette file. Every line is written to the file before
    {!append} returns, so a crash leaves a decodable partial recording. Safe to
    share between fibres: lines are written whole, in call order. The caller
    assigns [seq] and must append in increasing order. *)

val open_cassette :
  sw:Eio.Switch.t -> t -> Cassette.header -> (writer, error) result
(** Creates [runs/<header.run_id>/cassette.jsonl] with the header line. [Eio.Io]
    if the file exists. If the switch ends before {!close}, the file is closed
    without its end line and stays a partial recording. *)

val append : writer -> Cassette.exchange -> unit
(** Raises [Invalid_argument] after {!close}. *)

val close : writer -> unit
(** Writes the end line and closes the file. Idempotent. *)

val cassette_path :
  t -> run_id:string -> (Eio.Fs.dir_ty Eio.Path.t, error) result

val read_cassette : t -> run_id:string -> (Cassette.t, error) result

(** {1 Blobs} *)

val inline_limit : int
(** 65536 bytes. Payloads above it go to blobs (design §7). *)

val put_blob : t -> string -> string
(** Stores the bytes unless already present and returns their BLAKE2b-256 digest
    as 64 lowercase hex characters. *)

val get_blob : t -> string -> (string, error) result
(** Reads and verifies a blob by digest. *)

val payload : t -> string -> Cassette.payload
(** [Inline] up to {!inline_limit} bytes, otherwise [Blob] after {!put_blob}. *)

val read_payload : t -> Cassette.payload -> (string, error) result
