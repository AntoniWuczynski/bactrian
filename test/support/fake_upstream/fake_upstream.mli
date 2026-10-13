(** A fake model provider for tests. It listens on 127.0.0.1 and answers either
    by replaying recorded fixtures or by following a script, so proxy tests
    never reach a real provider. *)

type ending =
  | Complete  (** The body ends cleanly. *)
  | Drop
      (** The connection is dropped after the last chunk, without the
          terminating chunk, so the client sees a truncated body. *)

type reply = {
  status : int;
  headers : (string * string) list;
  chunks : string list;  (** Body pieces, each written as its own HTTP chunk. *)
  ending : ending;
}

type request = {
  meth : string;
  path : string;
  query : string;  (** Without the leading ['?']; empty when absent. *)
  headers : (string * string) list;  (** Names lower-cased. *)
  body : string;
}

type source =
  | Fixtures of string
      (** A directory laid out as [test/fixtures/protocols]:
          [<protocol>/<case>/{meta.json,request.json,response.raw,chunks.json}].
          A request is answered by the first case (in name order) whose
          [meta.json] method, path and query equal the request's and whose
          [request.json] equals the request body after canonicalisation. The
          reply has the recorded status and [content-type], and the body of
          [response.raw] split at the recorded chunk boundaries. A request that
          matches no case gets a 404, a body that is not JSON gets a 400. *)
  | Script of reply list
      (** The n-th request gets the n-th reply, whatever it says. Requests past
          the end get a 500. *)
  | Handler of (request -> reply)
      (** Every request gets the reply the function gives. It runs in the
          connection's fibre, so it may wait on the Eio clock. *)

type error =
  | Unreadable of { path : string; reason : string }
  | Invalid_fixture of { case : string; reason : string }

val pp_error : Format.formatter -> error -> unit

val case_reply : string -> (reply, error) result
(** The reply recorded in one fixture case directory ([meta.json] and
    [response.raw] split at [chunks.json]'s boundaries), as {!Fixtures} would
    serve it. *)

type t

val start : sw:Eio.Switch.t -> net:_ Eio.Net.t -> source -> (t, error) result
(** Fixtures are loaded eagerly. The server runs until [sw] ends. *)

val port : t -> int

val base_url : t -> string
(** ["http://127.0.0.1:<port>"], without a trailing slash. *)

val requests : t -> request list
(** Every request received so far, oldest first, including unmatched ones. *)
