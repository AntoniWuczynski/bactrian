(** An HTTP-mode agent server for tests (design §5.1). It keeps one session per
    run ID, taken from the [X-Bactrian-Run] header, and holds the [env] of the
    run's [start] message. On a turn it makes one model call through that
    [env]'s [OPENAI_BASE_URL] with its [OPENAI_API_KEY], and replies with the
    call's status. *)

type session = {
  run_id : string;
  namespace : string option;  (** The namespace header's value. *)
  env : (string * string) list;
  start_body : string;  (** The request body of the [start] message. *)
  mutable model_status : int option;  (** The status of its model call. *)
}

type t

val start :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  namespace_header:string ->
  start_delay:float ->
  model_request:string ->
  t
(** [start_delay] seconds pass before the answer to each [start], so that
    concurrent runs overlap. [model_request] is the body of the model call. *)

val url : t -> string

val sessions : t -> session list
(** In the order the runs started. *)
