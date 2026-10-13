(** A recording of one run as JSON Lines: a header line, one line per exchange,
    and an end line written when the run finishes cleanly. A file without its
    end line is a partial recording. *)

val schema_version : int

type channel = Model | Mcp | Http

type payload =
  | Inline of string
  | Blob of string
      (** BLAKE2b-256 hex digest of content stored beside the cassette. *)

type response_body =
  | Whole of payload
  | Stream of { chunks : (int * string) list; complete : bool }
      (** Chunks in arrival order: milliseconds since the previous chunk (or
          since the request for the first), and the chunk's bytes. [complete] is
          false when the stream was cut off (upstream dropped, or the agent went
          away): written as ["complete": false] beside ["stream"], and absent
          when true, so older cassettes decode as complete. *)

type response = {
  status : int;
  headers : (string * string) list;  (** Allowlisted only (spec §7). *)
  body : response_body;
}

type exchange = {
  seq : int;
  t_ms : int;  (** Milliseconds since the run started, at request time. *)
  channel : channel;
  protocol : string;  (** Adapter name, ["mcp"] or ["http"]. *)
  meth : string;
  path : string;
  query : (string * string) list;
      (** Normalised: sorted, volatile keys removed. *)
  key : string;
  occurrence : int;  (** Earlier exchanges with the same key on this channel. *)
  canonical : Canon.t;  (** The canonical request, redacted. *)
  request : payload;  (** The request body as sent, redacted. *)
  outcome : outcome;
}

and outcome =
  | Response of response
  | Transport_error of string
      (** No response, e.g. connection refused upstream. *)

type header = {
  run_id : string;
  scenario : string;
  variant : string;
  bactrian_version : string;
  recorded_at : string;  (** RFC 3339. *)
  sim_now : string;
  clock : Trace.clock;
}

type t = { header : header; exchanges : exchange list; complete : bool }

type error =
  | Invalid_json of { line : int; reason : string }
  | Unsupported_version of int
  | Bad_line of { line : int; reason : string }
  | Missing_header
  | Out_of_order of { line : int }  (** [seq] not strictly increasing. *)

val header_line : header -> string
val exchange_line : exchange -> string
val end_line : string

val of_lines : string list -> (t, error) result
(** Decodes a whole file split into lines (empty trailing lines ignored).
    [complete] is true only when the last line is the end line.

    A last non-empty line (after the header) that is not valid JSON is a write
    cut off by a killed run: it is dropped and [complete] is false. An invalid
    line anywhere else is an error.

    Payloads and stream chunks that are not valid UTF-8 are written as base64 so
    every line is valid JSON: [{"inline_b64": ...}] for a payload and
    [[ms, {"b64": ...}]] for a chunk. *)

val equal_exchange : exchange -> exchange -> bool
val pp_error : Format.formatter -> error -> unit
