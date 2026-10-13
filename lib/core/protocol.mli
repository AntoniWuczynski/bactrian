(** Provider wire protocols. An adapter recognises its requests, reduces a
    request to the canonical form used as its replay key, and summarises a
    response for the trace and for cost accounting. Replay sends recorded bytes
    back unchanged, so adapters never re-serialise responses. *)

type tool_call = {
  name : string;
  arguments : Canon.t;  (** Parsed arguments. Provider call IDs are dropped. *)
}

type tool_result = {
  call_index : int;
      (** Position of the answered call among all tool calls in the request
          history, counting from 0. *)
  content : string;
  is_error : bool;
}

type summary = {
  model : string option;  (** As reported by the provider. *)
  text : string;  (** All assistant text, concatenated in order. *)
  tool_calls : tool_call list;  (** In the order the model made them. *)
  usage : Trace.usage option;
  stop_reason : string option;  (** The provider's own value, unchanged. *)
  error : string option;
      (** A provider error, including an error frame inside a 200 stream. *)
}

type body =
  | Json of Canon.t  (** A whole JSON response. *)
  | Sse of Sse.item list  (** A Server-Sent Events stream. *)
  | Json_stream of Canon.t list
      (** A streamed JSON array, one element per item (Gemini without
          [alt=sse]). *)

type error = Malformed of { path : string; reason : string }

module type S = sig
  val name : string
  (** Stable identifier recorded in cassettes, e.g. ["openai_chat"]. *)

  val matches : meth:string -> path:string -> bool
  (** [path] is relative to the provider's base, without the query string, e.g.
      ["/v1/chat/completions"]. *)

  val ignored_fields : (string * string) list
  (** Request fields removed from the key, as (dotted path, reason). *)

  val canonical_request : path:string -> Canon.t -> (Canon.t, error) result
  (** The replay key's source: ignored fields removed, tool-call and item IDs
      replaced by positions, Anthropic thinking signatures removed. Includes the
      model even when the protocol puts it in the path. *)

  val streaming : path:string -> query:string -> Canon.t -> bool
  (** Whether the response will be streamed, and so how to read it. *)

  val summarise : body -> (summary, error) result

  val tool_results : Canon.t -> tool_result list
  (** Tool results carried in the request history, in order. *)
end

val pp_error : Format.formatter -> error -> unit

(** {1 Field readers}

    [None] when the field is absent or has another shape. *)

val str_member : string -> Canon.t -> string option
val int_member : string -> Canon.t -> int option
val bool_member : string -> Canon.t -> bool option
val list_member : string -> Canon.t -> Canon.t list option
