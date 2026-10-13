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
      replaced by positions (a result gets the index {!tool_results} reports for
      it, or null when it answers no call), opaque per-run reasoning blobs
      removed from history items. Includes the model even when the protocol puts
      it in the path. The query string and headers are not seen here: they join
      the key in [Matcher.key]. *)

  val streaming : path:string -> query:string -> Canon.t -> bool
  (** Whether the response will be streamed, and so how to read it. *)

  val summarise : body -> (summary, error) result

  val tool_results : Canon.t -> tool_result list
  (** Tool results carried in the request history, in order, linked to their
      calls by {!Protocol.link_history}. A result that answers no call is left
      out. *)
end

val pp_error : Format.formatter -> error -> unit

(** {1 Helpers shared by the adapters} *)

val malformed : string -> string -> ('a, error) result
(** [malformed path reason] is [Error (Malformed { path; reason })]. *)

val canon_error : string -> Canon.error -> error
(** A JSON parse error at [path], as a [Malformed]. *)

val traverse : ('a -> ('b, error) result) -> 'a list -> ('b list, error) result
(** [List.map] that stops at the first error. *)

val remove_ignored : (string * string) list -> Canon.t -> Canon.t
(** Remove every path of an adapter's [ignored_fields] table from a request. *)

(** {1 Linking tool results to tool calls}

    Every adapter links results to calls with this one rule, both for
    [tool_results] and for rewriting call IDs in [canonical_request], so the key
    and the reported [call_index] always agree. *)

type history_step =
  | Called of string  (** A tool call with this provider ID (["" ] if none). *)
  | Answered of string
      (** A tool result for this provider ID (["" ] if none). *)

val link_history : history_step list -> int option list
(** [link_history steps] takes the steps in the order they appear in the request
    history and returns one entry per step. A [Called] step gets its own call
    index, counting from 0 over all calls. An [Answered] step gets the index of
    the call it answers. The walk keeps a first-in first-out queue of call
    indices per ID: a call appends to its ID's queue, and a result removes the
    front of its ID's queue. So parallel calls that share an ID, even an empty
    one, are answered in order, and an ID reused in a later turn links to the
    latest unanswered call with it. A result whose queue is empty is unlinked
    ([None]). Adapters leave unlinked results out of [tool_results] and write
    null in their place in the canonical request. *)

val pop_link : int option list -> int option list * int option
(** Take the next entry of a [link_history] result, for a rewrite that walks the
    history in the same order. An exhausted list gives [None]. *)

val linked_results : (history_step * 'a) list -> ('a * int) list
(** [linked_results steps] links the steps, each carrying a payload, with
    [link_history] and returns the payload and call index of every [Answered]
    step that is linked, in order. This is the shape of [tool_results]. *)
