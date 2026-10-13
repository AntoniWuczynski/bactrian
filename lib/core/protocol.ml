type tool_call = { name : string; arguments : Canon.t }
type tool_result = { call_index : int; content : string; is_error : bool }

type summary = {
  model : string option;
  text : string;
  tool_calls : tool_call list;
  usage : Trace.usage option;
  stop_reason : string option;
  error : string option;
}

type body =
  | Json of Canon.t
  | Sse of Sse.item list
  | Json_stream of Canon.t list

type error = Malformed of { path : string; reason : string }

module type S = sig
  val name : string
  val matches : meth:string -> path:string -> bool
  val ignored_fields : (string * string) list
  val canonical_request : path:string -> Canon.t -> (Canon.t, error) result
  val streaming : path:string -> query:string -> Canon.t -> bool
  val summarise : body -> (summary, error) result
  val tool_results : Canon.t -> tool_result list
end

let pp_error ppf (Malformed { path; reason }) =
  Format.fprintf ppf "malformed %s: %s" path reason

let str_member k v = Option.bind (Canon.member k v) Canon.to_string_opt
let int_member k v = Option.bind (Canon.member k v) Canon.to_int_opt
let bool_member k v = Option.bind (Canon.member k v) Canon.to_bool_opt
let list_member k v = Option.bind (Canon.member k v) Canon.to_list_opt
