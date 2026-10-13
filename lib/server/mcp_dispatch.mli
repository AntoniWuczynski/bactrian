(** The protocol side of the MCP server, without IO: which era a connection
    speaks, the answers to [server/discover], [initialize], [ping] and
    [tools/list], the checks on [tools/call], and the error for everything else
    (decision record 0011). The first request that names an era fixes it for the
    connection: [initialize] selects the legacy revisions (2025-11-25 and
    earlier), a [_meta] with [io.modelcontextprotocol/protocolVersion] selects
    2026-07-28. *)

open Bactrian_core

val modern_version : string
(** ["2026-07-28"]. *)

val legacy_versions : string list
(** The revisions [initialize] can agree on, newest first: ["2025-11-25"],
    ["2025-06-18"] and ["2025-03-26"]. *)

type era =
  | Undecided
  | Legacy of string  (** The revision [initialize] agreed on. *)
  | Modern

type server = {
  name : string;
  version : string;
  instructions : string;
  tools : (string * Canon.t) list;
      (** Each tool's name and definition, in the order [tools/list] gives them.
      *)
}

type t

val create : server -> t
val era : t -> era

type action =
  | Reply of Mcp_msg.message  (** To send at once. *)
  | Call of { id : Mcp_msg.id; name : string; arguments : Canon.t option }
      (** A known tool to run; answer with {!tool_reply}. *)
  | Cancel of Mcp_msg.id  (** Stop the request in flight with this id. *)
  | Ignore

val receive : t -> Mcp_msg.message -> action
(** The action for one message from the client, deciding the era if it is still
    undecided. Responses from the client are ignored, as is a notification other
    than [notifications/cancelled]. *)

val tool_reply : t -> Mcp_msg.id -> Canon.t -> Mcp_msg.message
(** The response carrying a [tools/call] result, with [resultType: "complete"]
    added under 2026-07-28. *)

val undecodable : Mcp_msg.error -> Mcp_msg.message
(** The error for a line that is not a JSON-RPC message: [-32700] for text that
    is not JSON, [-32600] otherwise, with a [null] id. *)
