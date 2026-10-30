(** The protocol side of the MCP server, without IO: which era a connection
    speaks, the answers to [server/discover], [initialize], [ping] and
    [tools/list], the checks on [tools/call], and the error for everything else
    (decision record 0011). The first request that names an era and is accepted
    (it succeeds, or gets [-32022]) fixes it for the connection: [initialize]
    selects the legacy revisions (2025-11-25 and earlier), a [_meta] with
    [io.modelcontextprotocol/protocolVersion] selects 2026-07-28.

    In 2026-07-28 the server offers the Tasks extension
    ([io.modelcontextprotocol/tasks], decision record 0016): a [tools/call]
    whose request declares it may be answered with a task, and [tasks/get],
    [tasks/update] and [tasks/cancel] answer for one. *)

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

val create : ?era:era -> server -> t
(** [era] (default [Undecided]) is what a transport already knows: Streamable
    HTTP names the revision of each request in a header. *)

val era : t -> era

type task_op = Get_task | Update_task | Cancel_task

type tool_result =
  | Complete of Canon.t  (** A [CallToolResult]. *)
  | Task of Canon.t
      (** A [Task], for a call whose request declared the Tasks extension. *)

type handlers = {
  call : tasks:bool -> string -> Canon.t option -> tool_result;
      (** Runs a tool. [tasks] says whether the request declared the Tasks
          extension, without which the result must be [Complete]. *)
  task : task_op -> string -> Canon.t option;
      (** Acts on the task with this ID and gives its current state as a
          [DetailedTask], or [None] when there is no such task. *)
}
(** What a transport runs for the actions of {!receive}. *)

type action =
  | Reply of Mcp_msg.message  (** To send at once. *)
  | Call of {
      id : Mcp_msg.id;
      name : string;
      arguments : Canon.t option;
      tasks : bool;  (** The request declared the Tasks extension. *)
    }  (** A known tool to run; answer with {!tool_reply}. *)
  | Task_op of { id : Mcp_msg.id; op : task_op; task_id : string }
      (** A task method from a request that declared the Tasks extension; answer
          with {!task_reply}. *)
  | Cancel of Mcp_msg.id  (** Stop the request in flight with this id. *)
  | Ignore

val receive : t -> Mcp_msg.message -> action
(** The action for one message from the client, deciding the era if it is still
    undecided. Responses from the client are ignored, as is a notification other
    than [notifications/cancelled]. *)

val tool_reply : t -> Mcp_msg.id -> tool_result -> Mcp_msg.message
(** The response carrying a [tools/call] result, with [resultType] added under
    2026-07-28: ["complete"], or ["task"] for a task. *)

val task_reply : t -> Mcp_msg.id -> task_op -> Canon.t option -> Mcp_msg.message
(** The response to a task method, given the task's state: the task for
    [tasks/get], an empty acknowledgement for [tasks/update] and [tasks/cancel],
    each with [resultType: "complete"]. [None] (no such task) is invalid params,
    [-32602]. *)

val undecodable : Mcp_msg.error -> Mcp_msg.message
(** The error for a line that is not a JSON-RPC message: [-32700] for text that
    is not JSON, [-32600] otherwise, with a [null] id. *)
