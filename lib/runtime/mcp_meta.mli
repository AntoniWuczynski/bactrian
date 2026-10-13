(** A run's MCP servers as [meta.json] keeps them, and how a replay gets them
    back.

    Each server is an object with its [name], its [transport] ([stdio] or
    [http]) and its [policy] ([record], [replay], [passthrough] or [block]).
    Nothing else is stored: a server's command, environment and URL may hold
    secrets, and a replay needs none of them. *)

val to_json : Mcp_proxy.config list -> Yojson.Safe.t
(** The value of [meta.json]'s [mcp] field. *)

val replaying : Yojson.Safe.t -> (Mcp_proxy.config list, string) result
(** The servers of a recording, for a replay of it: each one answers from the
    recording ([Replay]), except one that was blocked, which stays [Block]
    because the recording holds nothing for it and the agent met the same
    refusal then. A stdio server has an empty command and an HTTP server a
    placeholder URL, neither of which a replay ever uses. [Error] says what is
    wrong with the field: not a list, a missing or non-string key, an invalid
    name, an unknown transport or policy. *)
