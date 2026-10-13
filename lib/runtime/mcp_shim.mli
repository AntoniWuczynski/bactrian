(** [bactrian mcp-shim NAME]: what an agent's MCP configuration launches in
    place of a stdio server. It bridges newline-delimited JSON-RPC on its stdin
    and stdout to bactrian's [/r/<token>/mcp/NAME] route, one POST per message,
    and leaves the real server to bactrian (decision 0006). *)

val run :
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  stdin:_ Eio.Flow.source ->
  stdout:_ Eio.Flow.sink ->
  url:string ->
  name:string ->
  unit
(** [url] is the value of {!Mcp_proxy.url_variable}. Each non-blank stdin line
    is posted to [url ^ "/" ^ name] in its own fibre, so parallel requests stay
    parallel, and each reply that is a JSON-RPC message is written to stdout as
    one line, in the order replies arrive. Nothing else is ever written to
    stdout. A request whose post fails, or whose reply is not JSON-RPC, gets a
    JSON-RPC error [-32000] with its id. Returns once stdin reaches end of file
    (or a line over 64 MiB arrives) and every reply has been written. *)
