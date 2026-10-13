(** The MCP flags of [bactrian record]: [--mcp-stdio NAME=JSON],
    [--mcp-http NAME=URL] and [--mcp-policy NAME=POLICY], each repeatable. *)

val stdio_name : string
(** ["mcp-stdio"], the flag without its dashes. *)

val http_name : string
(** ["mcp-http"]. *)

val policy_name : string
(** ["mcp-policy"]. *)

val parse :
  stdio:string list ->
  http:string list ->
  policy:string list ->
  (Mcp_proxy.config list, string) result
(** The servers the flags declare, in name order, from the values of the three
    flags. A stdio server's command is a JSON array of strings, run in
    bactrian's own directory with an empty environment (beyond what {!Mcp_proxy}
    always gives). A server's policy is [record] unless [policy] says
    [passthrough] or [block]. The error names the flag at fault: names and URLs
    are checked as an experiment's are ({!Bactrian_core.Experiment}), a name is
    declared once only, two HTTP servers cannot share a variable, a policy needs
    a declared server and [replay] is refused, because a recording run has
    nothing to replay from. *)
