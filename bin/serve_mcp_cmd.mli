(** [bactrian serve-mcp]: bactrian's commands as tools of an MCP server on
    standard input and output. *)

val cmd : bactrian_exe:string -> int Cmdliner.Cmd.t
