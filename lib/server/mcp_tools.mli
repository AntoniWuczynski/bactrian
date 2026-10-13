(** The tools of [bactrian serve-mcp], one per command (decision record 0011).
    Each decodes its arguments strictly, keeps paths inside the root, calls the
    command in {!Bactrian_app} and returns that command's [--json] output, as
    [structuredContent] and as the same JSON in a text block. [run], [record]
    and [replay] start a job instead and return its ID. *)

open Bactrian_core

type context = {
  env : Eio_unix.Stdenv.base;
  root : Confine.t;  (** The server's working directory. *)
  policy : Policy.t;
  jobs : Jobs.t;
  bactrian_exe : string;
  on_progress : Bactrian_app.Progress.t -> unit;
      (** Warnings of the commands, for standard error. *)
}

val definitions : (string * Canon.t) list
(** Each tool's name and its [tools/list] entry: [name], [title], [description],
    [inputSchema] and [annotations]. In a fixed order. *)

val call : context -> string -> Canon.t option -> Canon.t
(** [call context name arguments] runs the tool and gives its [tools/call]
    result: [content], [structuredContent] and [isError]. Bad arguments, a path
    outside the root and a failed command are results with [isError: true] whose
    JSON is the command's error as [--json] prints it. An unknown [name] is one
    too, though {!Mcp_dispatch} refuses it before. *)
