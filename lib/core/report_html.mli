(** The self-contained HTML report (spec §8, Outputs). One file: inline CSS and
    SVG, no scripts, no external assets. Light and dark themes follow
    [prefers-color-scheme]. Traces fold away in [details] elements. *)

val render : Report_data.t -> string
(** A complete HTML5 document. Pure and deterministic. *)
