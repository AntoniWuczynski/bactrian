(** The report's stylesheet: light and dark themes from CSS custom properties
    and [prefers-color-scheme], laid out from phone width up. It holds no [>],
    [<] or [&], which the HTML printer would escape. *)

val css : string
