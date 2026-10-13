type error = { path : string; message : string }

let pp ppf { path; message } =
  if String.equal path "" then Format.pp_print_string ppf message
  else Format.fprintf ppf "%s: %s" path message
