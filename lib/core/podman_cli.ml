type info = { arch : string; rootless : bool; remote : bool }

let info_format =
  "{{.Host.Arch}} {{.Host.Security.Rootless}} {{.Host.ServiceIsRemote}}"

let info text =
  match String.split_on_char ' ' (String.trim text) with
  | [ arch; (("true" | "false") as rootless); (("true" | "false") as remote) ]
    when not (String.equal arch "") ->
      Ok
        {
          arch;
          rootless = String.equal rootless "true";
          remote = String.equal remote "true";
        }
  | _ ->
      Error
        (Printf.sprintf
           "gave %S, not an architecture, whether it is rootless and whether \
            the client is remote"
           (String.trim text))

let image_id text =
  match String.trim text with
  | d
    when String.length d = 64
         && String.for_all
              (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
              d ->
      Ok ("sha256:" ^ d)
  | d -> Error (Printf.sprintf "gave %S, not an image ID" d)

let ps_label key = Printf.sprintf {|{{index .Labels "%s"}}|} key
