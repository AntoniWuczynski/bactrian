type exchange = { request : string; replies : string list }

let parse text =
  let finish current acc =
    match current with
    | None -> acc
    | Some (request, replies) -> { request; replies = List.rev replies } :: acc
  in
  let rec go current acc = function
    | [] -> Ok (List.rev (finish current acc))
    | (n, line) :: rest -> (
        if String.equal line "" || String.starts_with ~prefix:"#" line then
          go current acc rest
        else
          match
            ( String.starts_with ~prefix:"> " line,
              String.starts_with ~prefix:"< " line,
              current )
          with
          | true, _, _ ->
              go
                (Some (String.sub line 2 (String.length line - 2), []))
                (finish current acc) rest
          | false, true, Some (request, replies) ->
              go
                (Some
                   ( request,
                     String.sub line 2 (String.length line - 2) :: replies ))
                acc rest
          | false, true, None ->
              Error (Printf.sprintf "line %d: a reply before any request" n)
          | false, false, _ ->
              Error
                (Printf.sprintf "line %d: expected \"> \", \"< \" or \"#\"" n))
  in
  go None []
    (List.mapi (fun i l -> (i + 1, l)) (String.split_on_char '\n' text))

let load path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> parse text
  | exception Sys_error m -> Error m
