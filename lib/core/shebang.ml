type t = Interpreter of string | Env_program of string

let first_line s =
  match String.index_opt s '\n' with Some i -> String.sub s 0 i | None -> s

let words line =
  String.split_on_char ' '
    (String.map (fun c -> if c = '\t' then ' ' else c) line)
  |> List.filter (fun w -> not (String.equal w ""))

let parse s =
  if not (String.starts_with ~prefix:"#!" s) then None
  else
    let line = first_line (String.sub s 2 (String.length s - 2)) in
    match words line with
    | [] -> None
    | interpreter :: rest ->
        if String.equal (Filename.basename interpreter) "env" then
          List.find_opt
            (fun w ->
              not (String.starts_with ~prefix:"-" w || String.contains w '='))
            rest
          |> Option.map (fun name -> Env_program name)
        else Some (Interpreter interpreter)
