open Bactrian_runtime

type t =
  | Usage of string
  | Environment of string
  | Internal of string
  | Configuration of Config_loader.located_error list
  | Interrupted of { signal : int; experiment_id : string option }

let exit_code = function
  | Usage _ | Configuration _ -> Exit_code.usage
  | Environment _ -> Exit_code.environment
  | Internal _ -> Exit_code.internal
  | Interrupted _ -> Interrupt.exit_status

let message = function
  | Usage m | Environment m | Internal m -> m
  | Configuration errors ->
      String.concat "\n" (List.map Config_loader.located_line errors)
  | Interrupted { signal; _ } ->
      "interrupted by " ^ Proc_util.signal_name signal

let located_json (e : Config_loader.located_error) =
  `Assoc
    [
      ("file", `String e.file);
      ("line", `Int e.line);
      ("column", `Int e.column);
      ("path", `String e.path);
      ("message", `String e.message);
    ]

let to_json e =
  match e with
  | Configuration errors ->
      `Assoc
        [
          ("errors", `List (List.map located_json errors));
          ("exit", `Int (exit_code e));
        ]
  | Interrupted { experiment_id = Some id; _ } ->
      `Assoc
        [
          ("error", `String (message e));
          ("exit", `Int (exit_code e));
          ("experiment_id", `String id);
        ]
  | Usage _ | Environment _ | Internal _
  | Interrupted { experiment_id = None; _ } ->
      `Assoc [ ("error", `String (message e)); ("exit", `Int (exit_code e)) ]
