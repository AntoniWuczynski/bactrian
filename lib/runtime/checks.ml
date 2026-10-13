open Bactrian_core

type verdict = Pass | Fail of string | Checker_failed of string
type item = { name : string; verdict : verdict }

let cost ?(table = Pricing.default) ~date entries =
  List.fold_left
    (fun acc ({ event; _ } : Trace.entry) ->
      match (acc, event) with
      | None, _ -> None
      | Some total, Model_call { model; usage; _ } ->
          Option.bind usage (fun u ->
              Option.map
                (fun c -> total +. c)
                (Pricing.cost_in table ~model ~date u))
      | ( Some _,
          ( Run_start _ | Turn_start _ | Tool_call _ | Tool_result _
          | Http_call _ | Agent_event _ | Reply _ | Sandbox_violation _ | Done
            ) ) ->
          acc)
    (Some 0.) entries

let kind : Assertion.t -> string = function
  | Tool_sequence _ -> "tool_sequence"
  | Tool_args _ -> "tool_args"
  | Max_steps _ -> "max_steps"
  | Output _ -> "output"
  | Max_cost_usd _ -> "max_cost_usd"
  | Max_tokens _ -> "max_tokens"
  | Max_latency_ms _ -> "max_latency"
  | No_egress _ -> "no_egress"

let evaluate ?table ~process_mgr ~clock ~timeout ~cwd ~date ~assertions
    ~checkers entries =
  let assertion i a =
    let verdict =
      match Assertion.evaluate ~cost:(cost ?table ~date) a entries with
      | Pass -> Pass
      | Fail reason -> Fail reason
    in
    { name = Printf.sprintf "assertions[%d] %s" i (kind a); verdict }
  in
  let checker i argv =
    let verdict =
      match
        Checker_runner.run ~process_mgr ~clock ~timeout ~argv ~cwd entries
      with
      | Verdict { pass = true; _ } -> Pass
      | Verdict { pass = false; message; _ } ->
          Fail (Option.value message ~default:"the checker said fail")
      | Checker_error reason -> Checker_failed reason
    in
    {
      name = Printf.sprintf "checkers[%d] %s" i (String.concat " " argv);
      verdict;
    }
  in
  List.mapi assertion assertions @ List.mapi checker checkers

let problems items =
  List.filter_map
    (fun { name; verdict } ->
      match verdict with
      | Pass -> None
      | Fail reason -> Some (Printf.sprintf "%s: %s" name reason)
      | Checker_failed reason ->
          Some (Printf.sprintf "%s: checker error: %s" name reason))
    items

let item_json { name; verdict } =
  let label, reason =
    match verdict with
    | Pass -> ("pass", [])
    | Fail r -> ("fail", [ ("reason", `String r) ])
    | Checker_failed r -> ("checker_failed", [ ("reason", `String r) ])
  in
  `Assoc ([ ("name", `String name); ("verdict", `String label) ] @ reason)

let to_json items =
  `Assoc
    [
      ("passed", `Bool (List.is_empty (problems items)));
      ("items", `List (List.map item_json items));
    ]
