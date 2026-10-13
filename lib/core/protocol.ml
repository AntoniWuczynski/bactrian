type tool_call = { name : string; arguments : Canon.t }
type tool_result = { call_index : int; content : string; is_error : bool }

type summary = {
  model : string option;
  text : string;
  tool_calls : tool_call list;
  usage : Trace.usage option;
  stop_reason : string option;
  error : string option;
}

type body =
  | Json of Canon.t
  | Sse of Sse.item list
  | Json_stream of Canon.t list

type error = Malformed of { path : string; reason : string }

module type S = sig
  val name : string
  val matches : meth:string -> path:string -> bool
  val ignored_fields : (string * string) list
  val canonical_request : path:string -> Canon.t -> (Canon.t, error) result
  val streaming : path:string -> query:string -> Canon.t -> bool
  val summarise : body -> (summary, error) result
  val tool_results : Canon.t -> tool_result list
end

let malformed path reason = Error (Malformed { path; reason })

let canon_error path e =
  Malformed { path; reason = Format.asprintf "%a" Canon.pp_error e }

let traverse f l =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: rest -> (
        match f x with Ok y -> go (y :: acc) rest | Error _ as e -> e)
  in
  go [] l

let remove_ignored ignored req =
  List.fold_left
    (fun v (p, _) -> Canon.remove_at (Canon.path_of_string p) v)
    req ignored

type history_step = Called of string | Answered of string

module Id_map = Map.Make (String)

let link_history steps =
  let step (next, queues, acc) = function
    | Called id ->
        let queue = Option.value (Id_map.find_opt id queues) ~default:[] in
        (next + 1, Id_map.add id (queue @ [ next ]) queues, Some next :: acc)
    | Answered id -> (
        match Id_map.find_opt id queues with
        | Some (call :: rest) ->
            (next, Id_map.add id rest queues, Some call :: acc)
        | Some [] | None -> (next, queues, None :: acc))
  in
  let _, _, rev = List.fold_left step (0, Id_map.empty, []) steps in
  List.rev rev

let pop_link = function [] -> ([], None) | link :: rest -> (rest, link)

let linked_results steps =
  Seq.zip (List.to_seq steps) (List.to_seq (link_history (List.map fst steps)))
  |> Seq.filter_map (fun ((step, payload), link) ->
      match (step, link) with
      | Answered _, Some call_index -> Some (payload, call_index)
      | (Called _ | Answered _), _ -> None)
  |> List.of_seq

let pp_error ppf (Malformed { path; reason }) =
  Format.fprintf ppf "malformed %s: %s" path reason
