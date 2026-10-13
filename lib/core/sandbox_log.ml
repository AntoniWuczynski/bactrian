type violation = { kind : string; detail : string }

let tag id =
  "bactrian-sandbox:"
  ^ String.map
      (function
        | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_') as c -> c
        | _ -> '_')
      id

let end_tag tag = tag ^ "-end"

let predicate ~tag =
  Printf.sprintf "sender == \"Sandbox\" AND eventMessage CONTAINS \"%s\"" tag

let find_sub ~sub s =
  let n = String.length sub in
  let rec go i =
    if i + n > String.length s then None
    else if String.equal (String.sub s i n) sub then Some i
    else go (i + 1)
  in
  go 0

let after ~sub s =
  Option.map
    (fun i ->
      let start = i + String.length sub in
      String.sub s start (String.length s - start))
    (find_sub ~sub s)

(* "NAME(PID) deny(N) OPERATION TARGET": the process name may hold spaces and
   parentheses, so the split is at " deny(". *)
let denial body =
  match find_sub ~sub:" deny(" body with
  | None -> None
  | Some i -> (
      let process = String.sub body 0 i in
      let rest = String.sub body (i + 1) (String.length body - i - 1) in
      match String.index_opt rest ' ' with
      | None -> None
      | Some j -> (
          let rest = String.sub rest (j + 1) (String.length rest - j - 1) in
          match String.index_opt rest ' ' with
          | None -> Some { kind = rest; detail = "by " ^ process }
          | Some k ->
              Some
                {
                  kind = String.sub rest 0 k;
                  detail =
                    String.sub rest (k + 1) (String.length rest - k - 1)
                    ^ " by " ^ process;
                }))

let of_seatbelt_message ~tag message =
  let suffix = "\n" ^ tag in
  if not (String.ends_with ~suffix message) then None
  else
    let message =
      String.sub message 0 (String.length message - String.length suffix)
    in
    Option.bind (after ~sub:"Sandbox: " message) denial

let of_seatbelt_ndjson ~tag line =
  match Yojson.Safe.from_string line with
  | exception Yojson.Json_error _ -> None
  | `Assoc fields -> (
      match List.assoc_opt "eventMessage" fields with
      | Some (`String m) -> of_seatbelt_message ~tag m
      | Some
          (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `List _ | `Assoc _)
      | None ->
          None)
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
      None

type relay = { kind : string; port : int; socket : string; reason : string }
[@@deriving of_yojson]

let poll_interval = 0.5
let settle_interval = 1.

let equal_violation (a : violation) (b : violation) =
  String.equal a.kind b.kind && String.equal a.detail b.detail

type settled =
  | Settled of violation list
  | Unsettled of violation list
  | No_marker

let settle ~tag ~read ~wait ~now ~deadline =
  let marked =
    List.exists (fun l ->
        Option.is_some (of_seatbelt_ndjson ~tag:(end_tag tag) l))
  in
  let parse = List.filter_map (of_seatbelt_ndjson ~tag) in
  let rec until_marker () =
    let lines = read () in
    if marked lines then until_stable (parse lines)
    else if now () >= deadline then No_marker
    else begin
      wait poll_interval;
      until_marker ()
    end
  and until_stable previous =
    if now () +. settle_interval > deadline then Unsettled previous
    else begin
      wait settle_interval;
      let next = parse (read ()) in
      if List.equal equal_violation previous next then Settled next
      else until_stable next
    end
  in
  until_marker ()

let of_relay_line line =
  match Yojson.Safe.from_string line with
  | exception Yojson.Json_error m -> Error m
  | json -> (
      match relay_of_yojson json with
      | Error m -> Error m
      | Ok { kind; port; socket; reason } ->
          Ok
            {
              kind;
              detail =
                Printf.sprintf "127.0.0.1:%d to %s: %s" port socket reason;
            })
