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

(* Seatbelt's operation names: whole families by prefix, and the few that
   stand alone. *)
let operation_families =
  [
    "file-";
    "network-";
    "mach-";
    "process-";
    "ipc-";
    "iokit-";
    "sysctl-";
    "system-";
    "socket-";
    "nvram-";
    "user-preference-";
    "device-";
    "hid-";
    "darwin-notification-";
    "distributed-notification-";
  ]

let single_operations =
  [
    "signal";
    "job-creation";
    "pseudo-tty";
    "lsopen";
    "appleevent-send";
    "dynamic-code-generation";
    "authorization-right-obtain";
    "load-unsigned-code";
    "boot-arg-set";
    "generic-issue-extension";
  ]

let known_operation op =
  String.for_all (function 'a' .. 'z' | '-' -> true | _ -> false) op
  && (List.mem op single_operations
     || List.exists
          (fun prefix ->
            String.starts_with ~prefix op
            && String.length op > String.length prefix)
          operation_families)

let digits s =
  (not (String.equal s ""))
  && String.for_all (function '0' .. '9' -> true | _ -> false) s

(* Whether [process] ends with "(PID)". *)
let ends_with_pid process =
  String.ends_with ~suffix:")" process
  &&
  match String.rindex_opt process '(' with
  | None -> false
  | Some i ->
      digits (String.sub process (i + 1) (String.length process - i - 2))

(* [rest] after " deny(": "N) OPERATION TARGET" or "N) OPERATION". *)
let operation rest =
  match String.index_opt rest ')' with
  | None -> None
  | Some j when not (digits (String.sub rest 0 j)) -> None
  | Some j ->
      let after = String.sub rest (j + 1) (String.length rest - j - 1) in
      if not (String.starts_with ~prefix:" " after) then None
      else
        let after = String.sub after 1 (String.length after - 1) in
        let op, target =
          match String.index_opt after ' ' with
          | None -> (after, None)
          | Some k ->
              ( String.sub after 0 k,
                Some (String.sub after (k + 1) (String.length after - k - 1)) )
        in
        if known_operation op then Some (op, target) else None

(* "NAME(PID) deny(N) OPERATION TARGET": the process name and the target are
   the agent's to choose, so they can hold " deny(" too. The first place where
   "(PID) deny(N) " is followed by a known operation is taken, and a message
   with none is kept whole as [unparsed]. *)
let denial body =
  let marker = " deny(" in
  let rec scan from =
    match
      find_sub ~sub:marker (String.sub body from (String.length body - from))
    with
    | None -> { kind = "unparsed"; detail = body }
    | Some i -> (
        let i = from + i in
        let process = String.sub body 0 i in
        let start = i + String.length marker in
        match
          if ends_with_pid process then
            operation (String.sub body start (String.length body - start))
          else None
        with
        | Some (kind, Some target) ->
            { kind; detail = target ^ " by " ^ process }
        | Some (kind, None) -> { kind; detail = "by " ^ process }
        | None -> scan (i + 1))
  in
  scan 0

let of_seatbelt_message ~tag message =
  let suffix = "\n" ^ tag in
  if not (String.ends_with ~suffix message) then None
  else
    let message =
      String.sub message 0 (String.length message - String.length suffix)
    in
    Option.map denial (after ~sub:"Sandbox: " message)

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

let of_relay_log text =
  let lines =
    List.filter
      (fun l -> not (String.equal l ""))
      (String.split_on_char '\n' text)
  in
  let items =
    List.filter_map (fun l -> Result.to_option (of_relay_line l)) lines
  in
  (items, List.length lines - List.length items)
