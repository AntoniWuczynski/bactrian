type sequence_mode = Exact | In_order | Contains
type args_mode = Exact_args | Partial | Schema

type output_check =
  | Regex of string
  | Jsonpath of { path : string; equals : Canon.t option }
  | Equals of string

type t =
  | Tool_sequence of { mode : sequence_mode; tools : string list }
  | Tool_args of { tool : string; mode : args_mode; expected : Canon.t }
  | Max_steps of int
  | Output of { turn : [ `Last | `Index of int ]; check : output_check }
  | Max_cost_usd of float
  | Max_tokens of int
  | Max_latency_ms of int
  | No_egress of string list

type outcome = Pass | Fail of string

(* Decoding *)

module D = Config_decode

(* Perl syntax from [Re.Perl], plus a leading [(?i)] for case-insensitive
   matching, which [Re.Perl] does not parse. *)
let compile_regex source =
  let cased, opts =
    match String.starts_with ~prefix:"(?i)" source with
    | true -> (String.sub source 4 (String.length source - 4), [ `Caseless ])
    | false -> (source, [])
  in
  match Re.Perl.re_result ~opts cased with
  | Ok re -> Ok (Re.compile re)
  | Error (`Not_supported | `Parse_error) ->
      Error (Printf.sprintf "%S is not a valid regular expression" source)

let raw ~path:_ v = Ok v
let comma = String.concat ", "

let decode_tool_sequence ~path body =
  D.object_ ~path body (fun r ->
      let open D in
      let+ mode =
        required r "mode"
          (enum ~what:"mode"
             [
               ("exact", Exact); ("in_order", In_order); ("contains", Contains);
             ])
      and+ tools = required r "tools" (list string) in
      Tool_sequence { mode; tools })

(* [match] gives the expected arguments for exact and partial, [schema] for
   schema mode. The other one is an error. *)
let expected_args r mode =
  let open D in
  let matched = optional r "match" raw in
  let schema = optional r "schema" raw in
  let at k = field_path (D.path r) k in
  let unused k = fail ~path:(at k) "not used with this mode" in
  match (matched, schema) with
  | (Error _ as e), _ | _, (Error _ as e) -> e
  | Ok matched, Ok schema -> (
      match (mode, matched, schema) with
      | Schema, None, Some s -> (
          match Json_schema.check s with
          | [] -> Ok s
          | problems ->
              Error
                (List.map
                   (fun (e : Config_error.error) ->
                     { e with path = nest (at "schema") e.path })
                   problems))
      | Schema, None, None -> fail ~path:(at "schema") "missing required field"
      | Schema, Some _, _ -> unused "match"
      | (Exact_args | Partial), Some m, None -> Ok m
      | (Exact_args | Partial), None, _ ->
          fail ~path:(at "match") "missing required field"
      | (Exact_args | Partial), Some _, Some _ -> unused "schema")

let decode_tool_args ~path body =
  D.object_ ~path body (fun r ->
      let open D in
      let tool = required r "tool" non_empty_string in
      let mode =
        default r "mode" Partial
          (enum ~what:"mode"
             [ ("exact", Exact_args); ("partial", Partial); ("schema", Schema) ])
      in
      let mode_expected =
        Result.bind mode (fun m ->
            Result.map (fun e -> (m, e)) (expected_args r m))
      in
      let+ tool = tool and+ mode, expected = mode_expected in
      Tool_args { tool; mode; expected })

let regex_source ~path v =
  let ( let* ) = D.( let* ) in
  let* source = D.string ~path v in
  match compile_regex source with
  | Ok _ -> Ok source
  | Error m -> D.fail ~path m

let decode_turn ~path v =
  match Canon.to_string_opt v with
  | Some "last" -> Ok `Last
  | Some _ | None -> Result.map (fun n -> `Index n) (D.int_at_least 1 ~path v)

let json_path ~path v =
  let ( let* ) = D.( let* ) in
  let* s = D.string ~path v in
  match Json_path.parse s with
  | Ok _ -> Ok s
  | Error m -> D.fail ~path (Printf.sprintf "invalid JSONPath %S: %s" s m)

let decode_output ~path body =
  D.object_ ~path body (fun r ->
      let open D in
      let* turn, regex, jsonpath, equals =
        let+ turn = default r "turn" `Last decode_turn
        and+ regex = optional r "regex" regex_source
        and+ jsonpath = optional r "jsonpath" json_path
        and+ equals = optional r "equals" raw in
        (turn, regex, jsonpath, equals)
      in
      let+ check =
        match (regex, jsonpath, equals) with
        | Some re, None, None -> Ok (Regex re)
        | None, Some p, equals -> Ok (Jsonpath { path = p; equals })
        | None, None, Some e ->
            let+ s = string ~path:(field_path (D.path r) "equals") e in
            Equals s
        | None, None, None | Some _, _, _ ->
            fail ~path:(D.path r)
              "needs exactly one of regex, equals or jsonpath"
      in
      Output { turn; check })

let max_latency ~path v =
  match Canon.to_int_opt v with
  | Some _ -> D.int_at_least 0 ~path v
  | None ->
      Result.map
        (fun s -> Float.to_int (Float.round (s *. 1000.)))
        (D.duration ~path v)

let kinds =
  [
    "tool_sequence";
    "tool_args";
    "max_steps";
    "output";
    "max_cost_usd";
    "max_tokens";
    "max_latency";
    "no_egress";
  ]

let decode v =
  let open D in
  match Canon.to_fields_opt v with
  | None ->
      fail ~path:""
        "an assertion is an object with one key, the kind of assertion"
  | Some [ (kind, body) ] -> (
      let path = field_path "" kind in
      match kind with
      | "tool_sequence" -> decode_tool_sequence ~path body
      | "tool_args" -> decode_tool_args ~path body
      | "output" -> decode_output ~path body
      | "max_steps" ->
          Result.map (fun n -> Max_steps n) (int_at_least 0 ~path body)
      | "max_tokens" ->
          Result.map (fun n -> Max_tokens n) (int_at_least 0 ~path body)
      | "max_cost_usd" ->
          Result.map (fun x -> Max_cost_usd x) (float_at_least 0. ~path body)
      | "max_latency" ->
          Result.map (fun n -> Max_latency_ms n) (max_latency ~path body)
      | "no_egress" ->
          Result.map (fun hs -> No_egress hs) (list string ~path body)
      | other ->
          fail ~path
            (Printf.sprintf "unknown assertion kind %S, expected one of: %s"
               other (comma kinds)))
  | Some fields ->
      fail ~path:""
        (Printf.sprintf "an assertion has exactly one key, found %d: %s"
           (List.length fields)
           (comma (List.map fst fields)))

(* Evaluation *)

let ( let* ) = Result.bind

let tool_calls trace =
  List.filter_map
    (fun (e : Trace.entry) ->
      match e.event with
      | Tool_call { name; arguments; _ } -> Some (name, arguments)
      | Run_start _ | Turn_start _ | Model_call _ | Tool_result _ | Http_call _
      | Agent_event _ | Reply _ | Sandbox_violation _ | Done ->
          None)
    trace

let rec is_subsequence wanted have =
  match (wanted, have) with
  | [], _ -> true
  | _ :: _, [] -> false
  | w :: ws, h :: hs ->
      if String.equal w h then is_subsequence ws hs
      else is_subsequence wanted hs

let count x l = List.length (List.filter (String.equal x) l)
let listing l = if l = [] then "(none)" else comma l

let check_sequence mode tools trace =
  let called = List.map fst (tool_calls trace) in
  let actual = "called: " ^ listing called in
  match mode with
  | Exact ->
      if List.equal String.equal tools called then Pass
      else
        Fail
          (Printf.sprintf "tool_sequence exact: expected %s, %s" (listing tools)
             actual)
  | In_order ->
      if is_subsequence tools called then Pass
      else
        Fail
          (Printf.sprintf "tool_sequence in_order: %s is not a subsequence, %s"
             (listing tools) actual)
  | Contains -> (
      let short =
        List.sort_uniq String.compare tools
        |> List.filter (fun t -> count t called < count t tools)
      in
      match short with
      | [] -> Pass
      | missing ->
          Fail
            (Printf.sprintf "tool_sequence contains: missing %s, %s"
               (comma missing) actual))

(* The first place where [actual] does not contain [expected], as a message. *)
let rec partial_mismatch path expected actual =
  let differs () =
    Some
      (Printf.sprintf "%s: expected %s, got %s" path (Canon.to_string expected)
         (Canon.to_string actual))
  in
  match expected with
  | Canon.Object fields -> (
      match Canon.to_fields_opt actual with
      | None -> differs ()
      | Some have ->
          List.find_map
            (fun (k, e) ->
              let p = path ^ "." ^ k in
              match List.assoc_opt k have with
              | None -> Some (Printf.sprintf "%s: missing" p)
              | Some a -> partial_mismatch p e a)
            fields)
  | List items -> (
      match Canon.to_list_opt actual with
      | None -> differs ()
      | Some have ->
          List.find_map
            (fun e ->
              if List.exists (fun a -> partial_mismatch path e a = None) have
              then None
              else
                Some
                  (Printf.sprintf "%s: no element matches %s" path
                     (Canon.to_string e)))
            items)
  | Null | Bool _ | Int _ | Big_int _ | Float _ | String _ ->
      if Canon.equal expected actual then None else differs ()

let args_mismatch mode expected actual =
  match mode with
  | Exact_args ->
      if Canon.equal expected actual then None
      else
        Some
          (Printf.sprintf "expected %s, got %s" (Canon.to_string expected)
             (Canon.to_string actual))
  | Partial -> partial_mismatch "$" expected actual
  | Schema -> (
      match Json_schema.validate ~schema:expected actual with
      | [] -> None
      | violations -> Some (String.concat "; " violations))

let check_args tool mode expected trace =
  let calls =
    List.filter_map
      (fun (n, a) -> if String.equal n tool then Some a else None)
      (tool_calls trace)
  in
  let mismatches = List.map (args_mismatch mode expected) calls in
  if calls = [] then
    Fail
      (Printf.sprintf "tool_args: %s was never called (called: %s)" tool
         (listing (List.map fst (tool_calls trace))))
  else if List.exists Option.is_none mismatches then Pass
  else
    Fail
      (Printf.sprintf "tool_args: no call of %s matched. %s" tool
         (String.concat " "
            (List.mapi
               (fun i m ->
                 Printf.sprintf "Call %d: %s." (i + 1)
                   (Option.value m ~default:""))
               mismatches)))

let steps trace =
  List.length
    (List.filter
       (fun (e : Trace.entry) ->
         match e.event with
         | Model_call _ -> true
         | Run_start _ | Turn_start _ | Tool_call _ | Tool_result _
         | Http_call _ | Agent_event _ | Reply _ | Sandbox_violation _ | Done ->
             false)
       trace)

let tokens trace =
  List.fold_left
    (fun acc (e : Trace.entry) ->
      match e.event with
      | Model_call { usage = Some u; _ } ->
          acc + u.input_tokens + u.output_tokens
      | Model_call { usage = None; _ }
      | Run_start _ | Turn_start _ | Tool_call _ | Tool_result _ | Http_call _
      | Agent_event _ | Reply _ | Sandbox_violation _ | Done ->
          acc)
    0 trace

let span_ms = function
  | [] -> 0
  | (first : Trace.entry) :: _ as trace ->
      List.fold_left (fun m (e : Trace.entry) -> max m e.t_ms) first.t_ms trace
      - first.t_ms

let limit name ~actual ~max ~show =
  if actual <= max then Pass
  else
    Fail
      (Printf.sprintf "%s: %s exceeds the limit of %s" name (show actual)
         (show max))

let check_egress hosts trace =
  let wanted = List.map String.lowercase_ascii hosts in
  let hits =
    List.filter_map
      (fun (e : Trace.entry) ->
        match e.event with
        | Http_call { host; _ }
          when List.mem (String.lowercase_ascii host) wanted ->
            Some host
        | Http_call _ | Run_start _ | Turn_start _ | Model_call _ | Tool_call _
        | Tool_result _ | Agent_event _ | Reply _ | Sandbox_violation _ | Done
          ->
            None)
      trace
  in
  match List.sort_uniq String.compare hits with
  | [] -> Pass
  | hosts -> Fail ("no_egress: HTTP calls to " ^ comma hosts)

let reply_of turn trace =
  let replies =
    List.filter_map
      (fun (e : Trace.entry) ->
        match e.event with
        | Reply { turn; content } -> Some (turn, content)
        | Run_start _ | Turn_start _ | Model_call _ | Tool_call _
        | Tool_result _ | Http_call _ | Agent_event _ | Sandbox_violation _
        | Done ->
            None)
      trace
  in
  let found =
    match turn with
    | `Last -> List.nth_opt (List.rev replies) 0
    | `Index n -> List.find_opt (fun (t, _) -> t = n) replies
  in
  match found with
  | Some r -> Ok r
  | None ->
      Error
        (match turn with
        | `Last -> "output: the agent never replied"
        | `Index n -> Printf.sprintf "output: no reply for turn %d" n)

let check_json_reply ~turn ~path ~equals content =
  let* doc =
    Result.map_error
      (fun _ ->
        Printf.sprintf "output jsonpath: the reply of turn %d is not JSON" turn)
      (Canon.of_string content)
  in
  let* segments =
    Result.map_error
      (fun m -> "output jsonpath: invalid path: " ^ m)
      (Json_path.parse path)
  in
  let* found =
    Result.map_error
      (fun m -> "output jsonpath: " ^ m)
      (Json_path.eval segments doc)
  in
  match equals with
  | Some e when not (Canon.equal e found) ->
      Error
        (Printf.sprintf "output jsonpath: %s is %s, expected %s" path
           (Canon.to_string found) (Canon.to_string e))
  | Some _ | None -> Ok ()

let check_output turn check trace =
  let* turn, content = reply_of turn trace in
  match check with
  | Regex source ->
      let* re =
        Result.map_error (fun m -> "output regex: " ^ m) (compile_regex source)
      in
      if Re.execp re content then Ok ()
      else
        Error
          (Printf.sprintf "output: the reply of turn %d does not match %S: %S"
             turn source content)
  | Equals expected ->
      if String.equal expected content then Ok ()
      else
        Error
          (Printf.sprintf "output: the reply of turn %d is %S, expected %S" turn
             content expected)
  | Jsonpath { path; equals } -> check_json_reply ~turn ~path ~equals content

let evaluate ~cost assertion trace =
  match assertion with
  | Tool_sequence { mode; tools } -> check_sequence mode tools trace
  | Tool_args { tool; mode; expected } -> check_args tool mode expected trace
  | Max_steps n ->
      limit "max_steps" ~actual:(steps trace) ~max:n ~show:string_of_int
  | Max_tokens n ->
      limit "max_tokens" ~actual:(tokens trace) ~max:n ~show:string_of_int
  | Max_latency_ms n ->
      limit "max_latency" ~actual:(span_ms trace) ~max:n ~show:(fun ms ->
          Printf.sprintf "%d ms" ms)
  | Max_cost_usd max -> (
      match cost trace with
      | None ->
          Fail "max_cost_usd: the trace cannot be priced (a model has no price)"
      | Some actual ->
          limit "max_cost_usd" ~actual ~max ~show:(Printf.sprintf "$%g"))
  | No_egress hosts -> check_egress hosts trace
  | Output { turn; check } -> (
      match check_output turn check trace with
      | Ok () -> Pass
      | Error m -> Fail m)
