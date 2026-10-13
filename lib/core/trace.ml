(* Wire format: one flat JSON object per entry. The envelope keys "v", "seq",
   "t_ms" and "type" sit beside the event's own fields.

   Usage encoding decision: a model call's token counts are FLATTENED to
   top-level OpenTelemetry-style keys ("gen_ai.usage.input_tokens", ...) rather
   than nested under a "usage" object. Decoding stays strict and simple because
   each payload is one ppx_deriving_yojson record whose optional keys use
   [@default None] (omitted when None, accepted when absent). A [usage] value
   is present on the wire exactly when both required counts appear. Optional
   counts without both required ones are rejected as a malformed entry.

   Each event has a private "wire" record deriving yojson (strict mode, so
   unknown fields are rejected). The public [event] type keeps inline records,
   so conversions between the two are written out by hand. The final object
   goes through [Canon] so key order is stable. *)

let schema_version = 1

type clock = Enforced | Contract_only

type usage = {
  input_tokens : int;
  output_tokens : int;
  cached_input_tokens : int option;
  cache_write_tokens : int option;
  reasoning_tokens : int option;
}

type join = Joined | Joined_by_order | Unjoined

type event =
  | Run_start of {
      run_id : string;
      scenario : string;
      variant : string;
      sim_now : string;
      clock : clock;
    }
  | Turn_start of { turn : int; content : string }
  | Model_call of {
      turn : int;
      protocol : string;
      model : string;
      status : int;
      usage : usage option;
      duration_ms : int;
      ttft_ms : int option;
    }
  | Tool_call of {
      turn : int;
      index : int;
      name : string;
      arguments : Canon.t;
      mcp : join;
    }
  | Tool_result of {
      turn : int;
      index : int;
      content : string;
      is_error : bool;
    }
  | Http_call of {
      turn : int;
      meth : string;
      host : string;
      path : string;
      status : int;
    }
  | Agent_event of { turn : int; name : string; data : Canon.t }
  | Reply of { turn : int; content : string }
  | Sandbox_violation of { kind : string; detail : string }
  | Done

type entry = { seq : int; t_ms : int; event : event }

type error =
  | Invalid_json of string
  | Unsupported_version of int
  | Unknown_type of string
  | Bad_entry of { type_ : string; reason : string }

(* ---- Field codecs picked up by the ppx by name ---- *)

let enum_of_yojson ~what table = function
  | `String s -> (
      match List.assoc_opt s table with
      | Some v -> Ok v
      | None -> Error (Printf.sprintf "unknown %s %S" what s))
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `List _ | `Assoc _ ->
      Error (what ^ " must be a string")

let clock_to_string = function
  | Enforced -> "enforced"
  | Contract_only -> "contract_only"

let clock_of_yojson =
  enum_of_yojson ~what:"clock"
    [ ("enforced", Enforced); ("contract_only", Contract_only) ]

let clock_to_yojson c = `String (clock_to_string c)

let join_to_string = function
  | Joined -> "joined"
  | Joined_by_order -> "joined_by_order"
  | Unjoined -> "unjoined"

let join_of_yojson =
  enum_of_yojson ~what:"join"
    [
      ("joined", Joined);
      ("joined_by_order", Joined_by_order);
      ("unjoined", Unjoined);
    ]

let join_to_yojson j = `String (join_to_string j)

module Canon_json = struct
  type t = Canon.t

  let to_yojson = Canon.to_yojson

  let of_yojson j =
    Result.map_error (Format.asprintf "%a" Canon.pp_error) (Canon.of_yojson j)
end

(* ---- Wire records, one per event ---- *)

type run_start_w = {
  run_id : string;
  scenario : string;
  variant : string;
  sim_now : string;
  clock : clock;
}
[@@deriving yojson]

type turn_start_w = { turn : int; content : string } [@@deriving yojson]

type model_call_w = {
  turn : int;
  protocol : string;
  model : string; [@key "gen_ai.request.model"]
  status : int;
  duration_ms : int;
  ttft_ms : int option; [@default None]
  input_tokens : int option; [@key "gen_ai.usage.input_tokens"] [@default None]
  output_tokens : int option;
      [@key "gen_ai.usage.output_tokens"] [@default None]
  cached_input_tokens : int option;
      [@key "gen_ai.usage.cache_read.input_tokens"] [@default None]
  cache_write_tokens : int option;
      [@key "gen_ai.usage.cache_write.input_tokens"] [@default None]
  reasoning_tokens : int option;
      [@key "gen_ai.usage.reasoning.output_tokens"] [@default None]
}
[@@deriving yojson]

type tool_call_w = {
  turn : int;
  index : int;
  name : string;
  arguments : Canon_json.t;
  mcp : join;
}
[@@deriving yojson]

type tool_result_w = {
  turn : int;
  index : int;
  content : string;
  is_error : bool;
}
[@@deriving yojson]

type http_call_w = {
  turn : int;
  meth : string; [@key "method"]
  host : string;
  path : string;
  status : int;
}
[@@deriving yojson]

type agent_event_w = { turn : int; name : string; data : Canon_json.t }
[@@deriving yojson]

type reply_w = { turn : int; content : string } [@@deriving yojson]

type sandbox_violation_w = { kind : string; detail : string }
[@@deriving yojson]

(* ---- Encoding ---- *)

let usage_fields : usage option -> _ = function
  | None -> (None, None, None, None, None)
  | Some u ->
      ( Some u.input_tokens,
        Some u.output_tokens,
        u.cached_input_tokens,
        u.cache_write_tokens,
        u.reasoning_tokens )

let type_and_payload : event -> string * Yojson.Safe.t = function
  | Run_start { run_id; scenario; variant; sim_now; clock } ->
      ( "run_start",
        run_start_w_to_yojson { run_id; scenario; variant; sim_now; clock } )
  | Turn_start { turn; content } ->
      ("turn_start", turn_start_w_to_yojson { turn; content })
  | Model_call { turn; protocol; model; status; usage; duration_ms; ttft_ms } ->
      let ( input_tokens,
            output_tokens,
            cached_input_tokens,
            cache_write_tokens,
            reasoning_tokens ) =
        usage_fields usage
      in
      ( "model_call",
        model_call_w_to_yojson
          {
            turn;
            protocol;
            model;
            status;
            duration_ms;
            ttft_ms;
            input_tokens;
            output_tokens;
            cached_input_tokens;
            cache_write_tokens;
            reasoning_tokens;
          } )
  | Tool_call { turn; index; name; arguments; mcp } ->
      ("tool_call", tool_call_w_to_yojson { turn; index; name; arguments; mcp })
  | Tool_result { turn; index; content; is_error } ->
      ("tool_result", tool_result_w_to_yojson { turn; index; content; is_error })
  | Http_call { turn; meth; host; path; status } ->
      ("http_call", http_call_w_to_yojson { turn; meth; host; path; status })
  | Agent_event { turn; name; data } ->
      ("agent_event", agent_event_w_to_yojson { turn; name; data })
  | Reply { turn; content } -> ("reply", reply_w_to_yojson { turn; content })
  | Sandbox_violation { kind; detail } ->
      ("sandbox_violation", sandbox_violation_w_to_yojson { kind; detail })
  | Done -> ("done", `Assoc [])

let to_line { seq; t_ms; event } =
  let ty, payload = type_and_payload event in
  let fields =
    match payload with
    | `Assoc fields -> fields
    | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
        []
  in
  let json =
    `Assoc
      (("v", `Int schema_version)
      :: ("seq", `Int seq)
      :: ("t_ms", `Int t_ms)
      :: ("type", `String ty)
      :: fields)
  in
  match Canon.of_yojson json with
  | Ok c -> Canon.to_string c
  | Error _ ->
      (* Only reachable if a payload held a non-finite float, which no event
         field can: fall back to the uncanonicalised text rather than lose the
         entry. *)
      Yojson.Safe.to_string json

(* ---- Decoding ---- *)

let usage_of_fields = function
  | None, None, None, None, None -> Ok None
  | ( Some input_tokens,
      Some output_tokens,
      cached_input_tokens,
      cache_write_tokens,
      reasoning_tokens ) ->
      Ok
        (Some
           {
             input_tokens;
             output_tokens;
             cached_input_tokens;
             cache_write_tokens;
             reasoning_tokens;
           })
  | None, _, _, _, _ | _, None, _, _, _ ->
      Error
        "gen_ai.usage.input_tokens and gen_ai.usage.output_tokens must both be \
         present when any usage field is"

let decode_model_call j =
  let open Result.Syntax in
  let* w = model_call_w_of_yojson j in
  let* usage =
    usage_of_fields
      ( w.input_tokens,
        w.output_tokens,
        w.cached_input_tokens,
        w.cache_write_tokens,
        w.reasoning_tokens )
  in
  Ok
    (Model_call
       {
         turn = w.turn;
         protocol = w.protocol;
         model = w.model;
         status = w.status;
         usage;
         duration_ms = w.duration_ms;
         ttft_ms = w.ttft_ms;
       })

let decoder_of_type : string -> (Yojson.Safe.t -> (event, string) result) option
    = function
  | "run_start" ->
      Some
        (fun j ->
          Result.map
            (fun { run_id; scenario; variant; sim_now; clock } ->
              Run_start { run_id; scenario; variant; sim_now; clock })
            (run_start_w_of_yojson j))
  | "turn_start" ->
      Some
        (fun j ->
          Result.map
            (fun ({ turn; content } : turn_start_w) ->
              Turn_start { turn; content })
            (turn_start_w_of_yojson j))
  | "model_call" -> Some decode_model_call
  | "tool_call" ->
      Some
        (fun j ->
          Result.map
            (fun ({ turn; index; name; arguments; mcp } : tool_call_w) ->
              Tool_call { turn; index; name; arguments; mcp })
            (tool_call_w_of_yojson j))
  | "tool_result" ->
      Some
        (fun j ->
          Result.map
            (fun ({ turn; index; content; is_error } : tool_result_w) ->
              Tool_result { turn; index; content; is_error })
            (tool_result_w_of_yojson j))
  | "http_call" ->
      Some
        (fun j ->
          Result.map
            (fun ({ turn; meth; host; path; status } : http_call_w) ->
              Http_call { turn; meth; host; path; status })
            (http_call_w_of_yojson j))
  | "agent_event" ->
      Some
        (fun j ->
          Result.map
            (fun ({ turn; name; data } : agent_event_w) ->
              Agent_event { turn; name; data })
            (agent_event_w_of_yojson j))
  | "reply" ->
      Some
        (fun j ->
          Result.map
            (fun ({ turn; content } : reply_w) -> Reply { turn; content })
            (reply_w_of_yojson j))
  | "sandbox_violation" ->
      Some
        (fun j ->
          Result.map
            (fun { kind; detail } -> Sandbox_violation { kind; detail })
            (sandbox_violation_w_of_yojson j))
  | "done" ->
      Some
        (function
        | `Assoc [] -> Ok Done
        | `Assoc _ -> Error "done carries no fields"
        | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _
          ->
            Error "expected an object")
  | _ -> None

let take key fields =
  let found, rest = List.partition (fun (k, _) -> String.equal k key) fields in
  (Option.map snd (List.nth_opt found 0), rest)

let bad ~type_ reason = Error (Bad_entry { type_; reason })

let int_field ~type_ key = function
  | Some (`Int n) -> Ok n
  | Some _ -> bad ~type_ (key ^ " must be an integer")
  | None -> bad ~type_ ("missing " ^ key)

let of_line line =
  let open Result.Syntax in
  let* doc =
    Result.map_error
      (function
        | Canon.Syntax m -> Invalid_json m
        | Canon.Duplicate_key k -> Invalid_json ("duplicate key " ^ k)
        | Canon.Non_finite_float -> Invalid_json "non-finite number")
      (Canon.of_string line)
  in
  match Canon.to_yojson doc with
  | `Assoc fields -> (
      let v, fields = take "v" fields in
      let* () =
        match v with
        | Some (`Int 1) -> Ok ()
        | Some (`Int n) -> Error (Unsupported_version n)
        | Some _ -> bad ~type_:"" "v must be an integer"
        | None -> bad ~type_:"" "missing v"
      in
      let ty, fields = take "type" fields in
      let* type_ =
        match ty with
        | Some (`String s) -> Ok s
        | Some _ -> bad ~type_:"" "type must be a string"
        | None -> bad ~type_:"" "missing type"
      in
      match decoder_of_type type_ with
      | None -> Error (Unknown_type type_)
      | Some decode ->
          let seq, fields = take "seq" fields in
          let t_ms, fields = take "t_ms" fields in
          let* seq = int_field ~type_ "seq" seq in
          let* t_ms = int_field ~type_ "t_ms" t_ms in
          let* event =
            Result.map_error
              (fun reason -> Bad_entry { type_; reason })
              (decode (`Assoc fields))
          in
          Ok { seq; t_ms; event })
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
      bad ~type_:"" "entry must be a JSON object"

(* ---- Equality and printing ---- *)

let clock_equal a b =
  match (a, b) with
  | Enforced, Enforced | Contract_only, Contract_only -> true
  | Enforced, Contract_only | Contract_only, Enforced -> false

let join_equal a b =
  match (a, b) with
  | Joined, Joined | Joined_by_order, Joined_by_order | Unjoined, Unjoined ->
      true
  | Joined, (Joined_by_order | Unjoined)
  | Joined_by_order, (Joined | Unjoined)
  | Unjoined, (Joined | Joined_by_order) ->
      false

let usage_equal (a : usage) (b : usage) =
  Int.equal a.input_tokens b.input_tokens
  && Int.equal a.output_tokens b.output_tokens
  && Option.equal Int.equal a.cached_input_tokens b.cached_input_tokens
  && Option.equal Int.equal a.cache_write_tokens b.cache_write_tokens
  && Option.equal Int.equal a.reasoning_tokens b.reasoning_tokens

let event_equal a b =
  match (a, b) with
  | Run_start x, Run_start y ->
      String.equal x.run_id y.run_id
      && String.equal x.scenario y.scenario
      && String.equal x.variant y.variant
      && String.equal x.sim_now y.sim_now
      && clock_equal x.clock y.clock
  | Turn_start x, Turn_start y ->
      Int.equal x.turn y.turn && String.equal x.content y.content
  | Model_call x, Model_call y ->
      Int.equal x.turn y.turn
      && String.equal x.protocol y.protocol
      && String.equal x.model y.model
      && Int.equal x.status y.status
      && Option.equal usage_equal x.usage y.usage
      && Int.equal x.duration_ms y.duration_ms
      && Option.equal Int.equal x.ttft_ms y.ttft_ms
  | Tool_call x, Tool_call y ->
      Int.equal x.turn y.turn && Int.equal x.index y.index
      && String.equal x.name y.name
      && Canon.equal x.arguments y.arguments
      && join_equal x.mcp y.mcp
  | Tool_result x, Tool_result y ->
      Int.equal x.turn y.turn && Int.equal x.index y.index
      && String.equal x.content y.content
      && Bool.equal x.is_error y.is_error
  | Http_call x, Http_call y ->
      Int.equal x.turn y.turn && String.equal x.meth y.meth
      && String.equal x.host y.host && String.equal x.path y.path
      && Int.equal x.status y.status
  | Agent_event x, Agent_event y ->
      Int.equal x.turn y.turn && String.equal x.name y.name
      && Canon.equal x.data y.data
  | Reply x, Reply y ->
      Int.equal x.turn y.turn && String.equal x.content y.content
  | Sandbox_violation x, Sandbox_violation y ->
      String.equal x.kind y.kind && String.equal x.detail y.detail
  | Done, Done -> true
  | ( Run_start _,
      ( Turn_start _ | Model_call _ | Tool_call _ | Tool_result _ | Http_call _
      | Agent_event _ | Reply _ | Sandbox_violation _ | Done ) )
  | ( Turn_start _,
      ( Run_start _ | Model_call _ | Tool_call _ | Tool_result _ | Http_call _
      | Agent_event _ | Reply _ | Sandbox_violation _ | Done ) )
  | ( Model_call _,
      ( Run_start _ | Turn_start _ | Tool_call _ | Tool_result _ | Http_call _
      | Agent_event _ | Reply _ | Sandbox_violation _ | Done ) )
  | ( Tool_call _,
      ( Run_start _ | Turn_start _ | Model_call _ | Tool_result _ | Http_call _
      | Agent_event _ | Reply _ | Sandbox_violation _ | Done ) )
  | ( Tool_result _,
      ( Run_start _ | Turn_start _ | Model_call _ | Tool_call _ | Http_call _
      | Agent_event _ | Reply _ | Sandbox_violation _ | Done ) )
  | ( Http_call _,
      ( Run_start _ | Turn_start _ | Model_call _ | Tool_call _ | Tool_result _
      | Agent_event _ | Reply _ | Sandbox_violation _ | Done ) )
  | ( Agent_event _,
      ( Run_start _ | Turn_start _ | Model_call _ | Tool_call _ | Tool_result _
      | Http_call _ | Reply _ | Sandbox_violation _ | Done ) )
  | ( Reply _,
      ( Run_start _ | Turn_start _ | Model_call _ | Tool_call _ | Tool_result _
      | Http_call _ | Agent_event _ | Sandbox_violation _ | Done ) )
  | ( Sandbox_violation _,
      ( Run_start _ | Turn_start _ | Model_call _ | Tool_call _ | Tool_result _
      | Http_call _ | Agent_event _ | Reply _ | Done ) )
  | ( Done,
      ( Run_start _ | Turn_start _ | Model_call _ | Tool_call _ | Tool_result _
      | Http_call _ | Agent_event _ | Reply _ | Sandbox_violation _ ) ) ->
      false

let equal_entry a b =
  Int.equal a.seq b.seq && Int.equal a.t_ms b.t_ms
  && event_equal a.event b.event

let pp_error ppf = function
  | Invalid_json m -> Format.fprintf ppf "invalid JSON: %s" m
  | Unsupported_version n -> Format.fprintf ppf "unsupported trace version %d" n
  | Unknown_type t -> Format.fprintf ppf "unknown entry type %S" t
  | Bad_entry { type_; reason } ->
      Format.fprintf ppf "bad %S entry: %s" type_ reason
