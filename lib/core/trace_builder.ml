type agent_event =
  | Sent_turn of { t_ms : int; turn : int; content : string }
  | Got_reply of { t_ms : int; turn : int; content : string }
  | Got_event of { t_ms : int; turn : int; name : string; data : Canon.t }
  | Got_error of { t_ms : int; turn : int; message : string }

type input = {
  run_id : string;
  scenario : string;
  variant : string;
  sim_now : string;
  clock : Trace.clock;
  cassette : Cassette.t;
  agent : agent_event list;
  resolve : Cassette.payload -> string option;
}

module Int_set = Set.Make (Int)

type state = { next_call : int; emitted : Int_set.t }

let adapter_of name =
  List.find_opt
    (fun (module A : Protocol.S) -> String.equal A.name name)
    Protocols.all

let header_value name headers =
  List.find_map
    (fun (k, v) ->
      if String.equal (String.lowercase_ascii k) name then Some v else None)
    headers

let is_sse headers =
  match header_value "content-type" headers with
  | Some ct -> String.starts_with ~prefix:"text/event-stream" ct
  | None -> false

let response_bytes resolve (r : Cassette.response) =
  match r.body with
  | Whole { payload; complete = _ } -> resolve payload
  | Stream { chunks; complete = _ } ->
      Some (String.concat "" (List.map snd chunks))

let body_of ~streamed (r : Cassette.response) bytes : Protocol.body option =
  if is_sse r.headers then
    let st, items = Sse.feed Sse.init bytes in
    match Sse.finish st with `Clean -> Some (Sse items) | `Truncated _ -> None
  else
    match Canon.of_string bytes with
    | Error _ -> None
    | Ok v -> (
        match Canon.to_list_opt v with
        | Some items when streamed -> Some (Json_stream items)
        | Some _ | None -> Some (Json v))

let timing (r : Cassette.response) =
  match r.body with
  | Whole _ -> (0, None)
  | Stream { chunks; complete = _ } -> (
      let total = List.fold_left (fun acc (dt, _) -> acc + dt) 0 chunks in
      match chunks with [] -> (total, None) | (dt, _) :: _ -> (total, Some dt))

(* The raw request when it parses, else the canonical form. *)
let request_json resolve (ex : Cassette.exchange) =
  Option.bind (resolve ex.request) (fun s ->
      Result.to_option (Canon.of_string s))
  |> Option.value ~default:ex.canonical

let model_call_entries ~resolve ~turn st (ex : Cassette.exchange) =
  let adapter = adapter_of ex.protocol in
  let request = request_json resolve ex in
  let response =
    match ex.outcome with Response r -> Some r | Transport_error _ -> None
  in
  let summary =
    match (adapter, response) with
    | Some (module A : Protocol.S), Some r ->
        Option.bind (response_bytes resolve r) (fun bytes ->
            let streamed = A.streaming ~path:ex.path ~query:"" request in
            Option.bind (body_of ~streamed r bytes) (fun body ->
                Result.to_option (A.summarise body)))
    | Some _, None | None, (Some _ | None) -> None
  in
  let results =
    match adapter with
    | Some (module A : Protocol.S) -> A.tool_results request
    | None -> []
  in
  let fresh =
    List.filter
      (fun (r : Protocol.tool_result) ->
        not (Int_set.mem r.call_index st.emitted))
      results
  in
  let result_entries =
    List.map
      (fun (r : Protocol.tool_result) ->
        ( ex.t_ms,
          Trace.Tool_result
            {
              turn;
              index = r.call_index;
              content = r.content;
              is_error = r.is_error;
            } ))
      fresh
  in
  let duration_ms, ttft_ms =
    match response with Some r -> timing r | None -> (0, None)
  in
  let model =
    match Canon.str_member "model" ex.canonical with
    | Some m -> m
    | None -> (
        match Option.bind summary (fun (s : Protocol.summary) -> s.model) with
        | Some m -> m
        | None -> "")
  in
  let call =
    ( ex.t_ms,
      Trace.Model_call
        {
          turn;
          protocol = ex.protocol;
          model;
          status = (match response with Some r -> r.status | None -> 0);
          usage = Option.bind summary (fun (s : Protocol.summary) -> s.usage);
          duration_ms;
          ttft_ms;
        } )
  in
  let calls = match summary with None -> [] | Some s -> s.tool_calls in
  let call_entries =
    List.mapi
      (fun i (c : Protocol.tool_call) ->
        ( ex.t_ms + duration_ms,
          Trace.Tool_call
            {
              turn;
              index = st.next_call + i;
              name = c.name;
              arguments = c.arguments;
              mcp = Unjoined;
            } ))
      calls
  in
  let st =
    {
      next_call = st.next_call + List.length calls;
      emitted =
        List.fold_left
          (fun acc (r : Protocol.tool_result) -> Int_set.add r.call_index acc)
          st.emitted fresh;
    }
  in
  (st, result_entries @ (call :: call_entries))

(* MCP calls and the join with the model's tool calls (spec section 6) *)

type mcp_call = {
  at : int;
  server : string;
  tool : string;
  arguments : Canon.t;
}

let request_message resolve (ex : Cassette.exchange) =
  match
    Option.bind (resolve ex.request) (fun s ->
        Result.to_option (Mcp_msg.decode s))
  with
  | Some _ as m -> m
  | None ->
      Option.map
        (fun method_ ->
          Mcp_msg.Request
            {
              id = Int_id 0;
              method_;
              params = Canon.member "params" ex.canonical;
            })
        (Canon.str_member "method" ex.canonical)

let server_of_path path =
  let prefix = "/mcp/" in
  if String.starts_with ~prefix path then
    String.sub path (String.length prefix)
      (String.length path - String.length prefix)
  else path

let mcp_call resolve (ex : Cassette.exchange) =
  Option.bind (request_message resolve ex) (fun m ->
      Option.map
        (fun (tool, arguments) ->
          { at = ex.t_ms; server = server_of_path ex.path; tool; arguments })
        (Mcp_msg.tool_call m))

let same_call (name, args) (name', args') =
  String.equal name name' && Canon.equal args args'

let count_same key keys = List.length (List.filter (same_call key) keys)

(* The k-th call with a key joins the k-th record with it. The join is exact
   when the key is unique on both sides and by order otherwise. *)
let join_turn (body : (int * Trace.event) list) (calls : mcp_call list) =
  let model_key : Trace.event -> (string * Canon.t) option = function
    | Tool_call { name; arguments; _ } -> Some (name, arguments)
    | Run_start _ | Turn_start _ | Model_call _ | Tool_result _ | Http_call _
    | Agent_event _ | Reply _ | Sandbox_violation _ | Done ->
        None
  in
  let mcp_key c = (c.tool, c.arguments) in
  let model_keys = List.filter_map (fun (_, e) -> model_key e) body in
  let mcp_keys = List.map mcp_key calls in
  let _, joined =
    List.fold_left
      (fun (seen, acc) (t, (event : Trace.event)) ->
        match event with
        | Tool_call c ->
            let key = (c.name, c.arguments) in
            let records = count_same key mcp_keys in
            let mcp : Trace.join =
              if count_same key seen >= records then Unjoined
              else if count_same key model_keys = 1 && records = 1 then Joined
              else Joined_by_order
            in
            (key :: seen, (t, Trace.Tool_call { c with mcp }) :: acc)
        | Run_start _ | Turn_start _ | Model_call _ | Tool_result _
        | Http_call _ | Agent_event _ | Reply _ | Sandbox_violation _ | Done ->
            (seen, (t, event) :: acc))
      ([], []) body
  in
  let _, lonely =
    List.fold_left
      (fun (seen, acc) c ->
        let key = mcp_key c in
        if count_same key seen < count_same key model_keys then
          (key :: seen, acc)
        else (key :: seen, c :: acc))
      ([], []) calls
  in
  (List.rev joined, List.rev lonely)

let mcp_event ~turn c =
  match
    Canon.obj
      [
        ("server", Canon.string c.server);
        ("tool", Canon.string c.tool);
        ("arguments", c.arguments);
      ]
  with
  | Ok data -> [ (c.at, Trace.Agent_event { turn; name = "mcp_call"; data }) ]
  | Error _ -> []

(* Inserts before the first entry that happened later. *)
let rec insert_by_time ((t, _) as entry) = function
  | [] -> [ entry ]
  | ((t', _) as head) :: rest ->
      if t' > t then entry :: head :: rest
      else head :: insert_by_time entry rest

type item = Exchange of Cassette.exchange | Say of int * Trace.event

let item_time = function Exchange ex -> ex.t_ms | Say (t, _) -> t

let build (i : input) =
  let sent =
    List.filter_map
      (function
        | Sent_turn { t_ms; turn; content } -> Some (t_ms, turn, content)
        | Got_reply _ | Got_event _ | Got_error _ -> None)
      i.agent
  in
  let numbers = List.map (fun (_, n, _) -> n) sent in
  let last_turn = List.fold_left (fun _ n -> n) 0 numbers in
  let known n = if List.mem n numbers then n else last_turn in
  let first_turn = match numbers with n :: _ -> n | [] -> 0 in
  let turn_of_time t =
    List.fold_left
      (fun acc (t0, n, _) -> if t0 <= t then n else acc)
      first_turn sent
  in
  let exchanges =
    List.filter
      (fun (e : Cassette.exchange) ->
        match e.channel with Model -> true | Mcp | Http -> false)
      i.cassette.exchanges
  in
  let mcp_calls =
    List.filter_map
      (fun (e : Cassette.exchange) ->
        match e.channel with
        | Mcp -> mcp_call i.resolve e
        | Model | Http -> None)
      i.cassette.exchanges
  in
  let events_of turn =
    List.filter_map
      (function
        | Got_event { t_ms; turn = t; name; data } when known t = turn ->
            Some (Say (t_ms, Trace.Agent_event { turn; name; data }))
        | Got_event _ | Sent_turn _ | Got_reply _ | Got_error _ -> None)
      i.agent
  in
  let closing_of turn =
    List.filter_map
      (function
        | Got_reply { t_ms; turn = t; content } when known t = turn ->
            Some (t_ms, Trace.Reply { turn; content })
        | Got_error { t_ms; turn = t; message } when known t = turn -> (
            match Canon.obj [ ("message", Canon.string message) ] with
            | Ok data ->
                Some (t_ms, Trace.Agent_event { turn; name = "error"; data })
            | Error _ -> None)
        | Got_reply _ | Got_error _ | Sent_turn _ | Got_event _ -> None)
      i.agent
  in
  let group st turn start =
    let mine =
      List.filter
        (fun (e : Cassette.exchange) -> turn_of_time e.t_ms = turn)
        exchanges
    in
    let timeline =
      List.stable_sort
        (fun a b -> Int.compare (item_time a) (item_time b))
        (events_of turn @ List.map (fun e -> Exchange e) mine)
    in
    let st, body =
      List.fold_left
        (fun (st, acc) item ->
          match item with
          | Say (t, ev) -> (st, (t, ev) :: acc)
          | Exchange ex ->
              let st, es = model_call_entries ~resolve:i.resolve ~turn st ex in
              (st, List.rev_append es acc))
        (st, []) timeline
    in
    let body, lonely =
      join_turn (List.rev body)
        (List.filter (fun c -> turn_of_time c.at = turn) mcp_calls)
    in
    let body =
      List.fold_left
        (fun body c ->
          List.fold_left
            (fun body entry -> insert_by_time entry body)
            body (mcp_event ~turn c))
        body lonely
    in
    (st, start @ body @ closing_of turn)
  in
  let groups =
    match sent with
    | [] -> [ (0, None) ]
    | _ -> List.map (fun (t, n, c) -> (n, Some (t, c))) sent
  in
  let _, rev =
    List.fold_left
      (fun (st, acc) (turn, opening) ->
        let start =
          match opening with
          | Some (t, content) -> [ (t, Trace.Turn_start { turn; content }) ]
          | None -> []
        in
        let st, es = group st turn start in
        (st, List.rev_append es acc))
      ({ next_call = 0; emitted = Int_set.empty }, [])
      groups
  in
  let body = List.rev rev in
  let finish = List.fold_left (fun m (t, _) -> max m t) 0 body in
  let all =
    ( 0,
      Trace.Run_start
        {
          run_id = i.run_id;
          scenario = i.scenario;
          variant = i.variant;
          sim_now = i.sim_now;
          clock = i.clock;
        } )
    :: body
    @ [ (finish, Trace.Done) ]
  in
  let _, rev =
    List.fold_left
      (fun (prev, acc) (t, event) ->
        let t_ms = max prev t in
        (t_ms, (t_ms, event) :: acc))
      (0, []) all
  in
  List.mapi (fun seq (t_ms, event) -> { Trace.seq; t_ms; event }) (List.rev rev)
