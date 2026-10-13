open Bactrian_core
module B = Trace_builder

let fixtures_root = "../fixtures/protocols/openai_chat"
let read path = In_channel.with_open_bin path In_channel.input_all

let canon s =
  match Canon.of_string s with
  | Ok v -> v
  | Error e -> Alcotest.failf "bad JSON: %a" Canon.pp_error e

let exchange ~seq ~t_ms ?(case_request = None) case : Cassette.exchange =
  let file name = Filename.concat (Filename.concat fixtures_root case) name in
  let request =
    Option.value case_request ~default:(read (file "request.json"))
  in
  let canonical =
    match Protocols.find ~meth:"POST" ~path:"/v1/chat/completions" with
    | Some (module A : Protocol.S) -> (
        match
          A.canonical_request ~path:"/v1/chat/completions" (canon request)
        with
        | Ok c -> c
        | Error e -> Alcotest.failf "canonical: %a" Protocol.pp_error e)
    | None -> Alcotest.fail "no adapter"
  in
  {
    seq;
    t_ms;
    channel = Model;
    protocol = "openai_chat";
    meth = "POST";
    path = "/v1/chat/completions";
    query = [];
    key = "k" ^ string_of_int seq;
    occurrence = 0;
    canonical;
    request = Inline request;
    outcome =
      Response
        {
          status = 200;
          headers = [ ("content-type", "application/json") ];
          body =
            Whole
              { payload = Inline (read (file "response.raw")); complete = true };
        };
  }

let cassette exchanges : Cassette.t =
  {
    header =
      {
        run_id = "r";
        scenario = "s";
        variant = "v";
        bactrian_version = "test";
        recorded_at = "2026-10-13T00:00:00Z";
        sim_now = "2026-10-13T00:00:00Z";
        clock = Contract_only;
      };
    exchanges;
    complete = true;
  }

let resolve : Cassette.payload -> string option = function
  | Inline s -> Some s
  | Blob _ -> None

let input ?(cassette = cassette []) agent : B.input =
  {
    run_id = "r";
    scenario = "s";
    variant = "v";
    sim_now = "2026-10-13T00:00:00Z";
    clock = Contract_only;
    cassette;
    agent;
    resolve;
  }

let kind (e : Trace.entry) =
  match e.event with
  | Run_start _ -> "run_start"
  | Turn_start { turn; _ } -> Printf.sprintf "turn_start %d" turn
  | Model_call { turn; _ } -> Printf.sprintf "model_call %d" turn
  | Tool_call { turn; name; _ } -> Printf.sprintf "tool_call %d %s" turn name
  | Tool_result { turn; index; _ } ->
      Printf.sprintf "tool_result %d #%d" turn index
  | Http_call _ -> "http_call"
  | Agent_event { name; _ } -> "event " ^ name
  | Reply { turn; _ } -> Printf.sprintf "reply %d" turn
  | Sandbox_violation _ -> "sandbox_violation"
  | Done -> "done"

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let check_has line frag =
  if not (contains line frag) then Alcotest.failf "%s lacks %s" line frag

let kinds entries = List.map kind entries
let strs = Alcotest.(list string)

let two_calls =
  [
    exchange ~seq:0 ~t_ms:100 "luna_nonstream";
    exchange ~seq:1 ~t_ms:300 "luna_turn2";
  ]

let agent_one_turn =
  [
    B.Sent_turn { t_ms = 50; turn = 1; content = "weather?" };
    Got_event { t_ms = 400; turn = 1; name = "note"; data = Canon.null };
    Got_reply { t_ms = 500; turn = 1; content = "18C" };
  ]

let test_sequence () =
  let entries = B.build (input ~cassette:(cassette two_calls) agent_one_turn) in
  Alcotest.check strs "sequence"
    [
      "run_start";
      "turn_start 1";
      "model_call 1";
      "tool_call 1 get_weather";
      "tool_result 1 #0";
      "model_call 1";
      "event note";
      "reply 1";
      "done";
    ]
    (kinds entries);
  Alcotest.(check (list int))
    "seqs"
    (List.init (List.length entries) Fun.id)
    (List.map (fun (e : Trace.entry) -> e.seq) entries);
  let ts = List.map (fun (e : Trace.entry) -> e.t_ms) entries in
  Alcotest.(check bool)
    "t_ms never decreases" true
    (List.sort Int.compare ts = ts);
  let line = Trace.to_line (List.nth entries 2) in
  List.iter
    (fun frag -> check_has line frag)
    [
      {|"gen_ai.usage.input_tokens":145|};
      {|"gen_ai.usage.output_tokens":17|};
      {|"gen_ai.request.model":"gpt-6-luna"|};
      {|"status":200|};
      {|"protocol":"openai_chat"|};
    ]

let test_tool_details () =
  let entries = B.build (input ~cassette:(cassette two_calls) agent_one_turn) in
  check_has (Trace.to_line (List.nth entries 3)) {|"city":"Paris"|};
  check_has (Trace.to_line (List.nth entries 3)) {|"index":0|};
  check_has (Trace.to_line (List.nth entries 4)) "18 degrees Celsius, cloudy";
  check_has (Trace.to_line (List.nth entries 4)) {|"is_error":false|}

let test_results_not_repeated () =
  let exchanges = two_calls @ [ exchange ~seq:2 ~t_ms:1200 "luna_turn2" ] in
  let agent =
    [
      B.Sent_turn { t_ms = 50; turn = 1; content = "weather?" };
      Got_reply { t_ms = 500; turn = 1; content = "18C" };
      Sent_turn { t_ms = 1000; turn = 2; content = "again" };
      Got_reply { t_ms = 1500; turn = 2; content = "still 18C" };
    ]
  in
  let entries = B.build (input ~cassette:(cassette exchanges) agent) in
  Alcotest.check strs "sequence"
    [
      "run_start";
      "turn_start 1";
      "model_call 1";
      "tool_call 1 get_weather";
      "tool_result 1 #0";
      "model_call 1";
      "reply 1";
      "turn_start 2";
      "model_call 2";
      "reply 2";
      "done";
    ]
    (kinds entries)

let test_unparseable_and_transport_error () =
  let broken =
    let e = exchange ~seq:0 ~t_ms:100 "luna_nonstream" in
    {
      e with
      outcome =
        Response
          {
            status = 502;
            headers = [];
            body = Whole { payload = Inline "not json"; complete = true };
          };
    }
  in
  let failed =
    {
      (exchange ~seq:1 ~t_ms:200 "luna_nonstream") with
      outcome = Transport_error "refused";
    }
  in
  let entries =
    B.build
      (input
         ~cassette:(cassette [ broken; failed ])
         [ Sent_turn { t_ms = 0; turn = 1; content = "x" } ])
  in
  Alcotest.check strs "sequence"
    [ "run_start"; "turn_start 1"; "model_call 1"; "model_call 1"; "done" ]
    (kinds entries);
  let line n = Trace.to_line (List.nth entries n) in
  check_has (line 2) {|"status":502|};
  check_has (line 3) {|"status":0|};
  check_has (line 3) {|"gen_ai.request.model":"gpt-6-luna"|};
  List.iter
    (fun n ->
      Alcotest.(check bool) "no usage" false (contains (line n) "gen_ai.usage"))
    [ 2; 3 ]

let test_agent_error_and_no_turns () =
  let entries =
    B.build
      (input
         [
           Sent_turn { t_ms = 0; turn = 1; content = "x" };
           Got_error { t_ms = 5; turn = 1; message = "boom" };
         ])
  in
  Alcotest.check strs "error"
    [ "run_start"; "turn_start 1"; "event error"; "done" ]
    (kinds entries);
  Alcotest.check strs "empty run" [ "run_start"; "done" ]
    (kinds (B.build (input [])))

let test_exchanges_before_first_turn () =
  let entries =
    B.build
      (input
         ~cassette:(cassette [ exchange ~seq:0 ~t_ms:10 "luna_nonstream" ])
         [ Sent_turn { t_ms = 50; turn = 1; content = "x" } ])
  in
  Alcotest.check strs "belongs to the first turn"
    [
      "run_start";
      "turn_start 1";
      "model_call 1";
      "tool_call 1 get_weather";
      "done";
    ]
    (kinds entries)

(* MCP joins (design §6) *)

let chat_calls ~seq ~t_ms calls =
  let base = exchange ~seq ~t_ms "luna_nonstream" in
  let call i (name, arguments) =
    `Assoc
      [
        ("id", `String (Printf.sprintf "call_%d" i));
        ("type", `String "function");
        ( "function",
          `Assoc [ ("name", `String name); ("arguments", `String arguments) ] );
      ]
  in
  let body =
    `Assoc
      [
        ("id", `String "x");
        ("object", `String "chat.completion");
        ("created", `Int 1);
        ("model", `String "gpt-6-luna");
        ( "choices",
          `List
            [
              `Assoc
                [
                  ("index", `Int 0);
                  ( "message",
                    `Assoc
                      [
                        ("role", `String "assistant");
                        ("content", `Null);
                        ("tool_calls", `List (List.mapi call calls));
                      ] );
                  ("finish_reason", `String "tool_calls");
                ];
            ] );
        ( "usage",
          `Assoc
            [
              ("prompt_tokens", `Int 1);
              ("completion_tokens", `Int 1);
              ("total_tokens", `Int 2);
            ] );
      ]
  in
  {
    base with
    outcome =
      Response
        {
          status = 200;
          headers = [ ("content-type", "application/json") ];
          body =
            Whole
              { payload = Inline (Yojson.Safe.to_string body); complete = true };
        };
  }

let mcp_request ~seq ~t_ms ?(server = "weather") ?(transport_error = false)
    method_ params : Cassette.exchange =
  let message =
    Mcp_msg.Request { id = Int_id seq; method_; params = Some (canon params) }
  in
  let canonical =
    match Mcp_msg.canonical_request message with
    | Some c -> c
    | None -> Alcotest.fail "no canonical form"
  in
  {
    seq;
    t_ms;
    channel = Mcp;
    protocol = "mcp";
    meth = "POST";
    path = "/mcp/" ^ server;
    query = [];
    key = "m" ^ string_of_int seq;
    occurrence = 0;
    canonical;
    request = Inline (Mcp_msg.encode message);
    outcome =
      (if transport_error then Transport_error "server crashed"
       else
         Response
           {
             status = 200;
             headers = [];
             body = Whole { payload = Inline "{}"; complete = true };
           });
  }

let mcp_tool ~seq ~t_ms ?server ?transport_error tool arguments =
  mcp_request ~seq ~t_ms ?server ?transport_error "tools/call"
    (Printf.sprintf {|{"name":%S,"arguments":%s}|} tool arguments)

let join_name : Trace.join -> string = function
  | Joined -> "joined"
  | Joined_by_order -> "joined_by_order"
  | Unjoined -> "unjoined"

(* Every tool call as "name join", then every MCP-only event as its data. *)
let joins entries =
  List.filter_map
    (fun (e : Trace.entry) ->
      match e.event with
      | Tool_call { name; mcp; _ } -> Some (name ^ " " ^ join_name mcp)
      | Run_start _ | Turn_start _ | Model_call _ | Tool_result _ | Http_call _
      | Agent_event _ | Reply _ | Sandbox_violation _ | Done ->
          None)
    entries

let unjoined_mcp entries =
  List.filter_map
    (fun (e : Trace.entry) ->
      match e.event with
      | Agent_event { name = "mcp_call"; data; _ } ->
          Some (Canon.to_string data)
      | Run_start _ | Turn_start _ | Model_call _ | Tool_call _ | Tool_result _
      | Http_call _ | Agent_event _ | Reply _ | Sandbox_violation _ | Done ->
          None)
    entries

let one_turn = [ B.Sent_turn { t_ms = 0; turn = 1; content = "x" } ]

let build_with exchanges agent =
  B.build (input ~cassette:(cassette exchanges) agent)

let paris = {|{"city":"Paris"}|}
let paris_args = {|{"city": "Paris"}|}

let test_join_single () =
  let entries =
    build_with
      [
        chat_calls ~seq:0 ~t_ms:100 [ ("get_weather", paris) ];
        mcp_tool ~seq:1 ~t_ms:120 "get_weather" paris_args;
      ]
      one_turn
  in
  Alcotest.check strs "joined" [ "get_weather joined" ] (joins entries);
  Alcotest.check strs "nothing on its own" [] (unjoined_mcp entries);
  Alcotest.check strs "no extra entries"
    [
      "run_start";
      "turn_start 1";
      "model_call 1";
      "tool_call 1 get_weather";
      "done";
    ]
    (kinds entries)

let test_join_by_order () =
  let entries =
    build_with
      [
        chat_calls ~seq:0 ~t_ms:100
          [ ("get_weather", paris); ("get_weather", paris) ];
        mcp_tool ~seq:1 ~t_ms:120 "get_weather" paris_args;
        mcp_tool ~seq:2 ~t_ms:121 "get_weather" paris_args;
      ]
      one_turn
  in
  Alcotest.check strs "several identical candidates"
    [ "get_weather joined_by_order"; "get_weather joined_by_order" ]
    (joins entries);
  Alcotest.check strs "nothing on its own" [] (unjoined_mcp entries)

let test_join_arguments_decide () =
  let entries =
    build_with
      [
        chat_calls ~seq:0 ~t_ms:100
          [ ("get_weather", paris); ("get_weather", {|{"city":"Rome"}|}) ];
        mcp_tool ~seq:1 ~t_ms:120 "get_weather" {|{"city": "Rome"}|};
        mcp_tool ~seq:2 ~t_ms:121 "get_time" paris_args;
      ]
      one_turn
  in
  Alcotest.check strs "only the Rome call has an MCP record"
    [ "get_weather unjoined"; "get_weather joined" ]
    (joins entries);
  Alcotest.check strs "the other MCP call appears on its own"
    [ {|{"arguments":{"city":"Paris"},"server":"weather","tool":"get_time"}|} ]
    (unjoined_mcp entries)

let test_join_surplus () =
  let entries =
    build_with
      [
        chat_calls ~seq:0 ~t_ms:100 [ ("get_weather", paris) ];
        mcp_tool ~seq:1 ~t_ms:120 "get_weather" paris_args;
        mcp_tool ~seq:2 ~t_ms:130 "get_weather" paris_args;
      ]
      one_turn
  in
  Alcotest.check strs "the first record joins, by order"
    [ "get_weather joined_by_order" ]
    (joins entries);
  Alcotest.(check int)
    "the second is on its own" 1
    (List.length (unjoined_mcp entries))

let test_join_within_the_turn () =
  let entries =
    build_with
      [
        chat_calls ~seq:0 ~t_ms:100 [ ("get_weather", paris) ];
        mcp_tool ~seq:1 ~t_ms:1100 "get_weather" paris_args;
      ]
      [
        B.Sent_turn { t_ms = 0; turn = 1; content = "a" };
        Got_reply { t_ms = 500; turn = 1; content = "b" };
        Sent_turn { t_ms = 1000; turn = 2; content = "c" };
        Got_reply { t_ms = 1500; turn = 2; content = "d" };
      ]
  in
  Alcotest.check strs "a record in turn 2 cannot join turn 1's call"
    [ "get_weather unjoined" ] (joins entries);
  Alcotest.check strs "and appears in turn 2"
    [
      "run_start";
      "turn_start 1";
      "model_call 1";
      "tool_call 1 get_weather";
      "reply 1";
      "turn_start 2";
      "event mcp_call";
      "reply 2";
      "done";
    ]
    (kinds entries)

let test_join_ignores_other_traffic () =
  let entries =
    build_with
      [
        chat_calls ~seq:0 ~t_ms:100 [ ("get_weather", paris) ];
        mcp_request ~seq:1 ~t_ms:110 "tools/list" "{}";
        mcp_request ~seq:2 ~t_ms:111 "server/discover" "{}";
        mcp_tool ~seq:3 ~t_ms:120 ~transport_error:true "get_weather" paris_args;
      ]
      one_turn
  in
  Alcotest.check strs "a failed call still joins" [ "get_weather joined" ]
    (joins entries);
  Alcotest.check strs "no entry for the other methods" [] (unjoined_mcp entries)

let test_join_in_time_order () =
  let entries =
    build_with
      [
        chat_calls ~seq:0 ~t_ms:100 [ ("get_weather", paris) ];
        mcp_tool ~seq:1 ~t_ms:150 "get_time" "{}";
        exchange ~seq:2 ~t_ms:300 "luna_turn2";
      ]
      one_turn
  in
  Alcotest.check strs "between the model calls"
    [
      "run_start";
      "turn_start 1";
      "model_call 1";
      "tool_call 1 get_weather";
      "event mcp_call";
      "tool_result 1 #0";
      "model_call 1";
      "done";
    ]
    (kinds entries);
  Alcotest.(check bool)
    "t_ms never decreases" true
    (let ts = List.map (fun (e : Trace.entry) -> e.t_ms) entries in
     List.sort Int.compare ts = ts)

let test_no_mcp_means_unjoined () =
  Alcotest.check strs "unchanged" [ "get_weather unjoined" ]
    (joins (build_with two_calls one_turn))

let () =
  Alcotest.run "trace_builder"
    [
      ( "build",
        [
          Alcotest.test_case "two-call tool-use sequence" `Quick test_sequence;
          Alcotest.test_case "tool call and result details" `Quick
            test_tool_details;
          Alcotest.test_case "tool results are not repeated" `Quick
            test_results_not_repeated;
          Alcotest.test_case "unparseable body and transport error" `Quick
            test_unparseable_and_transport_error;
          Alcotest.test_case "agent error and empty run" `Quick
            test_agent_error_and_no_turns;
          Alcotest.test_case "exchange before the first turn" `Quick
            test_exchanges_before_first_turn;
        ] );
      ( "mcp join",
        [
          Alcotest.test_case "one record, one call" `Quick test_join_single;
          Alcotest.test_case "identical candidates join by order" `Quick
            test_join_by_order;
          Alcotest.test_case "arguments and name decide" `Quick
            test_join_arguments_decide;
          Alcotest.test_case "surplus record is on its own" `Quick
            test_join_surplus;
          Alcotest.test_case "joins stay within the turn" `Quick
            test_join_within_the_turn;
          Alcotest.test_case "other methods and failures" `Quick
            test_join_ignores_other_traffic;
          Alcotest.test_case "placed in time order" `Quick
            test_join_in_time_order;
          Alcotest.test_case "no MCP traffic" `Quick test_no_mcp_means_unjoined;
        ] );
    ]
