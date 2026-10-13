open Bactrian_core
module P = Agent_protocol

let get_canon = function
  | Ok v -> v
  | Error _ -> Alcotest.fail "unexpected Canon error"

let doc s = get_canon (Canon.of_string s)

let error_equal a b =
  match (a, b) with
  | P.Invalid_json _, P.Invalid_json _ -> true
  | Unsupported_version x, Unsupported_version y -> Int.equal x y
  | Unknown_type x, Unknown_type y -> String.equal x y
  | Bad_message x, Bad_message y -> String.equal x.type_ y.type_
  | Invalid_json _, (Unsupported_version _ | Unknown_type _ | Bad_message _)
  | Unsupported_version _, (Invalid_json _ | Unknown_type _ | Bad_message _)
  | Unknown_type _, (Invalid_json _ | Unsupported_version _ | Bad_message _)
  | Bad_message _, (Invalid_json _ | Unsupported_version _ | Unknown_type _) ->
      false

let error_t = Alcotest.testable P.pp_error error_equal

let from_equal a b =
  match (a, b) with
  | P.Reply x, P.Reply y ->
      Int.equal x.turn y.turn && String.equal x.content y.content
  | Event x, Event y ->
      Int.equal x.turn y.turn && String.equal x.name y.name
      && Canon.equal x.data y.data
  | Agent_error x, Agent_error y ->
      Int.equal x.turn y.turn && String.equal x.message y.message
  | Reply _, (Event _ | Agent_error _)
  | Event _, (Reply _ | Agent_error _)
  | Agent_error _, (Reply _ | Event _) ->
      false

let from_pp ppf m = Fmt.string ppf (P.encode_from_agent m)
let from_t = Alcotest.testable from_pp from_equal

let to_equal a b =
  match (a, b) with
  | P.Start x, P.Start y ->
      String.equal x.run_id y.run_id
      && String.equal x.scenario y.scenario
      && String.equal x.variant y.variant
      && String.equal x.sim_now y.sim_now
      && String.equal x.state_dir y.state_dir
      && Option.equal
           (List.equal (fun (k, v) (k', v') ->
                String.equal k k' && String.equal v v'))
           x.env y.env
  | Turn x, Turn y ->
      Int.equal x.turn y.turn && String.equal x.content y.content
  | End, End -> true
  | Start _, (Turn _ | End) | Turn _, (Start _ | End) | End, (Start _ | Turn _)
    ->
      false

let to_t = Alcotest.testable (fun ppf m -> Fmt.string ppf (P.encode m)) to_equal
let decoded = Alcotest.result from_t error_t
let decoded_to = Alcotest.result to_t error_t

let start =
  P.Start
    {
      run_id = "r1";
      scenario = "s";
      variant = "v";
      sim_now = "2026-10-13T09:00:00Z";
      state_dir = "/tmp/x";
      env = None;
    }

let start_with_env =
  P.Start
    {
      run_id = "r1";
      scenario = "s";
      variant = "v";
      sim_now = "2026-10-13T09:00:00Z";
      state_dir = "/tmp/x";
      env = Some [ ("OPENAI_BASE_URL", "http://127.0.0.1:1/r/t/openai/v1") ];
    }

let to_msgs =
  [
    start;
    start_with_env;
    P.Turn { turn = 1; content = "hi \"there\"\n" };
    P.End;
  ]

let from_msgs =
  [
    P.Reply { turn = 2; content = "ok" };
    P.Event
      { turn = 1; name = "tool"; data = doc {|{"a":[1,2.5,null],"b":"x"}|} };
    P.Event { turn = 1; name = "empty"; data = Canon.empty_object };
    P.Agent_error { turn = 3; message = "gave up" };
  ]

let test_encode_wire () =
  Alcotest.(check string) "end" {|{"v":1,"type":"end"}|} (P.encode P.End);
  Alcotest.(check string)
    "turn" {|{"v":1,"type":"turn","turn":1,"content":"a\nb"}|}
    (P.encode (P.Turn { turn = 1; content = "a\nb" }));
  Alcotest.(check string)
    "start"
    {|{"v":1,"type":"start","run_id":"r1","scenario":"s","variant":"v","sim_now":"2026-10-13T09:00:00Z","state_dir":"/tmp/x"}|}
    (P.encode start);
  Alcotest.(check string)
    "start in HTTP mode"
    {|{"v":1,"type":"start","run_id":"r1","scenario":"s","variant":"v","sim_now":"2026-10-13T09:00:00Z","state_dir":"/tmp/x","env":{"OPENAI_BASE_URL":"http://127.0.0.1:1/r/t/openai/v1"}}|}
    (P.encode start_with_env);
  Alcotest.(check string)
    "reply" {|{"v":1,"type":"reply","turn":2,"content":"ok"}|}
    (P.encode_from_agent (P.Reply { turn = 2; content = "ok" }));
  Alcotest.(check string)
    "error" {|{"v":1,"type":"error","turn":3,"message":"m"}|}
    (P.encode_from_agent (P.Agent_error { turn = 3; message = "m" }));
  Alcotest.(check string)
    "event" {|{"v":1,"type":"event","turn":1,"name":"n","data":{"a":1}}|}
    (P.encode_from_agent
       (P.Event { turn = 1; name = "n"; data = doc {|{"a":1}|} }))

let test_one_line () =
  List.iter
    (fun m ->
      Alcotest.(check bool)
        "no newline" false
        (String.contains (P.encode m) '\n'))
    to_msgs;
  List.iter
    (fun m ->
      Alcotest.(check bool)
        "no newline" false
        (String.contains (P.encode_from_agent m) '\n'))
    from_msgs

let test_roundtrip_examples () =
  List.iter
    (fun m ->
      Alcotest.check decoded_to "to" (Ok m) (P.decode_to_agent (P.encode m)))
    to_msgs;
  List.iter
    (fun m ->
      Alcotest.check decoded "from" (Ok m) (P.decode (P.encode_from_agent m)))
    from_msgs

(* A start message without env is a command-mode one. *)
let test_start_env_optional () =
  let line =
    {|{"v":1,"type":"start","run_id":"r","scenario":"s","variant":"v","sim_now":"t","state_dir":"d"|}
  in
  Alcotest.(check bool)
    "absent" true
    (match P.decode_to_agent (line ^ "}") with
    | Ok (Start { env = None; _ }) -> true
    | Ok (Start _ | Turn _ | End) | Error _ -> false);
  Alcotest.(check bool)
    "empty object is not absent" true
    (match P.decode_to_agent (line ^ {|,"env":{}}|}) with
    | Ok (Start { env = Some []; _ }) -> true
    | Ok (Start _ | Turn _ | End) | Error _ -> false)

let test_array () =
  Alcotest.(check (result (list from_t) error_t))
    "empty" (Ok []) (P.decode_array "[]");
  Alcotest.(check (result (list from_t) error_t))
    "in order" (Ok from_msgs)
    (P.decode_array (P.encode_array from_msgs));
  List.iter
    (fun (name, body, error) ->
      Alcotest.(check (result (list from_t) error_t))
        name (Error error) (P.decode_array body))
    [
      ("not json", "[", P.Invalid_json "");
      ( "object",
        {|{"v":1,"type":"reply","turn":1,"content":"x"}|},
        P.Bad_message { type_ = ""; reason = "" } );
      ("empty body", "", P.Invalid_json "");
      ("not an envelope", "[1]", P.Bad_message { type_ = ""; reason = "" });
      ("unknown type", {|[{"v":1,"type":"ping"}]|}, P.Unknown_type "ping");
      ( "bad element after a good one",
        {|[{"v":1,"type":"reply","turn":1,"content":"x"},{"v":1,"type":"reply"}]|},
        P.Bad_message { type_ = "reply"; reason = "" } );
    ]

let test_decode_tolerates_whitespace_and_key_order () =
  Alcotest.check decoded "reordered"
    (Ok (P.Reply { turn = 1; content = "x" }))
    (P.decode {|  {"content":"x","turn":1,"type":"reply","v":1}  |})

let bad type_ = Error (P.Bad_message { type_; reason = "" })

let rejections =
  [
    ("not json", "nope", Error (P.Invalid_json ""));
    ("empty", "", Error (P.Invalid_json ""));
    ("truncated", {|{"v":1,"type":"reply"|}, Error (P.Invalid_json ""));
    ( "duplicate key",
      {|{"v":1,"v":1,"type":"reply","turn":1,"content":"x"}|},
      Error (P.Invalid_json "") );
    ("array", {|[1]|}, bad "");
    ("string", {|"reply"|}, bad "");
    ("missing v", {|{"type":"reply","turn":1,"content":"x"}|}, bad "");
    ("v not int", {|{"v":"1","type":"reply","turn":1,"content":"x"}|}, bad "");
    ( "v 2",
      {|{"v":2,"type":"reply","turn":1,"content":"x"}|},
      Error (P.Unsupported_version 2) );
    ( "v 0",
      {|{"v":0,"type":"reply","turn":1,"content":"x"}|},
      Error (P.Unsupported_version 0) );
    ("missing type", {|{"v":1,"turn":1,"content":"x"}|}, bad "");
    ("type not string", {|{"v":1,"type":3}|}, bad "");
    ("unknown type", {|{"v":1,"type":"ping"}|}, Error (P.Unknown_type "ping"));
    ( "to-agent type from agent",
      {|{"v":1,"type":"end"}|},
      Error (P.Unknown_type "end") );
    ( "turn type from agent",
      {|{"v":1,"type":"turn","turn":1,"content":"x"}|},
      Error (P.Unknown_type "turn") );
    ("reply missing turn", {|{"v":1,"type":"reply","content":"x"}|}, bad "reply");
    ("reply missing content", {|{"v":1,"type":"reply","turn":1}|}, bad "reply");
    ( "reply turn string",
      {|{"v":1,"type":"reply","turn":"1","content":"x"}|},
      bad "reply" );
    ( "reply turn float",
      {|{"v":1,"type":"reply","turn":1.5,"content":"x"}|},
      bad "reply" );
    ( "reply content int",
      {|{"v":1,"type":"reply","turn":1,"content":5}|},
      bad "reply" );
    ( "reply extra field",
      {|{"v":1,"type":"reply","turn":1,"content":"x","z":1}|},
      bad "reply" );
    ( "event missing data",
      {|{"v":1,"type":"event","turn":1,"name":"n"}|},
      bad "event" );
    ( "event data scalar",
      {|{"v":1,"type":"event","turn":1,"name":"n","data":3}|},
      bad "event" );
    ( "event data list",
      {|{"v":1,"type":"event","turn":1,"name":"n","data":[1]}|},
      bad "event" );
    ( "event missing name",
      {|{"v":1,"type":"event","turn":1,"data":{}}|},
      bad "event" );
    ("error missing message", {|{"v":1,"type":"error","turn":1}|}, bad "error");
    ( "error message null",
      {|{"v":1,"type":"error","turn":1,"message":null}|},
      bad "error" );
  ]

let to_rejections =
  [
    ( "start env not an object",
      {|{"v":1,"type":"start","run_id":"r","scenario":"s","variant":"v","sim_now":"t","state_dir":"d","env":["A"]}|},
      bad "start" );
    ( "start env value not a string",
      {|{"v":1,"type":"start","run_id":"r","scenario":"s","variant":"v","sim_now":"t","state_dir":"d","env":{"A":1}}|},
      bad "start" );
    ( "start missing field",
      {|{"v":1,"type":"start","run_id":"r","scenario":"s","variant":"v","sim_now":"t"}|},
      bad "start" );
    ( "start extra field",
      {|{"v":1,"type":"start","run_id":"r","scenario":"s","variant":"v","sim_now":"t","state_dir":"d","q":1}|},
      bad "start" );
    ( "start wrong type",
      {|{"v":1,"type":"start","run_id":1,"scenario":"s","variant":"v","sim_now":"t","state_dir":"d"}|},
      bad "start" );
    ("turn missing content", {|{"v":1,"type":"turn","turn":1}|}, bad "turn");
    ("end extra field", {|{"v":1,"type":"end","x":1}|}, bad "end");
    ("end with v 3", {|{"v":3,"type":"end"}|}, Error (P.Unsupported_version 3));
    ( "reply to agent",
      {|{"v":1,"type":"reply","turn":1,"content":"x"}|},
      Error (P.Unknown_type "reply") );
    ("unknown", {|{"v":1,"type":"stop"}|}, Error (P.Unknown_type "stop"));
    ("not json", "{", Error (P.Invalid_json ""));
  ]

let reject_cases dec t cases =
  List.map
    (fun (name, line, expected) ->
      Alcotest.test_case name `Quick (fun () ->
          Alcotest.check (Alcotest.result t error_t) name expected (dec line)))
    cases

(* Properties *)
let str_gen =
  QCheck.make ~print:Fun.id
    QCheck.Gen.(string_size ~gen:printable (int_range 0 40))

let turn_gen = QCheck.(int_range 0 1_000_000)

let canon_gen =
  QCheck.Gen.(
    let leaf =
      oneof
        [
          return Canon.null;
          map Canon.bool bool;
          map Canon.int (int_range (-1000) 1000);
          map Canon.string (string_size ~gen:printable (int_range 0 8));
        ]
    in
    let fields =
      map
        (fun l ->
          let l = List.sort_uniq (fun (a, _) (b, _) -> String.compare a b) l in
          match Canon.obj l with Ok o -> o | Error _ -> Canon.empty_object)
        (list_size (int_range 0 4)
           (pair (string_size ~gen:printable (int_range 0 5)) leaf))
    in
    fields)

let canon_arb = QCheck.make ~print:Canon.to_string canon_gen

let prop_from =
  QCheck.Test.make ~name:"from_agent round trip" ~count:300
    QCheck.(quad (int_range 0 2) turn_gen str_gen canon_arb)
    (fun (k, turn, s, d) ->
      let m =
        match k with
        | 0 -> P.Reply { turn; content = s }
        | 1 -> P.Event { turn; name = s; data = d }
        | _ -> P.Agent_error { turn; message = s }
      in
      match P.decode (P.encode_from_agent m) with
      | Ok m' -> from_equal m m'
      | Error _ -> false)

let prop_to =
  QCheck.Test.make ~name:"to_agent round trip" ~count:300
    QCheck.(quad (int_range 0 2) turn_gen str_gen str_gen)
    (fun (k, turn, a, b) ->
      let m =
        match k with
        | 0 ->
            P.Start
              {
                run_id = a;
                scenario = b;
                variant = a;
                sim_now = b;
                state_dir = a;
                env = (if turn mod 2 = 0 then None else Some [ (a, b) ]);
              }
        | 1 -> P.Turn { turn; content = a }
        | _ -> P.End
      in
      match P.decode_to_agent (P.encode m) with
      | Ok m' -> to_equal m m'
      | Error _ -> false)

let prop_no_exn =
  QCheck.Test.make ~name:"decode is total" ~count:300 str_gen (fun s ->
      match (P.decode s, P.decode_to_agent s) with _ -> true)

let test_pp_error () =
  let s e = Format.asprintf "%a" P.pp_error e in
  Alcotest.(check bool)
    "version names number" true
    (String.length (s (P.Unsupported_version 7)) > 0
    && String.contains (s (P.Unsupported_version 7)) '7');
  Alcotest.(check bool)
    "type named" true
    (let m = s (P.Unknown_type "zzz") in
     let n = String.length m in
     let rec has i =
       i + 3 <= n && (String.equal (String.sub m i 3) "zzz" || has (i + 1))
     in
     has 0)

let () =
  Alcotest.run "agent_protocol"
    [
      ( "wire",
        [
          Alcotest.test_case "encode" `Quick test_encode_wire;
          Alcotest.test_case "single line" `Quick test_one_line;
          Alcotest.test_case "round trip examples" `Quick
            test_roundtrip_examples;
          Alcotest.test_case "start env is optional" `Quick
            test_start_env_optional;
          Alcotest.test_case "response arrays" `Quick test_array;
          Alcotest.test_case "key order and whitespace" `Quick
            test_decode_tolerates_whitespace_and_key_order;
          Alcotest.test_case "pp_error" `Quick test_pp_error;
        ] );
      ("reject from_agent", reject_cases P.decode from_t rejections);
      ("reject to_agent", reject_cases P.decode_to_agent to_t to_rejections);
      ( "properties",
        List.map
          (fun t -> QCheck_alcotest.to_alcotest t)
          [ prop_from; prop_to; prop_no_exn ] );
    ]
