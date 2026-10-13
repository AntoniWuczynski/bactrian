open Bactrian_core

let get_canon = function
  | Ok v -> v
  | Error _ -> Alcotest.fail "unexpected Canon error"

let doc s = get_canon (Canon.of_string s)

let error_equal a b =
  match (a, b) with
  | Trace.Invalid_json _, Trace.Invalid_json _ -> true
  | Unsupported_version x, Unsupported_version y -> Int.equal x y
  | Unknown_type x, Unknown_type y -> String.equal x y
  | Bad_entry _, Bad_entry _ -> true
  | Invalid_json _, (Unsupported_version _ | Unknown_type _ | Bad_entry _)
  | Unsupported_version _, (Invalid_json _ | Unknown_type _ | Bad_entry _)
  | Unknown_type _, (Invalid_json _ | Unsupported_version _ | Bad_entry _)
  | Bad_entry _, (Invalid_json _ | Unsupported_version _ | Unknown_type _) ->
      false

let entry_t =
  Alcotest.testable
    (fun ppf e -> Fmt.string ppf (Trace.to_line e))
    Trace.equal_entry

let error_t = Alcotest.testable Trace.pp_error error_equal
let parsed = Alcotest.result entry_t error_t

let full_usage =
  {
    Trace.input_tokens = 10;
    output_tokens = 20;
    cached_input_tokens = Some 3;
    cache_write_tokens = Some 4;
    reasoning_tokens = Some 5;
  }

let bare_usage =
  {
    Trace.input_tokens = 1;
    output_tokens = 2;
    cached_input_tokens = None;
    cache_write_tokens = None;
    reasoning_tokens = None;
  }

let events : Trace.event list =
  [
    Run_start
      {
        run_id = "r1";
        scenario = "s";
        variant = "v";
        sim_now = "2026-01-01T00:00:00Z";
        clock = Enforced;
      };
    Run_start
      {
        run_id = "r2";
        scenario = "s";
        variant = "v";
        sim_now = "t";
        clock = Contract_only;
      };
    Turn_start { turn = 1; content = "hello \"world\"\n" };
    Model_call
      {
        turn = 1;
        protocol = "openai";
        model = "m";
        status = 200;
        usage = Some full_usage;
        duration_ms = 100;
        ttft_ms = Some 20;
      };
    Model_call
      {
        turn = 1;
        protocol = "openai";
        model = "m";
        status = 500;
        usage = Some bare_usage;
        duration_ms = 100;
        ttft_ms = None;
      };
    Model_call
      {
        turn = 2;
        protocol = "anthropic";
        model = "m";
        status = 429;
        usage = None;
        duration_ms = 5;
        ttft_ms = None;
      };
    Tool_call
      {
        turn = 1;
        index = 0;
        name = "send";
        arguments = doc "{\"b\":[1,2.5],\"a\":null}";
        mcp = Joined;
      };
    Tool_call
      {
        turn = 1;
        index = 1;
        name = "send";
        arguments = Canon.null;
        mcp = Joined_by_order;
      };
    Tool_call
      {
        turn = 1;
        index = 2;
        name = "send";
        arguments = Canon.string "x";
        mcp = Unjoined;
      };
    Tool_result { turn = 1; index = 0; content = "ok"; is_error = false };
    Tool_result { turn = 1; index = 1; content = "bad"; is_error = true };
    Http_call
      {
        turn = 1;
        meth = "GET";
        host = "example.com";
        path = "/a";
        status = 200;
      };
    Agent_event { turn = 1; name = "thought"; data = doc "{\"k\":true}" };
    Reply { turn = 1; content = "done" };
    Sandbox_violation { kind = "network"; detail = "blocked" };
    Done;
  ]

let entry_of i event = { Trace.seq = i; t_ms = i * 10; event }

let test_round_trip () =
  List.iteri
    (fun i ev ->
      let e = entry_of i ev in
      Alcotest.check parsed "round trip" (Ok e)
        (Trace.of_line (Trace.to_line e)))
    events

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i =
    i + n <= h && (String.equal (String.sub hay i n) needle || go (i + 1))
  in
  go 0

let check_has line s = Alcotest.(check bool) s true (contains line s)

let check_lacks line s =
  Alcotest.(check bool) ("no " ^ s) false (contains line s)

let test_model_call_wire () =
  let line =
    Trace.to_line
      (entry_of 3
         (Model_call
            {
              turn = 1;
              protocol = "openai";
              model = "m";
              status = 200;
              usage = Some bare_usage;
              duration_ms = 100;
              ttft_ms = None;
            }))
  in
  check_has line "\"gen_ai.request.model\":\"m\"";
  check_has line "\"gen_ai.usage.input_tokens\":1";
  check_has line "\"type\":\"model_call\"";
  check_has line "\"v\":1";
  check_lacks line "ttft_ms";
  check_lacks line "cached_input_tokens";
  check_lacks line "null"

let test_usage_wire_names () =
  let line =
    Trace.to_line
      (entry_of 0
         (Model_call
            {
              turn = 1;
              protocol = "p";
              model = "m";
              status = 200;
              usage = Some full_usage;
              duration_ms = 1;
              ttft_ms = None;
            }))
  in
  check_has line "\"gen_ai.usage.cache_read.input_tokens\":3";
  check_has line "\"gen_ai.usage.cache_write.input_tokens\":4";
  check_has line "\"gen_ai.usage.reasoning.output_tokens\":5";
  check_lacks line "cached_input_tokens";
  check_lacks line "cache_creation";
  check_lacks line "reasoning_tokens"

let test_other_wire () =
  let line_of ev = Trace.to_line (entry_of 0 ev) in
  check_has
    (line_of
       (Http_call { turn = 1; meth = "GET"; host = "h"; path = "/"; status = 1 }))
    "\"method\":\"GET\"";
  check_has (line_of Done) "\"type\":\"done\"";
  check_has
    (line_of
       (Run_start
          {
            run_id = "r";
            scenario = "s";
            variant = "v";
            sim_now = "t";
            clock = Contract_only;
          }))
    "\"clock\":\"contract_only\"";
  check_has
    (line_of
       (Tool_call
          {
            turn = 1;
            index = 0;
            name = "n";
            arguments = Canon.null;
            mcp = Joined_by_order;
          }))
    "\"mcp\":\"joined_by_order\""

let test_stable_order () =
  let line = Trace.to_line (entry_of 0 Done) in
  Alcotest.(check string)
    "canonical" "{\"seq\":0,\"t_ms\":0,\"type\":\"done\",\"v\":1}" line

let contains hay needle =
  match Re.exec_opt (Re.compile (Re.str needle)) hay with
  | Some _ -> true
  | None -> false

(* A rejected entry must carry the entry type and a reason naming the cause. *)
let check_bad name ~type_ ~mentions line =
  match Trace.of_line line with
  | Error (Trace.Bad_entry { type_ = t; reason }) ->
      Alcotest.(check string) (name ^ " type") type_ t;
      if not (contains reason mentions) then
        Alcotest.failf "%s: reason %S does not mention %S" name reason mentions
  | Error (Invalid_json _ | Unsupported_version _ | Unknown_type _) | Ok _ ->
      Alcotest.failf "%s: expected Bad_entry" name

let test_rejections () =
  check_bad "unknown field" ~type_:"done" ~mentions:"no fields"
    "{\"v\":1,\"seq\":0,\"t_ms\":0,\"type\":\"done\",\"extra\":1}";
  (* The decoder reports only the record type, so a control without the extra
     field shows the extra field is the cause. *)
  Alcotest.(check bool)
    "control decodes" true
    (Result.is_ok
       (Trace.of_line
          "{\"v\":1,\"seq\":0,\"t_ms\":0,\"type\":\"reply\",\"turn\":1,\"content\":\"a\"}"));
  check_bad "unknown field in payload" ~type_:"reply" ~mentions:"reply_w"
    "{\"v\":1,\"seq\":0,\"t_ms\":0,\"type\":\"reply\",\"turn\":1,\"content\":\"a\",\"extra\":1}";
  Alcotest.check parsed "unknown type" (Error (Trace.Unknown_type "x"))
    (Trace.of_line "{\"v\":1,\"seq\":0,\"t_ms\":0,\"type\":\"x\"}");
  Alcotest.check parsed "v 2" (Error (Trace.Unsupported_version 2))
    (Trace.of_line "{\"v\":2,\"seq\":0,\"t_ms\":0,\"type\":\"done\"}");
  Alcotest.check parsed "malformed" (Error (Trace.Invalid_json ""))
    (Trace.of_line "{");
  Alcotest.check parsed "duplicate key" (Error (Trace.Invalid_json ""))
    (Trace.of_line "{\"v\":1,\"v\":1,\"seq\":0,\"t_ms\":0,\"type\":\"done\"}");
  check_bad "missing field" ~type_:"reply" ~mentions:"content"
    "{\"v\":1,\"seq\":0,\"t_ms\":0,\"type\":\"reply\",\"turn\":1}";
  check_bad "missing envelope" ~type_:"done" ~mentions:"t_ms"
    "{\"v\":1,\"seq\":0,\"type\":\"done\"}";
  check_bad "not an object" ~type_:"" ~mentions:"object" "[1]";
  check_bad "half usage" ~type_:"model_call" ~mentions:"output_tokens"
    "{\"v\":1,\"seq\":0,\"t_ms\":0,\"type\":\"model_call\",\"turn\":1,\"protocol\":\"p\",\"gen_ai.request.model\":\"m\",\"status\":200,\"duration_ms\":1,\"gen_ai.usage.input_tokens\":1}";
  check_bad "bad clock" ~type_:"run_start" ~mentions:"clock"
    "{\"v\":1,\"seq\":0,\"t_ms\":0,\"type\":\"run_start\",\"run_id\":\"r\",\"scenario\":\"s\",\"variant\":\"v\",\"sim_now\":\"t\",\"clock\":\"nope\"}"

let test_error_type_carried () =
  match
    Trace.of_line "{\"v\":1,\"seq\":0,\"t_ms\":0,\"type\":\"reply\",\"turn\":1}"
  with
  | Error (Trace.Bad_entry { type_; _ }) ->
      Alcotest.(check string) "type" "reply" type_
  | Error (Invalid_json _ | Unsupported_version _ | Unknown_type _) | Ok _ ->
      Alcotest.fail "expected Bad_entry"

(* Generators *)
let gen_small_string = QCheck.Gen.(string_size ~gen:printable (int_range 0 8))
let gen_nat = QCheck.Gen.int_range 0 100000

let gen_canon : Canon.t QCheck.Gen.t =
  let open QCheck.Gen in
  let leaf =
    oneof
      [
        return Canon.null;
        map Canon.bool bool;
        map Canon.int (int_range (-1000) 1000);
        map Canon.string gen_small_string;
      ]
  in
  let obj_of kvs =
    match Canon.obj kvs with Ok v -> v | Error _ -> Canon.null
  in
  oneof_weighted
    [
      (3, leaf);
      (1, map Canon.list (list_size (int_range 0 3) leaf));
      (1, map obj_of (list_size (int_range 0 3) (pair gen_small_string leaf)));
    ]

let gen_usage : Trace.usage QCheck.Gen.t =
  let open QCheck.Gen in
  let* input_tokens = gen_nat in
  let* output_tokens = gen_nat in
  let* cached_input_tokens = option gen_nat in
  let* cache_write_tokens = option gen_nat in
  let* reasoning_tokens = option gen_nat in
  return
    {
      Trace.input_tokens;
      output_tokens;
      cached_input_tokens;
      cache_write_tokens;
      reasoning_tokens;
    }

let gen_event : Trace.event QCheck.Gen.t =
  let open QCheck.Gen in
  let s = gen_small_string in
  oneof_weighted
    [
      ( 1,
        let* run_id = s in
        let* scenario = s in
        let* variant = s in
        let* sim_now = s in
        let* clock = oneof_list [ Trace.Enforced; Contract_only ] in
        return (Trace.Run_start { run_id; scenario; variant; sim_now; clock })
      );
      ( 1,
        let* turn = gen_nat in
        let* content = s in
        return (Trace.Turn_start { turn; content }) );
      ( 2,
        let* turn = gen_nat in
        let* protocol = s in
        let* model = s in
        let* status = gen_nat in
        let* usage = option gen_usage in
        let* duration_ms = gen_nat in
        let* ttft_ms = option gen_nat in
        return
          (Trace.Model_call
             { turn; protocol; model; status; usage; duration_ms; ttft_ms }) );
      ( 2,
        let* turn = gen_nat in
        let* index = gen_nat in
        let* name = s in
        let* arguments = gen_canon in
        let* mcp = oneof_list [ Trace.Joined; Joined_by_order; Unjoined ] in
        return (Trace.Tool_call { turn; index; name; arguments; mcp }) );
      ( 1,
        let* turn = gen_nat in
        let* index = gen_nat in
        let* content = s in
        let* is_error = bool in
        return (Trace.Tool_result { turn; index; content; is_error }) );
      ( 1,
        let* turn = gen_nat in
        let* meth = s in
        let* host = s in
        let* path = s in
        let* status = gen_nat in
        return (Trace.Http_call { turn; meth; host; path; status }) );
      ( 1,
        let* turn = gen_nat in
        let* name = s in
        let* data = gen_canon in
        return (Trace.Agent_event { turn; name; data }) );
      ( 1,
        let* turn = gen_nat in
        let* content = s in
        return (Trace.Reply { turn; content }) );
      ( 1,
        let* kind = s in
        let* detail = s in
        return (Trace.Sandbox_violation { kind; detail }) );
      (1, return Trace.Done);
    ]

let gen_entry : Trace.entry QCheck.Gen.t =
  let open QCheck.Gen in
  let* seq = gen_nat in
  let* t_ms = gen_nat in
  let* event = gen_event in
  return { Trace.seq; t_ms; event }

let prop_round_trip =
  QCheck.Test.make ~name:"entries round-trip" ~count:300
    (QCheck.make ~print:Trace.to_line gen_entry) (fun e ->
      match Trace.of_line (Trace.to_line e) with
      | Ok e' -> Trace.equal_entry e e'
      | Error _ -> false)

let () =
  Alcotest.run "trace"
    [
      ( "unit",
        [
          Alcotest.test_case "round trip" `Quick test_round_trip;
          Alcotest.test_case "model_call wire" `Quick test_model_call_wire;
          Alcotest.test_case "usage wire names" `Quick test_usage_wire_names;
          Alcotest.test_case "other wire" `Quick test_other_wire;
          Alcotest.test_case "stable order" `Quick test_stable_order;
          Alcotest.test_case "rejections" `Quick test_rejections;
          Alcotest.test_case "error carries type" `Quick test_error_type_carried;
        ] );
      ("props", [ QCheck_alcotest.to_alcotest prop_round_trip ]);
    ]
