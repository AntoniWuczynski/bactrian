open Bactrian_core
module A = Assertion

let canon s =
  match Canon.of_string s with
  | Ok v -> v
  | Error e -> Alcotest.failf "bad test JSON %s: %a" s Canon.pp_error e

(* Trace construction *)

let entries events =
  List.mapi (fun i event -> { Trace.seq = i; t_ms = i * 100; event }) events

let call ?(turn = 1) name args =
  Trace.Tool_call
    { turn; index = 0; name; arguments = canon args; mcp = Joined }

let model ?(turn = 1) ?usage () =
  Trace.Model_call
    {
      turn;
      protocol = "openai_chat";
      model = "m";
      status = 200;
      usage;
      duration_ms = 10;
      ttft_ms = None;
    }

let usage i o =
  {
    Trace.input_tokens = i;
    output_tokens = o;
    cached_input_tokens = None;
    cache_write_tokens = None;
    reasoning_tokens = None;
  }

let reply turn content = Trace.Reply { turn; content }

let http host =
  Trace.Http_call { turn = 1; meth = "GET"; host; path = "/"; status = 200 }

let tools names = entries (List.map (fun n -> call n "{}") names)
let no_cost _ = None

(* Outcome checks *)

let outcome_pp ppf = function
  | A.Pass -> Format.pp_print_string ppf "Pass"
  | A.Fail r -> Format.fprintf ppf "Fail %S" r

let is_pass ?(cost = no_cost) ?(egress_intercepted = true) name a trace =
  match A.evaluate ~cost ~egress_intercepted a trace with
  | A.Pass -> ()
  | A.Fail r -> Alcotest.failf "%s: expected Pass, got Fail %S" name r

let fails ?(cost = no_cost) ?(egress_intercepted = true) ?(mentions = []) name a
    trace =
  match A.evaluate ~cost ~egress_intercepted a trace with
  | A.Pass -> Alcotest.failf "%s: expected Fail, got %a" name outcome_pp A.Pass
  | A.Fail r ->
      Alcotest.(check bool) (name ^ ": reason not empty") true (r <> "");
      List.iter
        (fun needle ->
          let found =
            try
              ignore (Re.exec (Re.compile (Re.str needle)) r);
              true
            with Not_found -> false
          in
          if not found then
            Alcotest.failf "%s: reason %S lacks %S" name r needle)
        mentions

(* tool_sequence *)

let test_sequence_exact () =
  let t = tools [ "a"; "b"; "a" ] in
  is_pass "exact equal"
    (A.Tool_sequence { mode = Exact; tools = [ "a"; "b"; "a" ] })
    t;
  fails "exact extra" ~mentions:[ "a, b, a" ]
    (A.Tool_sequence { mode = Exact; tools = [ "a"; "b" ] })
    t;
  fails "exact order"
    (A.Tool_sequence { mode = Exact; tools = [ "b"; "a"; "a" ] })
    t;
  is_pass "exact none"
    (A.Tool_sequence { mode = Exact; tools = [] })
    (entries [ Done ])

let test_sequence_in_order () =
  let t = tools [ "a"; "b"; "a"; "c" ] in
  let seq l = A.Tool_sequence { mode = In_order; tools = l } in
  is_pass "gaps" (seq [ "a"; "c" ]) t;
  is_pass "repeat" (seq [ "a"; "b"; "a" ]) t;
  is_pass "b then a" (seq [ "b"; "a"; "c" ]) t;
  fails "wrong order" ~mentions:[ "c" ] (seq [ "c"; "a" ]) t;
  fails "needs two b" (seq [ "b"; "b" ]) t;
  is_pass "empty" (seq []) t

let test_sequence_contains () =
  let t = tools [ "a"; "b"; "a" ] in
  let seq l = A.Tool_sequence { mode = Contains; tools = l } in
  is_pass "any order" (seq [ "b"; "a" ]) t;
  is_pass "twice" (seq [ "a"; "a" ]) t;
  fails "three times" ~mentions:[ "a" ] (seq [ "a"; "a"; "a" ]) t;
  fails "missing" ~mentions:[ "z" ] (seq [ "z" ]) t

(* tool_args *)

let args_trace =
  entries
    [
      call "cal"
        {|{"attendees":["amy@x","bob@x"],"when":"mon","opts":{"a":1,"b":2}}|};
      call "cal" {|{"attendees":["cy@x"],"when":"tue"}|};
      call "other" {|{"q":1}|};
    ]

let targs ?(tool = "cal") mode expected =
  A.Tool_args { tool; mode; expected = canon expected }

let test_args_exact () =
  is_pass "exact"
    (targs Exact_args {|{"attendees":["cy@x"],"when":"tue"}|})
    args_trace;
  fails "exact subset only" ~mentions:[ "cal" ]
    (targs Exact_args {|{"when":"tue"}|})
    args_trace;
  fails "no such tool" ~mentions:[ "nope"; "never called" ]
    (targs ~tool:"nope" Exact_args {|{}|})
    args_trace

let test_args_partial () =
  is_pass "subset of fields" (targs Partial {|{"when":"mon"}|}) args_trace;
  is_pass "list element" (targs Partial {|{"attendees":["bob@x"]}|}) args_trace;
  is_pass "nested" (targs Partial {|{"opts":{"b":2}}|}) args_trace;
  fails "wrong value" ~mentions:[ "when" ]
    (targs Partial {|{"when":"wed"}|})
    args_trace;
  fails "missing field" ~mentions:[ "zzz" ]
    (targs Partial {|{"zzz":1}|})
    args_trace;
  fails "list element absent" ~mentions:[ "attendees" ]
    (targs Partial {|{"attendees":["dan@x"]}|})
    args_trace;
  fails "nested mismatch" (targs Partial {|{"opts":{"b":3}}|}) args_trace

let schema_s =
  {|{"type":"object","required":["attendees","when"],
     "properties":{"attendees":{"type":"array","items":{"type":"string"}},
                   "when":{"type":"string","enum":["mon","tue"]}},
     "additionalProperties":false}|}

let test_args_schema () =
  is_pass "valid" (targs Schema schema_s) args_trace;
  let missing = entries [ call "cal" {|{"when":"mon"}|} ] in
  fails "missing required"
    ~mentions:[ "attendees"; "required" ]
    (targs Schema schema_s) missing;
  let wrong = entries [ call "cal" {|{"attendees":"amy","when":"mon"}|} ] in
  fails "wrong type" ~mentions:[ "attendees"; "array" ] (targs Schema schema_s)
    wrong;
  let extra = entries [ call "cal" {|{"attendees":[],"when":"mon","x":1}|} ] in
  fails "additional" ~mentions:[ "x" ] (targs Schema schema_s) extra;
  let bad_enum = entries [ call "cal" {|{"attendees":[],"when":"fri"}|} ] in
  fails "enum" ~mentions:[ "when" ] (targs Schema schema_s) bad_enum;
  let bad_item = entries [ call "cal" {|{"attendees":[1],"when":"mon"}|} ] in
  fails "items" ~mentions:[ "attendees[0]" ] (targs Schema schema_s) bad_item;
  fails "bad schema reported" ~mentions:[ "minLength" ]
    (targs Schema {|{"minLength":3}|})
    args_trace

(* Json_schema directly *)

let test_schema_types () =
  let ok schema inst =
    Json_schema.validate ~schema:(canon schema) (canon inst) = []
  in
  let t name b = Alcotest.(check bool) name true b in
  t "integer" (ok {|{"type":"integer"}|} "3");
  t "integer rejects float" (not (ok {|{"type":"integer"}|} "3.5"));
  t "number accepts int" (ok {|{"type":"number"}|} "3");
  t "number accepts float" (ok {|{"type":"number"}|} "3.5");
  t "number rejects string" (not (ok {|{"type":"number"}|} {|"3"|}));
  t "boolean" (ok {|{"type":"boolean"}|} "true");
  t "null" (ok {|{"type":"null"}|} "null");
  t "type list" (ok {|{"type":["string","null"]}|} "null");
  t "type list miss" (not (ok {|{"type":["string","null"]}|} "1"));
  t "enum" (ok {|{"enum":[1,"a",null]}|} {|"a"|});
  t "enum miss" (not (ok {|{"enum":[1,"a"]}|} "2"));
  t "additionalProperties true"
    (ok {|{"additionalProperties":true}|} {|{"z":1}|});
  t "nested items"
    (not
       (ok
          {|{"type":"array","items":{"type":"array","items":{"type":"integer"}}}|}
          "[[1],[\"x\"]]"));
  t "empty schema accepts anything" (ok "{}" {|[1]|})

let test_schema_check () =
  let paths s =
    List.map (fun e -> e.Config_error.path) (Json_schema.check (canon s))
  in
  Alcotest.(check (list string)) "ok" [] (paths schema_s);
  Alcotest.(check (list string))
    "unknown keyword" [ "minLength" ]
    (paths {|{"minLength":1}|});
  Alcotest.(check (list string))
    "nested path" [ "properties.a.type" ]
    (paths {|{"properties":{"a":{"type":"strng"}}}|});
  Alcotest.(check (list string))
    "required shape" [ "required" ]
    (paths {|{"required":"a"}|});
  Alcotest.(check (list string))
    "additionalProperties shape" [ "additionalProperties" ]
    (paths {|{"additionalProperties":{}}|});
  Alcotest.(check (list string)) "not an object" [ "" ] (paths "[]")

(* Json_path directly *)

let test_path () =
  let get p doc =
    match Json_path.parse p with
    | Error e -> Error e
    | Ok segs -> Json_path.eval segs (canon doc)
  in
  let doc = {|{"a":{"b":[10,{"c d":"x"}]},"k":null}|} in
  Alcotest.(check bool) "root" true (get "$" doc = Ok (canon doc));
  Alcotest.(check bool) "dot" true (get "$.a.b[0]" doc = Ok (canon "10"));
  Alcotest.(check bool)
    "quoted" true
    (get {|$.a.b[1]["c d"]|} doc = Ok (canon {|"x"|}));
  Alcotest.(check bool) "quoted top" true (get {|$["k"]|} doc = Ok Canon.null);
  Alcotest.(check bool) "missing key" true (Result.is_error (get "$.zz" doc));
  Alcotest.(check bool) "index oob" true (Result.is_error (get "$.a.b[5]" doc));
  Alcotest.(check bool) "key on list" true (Result.is_error (get "$.a.b.c" doc));
  List.iter
    (fun p ->
      Alcotest.(check bool)
        ("rejects " ^ p) true
        (Result.is_error (Json_path.parse p)))
    [
      "";
      "a";
      "$.";
      "$.a.";
      "$[x]";
      "$[1";
      {|$["a]|};
      "$..a";
      "$[*]";
      "$[-1]";
      "$a";
    ]

(* Simple limits *)

let test_max_steps () =
  let t = entries [ model (); model (); call "a" "{}"; model () ] in
  is_pass "at limit" (A.Max_steps 3) t;
  fails "over" ~mentions:[ "3"; "2" ] (A.Max_steps 2) t

let test_max_tokens () =
  let t =
    entries [ model ~usage:(usage 100 20) (); model ~usage:(usage 10 5) () ]
  in
  is_pass "at limit" (A.Max_tokens 135) t;
  fails "over" ~mentions:[ "135" ] (A.Max_tokens 134) t

(* A call without usage must not pass a token limit. *)
let test_max_tokens_usage_gap () =
  let t =
    entries
      [
        model ~usage:(usage 100 20) ();
        model ();
        model ();
        model ~usage:(usage 10 5) ();
      ]
  in
  fails "usage gap"
    ~mentions:[ "2 model calls have no usage" ]
    (A.Max_tokens 1_000_000) t;
  fails "singular"
    ~mentions:[ "1 model call has no usage" ]
    (A.Max_tokens 1_000_000)
    (entries [ model () ])

let test_max_cost () =
  let t = entries [ model () ] in
  is_pass "cheap" ~cost:(fun _ -> Some 0.01) (A.Max_cost_usd 0.05) t;
  is_pass "equal" ~cost:(fun _ -> Some 0.05) (A.Max_cost_usd 0.05) t;
  fails "dear"
    ~cost:(fun _ -> Some 0.06)
    ~mentions:[ "0.06"; "0.05" ] (A.Max_cost_usd 0.05) t;
  fails "unpriced" ~mentions:[ "price" ] (A.Max_cost_usd 0.05) t

let test_max_latency () =
  let t =
    List.map2
      (fun t_ms (e : Trace.entry) -> { e with t_ms })
      [ 50; 700; 1250 ]
      (entries [ model (); model (); Done ])
  in
  is_pass "ok" (A.Max_latency_ms 1200) t;
  fails "slow" ~mentions:[ "1200"; "1100" ] (A.Max_latency_ms 1100) t

let test_no_egress () =
  let t = entries [ http "ok.example"; http "Evil.example" ] in
  is_pass "clean" (A.No_egress [ "other.example" ]) t;
  fails "hit (case-insensitive)" ~mentions:[ "Evil.example" ]
    (A.No_egress [ "evil.example" ]) t;
  is_pass "empty trace" (A.No_egress [ "evil.example" ]) (entries [ Done ])

(* Nothing intercepts egress yet, so a clean trace proves nothing. *)
let test_no_egress_unverifiable () =
  fails "not intercepted" ~egress_intercepted:false
    ~mentions:[ "cannot be verified"; "not intercepted" ]
    (A.No_egress [ "evil.example" ]) (entries [ Done ])

(* List elements are matched multiset-wise. *)
let test_args_partial_multiset () =
  let two = {|{"attendees":["bob@x","bob@x"]}|} in
  let one = {|{"attendees":["bob@x"]}|} in
  let call_with a = entries [ call "cal" a ] in
  fails "expected twice, present once" (targs Partial two) (call_with one);
  is_pass "expected twice, present twice" (targs Partial two) (call_with two);
  is_pass "order is free"
    (targs Partial {|{"attendees":["b","a"]}|})
    (call_with {|{"attendees":["a","b"]}|})

(* output *)

let out_trace =
  entries
    [
      reply 1 "Booked for Wednesday";
      reply 2 {|{"status":{"ok":true},"items":[1,2,3],"n":2}|};
      reply 3 "all done";
    ]

let out ?(turn = `Last) check = A.Output { turn; check }

let test_output_regex () =
  is_pass "last" (out (Regex "done$")) out_trace;
  fails "last miss" ~mentions:[ "Wednesday" ] (out (Regex "Wednesday"))
    out_trace;
  is_pass "indexed" (out ~turn:(`Index 1) (Regex "Wednesday")) out_trace;
  is_pass "case-insensitive prefix"
    (out ~turn:(`Index 1) (Regex "(?i)wednesday"))
    out_trace;
  fails "case sensitive" (out ~turn:(`Index 1) (Regex "wednesday")) out_trace;
  fails "no such turn" ~mentions:[ "turn 9" ]
    (out ~turn:(`Index 9) (Regex "."))
    out_trace;
  fails "no replies" (out (Regex ".")) (entries [ Done ]);
  fails "bad regex" ~mentions:[ "regex" ] (out (Regex "(")) out_trace

let test_output_equals () =
  is_pass "equals" (out (Equals "all done")) out_trace;
  fails "not equal" ~mentions:[ "all done" ] (out (Equals "all")) out_trace

let test_output_jsonpath () =
  let jp ?equals path =
    out ~turn:(`Index 2) (Jsonpath { path; equals = Option.map canon equals })
  in
  is_pass "resolves" (jp "$.status.ok") out_trace;
  is_pass "equals" (jp ~equals:"true" "$.status.ok") out_trace;
  is_pass "index" (jp ~equals:"3" "$.items[2]") out_trace;
  fails "wrong value" ~mentions:[ "$.n"; "2" ] (jp ~equals:"5" "$.n") out_trace;
  fails "unresolved" ~mentions:[ "$.nope" ] (jp "$.nope") out_trace;
  fails "bad path" ~mentions:[ "path" ] (jp "n") out_trace;
  fails "non-JSON reply" ~mentions:[ "not JSON"; "turn 1" ]
    (out ~turn:(`Index 1) (Jsonpath { path = "$.a"; equals = None }))
    out_trace;
  fails "last reply non-JSON" ~mentions:[ "not JSON" ]
    (out (Jsonpath { path = "$"; equals = None }))
    out_trace

(* decode *)

let decode s = A.decode (canon s)

let decoded name s expected =
  match decode s with
  | Ok a -> Alcotest.(check bool) name true (a = expected)
  | Error es ->
      Alcotest.failf "%s: unexpected errors: %s" name
        (String.concat "; "
           (List.map (fun e -> e.Config_error.path ^ ": " ^ e.message) es))

let error_paths s =
  match decode s with
  | Ok _ -> Alcotest.failf "expected errors for %s" s
  | Error es -> List.map (fun e -> e.Config_error.path) es

let paths_t = Alcotest.(list string)

let test_decode_ok () =
  decoded "tool_sequence"
    {|{"tool_sequence":{"mode":"in_order","tools":["a","b"]}}|}
    (A.Tool_sequence { mode = In_order; tools = [ "a"; "b" ] });
  decoded "tool_args default partial"
    {|{"tool_args":{"tool":"cal","match":{"a":1}}}|}
    (A.Tool_args { tool = "cal"; mode = Partial; expected = canon {|{"a":1}|} });
  decoded "tool_args exact"
    {|{"tool_args":{"tool":"cal","mode":"exact","match":{}}}|}
    (A.Tool_args { tool = "cal"; mode = Exact_args; expected = canon "{}" });
  decoded "tool_args schema"
    {|{"tool_args":{"tool":"cal","mode":"schema","schema":{"type":"object"}}}|}
    (A.Tool_args
       { tool = "cal"; mode = Schema; expected = canon {|{"type":"object"}|} });
  decoded "max_steps" {|{"max_steps":12}|} (A.Max_steps 12);
  decoded "max_tokens" {|{"max_tokens":500}|} (A.Max_tokens 500);
  decoded "max_cost int" {|{"max_cost_usd":1}|} (A.Max_cost_usd 1.0);
  decoded "max_cost float" {|{"max_cost_usd":0.05}|} (A.Max_cost_usd 0.05);
  decoded "latency int" {|{"max_latency":250}|} (A.Max_latency_ms 250);
  decoded "latency bare float is ms" {|{"max_latency":2.5}|}
    (A.Max_latency_ms 3);
  decoded "latency ms" {|{"max_latency":"0.25s"}|} (A.Max_latency_ms 250);
  decoded "latency s" {|{"max_latency":"1.5s"}|} (A.Max_latency_ms 1500);
  decoded "latency m" {|{"max_latency":"2m"}|} (A.Max_latency_ms 120000);
  decoded "no_egress" {|{"no_egress":["a.example","b.example"]}|}
    (A.No_egress [ "a.example"; "b.example" ]);
  decoded "output default last regex" {|{"output":{"regex":"(?i)wednesday"}}|}
    (A.Output { turn = `Last; check = Regex "(?i)wednesday" });
  decoded "output last" {|{"output":{"turn":"last","regex":"x"}}|}
    (A.Output { turn = `Last; check = Regex "x" });
  decoded "output index equals" {|{"output":{"turn":2,"equals":"hi"}}|}
    (A.Output { turn = `Index 2; check = Equals "hi" });
  decoded "output jsonpath" {|{"output":{"jsonpath":"$.a"}}|}
    (A.Output { turn = `Last; check = Jsonpath { path = "$.a"; equals = None } });
  decoded "output jsonpath equals"
    {|{"output":{"jsonpath":"$.a","equals":[1]}}|}
    (A.Output
       {
         turn = `Last;
         check = Jsonpath { path = "$.a"; equals = Some (canon "[1]") };
       })

let test_decode_errors () =
  let c name expected s =
    Alcotest.check paths_t name expected (error_paths s)
  in
  c "not an object" [ "" ] {|"max_steps"|};
  c "two kinds" [ "" ] {|{"max_steps":1,"max_tokens":2}|};
  c "empty" [ "" ] "{}";
  c "unknown kind" [ "max_stepz" ] {|{"max_stepz":1}|};
  c "max_steps type" [ "max_steps" ] {|{"max_steps":"many"}|};
  c "max_steps negative" [ "max_steps" ] {|{"max_steps":-1}|};
  c "latency junk" [ "max_latency" ] {|{"max_latency":"soon"}|};
  c "no_egress item" [ "no_egress[1]" ] {|{"no_egress":["a",2]}|};
  c "tool_sequence collects all"
    [ "tool_sequence.mode"; "tool_sequence.tools[1]"; "tool_sequence.extra" ]
    {|{"tool_sequence":{"mode":"sorted","tools":["a",3],"extra":1}}|};
  c "tool_sequence missing"
    [ "tool_sequence.mode"; "tool_sequence.tools" ]
    {|{"tool_sequence":{}}|};
  c "tool_args missing tool and match"
    [ "tool_args.tool"; "tool_args.match" ]
    {|{"tool_args":{}}|};
  c "tool_args schema needs schema" [ "tool_args.schema" ]
    {|{"tool_args":{"tool":"t","mode":"schema"}}|};
  c "tool_args both" [ "tool_args.schema" ]
    {|{"tool_args":{"tool":"t","match":{},"schema":{}}}|};
  c "tool_args bad schema"
    [ "tool_args.schema.type" ]
    {|{"tool_args":{"tool":"t","mode":"schema","schema":{"type":"blob"}}}|};
  c "output none" [ "output" ] {|{"output":{}}|};
  c "output two" [ "output" ] {|{"output":{"regex":"a","equals":"b"}}|};
  c "output bad turn" [ "output.turn" ] {|{"output":{"turn":0,"regex":"a"}}|};
  c "output bad turn word" [ "output.turn" ]
    {|{"output":{"turn":"first","regex":"a"}}|};
  c "output bad regex" [ "output.regex" ] {|{"output":{"regex":"("}}|};
  c "output bad jsonpath" [ "output.jsonpath" ]
    {|{"output":{"jsonpath":"a.b"}}|};
  c "output equals non-string" [ "output.equals" ] {|{"output":{"equals":3}}|}

let () =
  Alcotest.run "assertion"
    [
      ( "tool_sequence",
        [
          Alcotest.test_case "exact" `Quick test_sequence_exact;
          Alcotest.test_case "in_order" `Quick test_sequence_in_order;
          Alcotest.test_case "contains" `Quick test_sequence_contains;
        ] );
      ( "tool_args",
        [
          Alcotest.test_case "exact" `Quick test_args_exact;
          Alcotest.test_case "partial" `Quick test_args_partial;
          Alcotest.test_case "schema" `Quick test_args_schema;
        ] );
      ( "json_schema",
        [
          Alcotest.test_case "types" `Quick test_schema_types;
          Alcotest.test_case "check" `Quick test_schema_check;
        ] );
      ("json_path", [ Alcotest.test_case "parse and eval" `Quick test_path ]);
      ( "limits",
        [
          Alcotest.test_case "max_steps" `Quick test_max_steps;
          Alcotest.test_case "max_tokens" `Quick test_max_tokens;
          Alcotest.test_case "max_tokens usage gap" `Quick
            test_max_tokens_usage_gap;
          Alcotest.test_case "max_cost_usd" `Quick test_max_cost;
          Alcotest.test_case "max_latency" `Quick test_max_latency;
          Alcotest.test_case "no_egress" `Quick test_no_egress;
          Alcotest.test_case "no_egress unverifiable" `Quick
            test_no_egress_unverifiable;
          Alcotest.test_case "partial multiset" `Quick
            test_args_partial_multiset;
        ] );
      ( "output",
        [
          Alcotest.test_case "regex" `Quick test_output_regex;
          Alcotest.test_case "equals" `Quick test_output_equals;
          Alcotest.test_case "jsonpath" `Quick test_output_jsonpath;
        ] );
      ( "decode",
        [
          Alcotest.test_case "valid" `Quick test_decode_ok;
          Alcotest.test_case "errors with paths" `Quick test_decode_errors;
        ] );
    ]
