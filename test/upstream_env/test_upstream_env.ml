open Bactrian_app
open Bactrian_runtime
module P = Model_proxy

let variable = "BACTRIAN_UPSTREAM_OPENAI"

let contains ~sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let accepted raw expected () =
  match Upstream_env.parse ~variable raw with
  | Ok uri -> Alcotest.(check string) raw expected (Uri.to_string uri)
  | Error m -> Alcotest.failf "%s was refused: %s" raw m

let refused raw reason () =
  match Upstream_env.parse ~variable raw with
  | Ok uri -> Alcotest.failf "%s was accepted as %s" raw (Uri.to_string uri)
  | Error m ->
      Alcotest.(check bool)
        "the message names the variable" true
        (String.starts_with ~prefix:variable m);
      Alcotest.(check bool)
        (Printf.sprintf "the message says %S: %s" reason m)
        true (contains ~sub:reason m);
      (* A URL with credentials must not be echoed into a log. *)
      Alcotest.(check bool)
        "the message does not repeat the value" false
        (String.length raw > 8 && contains ~sub:raw m)

let accepts =
  [
    ("a loopback port", "http://127.0.0.1:11434", "http://127.0.0.1:11434");
    ("a host name", "http://localhost:8000", "http://localhost:8000");
    ("no port", "http://llm.internal", "http://llm.internal");
    ("https", "https://gateway.example", "https://gateway.example");
    ("a path prefix", "http://host:4000/openai", "http://host:4000/openai");
    ("a trailing slash", "http://host:4000/v1/", "http://host:4000/v1/");
    ("upper case, host lowered", "HTTP://Host:80", "http://host:80");
    ("an IPv6 address", "http://[::1]:8080", "http://[::1]:8080");
    ("the highest port", "http://h:65535", "http://h:65535");
  ]

let refusals =
  [
    ("a relative URL", "/v1", "absolute http or https URL");
    ("a bare host and port", "localhost:11434", "absolute http or https URL");
    ("a host with no scheme", "127.0.0.1:11434", "absolute http or https URL");
    ("a scheme-relative URL", "//host/v1", "absolute http or https URL");
    ("ftp", "ftp://host", "absolute http or https URL");
    ("ws", "ws://host", "absolute http or https URL");
    ("no host", "http:///v1", "no host");
    ("an empty authority with a port", "http://:80", "no host");
    ("user and password", "http://user:secret@host", "user information");
    ("a user alone", "http://token@host:1/v1", "user information");
    ("an empty user", "http://@host", "user information");
    ("a query", "http://host/v1?key=1", "query or a fragment");
    ("an empty query", "http://host/?", "query or a fragment");
    ("a fragment", "http://host/v1#top", "query or a fragment");
    ("port 0", "http://host:0", "port");
    ("a port past 65535", "http://host:65536", "port");
    ("a port that is not a number", "http://host:http", "port");
    ("an empty port", "http://host:", "port");
    ("a space", "http://host/a b", "whitespace");
    ("a newline", "http://host\n", "whitespace");
    ("a host with a bad character", "http://ho$t", "invalid host");
    ("an unterminated IPv6 address", "http://[::1", "IPv6");
    ( "a name that begins with an address",
      "http://127.0.0.1.nip.io",
      "does not read back as itself" );
    ("a hyphen for a host", "http://-:80", "invalid host");
    ("a label ending in a hyphen", "http://a-.example", "invalid host");
    ("an empty label", "http://a..example", "invalid host");
  ]

(* record, replay and run warn before the real key goes out in clear text. *)
let test_cleartext () =
  let upstream url (_ : P.provider) = Uri.of_string url in
  let warnings caps url = Live_preflight.cleartext caps (upstream url) in
  Alcotest.(check int)
    "plain http to another machine" 1
    (List.length (warnings [ ("openai", 1.) ] "http://10.0.0.5:8000"));
  Alcotest.(check bool)
    "names the key variable" true
    (List.for_all
       (contains ~sub:"OPENAI_API_KEY")
       (warnings [ ("openai", 1.) ] "http://10.0.0.5:8000"));
  List.iter
    (fun (what, caps, url) ->
      Alcotest.(check (list string)) what [] (warnings caps url))
    [
      ("loopback", [ ("openai", 1.) ], "http://127.0.0.1:11434");
      ("https", [ ("openai", 1.) ], "https://gateway.example");
      ("no cap, no call", [ ("openai", 0.) ], "http://10.0.0.5");
    ]

let test_loopback () =
  List.iter
    (fun (url, expected) ->
      Alcotest.(check bool)
        url expected
        (Live_preflight.is_loopback (Uri.of_string url)))
    [
      ("http://localhost:1", true);
      ("http://127.0.0.1:1", true);
      ("http://127.9.8.7", true);
      ("http://[::1]:8080", true);
      ("http://[::ffff:127.0.0.1]:1", true);
      ("http://127.evil.example", false);
      ("http://localhost.evil.example", false);
      ("http://10.0.0.5", false);
      ("http://[::2]", false);
    ]

let test_variables () =
  Alcotest.(check (list string))
    "one per provider, from the provider table"
    [
      "BACTRIAN_UPSTREAM_OPENAI";
      "BACTRIAN_UPSTREAM_ANTHROPIC";
      "BACTRIAN_UPSTREAM_GEMINI";
      "BACTRIAN_UPSTREAM_OPENROUTER";
    ]
    (List.map snd Upstream_env.variables);
  Alcotest.(check int)
    "every provider has one"
    (List.length Run.providers)
    (List.length Upstream_env.variables)

let env pairs name = List.assoc_opt name pairs

let test_read () =
  let read pairs = Upstream_env.read (env pairs) in
  (match read [] with
  | Ok upstream ->
      List.iter
        (fun (p, _) ->
          Alcotest.(check string)
            "the default"
            (Uri.to_string (P.default_upstream p))
            (Uri.to_string (upstream p)))
        Run.providers
  | Error m -> Alcotest.fail m);
  (match
     read
       [
         (variable, "http://127.0.0.1:11434");
         ("BACTRIAN_UPSTREAM_GEMINI", "");
         ("BACTRIAN_UPSTREAM_OPENROUTER", "https://gw.example/or");
       ]
   with
  | Ok upstream ->
      Alcotest.(check string)
        "openai overridden" "http://127.0.0.1:11434"
        (Uri.to_string (upstream P.Openai));
      Alcotest.(check string)
        "openrouter overridden" "https://gw.example/or"
        (Uri.to_string (upstream P.Openrouter));
      Alcotest.(check string)
        "an empty variable is unset"
        (Uri.to_string (P.default_upstream P.Gemini))
        (Uri.to_string (upstream P.Gemini));
      Alcotest.(check string)
        "the others keep the default"
        (Uri.to_string (P.default_upstream P.Anthropic))
        (Uri.to_string (upstream P.Anthropic))
  | Error m -> Alcotest.fail m);
  match read [ ("BACTRIAN_UPSTREAM_ANTHROPIC", "http://u:p@host") ] with
  | Ok _ -> Alcotest.fail "credentials were accepted"
  | Error m ->
      Alcotest.(check bool)
        "the invalid variable is named" true
        (String.starts_with ~prefix:"BACTRIAN_UPSTREAM_ANTHROPIC " m);
      Alcotest.(check bool)
        "the password is not repeated" false (contains ~sub:"p@host" m)

let () =
  Alcotest.run "upstream_env"
    [
      ( "accepted",
        List.map
          (fun (name, raw, expected) ->
            Alcotest.test_case name `Quick (accepted raw expected))
          accepts );
      ( "refused",
        List.map
          (fun (name, raw, reason) ->
            Alcotest.test_case name `Quick (refused raw reason))
          refusals );
      ( "reading",
        [
          Alcotest.test_case "the variables" `Quick test_variables;
          Alcotest.test_case "read" `Quick test_read;
          Alcotest.test_case "this machine" `Quick test_loopback;
          Alcotest.test_case "a key in clear text" `Quick test_cleartext;
        ] );
    ]
