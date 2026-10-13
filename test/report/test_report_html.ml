open Bactrian_core
module R = Report_data

let html = Report_html.render (Fixture.data ())

let count ?(opts = []) re s =
  List.length (Re.all (Re.compile (Re.Perl.re ~opts re)) s)

let contains sub s = Re.execp (Re.compile (Re.str sub)) s

let test_self_contained () =
  Alcotest.(check int)
    "no src or href to anywhere" 0
    (count ~opts:[ `Caseless ] {|\b(src|href|action|poster|data)\s*=|} html);
  Alcotest.(check int)
    "no external reference in styles" 0
    (count ~opts:[ `Caseless ]
       {|url\(|@import|<link|<script|<iframe|<img|<object|<embed|<form|\son[a-z]+\s*=|}
       html);
  (* The only absolute URLs are XML namespace declarations. *)
  let without_namespaces =
    Re.replace_string
      (Re.compile (Re.Perl.re {|xmlns(:xlink)?="[^"]*"|}))
      ~by:"" html
  in
  Alcotest.(check int)
    "every http is a namespace" 0
    (count {|https?://|} without_namespaces)

let test_theme_and_phone () =
  Alcotest.(check bool)
    "dark scheme" true
    (contains "@media (prefers-color-scheme: dark)" html);
  Alcotest.(check bool) "custom properties" true (contains "--bg:" html);
  Alcotest.(check bool)
    "viewport" true
    (contains
       {|<meta name="viewport" content="width=device-width, initial-scale=1"/>|}
       html);
  Alcotest.(check bool)
    "doctype" true
    (String.starts_with ~prefix:"<!DOCTYPE html>" html);
  Alcotest.(check bool) "language" true (contains {|lang="en"|} html)

let test_accessible () =
  let tables = count {|<table|} html in
  Alcotest.(check bool) "tables" true (tables >= 6);
  Alcotest.(check int)
    "every table has a caption" tables (count {|<caption>|} html);
  Alcotest.(check int)
    "every header cell has a scope" (count {|<th[ >]|} html)
    (count {|<th scope="(col|row)"|} html);
  let svgs = count {|<svg|} html in
  Alcotest.(check bool) "charts" true (svgs >= 3);
  Alcotest.(check int)
    "every chart is an image" svgs
    (count {|<svg[^>]* role="img"|} html);
  Alcotest.(check int)
    "with a label" svgs
    (count {|<svg[^>]* aria-label="[^"]+"|} html);
  Alcotest.(check int) "a title" svgs (count {|<svg[^>]*>\s*<title>|} html);
  Alcotest.(check int)
    "and a description" svgs
    (count {|</title>\s*<desc>|} html);
  Alcotest.(check bool)
    "no colour-only meaning: verdicts are words" true
    (contains "Inconclusive, leans worse" html)

let test_content () =
  Alcotest.(check bool)
    "estimand" true
    (contains "Suite: performance on exactly these scenarios" html);
  Alcotest.(check bool)
    "gate family" true
    (contains "pass_rate (threshold 10.0 percentage points)" html);
  Alcotest.(check bool) "seed" true (contains "7, 500 bootstrap resamples" html);
  Alcotest.(check bool)
    "missing data" true
    (contains "Missing data per arm" html);
  Alcotest.(check bool)
    "new behaviour flagged" true
    (contains "refund: 1 new behaviour" html
    && contains {|<span class="badge new">new</span>|} html);
  Alcotest.(check bool)
    "assertion message" true
    (contains "assertions[0] tool_sequence: expected lookup_order then refund"
       html);
  Alcotest.(check bool)
    "divergence diff" true
    (contains "messages[2].content" html && contains "added tools[1]" html);
  Alcotest.(check int)
    "a trace in a details element per failure"
    (List.length (Fixture.data ()).failures - 1)
    (count {|<h3>Trace</h3>|} html);
  Alcotest.(check bool)
    "execution order" true
    (contains "Runs in the order they started" html);
  Alcotest.(check int)
    "one row per run" 60
    (count {|<td>\s*<code>run-\d+</code>|} html)

let test_escaping () =
  let d = Fixture.data () in
  let evil = "<script>alert(1)</script> & \"q\"" in
  let out = Report_html.render { d with experiment = evil; baseline = evil } in
  Alcotest.(check bool)
    "escaped" true
    (contains "&lt;script&gt;alert(1)&lt;/script&gt;" out);
  Alcotest.(check int) "no script element" 0 (count {|<script|} out)

let test_deterministic () =
  Alcotest.(check string)
    "same data, same bytes" html
    (Report_html.render (Fixture.data ()))

let test_no_data () =
  (* A comparison with no cost, no failures and no traces still renders. *)
  let d = Fixture.data () in
  let bare =
    {
      d with
      failures = [];
      summaries =
        List.map
          (fun (s : R.summary) -> { s with cost = None; latency = None })
          d.summaries;
      behaviours = [];
    }
  in
  let out = Report_html.render bare in
  Alcotest.(check bool)
    "says so" true
    (contains "No run of either variant failed." out);
  Alcotest.(check bool) "no cost" true (contains "No cost was recorded." out)

let () =
  Alcotest.run "report_html"
    [
      ( "html",
        [
          Alcotest.test_case "self-contained" `Quick test_self_contained;
          Alcotest.test_case "theme and phone width" `Quick test_theme_and_phone;
          Alcotest.test_case "accessible" `Quick test_accessible;
          Alcotest.test_case "content" `Quick test_content;
          Alcotest.test_case "escaping" `Quick test_escaping;
          Alcotest.test_case "deterministic" `Quick test_deterministic;
          Alcotest.test_case "sparse data" `Quick test_no_data;
        ] );
    ]
