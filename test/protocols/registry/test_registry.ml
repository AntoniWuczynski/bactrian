open Bactrian_core

let root = "../../fixtures/protocols"

let fixtures () =
  Sys.readdir root |> Array.to_list
  |> List.concat_map (fun proto ->
      Sys.readdir (Filename.concat root proto)
      |> Array.to_list
      |> List.map (fun case ->
          (proto, Filename.concat (Filename.concat root proto) case)))

let meta dir =
  let text =
    In_channel.with_open_text
      (Filename.concat dir "meta.json")
      In_channel.input_all
  in
  let v = Result.get_ok (Canon.of_string text) in
  ( Option.get (Canon.str_member "method" v),
    Option.get (Canon.str_member "path" v) )

let test_each_fixture_has_one_adapter () =
  let cases = fixtures () in
  Alcotest.(check bool) "fixtures found" true (List.length cases > 10);
  List.iter
    (fun (proto, dir) ->
      let meth, path = meta dir in
      let accepting =
        List.filter
          (fun (module P : Protocol.S) -> P.matches ~meth ~path)
          Protocols.all
        |> List.map (fun (module P : Protocol.S) -> P.name)
      in
      Alcotest.(check (list string))
        (dir ^ " is claimed by its own adapter only")
        [ proto ] accepting;
      match Protocols.find ~meth ~path with
      | Some (module P) -> Alcotest.(check string) (dir ^ " find") proto P.name
      | None -> Alcotest.fail (dir ^ ": no adapter"))
    cases

let test_unknown_path () =
  Alcotest.(check bool)
    "unknown path" true
    (Option.is_none (Protocols.find ~meth:"POST" ~path:"/v1/embeddings"))

let links steps = Protocol.link_history steps
let link_t = Alcotest.(list (option int))

let test_link_history () =
  let c id = Protocol.Called id and a id = Protocol.Answered id in
  Alcotest.check link_t "answers in any order"
    [ Some 0; Some 1; Some 1; Some 0 ]
    (links [ c "x"; c "y"; a "y"; a "x" ]);
  Alcotest.check link_t "equal ids link in order"
    [ Some 0; Some 1; Some 0; Some 1 ]
    (links [ c "x"; c "x"; a "x"; a "x" ]);
  Alcotest.check link_t "empty ids link in order"
    [ Some 0; Some 1; Some 0; Some 1 ]
    (links [ c ""; c ""; a ""; a "" ]);
  Alcotest.check link_t "id reused across turns"
    [ Some 0; Some 0; Some 1; Some 1 ]
    (links [ c "x"; a "x"; c "x"; a "x" ]);
  Alcotest.check link_t "result with nothing to answer is unlinked"
    [ None; Some 0; Some 0; None ]
    (links [ a "x"; c "x"; a "x"; a "x" ]);
  Alcotest.check link_t "result before its call is unlinked" [ None; Some 0 ]
    (links [ a "x"; c "x" ])

let test_remove_ignored () =
  let v = Result.get_ok (Canon.of_string {|{"a":1,"b":{"c":2,"d":3}}|}) in
  Alcotest.(check string)
    "removed" {|{"b":{"d":3}}|}
    (Canon.to_string (Protocol.remove_ignored [ ("a", ""); ("b.c", "") ] v))

let () =
  Alcotest.run "protocols"
    [
      ( "registry",
        [
          Alcotest.test_case "each fixture has one adapter" `Quick
            test_each_fixture_has_one_adapter;
          Alcotest.test_case "unknown path" `Quick test_unknown_path;
        ] );
      ( "protocol",
        [
          Alcotest.test_case "link_history" `Quick test_link_history;
          Alcotest.test_case "remove_ignored" `Quick test_remove_ignored;
        ] );
    ]
